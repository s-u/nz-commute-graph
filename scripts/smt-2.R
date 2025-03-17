library(torch)
library(luz)
library(purrr)
library(dplyr)
library(tidyr)
library(rhdf5)
library(cli)

source("scripts/smt-models.R")

model_fn = if(nzchar(.<-Sys.getenv("MODEL"))) . else "model.luz"

train_model = TRUE

model_fn = if(nzchar(.<-Sys.getenv("MODEL"))) . else "model.luz"

default_device <- if (cuda_is_available()) {
    torch_device("cuda:0") 
  } else if (backends_mps_is_available()) {
    torch_device("mps") 
  } else {
    "cpu"
  }


train_model = TRUE
epochs = 50
## you could try "mps" on arm64 macOS
default_device <- if (cuda_is_available()) {
    torch_device("cuda:0") 
  } else if (backends_mps_is_available()) {
    torch_device("mps") 
  } else {
    torch_device("cpu")
  }

if (!exists("x")) {
  x = readRDS("artifacts/smt-data.rds")
}

encode_seq = function(x, ndim, device = "cpu") {
  torch_tensor(x + 1, dtype = torch_long(), device = device) |>
    nnf_one_hot(num_classes = ndim + 1) |>
    (\(x) x[,seq_len(ncol(x))[-1], drop = TRUE])() |>
    torch_tensor(dtype = torch_float(), device = device) 
}

AroTokenData = dataset(
  name = "AroTokenData",
  initialize = function(
    x,
    y,
    sample_id,
    h5_file,
    ndim = max(max(unlist(y)), max(unlist(x))),
    device = default_device,
    .progress = interactive()) {

    if (!missing(x)) {
      self$ndim = ndim
      self$device = device
      self$h5_file = h5_file
      h5createFile(self$h5_file)
      h5write(self$ndim, self$h5_file,"ndim")
      h5write(length(x), self$h5_file,"length")
      if (!missing(sample_id)) {
        if (length(sample_id) != length(x)) {
          stop("`sample_id` should be the same length as `length(x)`.")
        }
        h5write(sample_id, self$h5_file, "sample_id") 
      }
      if (!(length(y) == length(x))) {
        stop("`x` and `y` should have the same length.")
      }
      h5createGroup(self$h5_file, "x")
      h5createGroup(self$h5_file, "y")
      self$numchar = length(x) |> as.character() |> nchar()
      h5write(self$numchar, self$h5_file, "numchar") 
      self$fmt = paste0("%0", self$numchar, "d")
      if (.progress) {
        cli_alert_info("Writing x to hdf5.")
      }
      walk(
        seq_along(x),
        ~ encode_seq(x[[.x]], ndim = self$ndim) |> 
            as.matrix() |>
            h5write(self$h5_file, self$make_array_key(.x, "x")),
        .progress = .progress
      )
      if (.progress) {
        cli_alert_info("Writing y to hdf5.")
      }
      walk(
        seq_along(y),
        ~ encode_seq(y[[.x]], ndim = self$ndim) |> 
            as.matrix() |>
            h5write(self$h5_file, self$make_array_key(.x, "y")),
        .progress = .progress
      )
    } else {
      self$h5_file = h5_file
      self$numchar = h5read(self$h5_file, "numchar")
      self$ndim = h5read(self$h5_file, "ndim")
      self$fmt = paste0("%0", self$numchar, "d")
    }
  },
  get_sample_id = function() {
    h5read(self$h5_file, "sample_id")
  },
  make_array_key = function(i, var = "x") {
    sprintf(paste0(var, "/", self$fmt), i)
  },
  .getbatch = function(i) {
    if (length(i) == 1) {
      list(
        x = torch_tensor(
          h5read(self$h5_file, self$make_array_key(i, "x")),
          device = self$device
        ), 
        y = torch_tensor(
          h5read(self$h5_file, self$make_array_key(i, "y")),
          device = self$device
        )
      )
    } else {
      xs = map(i, ~ h5read(self$h5_file, self$make_array_key(.x, "x")))
      ys = map(i, ~ h5read(self$h5_file, self$make_array_key(.x, "y")))
      list(
        x = map(xs, ~ torch_tensor(.x, device = self$device)) |>
          torch_stack(dim = 1),
        y = map(ys, ~ torch_tensor(.x, device = self$device)) |>
          torch_cat(dim = 1)
      )
    }
  },
  .length = function() {
    h5read(self$h5_file, "length")
  }
)

AroTokenSampleSubsetData = dataset(
  name = "AroTokenSampleSubsetData",
  #' atd An AroTokenData object
  #' inds the inds to use.
  initialize = function(atd, sample_id) {
    if (!(all(unique(sample_id) %in% unique(atd$get_sample_id())))) {
      stop("`sample_id`s not found in atd.")
    }
    self$atd = atd
    self$sample_id = sample_id
    self$lookup = which(self$atd$get_sample_id() %in% sample_id)
  },
  get_sample_id = function() {
    as.vector(self$sample_id)
  },
  .getbatch = function(i) {
    self$atd[self$lookup[i]]
  },
  .length = function() {
    length(self$lookup)
  }
)

my_loss = function(input, target) {
  single_loss = \(inp, targ) {
    eps = 1e-8
    torch_mean(torch_sum(-targ * log(inp + eps), dim = 2))
  }
  eps = 1e-8
  return(torch_mean(torch_sum(-target * log(input + eps), dim = 2)))
}

if (!file.exists("x.h5")) {
  ds = AroTokenData(x = x$x, y = x$y, sample_id = x$ind, h5_file = "x.h5")
} else {
  ds = AroTokenData(h5_file = "x.h5")
}

#sds = AroTokenSampleSubsetData(ds, 2)

set.seed(1)
sample_alloc = tibble(
    sample_id = as.vector(sample(unique(ds$get_sample_id())))
  ) |>
  mutate(rn = row_number()) |>
  mutate(type = 
    case_when(
      rn %% 10 == 0 ~ "holdout",
      rn %% 10 == 9 ~ "valid",
      .default = "train"
    )
  ) |> 
  select(-rn)

ads = AroTokenSampleSubsetData(
  ds, 
  sample_alloc$sample_id[sample_alloc$type == "valid"]
)

if (train_model) {
  model = AroModel |>
    setup(
      loss = my_loss,
      optimizer = optim_adam
    ) |>
    set_hparams(
      in_seq_len = 20, #smt_data$seq_len, 
      out_seq_len = ds$ndim,
      ndim = ds$ndim
    ) |>
  #  set_opt_hparams(lr = 1e-4) |>
    fit(
      dataloader(
        AroTokenSampleSubsetData(
          ds, 
          sample_alloc$sample_id[sample_alloc$type == "valid"]
        ),
        batch_size = 128,
        shuffle = TRUE,
        num_workers = 0,
      ),
      epochs = epochs,
      valid_data = dataloader(
        AroTokenSampleSubsetData(
          ds, 
          sample_alloc$sample_id[sample_alloc$type == "valid"]
        ),
        batch_size = 32,
        shuffle = TRUE,
        num_workers = 0,
      ),
      callbacks = list(
        luz_callback_keep_best_model()
      )
    )

  luz_save(model, model_fn)
} else {
  luz_load(model_fn)
}

preds = predict(
  model,
  dataloader(
    AroTokenSampleSubsetData(
      ds,
      sample_alloc$sample_id[sample_alloc$type == "holdout"]
    ),
    batch_size = 256
  )
)

decode_row = function(xr) {
  apply(xr, 1, which.max) - 1
}

decode_tensor = function(xp) {
  if (length(xp$shape) == 3) {
    return(
      map(
        seq_len(xp$shape[1]),
        ~ {
            xp[.x,,] |> as.matrix() |>
            decode_row()
        }
      ) 
    )
  } else if (length(xp$shape == 2)) {
    return(
        xp |>
        as.matrix() |>
        decode_row()
    )
  }
  stop("Unsupported tensor length.")
}

sa = sample_alloc[sample_alloc$type == "holdout",]
sa$pred = decode_tensor(preds)
