library(torch)
library(luz)
library(purrr)
library(dplyr)
library(tidyr)
library(cli)
library(arrow)
library(rhdf5)
library(itertools)
library(furrr)
library(future)
plan("multicore", workers = 6, gc = TRUE)

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


epochs = 100
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

RDSAroTokenData = dataset(
  name = "RDSAroTokenData",
  initialize = function(
    x,
    y,
    sample_id,
    data_dir,
    ndim = max(max(unlist(y)), max(unlist(x))),
    device = default_device,
    .progress = interactive()) {

    if (!missing(x)) {
      self$ndim = ndim
      self$device = device
      self$data_dir = data_dir
      if (!missing(sample_id)) {
        if (length(sample_id) != length(x)) {
          stop("`sample_id` should be the same length as `length(x)`.")
        }
      }
      if (!(length(y) == length(x))) {
        stop("`x` and `y` should have the same length.")
      }
      numchar = length(x) |> as.character() |> nchar()
      fmt = paste0("%0", numchar, "d")
      dw = tibble(
        x = x,
        y = y,
        sample_id = sample_id
      )
      dw$x_path = sprintf(
        file.path(data_dir, paste0("x-", fmt, ".rds")),
        seq_len(nrow(dw))
      )
      dw$y_path = sprintf(
        file.path(data_dir, paste0("y-", fmt, ".rds")),
        seq_len(nrow(dw))
      )
      dir.create(data_dir)
      if (.progress) {
        cli_alert_info("Writing x.")
      }
      future_walk(
        seq_len(nrow(dw)), 
        ~ encode_seq(dw$x[[.x]], ndim = ndim) |>
          as.matrix() |>
          saveRDS(dw$x_path[.x]),
        .options = furrr_options(seed = TRUE),
        .progress = .progress
      )
      if (.progress) {
        cli_alert_info("Writing y.")
      }
      future_walk(
        seq_len(nrow(dw)), 
        ~ encode_seq(dw$y[[.x]], ndim = ndim) |>
          as.matrix() |>
          saveRDS(dw$y_path[.x]),
        .options = furrr_options(seed = TRUE),
        .progress = .progress
      )
      self$info = dw
      saveRDS(dw, file.path(data_dir, "info.rds"))
    } else {
      self$data_dir = data_dir
      self$info = readRDS(file.path(data_dir, "info.rds"))
    }
  },
  get_sample_id = function() {
    self$info$sample_id
  },
  get_ndim = function() {
    readRDS(self$info$x_path[[1]]) |> ncol()
  },
  .getbatch = function(i) {
    get_x = \(i) readRDS(self$info$x_path[i])
    get_y = \(i) readRDS(self$info$y_path[i])
    if (length(i) == 1) {
      list(
        x = torch_tensor(get_x(i), device = self$device),
        y = torch_tensor(get_y(i), device = self$device)
      )
    } else {
      list(
        x = map(i, ~ torch_tensor(get_x(.x), device = self$device)) |>
            torch_stack(dim = 1),
        y = map(i, ~ torch_tensor(get_y(.x), device = self$device)) |>
          torch_cat(dim = 1)
      )
    }
  },
  .length = function() {
    nrow(self$info)
  }
)

H5AroTokenData = dataset(
  name = "H5AroTokenData",
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
  get_ndim = function() {
    self$ndim
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

ArrowAroTokenData = dataset(
  name = "ArrowAroTokenData",
  initialize = function(
    x,
    y,
    sample_id,
    feather_dir,
    ndim = max(max(unlist(y)), max(unlist(x))),
    num_file_chunks = 100,
    device = default_device,
    .progress = interactive()) {

    if (!missing(x)) {
      self$nr = NULL
      self$device = device
      self$feather_dir = feather_dir

      if (!missing(sample_id)) {
        if (length(sample_id) != length(x)) {
          stop("`sample_id` should be the same length as `length(x)`.")
        }
      }
      if (!(length(y) == length(x))) {
        stop("`x` and `y` should have the same length.")
      }
      dw = tibble(
        x = x,
        y = y,
        sample_id = sample_id
      )
      dw$chunk = cut(
        seq_len(nrow(dw)), 
        num_file_chunks, 
        labels = FALSE, 
        right = FALSE
      )
      dw$rn = seq_len(nrow(dw))

      if (.progress) {
        cli_alert_info("Writing data")
      }
      dir.create(file.path(feather_dir))
      numchar = nchar(as.character(nrow(dw)))
      fmt = paste0("%0", numchar, "d.feather")
      future_walk(
        unique(dw$chunk),
        ~ {
          dws = dplyr::filter(dw, chunk == .x)
          dws$xt = map(dws$x, ~ as.matrix(encode_seq(.x, ndim = ndim)))
          dws$yt = map(dws$y, ~ as.matrix(encode_seq(.x, ndim = ndim)))
          dws$nr = map_dbl(dws$xt, ~ dim(.x)[1])
          dws$nc = map_dbl(dws$xt, ~ dim(.x)[2])
          write_feather(
            select(dws, -chunk, -x, -y),
            file.path(feather_dir, sprintf(fmt, .x))
          )
          rm(dws)
        },
        .options = furrr_options(seed = TRUE),
        .progress = .progress
      )
      self$nr = nrow(dw)
    } else {
      if (missing(feather_dir)) {
        stop("You must either specify a directory with feather files or data.")
      }
      self$feather_dir = feather_dir
    }
#    self$feather = map(
#      dir(self$feather_dir, full.names = TRUE),
#      read_feather
#    )
    self$feather = open_dataset(feather_dir, format = "feather", 
      unify_schemas = TRUE)
  },
  get_sample_id = function() {
    r = self$feather[,"sample_id"] |>
      collect()
    r$sample_id
  },
  get_ndim = function() {
    r = self$feather[1, "nc"] |> collect()
    r$nc
  },
  .getbatch = function(i) {
    ds = self$feather[i,] |> collect()
    get_x = \(ds) {
      matrix(ds$xt[[1]], nrow = ds$nr[1], ncol = ds$nc[1]) |>
        torch_tensor(device = self$device)
    }
    get_y = \(ds) {
      matrix(ds$yt[[1]], ncol = ds$nc[1]) |>
        torch_tensor(device = self$device)
    }
    if (length(i) == 1) {
      list(x = get_x(ds), y = get_y(ds))
    } else {
      list(
        x = map(
          seq_len(nrow(ds)),
          ~ get_x(ds[.x,])
          ) |>
          torch_stack(dim = 1),
        y = map(
          seq_len(nrow(ds)),
          ~ get_y(ds[.x,])
          ) |>
          torch_cat(dim = 1)
      )
    }
  },
  .length = function() {
    if (is.null(self$nr)) {
      r = self$feather |> 
        summarize(n = n()) |>
        collect()
      self$nr = r$n
    }
    self$nr
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

if (!dir.exists("data_dir")) {
  ds = RDSAroTokenData(x = x$x, y = x$y, sample_id = x$ind, 
    data_dir = "data_dir")
} else {
  ds = RDSAroTokenData(data_dir = "data_dir")
}

length(ds)
ds[1]
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
      TRUE ~ "train"
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
      out_seq_len = ds$get_ndim(),
      ndim = ds$get_ndim()
    ) |>
  #  set_opt_hparams(lr = 1e-4) |>
    fit(
      dataloader(
        AroTokenSampleSubsetData(
          ds, 
          sample_alloc$sample_id[sample_alloc$type == "train"]
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
        batch_size = 128,
        shuffle = TRUE,
        num_workers = 0,
      ),
      callbacks = list(
        luz_callback_keep_best_model()
      )
    )

  luz_save(model, model_fn)
} else {
  model = luz_load(model_fn)
}

holdout = sample_alloc |> filter(type == "holdout")
preds = predict(
  model,
  dataloader(
    RDSAroTokenData(
      data_dir = "data_dir"
    ),
    batch_size = 128
  )
)

decode_row = function(xr) {
  apply(xr, 1, which.max) 
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

x$pred = decode_tensor(preds)
x = left_join(x |> rename(sample_id = ind), sample_alloc, by = "sample_id")
