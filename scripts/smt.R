library(torch)
library(luz)
library(purrr)
library(dplyr)
library(tidyr)

model_fn = if(nzchar(.<-Sys.getenv("MODEL"))) . else "model.luz"

train_model = TRUE
epochs = 1000
## you could try "mps" on arm64 macOS
default_device <- if (cuda_is_available()) torch_device("cuda:0") else "cpu"

if (!exists("x")) {
  x = readRDS("data/stm-data.rds")
  ## change to a more efficient structure:
  ## x and y are integer matrices
  x = unclass(x)
  attr(x,"row.names")=NULL
  ## for some reason torch doesn't like 0 in index vectors
  x$x = t(sapply(x$x, as.integer)) + 1L
  x$y = t(sapply(x$y, as.integer)) + 1L
  x$n = nrow(x$x)

  ## padding variant?
  if (nzchar(Sys.getenv("PAD"))) {
     cat("Using padding instead of 0\n")
     .pad <- function(x) { i=match(1L, x); if (!is.na(i)) x[i:length(x)]=x[i-1]; x }
     x$x = t(apply(x$x, 1, .pad))
     x$y = t(apply(x$y, 1, .pad))
  }
  set.seed(1)
  ## assign each index a cv group
  icv = sample(1:10, max(x$ind), replace=TRUE)
  x$cv = icv[x$ind]
}

## filter dataset by cv group(s)
cv.filter <- function(d, cv, inv=FALSE) {
  ix = d$cv %in% cv
  if (inv) ix = !ix
  ind = d$ind[ix]
  list(ind=ind, x=d$x[ix,], y=d$y[ix,], n=length(ind), cv=x$cv[ix])
}

SMTData = dataset(
  name = "SMTData",
  initialize = function(x, num_tokens = max(x$y), device = default_device) {
    self$d = x
    self$num_tokens = num_tokens
    self$seq_len = ncol(x$y)
    self$device = device
  },
  .getbatch = function(i) {
    list(
      x = torch_tensor(self$d$x[i,], device=self$device) |>
          nnf_one_hot(num_classes=self$num_tokens) |> torch_tensor(torch_float()),
      y = torch_tensor(self$d$y[i,], device=self$device) |>
          nnf_one_hot(num_classes=self$num_tokens) |> torch_tensor(torch_float())
    )
  },
  .length = function() {
    self$d$n
  }
)

SMTModel = nn_module(
  "SMTModel",
  initialize = function(seq_len, num_tokens) {
    self$seq_len = seq_len
    self$num_tokens = num_tokens

    # Glorot (Xavier) Initialization
    xg = 1
    wq = matrix(
      runif(self$seq_len * self$num_tokens, min = -xg, max = xg),
      nrow = self$num_tokens,
      ncol = self$seq_len
    )
    wk = matrix(
      runif(self$seq_len * self$num_tokens, min = -xg, max = xg),
      nrow = self$num_tokens,
      ncol = self$seq_len
    )
    wv = matrix(
      runif(self$seq_len * self$num_tokens, min = -xg, max = xg),
      nrow = self$num_tokens,
      ncol = self$seq_len
    )
    w0 = matrix(
      runif(self$seq_len * self$num_tokens, min = -xg, max = xg),
      nrow = self$seq_len,
      ncol = self$num_tokens
    )
    b0 = matrix(
      runif(self$num_tokens, min = -xg, max = xg),
      nrow = 1,
      ncol = self$num_tokens
    )
    self$Wq = torch_tensor(wq, requires_grad = TRUE, device = default_device) |>
      nn_parameter()
    self$Wk = torch_tensor(wk, requires_grad = TRUE, device = default_device) |>
      nn_parameter()
    self$Wv = torch_tensor(wv, requires_grad = TRUE, device = default_device) |>
      nn_parameter()
    self$W0 = torch_tensor(w0, requires_grad = TRUE, device = default_device) |>
      nn_parameter()
    self$b0 = torch_tensor(b0, requires_grad = TRUE, device = default_device) |>
      nn_parameter()
  },
  forward = function(x) {
    forward_single = function(x) {
      # Calculate attention.
      Q = torch_matmul(x, self$Wq)
      K = torch_matmul(x, self$Wk)
      V = torch_matmul(x, self$Wv)
      if (length(K$shape) > 2) browser()
      QKt = torch_matmul(Q, K$t())
      QKt / sqrt(Q$shape[2])
      # No temperature for now.
      alpha = nnf_softmax(QKt, 1)
      attention = torch_matmul(alpha, V)

      # softmax(Attention * W0 + b0)
      r = nnf_softmax(torch_matmul(attention, self$W0) + self$b0, 1)
#      torch_argmax(r, 2)$to(dtype = torch_float())
    }
    # Is it batched?
    if (length(x$shape) == 2) {
      return(forward_single(x))
    } else if (length(x$shape == 3)) {
      r = map(
        seq_len(x$shape[1]),
        ~ forward_single(x[.x,,])
      )
      torch_stack(r, 1)
    } else {
      stop("Invalid input shape.")
    }
  }
)

smt_data = SMTData(x)
a = smt_data[1]

model = SMTModel(smt_data$seq_len, smt_data$num_tokens)
model(a$x)

my_loss = function(input, target) {
  single_loss = \(inp, targ) {
    eps = 1e-8
    torch_mean(torch_sum(-targ * log(inp + eps), dim = 2))
  }

  if (length(input$shape) == 2) {
    return(single_loss(input, target))
  } else if (length(input$shape == 3)) {
    r = map(
      seq_len(input$shape[1]),
      ~ single_loss(input[.x,,], target[.x,,])
    )
    return(mean(torch_stack(r)))
  } 
  stop("Invalid input for `my_loss`.")
}

if (train_model) {
  # cv 1 is cv.
  # cv 2 is holdout.
  # others are training.
  model = SMTModel |>
    setup(
      loss = my_loss,
      optimizer = optim_adam
    ) |>
    set_hparams(
      seq_len = smt_data$seq_len, 
      num_tokens = smt_data$num_tokens
    ) |>
  #  set_opt_hparams(lr = 1e-4) |>
    fit(
      dataloader(
        SMTData(x |> cv.filter(1:2, inv=TRUE)),
        batch_size = 128,
        shuffle = TRUE,
        num_workers = 0,
      ),
      epochs = epochs,
      valid_data = dataloader(
        SMTData(x |> cv.filter(1)),
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
  model = luz_load(model_fn)
}

holdouts = x |> cv.filter(2) |> head(10000)
preds = predict(
  model, 
  dataloader(
    SMTData(
      holdouts,
      num_tokens = model$model$num_tokens,
    ),
    batch_size = 128
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

holdouts$pred= decode_tensor(preds)
