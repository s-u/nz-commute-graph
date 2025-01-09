library(torch)
library(luz)
library(purrr)
library(dplyr)
library(tidyr)

train_model = TRUE
epochs = 1000
if (!exists("x")) {
  x = readRDS("data/stm-data.rds")
}

SMTData = dataset(
  name = "SMTData",
  initialize = function(x, num_tokens = max(unlist(x$y)) + 1, device = "cpu") {
    self$d = x
    self$num_tokens = num_tokens
    self$seq_len = length(x$y[[1]])
    self$device = device
  },
  .getitem = function(i) {
    xm = matrix(0, nrow = self$seq_len, ncol = self$num_tokens)
    # Encode the sequence.
    for (j in seq_len(self$seq_len)) {
      xm[j, self$d$x[[i]][j] + 1 ] = 1
    }
    ym = matrix(0, nrow = self$seq_len, ncol = self$num_tokens)
    for (j in seq_len(self$seq_len)) {
      ym[j, self$d$y[[i]][j] + 1] = 1
    }
    list(
      x = torch_tensor(xm, device = self$device),
      y = torch_tensor(ym, device = self$device)
    )
  },
  .length = function() {
    nrow(self$d)
  }
)

SMTModel = nn_module(
  "SMTModel",
  initialize = function(seq_len, num_tokens, sigma = 600) {
    self$seq_len = seq_len
    self$num_tokens = num_tokens

    
    if (1) {
      # Glorot (Xavier) Initialization
      #xg = sqrt(sigma / (self$seq_len * self$num_tokens))
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
      self$Wq = torch_tensor(wq, requires_grad = TRUE) |>
        nn_parameter()
      self$Wk = torch_tensor(wk, requires_grad = TRUE) |>
        nn_parameter()
      self$Wv = torch_tensor(wv, requires_grad = TRUE) |>
        nn_parameter()
      self$W0 = torch_tensor(w0, requires_grad = TRUE) |>
        nn_parameter()
      self$b0 = torch_tensor(b0, requires_grad = TRUE) |>
        nn_parameter()
    } else {
      self$Wq = nn_parameter(
        torch_randn(self$num_tokens, self$seq_len, requires_grad = TRUE) 
      )
      self$Wk = nn_parameter(
        torch_randn(self$num_tokens, self$seq_len, requires_grad = TRUE) 
      )
      self$Wv = nn_parameter(
        torch_randn(self$num_tokens, self$seq_len, requires_grad = TRUE) 
      )
      self$W0 = nn_parameter(
        torch_randn(self$seq_len, self$num_tokens, requires_grad = TRUE) 
      )
      self$b0 = nn_parameter(
        torch_randn(1, self$num_tokens, requires_grad = TRUE)
      )
    }
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

set.seed(1)
xcv = x |> 
  group_by(ind) |>
  group_nest() |>
  mutate(cv = sample(1:10, n(), replace = TRUE)) |>
  unnest(data)

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
        SMTData(xcv |> filter(!(cv %in% 1:2))),
        batch_size = 128,
        shuffle = TRUE,
        num_workers = 0,
      ),
      epochs = epochs,
      valid_data = dataloader(
        SMTData(xcv |> filter(cv == 1)),
        batch_size = 32,
        shuffle = TRUE,
        num_workers = 0,
      ),
      callbacks = list(
        luz_callback_keep_best_model()
      )
    )

  luz_save(model, "morning-seq.luz")
} else {
  model = luz_load("morning-seq.luz")
}

holdouts = xcv |> filter(cv == 2) |> head(10000)
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
          torch_tensor(xp[.x,,], device = "cpu") |>
            as.matrix() |>
            decode_row()
        }
      ) 
    )
  } else if (length(xp$shape == 2)) {
    return(
      torch_tensor(xp, device = "cpu") |>
        as.matrix() |>
        decode_row()
    )
  }
  stop("Unsupported tensor length.")
}

holdouts$pred= decode_tensor(preds)
