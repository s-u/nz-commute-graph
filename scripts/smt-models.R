library(torch)
library(purrr)

SMTModel = nn_module(
  "SMTModel",
  initialize = function(seq_len, num_tokens) {
    self$seq_len = seq_len
    self$num_tokens = num_tokens - 1

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
      Q = torch_matmul(x[self$seq_len,,drop = FALSE], self$Wq)
      K = torch_matmul(x, self$Wk)
      V = torch_matmul(x, self$Wv)
      if (length(K$shape) > 2) browser()
      QKt = torch_matmul(Q, K$t())
      QKt / sqrt(Q$shape[2])
      # No temperature for now.
      alpha = nnf_softmax(QKt, 2)
      attention = torch_matmul(alpha, V)

      # softmax(Attention * W0 + b0)
      nnf_softmax(torch_matmul(attention, self$W0) + self$b0, 2)
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
      torch_cat(r, 1)
    } else {
      stop("Invalid input shape.")
    }
  }
)

#' AroModel: Attention-Based Neural Network Module
#'
#' The `AroModel` is an attention-based neural network module implemented using `torch` in R. It performs attention mechanisms on input sequences to produce output sequences.
#'
#' @section Initialization Parameters:
#' - `in_seq_len` (integer): Length of the input sequence.
#' - `ndim` (integer): Dimensionality of the input features.
#' - `out_seq_len` (integer, default = 1): Length of the output sequence.
#' - `fan_avg` (numeric, default = `in_seq_len + ndim`): Average fan-in 
#    and fan-out used for weight initialization.
#' - `init_sampler` (character, default = "uniform"): Method for weight 
#'   initialization. Options are "uniform" or "gaussian".
#'
#' @section Methods:
#' - `initialize(in_seq_len, ndim, out_seq_len = 1, 
#'               fan_avg = in_seq_len + ndim, 
#'               init_sampler = c("uniform", "gaussian"))`: 
#'   Constructor for the 
#'   `AroModel` class.
#' - `forward(x)`: Forward pass method. Processes the input tensor `x` 
#'   through the attention mechanism.
#'
#' @examples
#' \dontrun{
#' library(torch)
#' # Initialize the model
#' model <- AroModel(in_seq_len = 10, ndim = 16)
#' # Create a random input tensor
#' input_tensor <- torch_randn(10, 16)
#' # Perform a forward pass
#' output <- model$forward(input_tensor)
#' }
#' @export
AroModel = nn_module(
  "AroModel",
  initialize = function(
    in_seq_len, 
    ndim,
    out_seq_len = 1, 
    fan_avg = in_seq_len + ndim,
    init_sampler = c("uniform", "gaussian")) {

    self$in_seq_len = in_seq_len
    self$out_seq_len = out_seq_len
    self$ndim = ndim
    self$fan_avg = fan_avg
    if (!init_sampler[1] %in% c("uniform", "gaussian")) {
      stop("Unrecognized `init_sampler` argument.")
    }
    self$init_sampler = init_sampler[1]

    # Glorot (Xavier) Initialization
    if (init_sampler[1] == "uniform") {
      gf = sqrt(6 / self$fan_avg)
      self$gf = gf
      self$init_sampler = partial(runif, min = -gf, max = gf)
    } else {
      gf = sqrt(2 / fan_avg)
      self$gf = gf
      self$init_sampler = partial(rnorm, mean = 0, sd = gf)
    }
    
    self$Wqk = matrix(
        self$init_sampler(self$ndim * self$ndim),
        nrow = self$ndim,
        ncol = self$ndim
      ) |>
      torch_tensor(requires_grad = TRUE, device = default_device) |>
      nn_parameter()
    self$Wv = matrix(
        self$init_sampler(self$ndim * self$ndim),
        nrow = self$ndim,
        ncol = self$ndim
      ) |>
      torch_tensor(requires_grad = TRUE, device = default_device) |>
      nn_parameter()
    self$W0 = matrix(
        self$init_sampler(self$ndim * self$out_seq_len),
        nrow = self$ndim,
        ncol = self$out_seq_len
      ) |>
      torch_tensor(requires_grad = TRUE, device = default_device) |>
      nn_parameter()
    self$b0 = matrix(
        self$init_sampler(self$out_seq_len),
        nrow = 1,
        ncol = self$out_seq_len
      ) |>
      torch_tensor(requires_grad = TRUE, device = default_device) |>
      nn_parameter()
  },
  forward = function(x) {
    forward_single = function(x) {
      # Calculate attention.
      # alpha = softmax(x[1,] %*% Wq %*% t(Wk) %*% t(x))
      # attention = alpha %*% X %*% Wv
      alpha = nnf_softmax(
        torch_matmul(
          torch_matmul(
            x[1,, drop = FALSE], 
            self$Wqk
          ),
          x$t()
        ),
        dim = 2
      )
      attention = torch_matmul(
        alpha,
        torch_matmul(
          x,
          self$Wv
        )
      )
      nnf_softmax(torch_matmul(attention, self$W0) + self$b0, dim = 2)
    }
    # Is it batched?
    if (length(x$shape) == 2) {
      return(forward_single(x))
    } else if (length(x$shape == 3)) {
      r = map(
        seq_len(x$shape[1]),
        ~ forward_single(x[.x,,])
      )
      torch_cat(r, 1)
    } else {
      stop("Invalid input shape.")
    }
  }
)
