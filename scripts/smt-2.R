library(torch)
library(luz)
library(purrr)
library(dplyr)
library(tidyr)
library(rhdf5)


source("scripts/smt-models.R")

model_fn = if(nzchar(.<-Sys.getenv("MODEL"))) . else "model.luz"

train_model = TRUE
epochs = 50
## you could try "mps" on arm64 macOS
default_device <- if (cuda_is_available()) {
    torch_device("cuda:0") 
  } else if (backends_mps_is_available()) {
    torch_device("mps") 
  } else {
    "cpu"
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
    h5_file,
    ndim = max(max(unlist(x$y)), max(unlist(x$x))),
    device = default_device,
    .progress = FALSE) {

    if (!missing(x)) {
      self$ndim = ndim
      self$device = device
      self$h5_file = h5_file
      h5createFile(self$h5_file)
      h5write(self$ndim, self$h5_file,"ndim")
      h5write(nrow(x), self$h5_file,"length")
      h5write(x$ind, self$h5_file, "ind") 
      h5createGroup(self$h5_file, "x")
      h5createGroup(self$h5_file, "y")
      numchar = nrow(x) |> as.character() |> nchar()
      fmt = paste0("%0", numchar, "d")
      if (.progress) {
        print("Writing x to hdf5.")
      }
      walk(
        seq_len(nrow(x)),
        ~ encode_seq(x$x[[.x]], ndim = self$ndim) |> 
            as.matrix() |>
            h5write(self$h5_file, sprintf(paste0("x/", fmt), .x)),
        .progress = .progress
      )
      if (.progress) {
        print("Writing y to hdf5.")
      }
      walk(
        seq_len(nrow(x)),
        ~ encode_seq(x$y[[.x]], ndim = self$ndim) |> 
            as.matrix() |>
            h5write(self$h5_file, sprintf(paste0("y/", fmt), .x)),
        .progress = .progress
      )
      self$h5 = H5Fopen(self$h5_file)
    } else {
      self$h5_file = h5_file
      self$h5 = H5Fopen(self$h5_file)

      stop("Not implemented.")
    }
  },
  .getbatch = function(i) {
    if (length(i) == 1) {
      list(
        x = torch_tensor(self$h5$x[[i]], device = self$device),
        y = torch_tensor(self$h5$y[[i]], device = self$device)
      )
    } else {
      list(
        x = map(self$h5$x[i], ~ torch_tensor(.x, device = self$device)) |>
          torch_stack(dim = 1),
        y = map(self$h5$y[i], ~ torch_tensor(.x, device = self$device)) |>
          torch_stack(dim = 1)
      )
    }
  },
  .length = function() {
    self$h5$length
  }
)


ds = AroTokenData(x, h5_file = "x.h5")
a = ds[1:2]

