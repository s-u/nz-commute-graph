library(purrr)
library(furrr)
library(future)
library(tibble)
plan(multicore, gc = TRUE, workers = 7)

traj = readRDS("artifacts/strings.rds")

thresh_len = 20
#thresh_len = Inf

# Get rid of the NA trajectory locations.
# Get rid of the trajectories, not the NA elements!

traj = map(traj, compose(na.omit, as.vector)) |>
  keep(~ !(is.null(.x) || any(is.na(.x))))

# The max trajectory length.
lens = map_dbl(traj, length) 

# TODO: truncate or eliminate if greater than thresh_len
lens_thresh = map_dbl(traj, ~ min(c(length(.x)), thresh_len))
#hist(lens_thresh)

# Only consider the trajectories up to 20 locations.
threshold_trajectory = function(traj, thresh = thresh_len) {
  if (thresh <= 0) {
    stop("`thresh` must be at least 1.")
  }
  if (length(traj) > thresh) {
    traj[1:thresh]
  } else {
    c(traj, rep(traj[length(traj)], thresh - length(traj)))
#    c(traj, rep(0, thresh - length(traj)))
  }
}

traj = map(traj, threshold_trajectory)

non_movers = keep(traj, ~ length(unique(.x)) == 1)

zero_on = function(traj_sample, ind) {
  rev(c(rep(0, length(traj_sample) - ind + 1), traj_sample[1:(ind-1)]))
}

finish_traj = function(traj_sample, ind) {
  traj_sample[ind]
  #r = traj_sample[ind:length(traj_sample)]
  #c(r, rep(tail(r, 1), ind - 1))
}

all_sub_trajs = function(traj_sample, ind) {
#  first_zero = min(min(which(traj_sample == 0)), length(traj_sample) + 1)
  map_dfr(
    2:length(traj_sample),
    ~ tibble(
      ind = ind,
      x = list(zero_on(traj_sample, .x)),
      y = finish_traj(traj_sample, .x)#)#traj_sample[.x],
    )
  )
}

x = future_map_dfr(
  seq_along(traj),
  ~ {
    all_sub_trajs(traj[[.x]], .x)
  },
  .progress = TRUE
)

saveRDS(x, "artifacts/smt-data.rds")
