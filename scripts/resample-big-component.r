library(tidygraph)
library(igraph)
library(graphmobility)
library(foreach)
library(doMC)
library(ggplot2)
library(tibble)
library(tidyr)
library(Matrix)

registerDoMC(cores = 10)

if (!file.exists("data/2018-census-main-means-of-travel-to-work-by-statistical-a.csv")) {

  stop("Please run this from the project root")
}

if (!file.exists("artifacts/strings.rds")) {
  stop("Please run strings.R first")
}

g <- readRDS("artifacts/mobility-graph.rds")

big_component <- 
  as.integer(
    names(sort(table((g %N>% as_tibble())$component), decreasing = TRUE))[1]
  )

bg <- to_subgraph(g, component == big_component, subset_by = "nodes")[[1]]
saveRDS(bg, "artifacts/big-component.rds")

ptm <- bg |>
    as_adjacency_matrix(attr = "count") |>
    am_to_ptm()

saveRDS(ptm, "artifacts/prob-transition-matrix-morning.rds")

# Morning
rsm <- foreach(i = seq_len(10000), .combine = rbind) %dopar% {
  registerDoSEQ()
  am <- bg |>
    as_adjacency_matrix(attr = "count")
  am@x <- sample(am@x)
  
  ptm <- am_to_ptm(am)
  stationary_distr(ptm)[,1]
}

saveRDS(rsm, "artifacts/resample-morning.rds")

# Evening
rse <- foreach(i = seq_len(10000), .combine = rbind) %dopar% {
  registerDoSEQ()
  am <- bg |>
    as_adjacency_matrix(attr = "count") |>
    t()

  am@x <- sample(am@x)
  
  ptm <- am_to_ptm(am)
  stationary_distr(ptm)[,1]
}

saveRDS(rse, "artifacts/resample-evening.rds")

pte <- bg |>
  as_adjacency_matrix(attr = "count") |>
  am_to_ptm() |>
  t()

saveRDS(pte, "artifacts/prob-transition-matrix-evening.rds")

ordm <- order(stationary_distr(ptm)[,1], decreasing = TRUE)
saveRDS(ordm, "artifacts/order-morning.rds")
orde <- order(stationary_distr(pte)[,1], decreasing = TRUE)
saveRDS(orde, "artifacts/order-evening.rds")

x <- tibble(
  `Stat. Distr.` = stationary_distr(ptm)[,1][ordm],
  Index = seq_along(`Stat. Distr.`),
  `95% Null Quantile` = apply(rsm[,ordm], 2, quantile, .95),
  Scale = "Original Scale"
)

y <- tibble(
  `Stat. Distr.` = log(stationary_distr(ptm)[,1][ordm]),
  Index = seq_along(`Stat. Distr.`),
  `95% Null Quantile` = log(apply(rsm[,ordm], 2, quantile, .95)),
  Scale = "Log Scale"
)

zm <- rbind(x, y)
zm$Commute <- "Morning"

x <- tibble(
  `Stat. Distr.` = stationary_distr(pte)[,1][orde],
  Index = seq_along(`Stat. Distr.`),
  `95% Null Quantile` = apply(rse[,orde], 2, quantile, .95),
  Scale = "Original Scale"
)

y <- tibble(
  `Stat. Distr.` = log(stationary_distr(pte)[,1][orde]),
  Index = seq_along(`Stat. Distr.`),
  `95% Null Quantile` = log(apply(rse[,orde], 2, quantile, .95)),
  Scale = "Log Scale"
)

ze <- rbind(x, y)
ze$Commute <- "Evening"

x <- rbind(zm, ze)

saveRDS(x, "artifacts/stat-distr-data.rds")

