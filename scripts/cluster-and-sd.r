library(dplyr)
library(tibble)
library(tidygraph)
library(graphmobility)
library(igraph)
library(foreach)
library(checkmate)

if (!file.exists("data/2018-census-main-means-of-travel-to-work-by-statistical-a.csv")) {

  stop("Please run this from the project root")
}

if (!file.exists("artifacts/strings.rds")) {
  stop("Please run strings.R first")
}

tm <- readRDS("artifacts/trans-matrix.rds") |>
  filter(count > 0) |>
  as_tibble()

g <- tm |>
  as_tbl_graph(directed = TRUE)

comps <- components(g, mode = "strong")

g <- g %N>%
  mutate(component = comps$membership,
         stat_distr = NA)

# for each of the components, get the stationary distribution.

ni <- foreach (comp = unique(comps$membership), .combine = bind_rows) %do% {
  gs <- g %>% to_subgraph(component == comp, subset_by = "nodes") 
  ptm <- gs[[1]] |> 
    as_adjacency_matrix(attr = "count") |>
    am_to_ptm() 
  if (all(dim(ptm) == c(1, 1))) {
    gs[[1]] <- gs[[1]] %N>%
      mutate(stat_dist = 0)
  } else {
    sd <- stationary_distr(ptm)
    sd[sd < 0, drop = FALSE] <- 0
    gs[[1]] <- gs[[1]] %N>%
      mutate(stat_dist = sd[,1])
  }
  gs[[1]] %N>%
    as_tibble()
}

ni <- ni[match((g %N>% as_tibble())$name, ni$name),]
assert(all( (g %N>% as_tibble())$name == ni$name ))
g <- g %N>%
  mutate(stat_dist = ni$stat_dist)

saveRDS(g, "artifacts/mobility-graph.rds")
saveRDS(g %N>% as_tibble(), "artifacts/stat-distr.rds")
