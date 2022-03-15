library(ggplot2)
library(dplyr)
library(tidyr)
library(graphmobility)
library(igraph)
library(tidygraph)

x <- readRDS("artifacts/stat-distr-data.rds")
ptm <- readRDS("artifacts/prob-transition-matrix-morning.rds")
pte <- readRDS("artifacts/prob-transition-matrix-evening.rds")
bg <- readRDS("artifacts/big-component.rds")

width <- height <- 7

p <- x |>
  select(-vertex_id) |>
  mutate(Commute = factor(Commute, c("Morning", "Evening"))) |>
  pivot_longer(-c(Index, Scale, Commute)) |>
  rename(Likelihood = value) |>
  mutate(Scale = factor(Scale, levels = rev(sort(unique(Scale))))) |>
  ggplot(aes(x = Index, y = Likelihood, color = name)) +
    geom_line() +
    scale_color_manual(values = c("dark red", "black")) +
    facet_grid(Scale ~ Commute, scale = "free_y") +
    theme_bw() +
    theme(legend.position="none")
ggsave("visualizations/perm-test.png", p, width = width, height = height)

# ptm edge properties

tpm <- tibble(x = ptm@x)

pm <- ggplot(tpm, aes(x = x)) +
  geom_histogram(bins = 51) +
  theme_bw() +
  ylab("Count") +
  xlab("Transition Probability") 

ggsave("visualizations/trans-prob-test.png", pm, width = width, height = height)

am <- bg |>
    as_adjacency_matrix(attr = "count")

ac <- tibble(x = am@x, index = seq_along(am@x), Count = "Absolute") 
    
library(patchwork)
pc <- ggplot(ac, aes(x = x)) +
  geom_histogram(bins = 51) +
  theme_bw() +
  ylab("Count") +
  xlab("Absolute Edge Count") 

pcl <- ggplot(ac, aes(x = x)) +
  geom_histogram(bins = 51) +
  theme_bw() +
  scale_x_log10() + 
  ylab("Count") +
  xlab("Log Edge Count") 

ggsave("visualizations/edge-counts.png", pc / pcl, width = width, 
       height = height)

deg <- tibble(
    `Out Degree` = apply(as.matrix(am), 1, function(x) sum(x > 0)),
    `In Degree` = apply(as.matrix(am), 2, function(x) sum(x > 0)),
    index = seq_along(`Out Degree`)
  ) |>
  pivot_longer(-index)

p <- ggplot(deg, aes(x = value)) +
  geom_histogram(bins = 17) +
  facet_grid(name ~ .) +
  theme_bw() +
  xlab("Degree") +
  ylab("Count")
  
ggsave("visualizations/degree-hists.png", p, width = width, height = height)

rm <- readRDS("artifacts/resample-morning.rds")
sd <- readRDS("artifacts/stat-distr-data.rds") |>
  filter(Commute == "Morning" & Scale == "Original Scale")

tail_p <- 1 - 
  vapply(
    seq_along(sd$`Stat. Distr.`), 
    function(i) ecdf(rm[,i])(sd$`Stat. Distr.`[i]), 
    NA_real_
  )

x <- bind_rows(
  tibble(
    prob = tail_p, 
    Index = seq_along(tail_p), 
    vertex_id = sd$vertex_id,
    type = "Tail Probability"
  ),
  sd %>% 
    rename(prob = `Stat. Distr.`) %>% 
    select(prob, Index, vertex_id) %>%
    mutate(type = "Stationary Distribution")
)
x$type <- factor(x$type, rev(sort(unique(x$type))))

p <- ggplot(x, aes(x = Index, y = prob)) +
  geom_line() +
  theme_bw() +
  facet_grid(type ~ ., scales = "free_y") +
  ylab("") +
  xlab("Stationary Distribution Values in Decreasing Order")

ggsave("visualizations/tail-probs.png", p, width = width, height = height)

num_signif <- 
  vapply(
    seq_along(tail_p), 
    function(i) 
      sum(p.adjust(tail_p[seq_len(i)], method = "fdr") < 0.05),
    NA_real_
  )

p <- tibble(ns = num_signif, x = seq_along(num_signif)) %>%
  ggplot(aes(x = x, y = ns)) +
  geom_line() +
  ylab("Number of Significant Loci") +
  xlab("Number of Top Stationary Distribution Elements Tested") +
  theme_bw()

ggsave("visualizations/signif-attractors.png", p, width = width, 
       height = height)

ms <- which.max(num_signif)

aps <- p.adjust(tail_p[seq_len(ms)], method = "fdr")

p <- tibble(aps = aps, Index = seq_along(aps)) |>
  ggplot(aes(x = Index, y = aps)) +
    geom_point() +
    theme_bw() +
    ylab("Adjusted P Value") +
    xlab("Top Stationary Distribution Elements") +
    geom_hline(yintercept = 0.05, color = "dark red")
  
ggsave("visualizations/top-adjusted-p.png", p, width = width, height = height)

xs <- x[seq_len(ms),] %>%
  rename(tail_p = prob) %>%
  filter(type == "Tail Probability")

xs$adj_tail_p <- p.adjust(xs$tail_p, method = "fdr")
xs <- xs[xs$adj_tail_p <= 0.05,]

saveRDS(xs, "artifacts/attractor-indices.rds")

ni <- readRDS("artifacts/mobility-graph.rds")

nis <- (ni %>% 
  to_subgraph(as.integer(name) %in% xs$vertex_id, subset_by = "nodes"))[[1]]

# These are the same
wc <- components(nis, mode = "weak")
sc <- components(nis, mode = "strong")

nis <- nis %N>%
  mutate(weak_comp = wc$membership, strong_comp = sc$membership)

saveRDS(nis %N>% as_tibble(), "artifacts/attractor-components.rds")

ns <- nis %N>% as_tibble()
table(ns$weak_comp, ns$strong_comp)
