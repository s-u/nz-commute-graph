library(ggplot2)
library(dplyr)
library(tidyr)
library(graphmobility)
library(igraph)

x <- readRDS("artifacts/stat-distr-data.rds")
ptm <- readRDS("artifacts/prob-transition-matrix-morning.rds")
pte <- readRDS("artifacts/prob-transition-matrix-evening.rds")
bg <- readRDS("artifacts/big-component.rds")

p <- x |>
  pivot_longer(-c(Index, Scale, Commute)) |>
  rename(Likelihood = value) |>
  mutate(Scale = factor(Scale, levels = rev(sort(unique(Scale))))) |>
  ggplot(aes(x = Index, y = Likelihood, color = name)) +
    geom_line() +
    scale_color_manual(values = c("dark red", "black")) +
    facet_grid(Scale ~ Commute, scale = "free_y") +
    theme_bw() +
    theme(legend.position="none")
ggsave("visualizations/perm-test.png", p, width = 8, height = 6)

# ptm edge properties

tpm <- tibble(x = ptm@x)

pm <- ggplot(tpm, aes(x = x)) +
  geom_histogram(bins = 51) +
  theme_bw() +
  ylab("Count") +
  xlab("Transition Probability") 

ggsave("visualizations/trans-prob-test.png", pm, width = 8, height = 6)

am <- bg |>
    as_adjacency_matrix(attr = "count")

ac <- tibble(x = am@x, index = seq_along(am@x), Count = "Absolute") 
    
acl <- tibble(x = log(am@x), index = seq_along(am@x), Count = "Log")

library(patchwork)
pc <- ggplot(ac, aes(x = x)) +
  geom_histogram(bins = 51) +
  theme_bw() +
  ylab("Count") +
  xlab("Absolute Edge Count") 

pcl <- ggplot(acl, aes(x = x)) +
  geom_histogram(bins = 51) +
  theme_bw() +
  ylab("Count") +
  xlab("Log Edge Count") 

ggsave("visualizations/edge-counts.png", pc / pcl, width = 8, height = 6)

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
  
ggsave("visualizations/degree-hists.png", p, width = 8, height = 6)

rm <- readRDS("artifacts/resample-morning.rds")
sd <- (readRDS("artifacts/stat-distr-data.rds") |>
  filter(Commute == "Morning" & Scale == "Original Scale"))[[1]]

tail_p <- 1 - vapply(seq_along(sd), function(i) ecdf(rm[,i])(sd[i]), NA_real_)
x <- tibble(tail_p = tail_p, Index = seq_along(tail_p))

p <- ggplot(x, aes(x = Index, y = tail_p)) +
  geom_line() +
  theme_bw() +
  ylab("Empirical Tail Probability")

ggsave("visualizations/tail-probs.png", p, width = 8, height = 6)

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
  ylab("Number of Significant Attractors") +
  xlab("Number of Top Attractors") +
  theme_bw()

ggsave("visualizations/signif-attractors.png", p, width = 8, height = 6)

ms <- which.max(num_signif)

aps <- p.adjust(tail_p[seq_len(ms)], method = "fdr")

tibble(aps = aps, Index = seq_along(aps)) |>
  ggplot(aes(x = Index, y = aps)) +
    geom_point() +
    theme_bw() +
    ylab("Adjusted P Value") +
    xlab("Top Attractors") +
    geom_hline(yintercept = 0.05, color = "dark red")
  
