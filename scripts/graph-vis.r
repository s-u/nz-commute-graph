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
    theme(legend.position="none") +
    xlab("Decreasing Morning-Stationary-Distribution Order")
ggsave("visualizations/perm-test.png", p, width = width, height = height)

# ptm edge properties

tpm <- tibble(x = ptm@x)

pm <- ggplot(tpm, aes(x = x)) +
  geom_histogram(bins = 51) +
  theme_bw() +
  ylab("Count") +
  xlab("Transition Probability") 

ggsave("visualizations/trans-prob-test.png", pm, 
       width = 7, height = 3.5)

am <- bg |>
    as_adjacency_matrix(attr = "count")

ac <- tibble(x = am@x, index = seq_along(am@x), Count = "Absolute") 
    
library(patchwork)
pc <- ggplot(ac, aes(x = x)) +
  geom_histogram(bins = 51) +
  theme_bw() +
  ylab("Count") +
  xlab("Edge Weight") 

pcl <- ggplot(ac, aes(x = x)) +
  geom_histogram(bins = 51) +
  theme_bw() +
  scale_x_log10() + 
  ylab("Count") +
  xlab("Log Edge Weight") 

ggsave("visualizations/edge-counts.png", pc / pcl, width = width, 
       height = height)

deg <- tibble(
    `Out Degree` = apply(as.matrix(am), 1, function(x) sum(x)),
    `In Degree` = apply(as.matrix(am), 2, function(x) sum(x)),
    index = seq_along(`Out Degree`)
  ) |>
  pivot_longer(-index)

p <- ggplot(deg, aes(x = log(value))) +
  geom_histogram(bins = 17) +
  facet_grid(name ~ .) +
  theme_bw() +
  xlab("Degree") +
  ylab("Weighted Log Count")

ggsave("visualizations/weighted-log-hists.png", p, width = width, height = height)
  
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

# Morning
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
    type = "Tail Probability",
    commute = "Morning"
  ),
  sd %>% 
    rename(prob = `Stat. Distr.`) %>% 
    select(prob, Index, vertex_id) %>%
    mutate(type = "Stationary Distribution", commute = "Morning")
)
x$type <- factor(x$type, rev(sort(unique(x$type))))

# Evening
re <- readRDS("artifacts/resample-evening.rds")
sd <- readRDS("artifacts/stat-distr-data.rds") |>
  filter(Commute == "Evening" & Scale == "Original Scale")

tail_p <- 1 -
  vapply(
    seq_along(sd$`Stat. Distr.`),
    function(i) ecdf(re[,i])(sd$`Stat. Distr.`[i]),
    NA_real_
  )

xe <- bind_rows(
  tibble(
    prob = tail_p, 
    vertex_id = sd$vertex_id,
    type = "Tail Probability",
    commute = "Evening"
  ),
  sd %>% 
    rename(prob = `Stat. Distr.`) %>% 
    select(prob, vertex_id) %>%
    mutate(type = "Stationary Distribution", commute = "Evening")
)
xe$type <- factor(xe$type, rev(sort(unique(xe$type))))

xe <- xe |>
  left_join(x |> select(Index, vertex_id), by = "vertex_id")

x <- bind_rows(x, xe)

x$commute <- factor(x$commute, rev(sort(unique(x$commute))))
p <- ggplot(x |> filter(type == "Tail Probability") , 
            aes(x = Index, y = prob)) +
  geom_line() +
  theme_bw() +
  facet_grid( ~ commute, scales = "free_y") +
  ylab("") +
  xlab("Decreasing Morning-Stationary-Distribution Order")

ggsave("visualizations/tail-probs.png", p, width = height, height = width / 2)

rm <- readRDS("artifacts/resample-morning.rds")
sd <- readRDS("artifacts/stat-distr-data.rds") |>
  filter(Commute == "Morning" & Scale == "Original Scale")

tail_p <- 1 - 
  vapply(
    seq_along(sd$`Stat. Distr.`), 
    function(i) ecdf(rm[,i])(sd$`Stat. Distr.`[i]), 
    NA_real_
  )


num_signif <- 
  vapply(
    seq_along(tail_p), 
    function(i) 
      sum(p.adjust(tail_p[seq_len(i)], method = "fdr") < 0.05),
    NA_real_
  )

for (adj in c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY")) {
  which.max(
    vapply(
      seq_along(tail_p), 
      function(i) 
        sum(p.adjust(tail_p[seq_len(i)], method = adj) < 0.05),
      NA_real_
    )
  ) %>% print()
}

p <- tibble(ns = num_signif, x = seq_along(num_signif)) %>%
  ggplot(aes(x = x, y = ns)) +
  geom_line() +
  ylab("Number of Significant Loci") +
  xlab("Number of Top Stationary Distribution Elements Tested") +
  theme_bw()

ggsave("visualizations/signif-attractors.png", p, width = 7, 
       height = 3.5)

ms <- which.max(num_signif)

aps <- p.adjust(tail_p[seq_len(ms)], method = "fdr")

p <- tibble(aps = aps, Index = seq_along(aps)) |>
  ggplot(aes(x = Index, y = aps)) +
    geom_point() +
    theme_bw() +
    ylab("Adjusted P Value") +
    xlab("Top Stationary Distribution Elements") +
    geom_hline(yintercept = 0.05, color = "dark red")
  
ggsave("visualizations/top-adjusted-p.png", p, 
       width = 7, height = 3.5)

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
