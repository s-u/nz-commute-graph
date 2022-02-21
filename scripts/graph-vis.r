library(ggplot2)

x <- readRDS("artifacts/stat-distr-data.rds")
ptm <- readRDS("artifacts/prob-transition-matrix-morning.rds")
pte <- readRDS("artifacts/prob-transition-matrix-evening.rds")

ed <- x |>
  pivot_longer(-c(Index, Scale, Commute)) |>
  rename(Likelihood = value) |>
  mutate(Scale = factor(Scale, levels = rev(sort(unique(Scale))))) |>
  ggplot(aes(x = Index, y = Likelihood, color = name)) +
    geom_line() +
    scale_color_manual(values = c("dark red", "black")) +
    facet_grid(Scale ~ Commute, scale = "free_y") +
    theme_bw() +
    theme(legend.position="none")
ggsave("visualizations/perm-test.png", p, width = 8, height = 10)

# ptm edge properties

tpm <- tibble(x = ptm@x)

pm <- ggplot(tp, aes(x = x)) +
  geom_histogram(bins = 51) +
  theme_bw() +
  ylab("Count") +
  xlab("Transition Probability") 

ggsave("visualizations/trans-prob-test.png", p, width = 8, height = 6)

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

ggsave("visualizations/edge-counts.png", pc / pcl, width = 8, height = 10)

deg <- tibble(
    `Out Degree` = apply(as.matrix(am), 1, function(x) sum(x > 0)),
    `In Degree` = apply(as.matrix(am), 2, function(x) sum(x > 0)),
    index = seq_along(out_degree)
  ) |>
  pivot_longer(-index)

p <- ggplot(deg, aes(x = value)) +
  geom_histogram(bins = 17) +
  facet_grid(name ~ .) +
  theme_bw() +
  xlab("Degree") +
  ylab("Count")
  
ggsave("visualizations/degree-hists.png", p, width = 8, height = 10)
