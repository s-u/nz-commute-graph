library(tidygraph)
library(dplyr)
library(ggplot2)
library(tidyr)

if (!file.exists("data/2018-census-main-means-of-travel-to-work-by-statistical-a.csv")) {

  stop("Please run this from the project root")
}

if (!file.exists("artifacts/strings.rds")) {
  stop("Please run strings.R first")
}

# Get the big community on the north island.
bc <- readRDS("artifacts/big-component.rds")

x <- bc %N>% 
  select(-component) %N>% 
  as_tibble() |>
  na.omit()


et <- bc %E>% as_tibble()

x <- et |> 
  group_by(to) |>
  summarize(`In-degree` = n()) |>
  rename(name = to) |>
  mutate(name = as.character(name)) |>
  right_join(x, by = "name")

x <- et |>
  group_by(to) |>
  summarize(`Weighted in-degree` = sum(count)) |>
  rename(name = to) |>
  mutate(name = as.character(name)) |>
  right_join(x, by = "name")

xl <- x %>%
  pivot_longer(cols = ends_with("degree"), names_to = "degree")

ggplot(xl |> na.omit(), aes(x = stat_dist, y = value)) +
  geom_point() +
  facet_grid(degree ~ ., scales = "free_y") +
  theme_bw()


