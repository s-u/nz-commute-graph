library(tidygraph)
library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)
library(parallel)

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

# Simon's code in-degree from home.
tr = readRDS("artifacts/transitions.rds")       
tr0 = function(x) { x[x < 0] = 0; x }
tr$cts = rowSums(tr0(tr[,10:12]))
sa = st_read("data/statistical-area-2-2018-clipped-generalised.shp")

tr$src=match(tr[[1]], sa[[1]])
tr$dst=match(tr[[5]], sa[[1]])

hid <- tr |> 
  select(src, dst) |>
  na.omit() |>
  group_by(dst) |>
  summarize(`Total incoming traffic` = n()) |>
  rename(name = dst) |>
  mutate(name = as.character(name))

x <- left_join(x, hid, by = "name")
x[[5]][is.na(x[[5]])] <- 0

xl <- x %>%
  pivot_longer(cols = c(contains("degree"), contains("traffic")), 
               names_to = "degree") |>
  mutate(degree = factor(degree, 
                         levels = c("In-degree", "Weighted in-degree", 
                                    "Total incoming traffic")))

ggplot(xl |> na.omit(), aes(x = stat_dist, y = value)) +
  geom_point() +
  facet_grid(degree ~ ., scales = "free_y") +
  xlab("Stationary Distribution Value") +
  ylab("") +
  theme_bw()

library(randomForestSRC)
x <- na.omit(x)
fit_wid <- rfsrc(stat_dist ~ `Weighted in-degree`, 
             data = 
              x |> select(`Weighted in-degree`, stat_dist) |> as.data.frame())

fit_id <- rfsrc(stat_dist ~ `In-degree`, 
                data = x |> select(`In-degree`, stat_dist) |> as.data.frame())

fit_idh <- rfsrc(stat_dist ~ `Total incoming traffic`, 
                data = x |> select(`Total incoming traffic`, stat_dist) |> 
                  as.data.frame())

tibble(
  Metric = c("In-degree", "Weighted in-degree", "Total incoming traffic"),
  `P-value` = c(
    summary(lm(stat_dist ~ `In-degree`, x))$coefficients[2, 4],
    summary(lm(stat_dist ~ `Weighted in-degree`, x))$coefficients[2, 4],
    summary(lm(stat_dist ~ `Total incoming traffic`, x))$coefficients[2, 4]),
  `Adjusted r-squared` = c(
    summary(lm(stat_dist ~ `In-degree`, x))$adj.r.squared,
    summary(lm(stat_dist ~ `Weighted in-degree`, x))$adj.r.squared,
    summary(lm(stat_dist ~ `Total incoming traffic`, x))$adj.r.squared)
#    1 - sum( (fit_id$predicted.oob - x$stat_dist)^2) / 
#      sum( (x$stat_dist - mean(x$stat_dist))^2 ),
#    1 - sum( (fit_wid$predicted.oob - x$stat_dist)^2) / 
#      sum( (x$stat_dist - mean(x$stat_dist))^2 ),
#    1 - sum( (fit_idh$predicted.oob - x$stat_dist)^2) /
#      sum( (x$stat_dist - mean(x$stat_dist))^2 ))
)

rt <- readRDS("artifacts/routes-car.rds")

sa$geometry <- st_transform(sa$geometry, crs = 4326)
sa$centroid <- st_centroid(sa$geometry)
sa$centroid <- st_transform(sa$centroid, crs = 4326)

library(foreach)
library(doMC)
registerDoMC(12)

sa2_inds <- foreach(i = seq_along(rt), .combine = c) %dopar% {
  if (!is.null(rt[[i]])) {
    ps <- as.data.frame(rt[[i]]) |>
        st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
        st_geometry()
    d <- st_distance(ps, sa$centroid)
    apply(d, 1, which.min)
  } else {
    NULL
  }
}

tc <- tibble(name = sa2_inds) |>
  group_by(name) |>
  summarize(n = n()) |>
  mutate(name = as.character(name))

saveRDS(tc, "artifacts/total-traffic-count.rds")

# Sanity check:
# plot(sa[unique(apply(d, 1, which.min)),]$geometry)
# plot(ps, col = "red", type = "l", add = TRUE)



