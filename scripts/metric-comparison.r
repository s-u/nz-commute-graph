library(tidygraph)
library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)
library(parallel)
library(patchwork)

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

x[['In-degree']][is.na(x[['In-degree']])] <- 0

x <- et |>
  group_by(to) |>
  summarize(`Weighted in-degree` = sum(count)) |>
  rename(name = to) |>
  mutate(name = as.character(name)) |>
  right_join(x, by = "name")

x[['Weighted in-degree']][is.na(x[['Weighted in-degree']])] <- 0


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

# First, fix this so that we have non-repeated sa2's.
# Do analysis for weighted out degree minus weighted in degree.
if (!file.exists("artifacts/total-traffic-count.rds")) {
  library(purrr)
  library(foreach)
  library(doMC)
  registerDoMC()
  rt <- readRDS("artifacts/routes-car.rds")

  sa2_inds <- foreach(i = seq_along(rt), .combine = c, .inorder = FALSE) %dopar% {
    if (i %% 100 == 0) {
      cat(i, "of", length(rt), "\n")
    }
    if (!is.null(rt[[i]])) {
      ps <- as.data.frame(rt[[i]]) |>
          st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
          st_geometry()
      ps <- st_transform(ps, st_crs(sa$geometry[1]))
      ints <- map(unclass(st_intersects(sa$geometry, ps)), length) |>
        unlist() |>
        as.logical() |>
        which()
      
    } else {
      c()
    }
  }
  tc <- tibble(name = sa2_inds) |>
    group_by(name) |>
    summarize(n = n()) |>
    mutate(name = as.character(name))

  saveRDS(tc, "artifacts/total-traffic-count.rds")
} else {
  tc <- readRDS("artifacts/total-traffic-count.rds")
}

# Sanity check:
# plot(sa[unique(apply(d, 1, which.min)),]$geometry)
# plot(ps, col = "red", type = "l", add = TRUE)

x <- left_join(x, tc |> rename(`Total traffic` = n), by = "name")

loci <- readRDS("artifacts/attractor-indices.rds") |> 
  as_tibble() |>
  select(vertex_id, adj_tail_p) |> 
  rename(name = vertex_id) |>
  mutate(name = as.character(name)) 

loci[['SA2 Type']][loci$adj_tail_p <= 0.05] <- "Loci"

x <- left_join(x, loci |> select(-adj_tail_p), by = "name")
x[['SA2 Type']][is.na(x[['SA2 Type']])] <- "Non-loci"

x$stat_dist[x$stat_dist < 0] <- 0
x$stat_dist <- sqrt(x$stat_dist)

xl <- x %>%
  pivot_longer(cols = c(contains("degree"), contains("traffic")), 
               names_to = "degree") |>
  mutate(degree = factor(degree, 
                         levels = c("In-degree", "Weighted in-degree", 
                                    "Total incoming traffic",
                                    "Total traffic")))

ggplot(xl |> na.omit(), aes(x = sqrt(stat_dist), y = value, color = `SA2 Type`)) +
  geom_point(size = .75, alpha = 0.7) +
  facet_grid(degree ~ ., scales = "free_y") +
  xlab("Square Root of the Stationary Distribution Value") +
  ylab("") +
  theme_bw()

ggsave("visualizations/local-global-scatter.png")

library(randomForestSRC)
x <- na.omit(x)
x$sqrt_stat_dist <- sqrt(x$stat_dist)
fit_wid <- rfsrc(sqrt_stat_dist ~ `Weighted in-degree`, 
             data = 
              x |> select(`Weighted in-degree`, sqrt_stat_dist) |> as.data.frame())

fit_id <- rfsrc(stat_dist ~ `In-degree`, 
                data = x |> select(`In-degree`, stat_dist) |> as.data.frame())

fit_idh <- rfsrc(stat_dist ~ `Total incoming traffic`, 
                data = x |> select(`Total incoming traffic`, stat_dist) |> 
                  as.data.frame())

rt <- readRDS("artifacts/routes-car.rds")

sa$geometry <- st_transform(sa$geometry, crs = 4326)
sa$centroid <- st_centroid(sa$geometry)
sa$centroid <- st_transform(sa$centroid, crs = 4326)

fit_tc <- rfsrc(stat_dist ~ `Total traffic`,
                data = x |> as.data.frame())


tibble(
  Metric = c("In-degree", "Weighted in-degree", "Total incoming traffic", "Total traffic"),
  `p-value` = c(
    summary(lm(stat_dist ~ `In-degree`, x))$coefficients[2, 4],
    summary(lm(stat_dist ~ `Weighted in-degree`, x))$coefficients[2, 4],
    summary(lm(stat_dist ~ `Total incoming traffic`, x))$coefficients[2, 4],
    summary(lm(stat_dist ~ `Total traffic`, x))$coefficients[2, 4]),
  `Linear reg. adj. r-squared` = c(
    summary(lm(stat_dist ~ `In-degree`, x))$adj.r.squared,
    summary(lm(stat_dist ~ `Weighted in-degree`, x))$adj.r.squared,
    summary(lm(stat_dist ~ `Total traffic`, x))$adj.r.squared,
    summary(lm(stat_dist ~ `Total incoming traffic`, x))$adj.r.squared),
  `RF OOB r-squared` = c(
    1 - sum( (fit_id$predicted.oob - x$stat_dist)^2) / 
      sum( (x$stat_dist - mean(x$stat_dist))^2 ),
    1 - sum( (fit_wid$predicted.oob - x$stat_dist)^2) / 
      sum( (x$stat_dist - mean(x$stat_dist))^2 ),
    1 - sum( (fit_idh$predicted.oob - x$stat_dist)^2) /
      sum( (x$stat_dist - mean(x$stat_dist))^2 ),
    1 - sum( (fit_tc$predicted.oob - x$stat_dist)^2) /
      sum( (x$stat_dist - mean(x$stat_dist))^2 ))
)

ggplot(x, aes(x = stat_dist, y = `Total traffic`)) +
  geom_point()


tibble(
  Metric = c(
    "Stat Dist ~ Total incoming traffic",
    "Stat Dist ~ Total traffic",
    "Total incoming traffic ~ Total traffic",
    "Stat Dist ~ Total incoming traffic + Total traffic"
  ),
  `Linear reg. adj. r-squared` = c(
    summary(lm(stat_dist ~ `Total incoming traffic`, x))$adj.r.squared,
    summary(lm(stat_dist ~ `Total traffic`, x))$adj.r.squared,
    summary(lm(`Total incoming traffic` ~ `Total traffic`, x))$adj.r.squared,
    summary(
      lm(
        stat_dist~`Total incoming traffic`+`Total traffic`, 
        x)
    )$adj.r.squared
  ),
  `RF OOB r-squared` = c(
    1 - 
      sum((rfsrc(stat_dist ~ `Total incoming traffic`, 
                 data = x |> as.data.frame())$predicted.oob - x$stat_dist)^2) /
      sum( (x$stat_dist - mean(x$stat_dist))^2 ),
    1 - 
      sum((rfsrc(stat_dist ~ `Total traffic`, 
                 data = x |> as.data.frame())$predicted.oob - x$stat_dist)^2) /
      sum( (x$stat_dist - mean(x$stat_dist))^2 ),
    1 - 
      sum((rfsrc(`Total incoming traffic` ~ `Total traffic`, 
                  data = x |> as.data.frame())$predicted.oob - x$`Total incoming traffic`)^2) /
      sum( (x$`Total incoming traffic` - mean(x$`Total incoming traffic`))^2 ),
    1 - sum((rfsrc(stat_dist ~ `Total incoming traffic` + `Total traffic`, 
                   data = x |> as.data.frame())$predicted.oob - x$stat_dist)^2) /
      sum( (x$stat_dist - mean(x$stat_dist))^2 )
  )
)
 
x$row_num <- seq_len(nrow(x))
 
fmf <- stat_dist ~ `Total incoming traffic` + `Total traffic`
fmrf <- rfsrc(fmf, data = x |> as.data.frame())
fmlm <- lm(fmf, data = x |> as.data.frame())  

itlm <- lm(`Total incoming traffic` ~ `Total traffic`, data = x)

loci <- readRDS("artifacts/attractor-indices.rds") |> 
  as_tibble() |>
  select(vertex_id, adj_tail_p) |> 
  rename(name = vertex_id) |>
  mutate(name = as.character(name))


sdm <- tibble(`Fitted values` = fmlm$fitted.values,
              `Residuals` = fmlm$residuals,
              name = na.omit(x)$name,
              row_num = na.omit(x)$row_num) |>
  left_join(loci, by = "name") 

sdm[["SA2 type"]] <- "Non-loci"
sdm[["SA2 type"]][!is.na(sdm$adj_tail_p)] <- "Loci"

pl <- ggplot(sdm, 
             aes(x = `Fitted values`, y = `Residuals`, color = `SA2 type`)) +
  geom_point() +
  ggtitle("Sqrt(Stat. Dist.) ~ Total incoming traffic + Total traffic") +
  theme_bw() +
  theme(legend.position="none") 

itm <- tibble(`Fitted values` = itlm$fitted.values,
              `Residuals` = itlm$residuals,
              name = na.omit(x)$name,
              row_num = na.omit(x)$row_num) |>
  left_join(loci, by = "name")

xx <- left_join(x, loci, by = "name")

itm[["SA2 type"]] <- "Non-loci"
itm[["SA2 type"]][!is.na(itm$adj_tail_p)] <- "Loci"

pr <- ggplot(itm, 
             aes(x = `Fitted values`, y = `Residuals`, color = `SA2 type`)) +
  geom_point() +
  ggtitle("Total incoming traffic ~ Total traffic") +
  theme_bw()

ggsave("visualizations/fitted-vs-residuals.png", pl + pr, width = 10.5, 
       height = 5.25)

