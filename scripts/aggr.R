if (!file.exists("data/2018-census-main-means-of-travel-to-work-by-statistical-a.csv")) stop("Please run this from the project root")

if (!file.exists("artifacts/strings.rds")) stop("Please run strings.R first")

year <- if (nzchar(year <- Sys.getenv("YEAR"))) as.integer(year)
cfn <- function(what) if (is.null(year)) paste0("artifacts/", what, ".rds") else paste0("artifacts/", what, "-", year, ".rds")

s = readRDS(cfn("strings"))
year <- attr(s, "year")
if (is.null(year)) stop("strings are too old (no year included) please re-run strings.R")

tr = readRDS("artifacts/transitions.rds")
tr0 = function(x) { x[x < 0] = 0; x }

## car-related transitions
cts = rowSums(tr0(tr[,10:12]))

## create pairs from each string with counts
ps = lapply(seq_along(s), function(i) {
    si = s[[i]]
    si = si[!is.na(si)]
    if (length(si) > 1) si = si[c(TRUE, diff(si) != 0)]
    if (is.numeric(si) && length(si) > 1) {
       cbind(route=i, from=si[-length(si)], to=si[-1], count=cts[i])
    } else NULL
})

## all pairs + counts
m = do.call(rbind, ps)
saveRDS(m, file=paste0("artifacts/full-matrix-", year, ".rds"))

## aggregate by pair
ag = aggregate(m[,4], list(from=m[,2], to=m[,3]), sum)
names(ag)[3] = "count"
saveRDS(ag, file=paste0("artifacts/trans-matrix-", year, ".rds"))

system(paste0("ln -sfn full-matrix-", year, ".rds artifacts/full-matrix.rds"))
system(paste0("ln -sfn trans-matrix-", year, ".rds artifacts/trans-matrix.rds"))

## to match our index to SA2s
## sa = st_read("data/statistical-area-2-2018-clipped-generalised.shp")
## sa[[1]][s[[1]]]
