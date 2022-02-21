if (!file.exists("data/2018-census-main-means-of-travel-to-work-by-statistical-a.csv")) stop("Please run this from the project root")

if (!file.exists("artifacts/strings.rds")) stop("Please run strings.R first")

tr = readRDS("artifacts/transitions.rds")	
tr0 = function(x) { x[x < 0] = 0; x }

## car-related transitions
cts = rowSums(tr0(tr[,10:12]))

s = readRDS("artifacts/strings.rds")

## create pairs from each string with counts
ps = lapply(seq_along(s), function(i) {
    si = s[[i]]
    if (is.numeric(si) && length(si) > 1) {
       cbind(route=i, from=si[-length(si)], to=si[-1], count=cts[i])
    } else NULL
})

## all pairs + counts
m = do.call(rbind, ps)
saveRDS(m, file="artifacts/full-matrix.rds")

## aggregate by pair
ag = aggregate(m[,4], list(from=m[,2], to=m[,3]), sum)
names(ag)[3] = "count"
saveRDS(ag, file="artifacts/trans-matrix.rds")

## to match our index to SA2s
## sa = st_read("data/statistical-area-2-2018-clipped-generalised.shp")
## sa[[1]][s[[1]]]
