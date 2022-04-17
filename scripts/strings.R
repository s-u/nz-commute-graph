## Take routes and create a string of polygon IDs
## listing the polygons along the route.
## When on the edge, it will stay in the same polygon
## if possible to avoid flip-flopping.

packages <- c("sf", "parallel")
for (pkg in packages) library(pkg, character.only=TRUE)

if (!file.exists("data/2018-census-main-means-of-travel-to-work-by-statistical-a.csv")) stop("Please run this from the project root")

if (!file.exists("artifacts/routes-car.rds")) stop("Please run routes.R first")

a=readRDS("artifacts/routes-car.rds")
d=st_read("data/statistical-area-2-2018-clipped-generalised.shp")
geo=st_geometry(d)
geoll = st_transform(geo, crs=4326)

if (!file.exists("src/foldw.so"))
    system("cd src && R CMD SHLIB foldw.c")
dyn.load("src/foldw.so")

l <- mclapply(seq_along(a), function(i) tryCatch({
    if (i %% 100 == 0) cat(i, "\n")
    if (is.character(a[[i]]) || !length(a[[i]])) return (a[[i]])
    ls   = st_linestring(a[[i]][,2:1])
    lsc  = st_sfc(ls, crs=4326)

    lsnz = st_transform(lsc, st_crs(geo))
    int  = st_intersection(lsnz, geo)
    ## int can flip-flop, so need to find the boundary cases

    lss  = st_segmentize(lsc, 100) ## this doesn't work on ls alone - needs lsc
    ## extract points from linestring for the within operation
    ptm  = as.matrix(lss[[1]])
    lsl  = st_as_sf(data.frame(x=ptm[,1], y=ptm[,2]), coords=c("x","y"), crs=4326)
    ## within for each point
    wid <- st_is_within_distance(lsl, geoll[attr(int, "idx")[,2]], 10)
    fwid = .Call("foldw", wid)
    trs = attr(int, "idx")[rle(fwid)$values,2]
}, error=function(e) e), mc.cores=64)

saveRDS(l, file="artifacts/strings.rds")
