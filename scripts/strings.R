## Take routes and create a string of polygon IDs
## listing the polygons along the route.
## When on the edge, it will stay in the same polygon
## if possible to avoid flip-flopping.

packages <- c("sf", "parallel")
for (pkg in packages) library(pkg, character.only=TRUE)

if (!file.exists("data/2018-census-main-means-of-travel-to-work-by-statistical-a.csv")) stop("Please run this from the project root")

if (!file.exists("artifacts/routes-car.rds")) stop("Please run routes.R first")

a=readRDS("artifacts/routes-car.rds")
d=st_read("data/statistical-area-2-2021-clipped-generalised.shp")
geo=st_geometry(d)
geoll = st_transform(geo, crs=4326)

if (!file.exists("src/foldw.so"))
    system("cd src && R CMD SHLIB foldw.c")
dyn.load("src/foldw.so")

l <- mclapply(seq_along(a), function(i) tryCatch({
    cat(i, "\n")
    if (is.character(a[[i]])) return (a[[i]])
    ls   = st_linestring(a[[i]]$points[,2:1])
    lsnz = st_transform(st_sfc(ls, crs=4326), st_crs(geo))
    lsl  = st_as_sf(data.frame(x=a[[i]]$points[,2], y=a[[i]]$points[,1]), coords=c("x","y"), crs=4326)
    int  = st_intersection(lsnz, geo)
    wid <- st_is_within_distance(lsl, geoll[attr(int, "idx")[,2]], 10)
    fwid = .Call("foldw", wid)
    trs = attr(int, "idx")[rle(fwid)$values,2]
}, error=function(e) e), mc.cores=32)

saveRDS(l, file="artifacts/strings.rds")
