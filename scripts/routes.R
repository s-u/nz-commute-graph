## this computes routes from all pairs
## NOTE: quite computationally heavy, assumes it's
## ok to bash the machine with 64 parallel jobs

packages <- c("ghroute", "proj4", "parallel")
for (pkg in packages) library(pkg, character.only=TRUE)

if (!file.exists("data/2018-census-main-means-of-travel-to-work-by-statistical-a.csv")) stop("Please run this from the project root")

if (!file.exists("osm/new-zealand-latest.osm.pbf")) stop("Please run\nmake -C osm\nto donwload OSM data")

jtw <-  read.csv("data/2018-census-main-means-of-travel-to-work-by-statistical-a.csv", fileEncoding="UTF-8-BOM")
nzgd2000="+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
ll.r = proj4::project(jtw[,c("SA2_usual_residence_easting","SA2_usual_residence_northing")], nzgd2000, inv=TRUE)
jtw$r.lat = ll.r$y
jtw$r.lon = ll.r$x

ll.w = proj4::project(jtw[,c("SA2_workplace_easting","SA2_workplace_northing")], nzgd2000, inv=TRUE)
jtw$w.lat = ll.w$y
jtw$w.lon = ll.w$x

# remove same-SA commutes
tr = jtw[jtw$SA2_code_usual_residence_address != jtw$SA2_code_workplace_address,]

saveRDS(tr, file="artifacts/pairs.rds")

## parallel
s = split(tr, list(shard=as.character(as.integer(1:nrow(tr)/1500))))

t <- Sys.time()

cache.dir <- paste0("osm/gh/routing-graph-cache-", Sys.info()["user"])
res = parallel::mclapply(s, function(tr) {
  library(ghroute)
  router("osm/new-zealand-latest.osm.pbf", cache.dir)
  lapply(seq.int(nrow(tr)), function(i)
    tryCatch(route(tr$r.lat[i], tr$r.lon[i], tr$w.lat[i], tr$w.lon[i])[[1]][c(1,2,3,6)],
      error=function(e) { if (inherits(e, "GHRoutingError")) e$errors$toString() else e }))
  }, mc.cores=64)

as.numeric(Sys.time()) - as.numeric(t)

r = res[order(as.integer(names(res)))]
rt = do.call(c, r)

saveRDS(rt, file="artifacts/routes-car.rds")
