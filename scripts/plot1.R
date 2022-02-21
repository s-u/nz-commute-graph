packages <- c("sf", "snippets", "RColorBrewer")
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

i = 1

    ls   = st_linestring(a[[i]]$points[,2:1])
    lsnz = st_transform(st_sfc(ls, crs=4326), st_crs(geo))
    lsl  = st_as_sf(data.frame(x=a[[i]]$points[,2], y=a[[i]]$points[,1]), coords=c("x","y"), crs=4326)
    int  = st_intersection(lsnz, geo)
    wid <- st_is_within_distance(lsl, geoll[attr(int, "idx")[,2]], 10)
    fwid = .Call("foldw", wid)
    trs = attr(int, "idx")[rle(fwid)$values,2]

for.paper <- identical(Sys.getenv("FOR_PAPER"),"1")
if (for.paper) pdf("plot1.pdf", 7, 7)

par(mar=rep(0,4))
plot(geoll[trs])
osmap(cache.dir="tmp/osm")
plot(geoll[trs], add=T, lwd=1, col=paste0(brewer.pal(9, "Set1"), "30"))
segments(a[[i]]$points[-length(fwid),2], a[[i]]$points[-length(fwid),1],
         a[[i]]$points[-1,2], a[[i]]$points[-1,1],
         col=brewer.pal(9, "Set1")[fwid], lwd=3)
text(t(sapply(geoll[trs], st_centroid)),, seq_along(trs), cex=1.5)

if (for.paper) dev.off()
