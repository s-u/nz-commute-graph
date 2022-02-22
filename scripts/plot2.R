packages <- c("sf", "snippets", "RColorBrewer")
for (pkg in packages) library(pkg, character.only=TRUE)

if (!file.exists("data/2018-census-main-means-of-travel-to-work-by-statistical-a.csv")) stop("Please run this from the project root")

sa = st_read("data/statistical-area-2-2018-clipped-generalised.shp")
geo = st_geometry(sa)
geoll = st_transform(geo, crs=4326)
g=readRDS("artifacts/trans-matrix.rds")
gc = t(sapply(geoll, st_centroid))

## tag pairs with one ID
g$pair=as.integer(factor(ifelse(g$from > g$to, paste(g$to, g$from), paste(g$from, g$to))))

## compute max per pair
px = unlist(lapply(split(g$count, g[,"pair"]), max))

## dominant if that direction is the max
g$dom = g$count == px[g$pair]

## drawing order - from thickest to thinnest
so=order(g$count, decreasing=TRUE)

## for paper:
for.paper <- identical(Sys.getenv("FOR_PAPER"),"1")
if (for.paper) pdf("visualizations/plot2.pdf", 15, 11)

par(mar=rep(0,4))
# bb = c(174.74252, 174.92753, -36.94245, -36.83235)
bb = c(174.70, 174.92753, -36.94245, -36.8)
plot(bb[1:2], bb[3:4], ty='n', axes=F, asp=1/cos(mean(bb[3:4])/180*pi))
bb = par("usr")
osmap(cache.dir="tmp/osm")
in.view = st_intersects(st_polygon(list(matrix(bb[c(1,2,2,1,1,3,3,4,4,3)],5))), geoll)[[1]]
plot(geoll[in.view], add=T, col=NA, border=1, lty=2)

arrows(gc[g$from,1][so], gc[g$from,2][so], gc[g$to,1][so], gc[g$to,2][so],
       lwd=(sqrt(g$count)/224 * 30)[so], col=ifelse(g$dom, "#DF536B", "#2297E6")[so],
       angle=10, length=.3)

lw = c(100, 500, 2500, 5000, 10000); par(lend=1); legend("topright",,c(ifelse(lw >= 1000, gsub(".",",",sprintf("%.3f", lw/1000), fixed=TRUE), lw),"","major direction","counter-major"),lwd=c(sqrt(lw)/224 * 30, 0, 6, 6), col=c(rep("#DF536B", length(lw)), NA, "#DF536B","#2297E6"), bg="#ffffffa0", seg.len=3, inset=0.05)

if (for.paper) dev.off()

