packages <- c("sf", "snippets", "RColorBrewer")
for (pkg in packages) library(pkg, character.only=TRUE)

if (!file.exists("data/2018-census-main-means-of-travel-to-work-by-statistical-a.csv")) stop("Please run this from the project root")

sa = st_read("data/statistical-area-2-2018-clipped-generalised.shp")
geo = st_geometry(sa)
geoll = st_transform(geo, crs=4326)

tiles <- function() osmap(cache.dir="tmp/osm")
col.fn <- function(x, fn=sqrt, col=heat.colors(32, alpha=0.7)) col[fn(x / max(x, na.rm=TRUE)) * (length(col) - 1) + 1]

## for paper:
for.paper <- identical(Sys.getenv("FOR_PAPER"),"1")
dev.start <- if (for.paper) function() pdf(paste0("visualizations/plot-attr.pdf"), 8, 8) else function() NULL
dev.end <- if (for.paper) function() dev.off() else function() NULL

dev.start()
par(mar=rep(0,4))

bb = c(174.70, 174.92753, -36.94245, -36.8)
plot(bb[1:2], bb[3:4], ty='n', axes=F, asp=1/cos(mean(bb[3:4])/180*pi))
bb = par("usr")
tiles()

## attractors
a = readRDS("artifacts/attractor-components.rds")
sdd = readRDS("artifacts/stat-distr-data.rds")
a$sd = sdd[[2]][match(as.integer(a$name), sdd$vertex_id)]
a$sd0 = ifelse(a$sd < 0, 0, a$sd)
plot(geoll[as.integer(a$name)], col=col.fn(a$sd0), border="#00000040", add=TRUE)

## create geometry for components
ageo = lapply(split(a, a$strong_comp), function(d) st_union(geoll[as.integer(d$name)]))

## draw components and their IDs
for (i in seq_along(ageo)) {
    plot(ageo[[i]], add=TRUE, col=NA, border=1)
    cx <- as.vector(st_centroid(ageo[[i]])[[1]])
    text(cx[1],cx[2], names(ageo)[i])
}

dev.end()
