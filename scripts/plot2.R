packages <- c("sf", "snippets", "RColorBrewer")
for (pkg in packages) library(pkg, character.only=TRUE)

if (!file.exists("data/2018-census-main-means-of-travel-to-work-by-statistical-a.csv")) stop("Please run this from the project root")

sa = readRDS("artifacts/sa2.rds")
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

tiles <- function() osmap(cache.dir="tmp/osm")

## for paper:
for.paper <- identical(Sys.getenv("FOR_PAPER"),"1")
dev.page <- 0
dev.start <- if (for.paper) function() pdf(paste0("visualizations/plot2-", (dev.page <<- dev.page + 1L), ".pdf"), 16, 12) else function() NULL
dev.end <- if (for.paper) function() dev.off() else function() NULL

## draw flows
dev.start()
scale <- 0.9
par(mar=rep(0,4))
bb = c(174.70, 174.92753, -36.94245, -36.8)
plot(bb[1:2], bb[3:4], ty='n', axes=F, asp=1/cos(mean(bb[3:4])/180*pi))
bb = par("usr")
tiles()

in.view = st_intersects(st_polygon(list(matrix(bb[c(1,2,2,1,1,3,3,4,4,3)],5))), geoll)[[1]]
plot(geoll[in.view], add=T, col=NA, border=1, lty=2)

arrows(gc[g$from,1][so], gc[g$from,2][so], gc[g$to,1][so], gc[g$to,2][so],
       lwd=scale*(sqrt(g$count)/224 * 30)[so], col=ifelse(g$dom, "#DF536B", "#2297E6")[so],
       angle=10, length=.3*scale)

lw = c(100, 500, 2500, 5000, 10000); par(lend=1); legend("topright",,c(ifelse(lw >= 1000, gsub(".",",",sprintf("%.3f", lw/1000), fixed=TRUE), lw),"","major direction","counter-major"),lwd=c(scale * sqrt(lw)/224 * 30, 0, 6, 6), col=c(rep("#404040", length(lw)), NA, "#DF536B","#2297E6"), bg="#ffffffa0", seg.len=3, inset=0.05, cex=1.3)

dev.end()

## baseline distributions in the same region

## aggregate work/home endpoint counts from transitions
tr = readRDS("artifacts/transitions.rds")
tr0 = function(x) { x[x < 0] = 0; x }
cts = rowSums(tr0(tr[,10:12]))
work.ct = aggregate(cts, tr[,"SA2_code_workplace_address",drop=F], sum, na.rm=T)
home.ct = aggregate(cts, tr[,"SA2_code_usual_residence_address",drop=F], sum, na.rm=T)

sa$work.ct=work.ct$x[match(sa[[1]], work.ct$SA2_code_workplace_address)]
sa$home.ct=home.ct$x[match(sa[[1]], home.ct$SA2_code_usual_residence_address)]
sa$work.ct[is.na(sa$work.ct)] = 0
sa$home.ct[is.na(sa$home.ct)] = 0

col.fn = function(x, fn=sqrt, col=heat.colors(32, alpha=0.7)) col[fn(x / max(x, na.rm=TRUE)) * (length(col) - 1) + 1]

## home
dev.start()
par(mar=rep(0,4))
plot(bb[1:2], bb[3:4], ty='n', axes=F, asp=1/cos(mean(bb[3:4])/180*pi))
tiles()
plot(geoll[in.view], add=T, col=col.fn(sa$home.ct[in.view]), border="#00000040")
dev.end()

## work
dev.start()
par(mar=rep(0,4))
plot(bb[1:2], bb[3:4], ty='n', axes=F, asp=1/cos(mean(bb[3:4])/180*pi))
tiles()
plot(geoll[in.view], add=T, col=col.fn(sa$work.ct[in.view]), border="#00000040")
dev.end()

## take counts from the matrix
m = readRDS("artifacts/trans-matrix.rds")

m.to = aggregate(m$count, m[,"to", drop=FALSE], sum, na.rm=TRUE)
sa$to = numeric(nrow(sa))
sa$to[m.to$to] = m.to$x

m.from = aggregate(m$count, m[,"from", drop=FALSE], sum, na.rm=TRUE)
sa$from = numeric(nrow(sa))
sa$from[m.from$from] = m.from$x

# from to -- from and to are almost the same by definition
dev.start()
par(mar=rep(0,4))
plot(bb[1:2], bb[3:4], ty='n', axes=F, asp=1/cos(mean(bb[3:4])/180*pi))
tiles()
plot(geoll[in.view], add=T, col=col.fn(sa$to[in.view]), border="#00000040")
dev.end()
