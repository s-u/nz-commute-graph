packages <- c("sf", "snippets", "RColorBrewer")

if (!file.exists("data/2018-census-main-means-of-travel-to-work-by-statistical-a.csv")) stop("Please run this from the project root")

source("scripts/common.R")

use.packages(packages)

sa = st_read("data/statistical-area-2-2018-clipped-generalised.shp")
geo = st_geometry(sa)
geoll = st_transform(geo, crs=4326)

dev.start("plot-attr", 8, 8)
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
asc = split(a, a$strong_comp)
ageo = lapply(asc, function(d) st_union(geoll[as.integer(d$name)]))

## draw components and their IDs
for (i in seq_along(ageo)) {
    plot(ageo[[i]], add=TRUE, col=NA, border=1)
    cx <- as.vector(st_centroid(ageo[[i]])[[1]])
    text(cx[1],cx[2], names(ageo)[i])
}

dev.end()

dev.start("loci-maps", 8.3, 11.7)
par(mar=rep(0,4))

## split the loci according to areas, i.e., join loci
## that are close to each other. We just use centroid
## distance for this.
ct = t(sapply(seq_along(ageo), function(i) as.vector(st_centroid(ageo[[i]])[[1]])))
cd = as.matrix(dist(ct))
li = sapply(seq_along(ageo), function(i) which(cd[i,] < 0.3)[1])
lg = as.integer(factor(li))

ta = st_simplify(readRDS("artifacts/nz-simpl100.rds"),,1000)
tgeo = st_geometry(ta)
tgeoll = st_transform(tgeo, crs=4326)

## draw scale measure
dscale <- function(dist=1, lwd=3) {
    p <- par("usr")
    f <- cos(mean(p[3:4])/180*pi)
    km <- dist / 111.3194 
    c <- km / f
    cmx <- diff(p[1:2]) / par("din")[1] / 2.54
    cmy <- diff(p[3:4]) / par("din")[2] / 2.54
    x1 <- p[2] - 0.5 * cmx
    x2 <- p[2] - 0.5 * cmx - c
    y  <- p[3] + 0.5 * cmy
    segments(x1, y, x2, y, lend=1, lwd=lwd)
    segments(c(x1, x2), y - 0.3 * cmy, c(x1, x2), y + 0.3 * cmy, lend=1)
    text(mean(c(x1, x2)), y + 0.5 * cmy, paste(dist, "km"), adj=c(0.5, 0))
}


par(mfrow=c(4,2))

## draw north-island loci overview
ptn = c("Rotorua", "Wellington", "Wellington", "Palmerston\nNorth", "Hamilton",
        "Auckland", "Auckland", "Auckland", "Kaitaia", "Kerikeri")
bb = st_bbox(geoll[as.integer(a$name)])[c(1,3,2,4)]
plot(bb[1:2], bb[3:4], ty='n', axes=F, asp=1/cos(mean(bb[3:4])/180*pi))
plot(tgeoll, col="#ffffe0", add=TRUE, border="#b0b0b0")
#osmap(cache.dir="/tmp/qq")

for (i in unique(li)) # points(ct[i,1],ct[i,2], cex=2, col=2, pch=19)
    text(ct[i,1],ct[i,2], ptn[i], adj=c(-.2, 0.5))

for (i in seq_along(ageo))
    plot(ageo[[i]], add=TRUE, col="#ff8080", border="#ff8080")

dscale(100)

xsd = sapply(split(seq_along(lg), lg), function(o)
    max(do.call("rbind", asc[o])$sd0))

## draw the locations individually
for (o in split(seq_along(lg), lg)[order(xsd, decreasing=TRUE)]) {
    bb = st_bbox(do.call("c",ageo[o]))[c(1,3,2,4)]
    min.latd <- 0.1
    if (diff(bb[3:4]) < min.latd) bb[3:4] <- mean(bb[3:4]) + min.latd * c(-0.5, 0.5)
    plot(bb[1:2], bb[3:4], ty='n', axes=F, asp=1/cos(mean(bb[3:4])/180*pi))
    bb = par("usr")
    osmap(cache.dir="/tmp/qq")

    c = do.call("rbind", asc[o])
    ci = as.integer(c$name)
    col = col.fn(c$sd0, col=heat.colors(32, alpha = 0.7))
    print(range(c$sd0))

    ## draw individual SA2
    for (i in seq_along(ci))
        plot(geoll[[ci[i]]], add=TRUE, col=col[i], border="#00000040")

    ## draw the loci for this area
    for (i in seq_along(ageo)) {
        plot(ageo[[i]], add=TRUE, col=NA, border=1, lwd=1)
        #cx <- as.vector(st_centroid(ageo[[i]])[[1]])
        #text(cx[1],cx[2], names(ageo)[i])
    }

    text(bb[1], bb[4], ptn[li[o[1]]], cex=1.6, adj=c(-0.2, 2.5))
    box()
    dscale()
}

