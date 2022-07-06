## common functions

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

tiles <- function() osmap(cache.dir="tmp/osm")

col.fn <- function(x, fn=sqrt, col=heat.colors(32, alpha=0.7)) col[fn(x / max(x, na.rm=TRUE)) * (length(col) - 1) + 1]

for.paper <- identical(Sys.getenv("FOR_PAPER"),"1")

dev.end <- if (for.paper) function() dev.off() else function() NULL

year <- if (nzchar(year <- Sys.getenv("YEAR"))) as.integer(year)

cfn <- function(what, suff=".rds") if (is.null(year)) paste0("artifacts/", what, suff) else paste0("artifacts/", what, "-", year, suff)

