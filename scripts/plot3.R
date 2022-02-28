packages <- c("sf", "snippets", "RColorBrewer")
for (pkg in packages) library(pkg, character.only=TRUE)

if (!file.exists("data/2018-census-main-means-of-travel-to-work-by-statistical-a.csv")) stop("Please run this from the project root")

sa = st_read("data/statistical-area-2-2018-clipped-generalised.shp")
geo = st_geometry(sa)
geoll = st_transform(geo, crs=4326)

sc = readRDS("artifacts/strong-components-and-stat-distr.rds")
top.sc = as.integer(names(sort(table(sc$component), decreasing=TRUE)))[1:8]
sc.r = match(sc$component, top.sc)

for.paper <- identical(Sys.getenv("FOR_PAPER"),"1")
if (for.paper) pdf("visualizations/plot3.pdf", 8, 8)

par(mar=rep(0,4))
old.pal <- palette()
cols <- RColorBrewer::brewer.pal(9,"Set1")
cols[6] <- "#a0c022"
palette(cols)
plot(geoll[as.integer(sc$name)], col=ifelse(is.na(sc.r), "#d0d0a0", sc.r),
     border=NA) #"#0000020")
plot(geoll[!seq.int(length(geoll)) %in% as.integer(sc$name)],
     col="#ffffb0", add=TRUE, border=NA)
legend("topleft",,c(paste("component", 1:8), "other", "no transitions"), bty='n', pt.cex=1.5,
       col=c(palette()[1:8], "#d0d0a0", "#ffffb0"), pch=15, inset=0.02)
palette(old.pal)

if (for.paper) dev.off()
