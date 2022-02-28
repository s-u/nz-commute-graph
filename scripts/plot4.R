packages <- c("sf", "snippets", "RColorBrewer")
for (pkg in packages) library(pkg, character.only=TRUE)

if (!file.exists("data/2018-census-main-means-of-travel-to-work-by-statistical-a.csv")) stop("Please run this from the project root")

sa = st_read("data/statistical-area-2-2018-clipped-generalised.shp")
geo = st_geometry(sa)
geoll = st_transform(geo, crs=4326)

co = readRDS("artifacts/north-island-communities.rds")

for.paper <- identical(Sys.getenv("FOR_PAPER"),"1")
if (for.paper) pdf("visualizations/plot4.pdf", 8, 8)

par(mar=rep(0,4))
old.pal <- palette()
cols <- RColorBrewer::brewer.pal(9,"Set1")
cols[6] <- "#a0c022"
palette(cols)
plot(geoll[as.integer(co$name)], col=(co$community - 1L) %% 9 + 1,
     border=NA) #"#0000020")
plot(geoll[!seq.int(length(geoll)) %in% as.integer(co$name)],
     col="#ffffb0", add=TRUE, border=NA)
palette(old.pal)

d=structure(list(city = c("Auckland", "Wellington", "Hamilton", 
"Tauranga", "Palmerston North", "Napier", "Rotorua", "New Plymouth", 
"Whangarei", "Whanganui", "Taupo"), lat = c(-36.85, -41.2889, 
-37.7833, -37.6858, -40.3549, -39.4833, -38.1378, -39.0578, -35.725, 
-39.9333, -38.69), lng = c(174.7833, 174.7772, 175.2833, 176.1667, 
175.6095, 176.9167, 176.2514, 174.0742, 174.3236, 175.05, 176.08
)), class = "data.frame")
text(d$lng, d$lat, d$city)

if (for.paper) dev.off()
