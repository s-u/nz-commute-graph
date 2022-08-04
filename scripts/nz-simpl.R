## this one is really slow, so we cache teh result
if (!file.exists("artifacts/nz-simpl100.rds")) {
    library(sf)
    s = st_read("data/territorial-authority-2018-clipped-generalised.shp")
    ## this is the really slow part ...
    ## (we have to do the union first, because simplification
    ## in sf is too does not consider joint boundaries)
    su = st_union(s)
    ## simplify to 100m
    sus = st_simplify(su,, 100)
    saveRDS(sus, file="artifacts/nz-simpl100.rds")
}
