#!/bin/sh
# Simplifies shape files 
# (C)2022 Simon Urbanek <simon.urbanek@R-project.org>
# License: MIT
#
# SHP -> GeoJSON -> TopoJSON -thin-> TopoJSON -> GeoJSON -> SHP
# Requires: R (Rscript) with sf package, nodejs+npm (for topojson tools)
# Preserves CRS

if [ -z "$3" ]; then
    echo ''
    echo " Usage: $0 <input.shp> <output.shp> <tolerance>"
    echo ''
    echo ' NOTE: arguments should not contain spaces and both'
    echo '       filenames must end with .shp'
    echo ''
    exit 1
fi

## fail on error
set -e

## need to create a few temporary files
if [ -z "$TMPDIR" ]; then TMPDIR=/tmp; fi
FN="$TMPDIR/thin-shp-$$"

echo " - convert $1 to GeoJSON"
Rscript -e 'sf::write_sf(sf::read_sf("'$1'"), "'$FN'.geojson")'

echo " - convert from GeoJSON to TopoJSON"
npx geo2topo < "$FN.geojson" > "$FN.topojson"

echo " - simplify with tolerance $3"
npx toposimplify -p "$3" < "$FN.topojson" > "$FN-s.topojson"

echo " - convert from TopoJSON to GeoJSON"
npx topo2geo - < "$FN-s.topojson" > "$FN-s.geojson"

echo " - convert from GeoJSON to $2"
## TopoJSON does not preserve CRS so we force-copy it from the original
Rscript -e 'd=sf::read_sf("'$FN'-s.geojson");s=sf::read_sf("'$1'");sf::st_crs(d)=NA;sf::st_crs(d)=sf::st_crs(s);sf::write_sf(d, "'$2'")'

echo " - clean up temporary files"
rm -f "$FN.geojson" "$FN.topojson" "$FN-s.topojson" "$FN-s.geojson"

echo " - done"
