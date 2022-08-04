This repository contains materials to reproduce the results in the paper
"A Statistical Method for Identifying Areas of High Mobility Applied to Commuting Data 
for the Country of New Zealand." 

# Manifest (list of directories and the files they contain)

├── PREPROCESSING-DESCRIPTION.txt - A description of how the data were processed.
├── artifacts - Data files derived from raw data for analysis.
│   ├── attractor-components.rds
│   ├── attractor-indices.rds
│   ├── big-component.rds
│   ├── directed-community.txt
│   ├── directed-graph.txt
│   ├── full-matrix-2018.rds
│   ├── full-matrix-2021.rds
│   ├── full-matrix.rds -> full-matrix-2018.rds
│   ├── mobility-graph.rds
│   ├── north-island-communities.rds
│   ├── order-evening.rds
│   ├── order-morning.rds
│   ├── prob-transition-matrix-evening.rds
│   ├── prob-transition-matrix-morning.rds
│   ├── resample-evening.rds
│   ├── resample-morning.rds
│   ├── routes-car.rds
│   ├── sa2-2018.rds
│   ├── sa2-2021.rds
│   ├── sa2.rds -> sa2-2018.rds
│   ├── stat-distr-data.rds
│   ├── strings-2018.rds
│   ├── strings-2021.rds
│   ├── strings.rds -> strings-2018.rds
│   ├── strong-components-and-stat-distr.rds
│   ├── total-traffic-count.rds
│   ├── trans-matrix-2018.rds
│   ├── trans-matrix-2021.rds
│   ├── trans-matrix.rds -> trans-matrix-2018.rds
│   └── transitions.rds
├── data - raw data and shape files.
│   ├── 2018-census-main-means-of-travel-to-work-by-statistical-a.csv
│   ├── statistical-area-2-2018-clipped-generalised.cpg
│   ├── statistical-area-2-2018-clipped-generalised.dbf
│   ├── statistical-area-2-2018-clipped-generalised.prj
│   ├── statistical-area-2-2018-clipped-generalised.shp
│   ├── statistical-area-2-2018-clipped-generalised.shx
│   ├── statistical-area-2-2021-clipped-generalised.dbf
│   ├── statistical-area-2-2021-clipped-generalised.prj
│   ├── statistical-area-2-2021-clipped-generalised.shp
│   └── statistical-area-2-2021-clipped-generalised.shx
├── scripts - scripts to perform the analysis
│   ├── aggr.R
│   ├── cluster-and-sd.r
│   ├── graph-vis.r
│   ├── metric-comparison.r
│   ├── plot-attr.R
│   ├── plot1.R
│   ├── plot2.R
│   ├── plot3.R
│   ├── plot4.R
│   ├── resample-big-component.r
│   ├── routes.R
│   └── strings.R
├── src - C code used to speed up preprocessing.
│   ├── foldw.c
│   ├── foldw.o
│   └── foldw.so
├── tmp - temporary spatial files used for visualization.
│   └── osm
│       └── 12
│           ├── 4035-2498.png
│           ├── 4035-2499.png
│           ├── 4035-2500.png
│           ├── 4035-2501.png
│           ├── 4035-2502.png
│           ├── 4036-2498.png
│           ├── 4036-2499.png
│           ├── 4036-2500.png
│           ├── 4036-2501.png
│           ├── 4036-2502.png
│           ├── 4037-2498.png
│           ├── 4037-2499.png
│           ├── 4037-2500.png
│           ├── 4037-2501.png
│           ├── 4037-2502.png
│           ├── 4038-2498.png
│           ├── 4038-2499.png
│           ├── 4038-2500.png
│           ├── 4038-2501.png
│           ├── 4038-2502.png
│           ├── 4039-2498.png
│           ├── 4039-2499.png
│           ├── 4039-2500.png
│           ├── 4039-2501.png
│           └── 4039-2502.png
└── visualizations - png files used in visualization.
    ├── degree-hists.png
    ├── edge-counts.png
    ├── fitted-vs-residuals.png
    ├── local-global-scatter.png
    ├── perm-test.png
    ├── signif-attractors.png
    ├── tail-probs.png
    ├── top-adjusted-p.png
    ├── trans-prob-test.png
    └── weighted-log-hists.png

# Instructions for running the scripts.

It is assumed that the scripts for preprocessing and anaysis, contained in the `scripts/`
directory are run from the current directory. It is assumed that the scripts are run
on a machine with unix functionality including the `ln` command line application. 
Below is a list of the files contained in the directory, their package dependencie, their 
input file dependencies, and their outputs. They can be run in the order they are 
presented to reproduce preprocessing, analysis, and visualization generation.

1. routes.R
  - Package dependencies
  - Input file dependencies
    1. ghroute (https://rforge.net/ghroute/)
    2. proj4 (https://rforge.net/proj4/)
    3. parallel 
    4. osm/new-zealand-latest.osm.pbf (can be created with "make -C osm")
    5. data/2018-census-main-means-of-travel-to-work-by-statistical-a.csv
  - Output file dependencies
    1. artifacts/transitions.rds
    2. artifacts/routes-car.rds
2. strings.R
  - Package dependencies
    1. sf
    2. parallel
  - Input file dependencies
    1. artifacts/routes-car.rds
    2. data/statistical-area-2-2018-clipped-generalised.shp
    3. src/foldw.c
  - Output file dependencies
    1. artifacts/sa2-2018.rds
    2. artifacts/strings-2018.rds
    3. artifacts/strings.rds
    4. artifacts/sa2.rds
3. agg.R
  - Package dependencies
    (None)
  - Input file dependencies
    1. artifacts/strings.rds
    2. artifacts/transitions.rds
  - Output files
    1. artifacts/full-matrix-2018.rds
    2. artifacts/full-matrix-2018.rds
    3. artifacts/trans-matrix-2018.rds
    4. artifacts/full-matrix.rds
    5. artifacts/trans-matrix.rds
4. plot1.R
  - Package dependencies
    1. sf
    2. snippets (https://rforge.net/snippets/)
    3. RColorBrewer
  - Input file dependencies
    1. src/foldw.c
    2. artifacts/routes-car.rds
    3. artifacts.sa2.rds
  - Output files
    1. visualizations/plot1.pdf
5. plot2.R
  - Package dependencies
    1. sf
    2. snippets (https://rforge.net/snippets/)
    3. RColorBrewer
  - Input file dependencies
    1. artifacts/trans-matrix.rds
    2. artifacts/transitions.rds
  - Output files
    1. visualizations/plot2.pdf
6. cluster-and-sd.r
  - Package dependencies
    1. dplyr
    2. tibble
    3. tidygraph
    4. graphmobility (https://github.com/kaneplusplus/graphmobility)
    5. igraph
    6. foreach
    7. checkmate
    8. Matrix
  - Input file dependencies
    1. artifacts/strings.rds
    2. artifacts/trans-matrix.rds
  - Output files
    1. artifacts/mobility-graph.rds
    2. artifacts/strong-components-and-stat-distr.rds
    3. artifacts/directed-graph.txt
    4. artifacts/north-island-communities.rds
7. plot3.R
  - Package dependencies
    1. sf
    2. snippets (https://rforge.net/snippets/)
    3. RColorBrewer
  - Input file dependencies
    1. data/statistical-area-2-2018-clipped-generalised.shp
    2. artifacts/strong-components-and-stat-distr.rds
  - Output files
    1. visualizations/plot3.pdf
8. plot4.R
  - Package dependencies
    1. sf
    2. snippets (https://rforge.net/snippets/)
    3. RColorBrewer
  - Input file dependencies
    1. data/statistical-area-2-2018-clipped-generalised.shp
    2. artifacts/north-island-communities.rds
  - Output files
    1. visualizations/plot4.pdf
9. resample-big-component
  - Package dependencies
    1. tidygraph
    2. igraph
    3. graphmobility (https://github.com/kaneplusplus/graphmobility)
    4. foreach
    5. doMC
    6. ggplot2
    7. tibble
    8. tidyr
    9. Matrix
  - Input file dependencies
    1. artifacts/strings.rds
    2. artifacts/mobility-graph.rds
  - Output files
    1. artifacts/big-component.rds
    2. artifacts/prob-transition-matrix-morning.rds
    3. artifacts/resample-morning.rds
    4. artifacts/resample-evening.rds
    5. artifacts/prob-transition-matrix-evening.rds
    6. artifacts/stat-distr-data.rds
10. graph-vis.r
  - Package dependencies
    1. ggplot2
    2. dplyr
    3. tidyr
    4. graphmobility (https://github.com/kaneplusplus/graphmobility)
    5. igraph
    6. tidygraph
  - Input file dependencies
    1. artifacts/stat-distr-data.rds
    2. artifacts/prob-transition-matrix-morning.rds
    3. artifacts/prob-transition-matrix-evening.rds
    4. artifacts/big-component.rds
    5. artifacts/resample-evening.rds
    6. artifacts/stat-distr-data.rds
  - Output files
    1. visualizations/perm-test.png
    2. visualizations/trans-prob-test.png
    3. visualizations/edge-counts.png
    4. visualizations/weighted-log-hists.png
    5. visualizations/degree-hists.png
    6. visualizations/tail-probs.png
    7. artifacts/resample-morning.rds
    8. artifacts/stat-distr-data.rds
    9. visualizations/signif-attractors.png
    10. visualizations/top-adjusted-p.png
    11. artifacts/attractor-indices.rds
    12. artifacts/mobility-graph.rds
    13. artifacts/attractor-components.rds
11. metric-comparison.r
  - Package dependencies
    1. tidygraph
    2. dplyr
    3. tidyr
    4. sf
    5. parallel
    6. patchwork
    7. randomForestSRC
  - Input file dependencies
    1. artifacts/strings.rds
    2. artifacts/big-component.rds
    3. artifacts/transitions.rds
    4. data/statistical-area-2-2018-clipped-generalised.shp
    5. artifacts/routes-car.rds
    6. artifacts/total-traffic-count.rds
    7. artifacts/attractor-indices.rds
    8. artifacts/attractor-indices.rds
  - Output files
    1. artifacts/total-traffic-count.rds
    2. visualizations/local-global-scatter.png
    3. visualizations/fitted-vs-residuals.png
12. plot-attr.R
  - Package dependencies
    1. sf
    2. snippets (https://rforge.net/snippets/)
    3. RColorBrewer
  - Input file dependencies
    1. data/statistical-area-2-2018-clipped-generalised.shp
    2. artifacts/attractor-components.rds
    3. artifacts/stat-distr-data.rds
  - Output files
    1. visualizations/plot-attr.pdf
