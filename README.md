# Avian GAP Biodiversity Analyses

This repository houses scripts, functions, and outputs from analyses of biodiversity by GAP Status and CAL FIRE habitat type, and of biodiversity trends in focal species across Avian Knowledge Network's sampling window. 

## Structure

├── Analysis
│   ├── focal_species_selection.csv
│   ├── indicator_analsysis.qmd
│   └── Models
│       ├── glmm_tests.qmd
│       ├── GLMM.qmd
│       └── population_trends.qmd
├── Avian_GAP_Analysis.Rproj
├── data
│   ├── ACAD Regional 2024.06.03.xlsx
│   ├── area_search.csv
│   ├── data_processed
│   ├── ds1327
│   ├── fveg22_1.gdb
│   ├── PADUS4_1_State_CA_GDB_KMZ
│   ├── point_count.csv
│   ├── secretive_marshbird.csv
│   └── tl_2025_us_county
├── Outputs
│   ├── figures
│   ├── storymap.qmd
│   └── technical_memo_figures.qmd
├── R
│   ├── adj_fit_agg.R
│   ├── adj_fit_disagg.R
│   ├── autocorrelation_tests.R
│   ├── data_import.qmd
│   ├── fit_agg.R
│   ├── fit_disagg.R
│   ├── gt_summary.R
│   ├── partners_in_flight.R
│   ├── point_blue_theme.R
│   ├── population_trend.R
│   ├── rich_gini.R
│   ├── spatial_join.R
│   └── testing_species_obs.R
└── README.md



### Analysis

Contains quarto document **indicator_analysis** with data-driven indicator species analysis and the resulting csv **focal_species_selection.csv** containing the final indicator species list for GAP and habitat type. The **models** subfolder contains all the quarto documents used to assess biodviersity with general linear mixed effect models.

### Outputs

Contains final figures resulting from analysis including tables from GLMM outputs

### R

Stores the functions used to complete analyses. 

**spatial_join.R**: Runs spatial join on avian data with USGS GAP status classifications and CALFIRE habitat type layers using transformed coordinates, producing a unified dataframe linking species observations to protection status and habitat type across California. Due to the initial granularity of the raster (30x30 meter cells) we aggregated the raster by a factor of 20. *Start Here*

**rich_gini**: Calculates rich-gini alpha biodiversity for each GAPxhabitat area in California's perimeter, resulting in a column added to the spatially joined dataframe. 

