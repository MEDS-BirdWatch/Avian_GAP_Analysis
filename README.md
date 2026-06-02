# Avian GAP Biodiversity Analyses

This repository houses scripts, functions, and outputs from analyses of biodiversity by GAP Status and CAL FIRE habitat type, and of biodiversity trends in focal species across Avian Knowledge Network's sampling window (1990-2025).

## Structure

```         
├── Analysis
│   ├── focal_species_selection.csv
│   ├── indicator_analsysis.qmd
│   └── Models
│       ├── GLMM.qmd
│       └── population_trends.qmd
├── Avian_GAP_Analysis.Rproj
├── data
│   ├── ACAD Regional 2024.06.03.xlsx
│   ├── area_search.csv
│   ├── data_processed
│   ├── fveg22_1.gdb
│   ├── PADUS4_1_State_CA_GDB_KMZ 
│   └── point_count.csv
├── Outputs
│   ├── figures
│   └── technical_memo_figures.qmd
├── R
│   ├── adj_fit_agg.R
│   ├── adj_fit_disagg.R
│   ├── autocorrelation_tests.R
│   ├── fit_agg.R
│   ├── fit_disagg.R
│   ├── partners_in_flight.R
│   ├── point_blue_theme.R
│   ├── population_trend.R
│   ├── rich_gini.R
│   └── spatial_join.R
└── README.md
```

### Analysis

Contains quarto document **indicator_analysis** to run an indicator analysis using the {indicspecies} R package to obtain a list of possible focal species for each habitat type with a data-driven approach. The output is csv **focal_species_selection.csv** containing the final indicator species list for GAP and habitat type. 

The **models** subfolder contains all the quarto documents used to assess biodviersity with general linear mixed effect models.

### Outputs

Contains quarto document **technical_memo_figures.qmd** used to create all final figures generated in R to reflect patterns and trends in the data. **figures** subfolder contains all figures from technical memorandum quarto and those resulting from analysis including tables from GLMM outputs.

### R

Stores the functions used to complete analyses, including sourcing script and rich-gini-simpson function critical to running all analyses.

**spatial_join.R**: Runs spatial join on avian data with USGS GAP status classifications and CALFIRE habitat type layers using transformed coordinates, producing a unified dataframe linking species observations to protection status and habitat type across California. Due to the initial granularity of the raster (30x30 meter cells) we aggregated the raster by a factor of 20.

**rich_gini**: Calculates rich-gini alpha biodiversity for each GAPxhabitat area in California's perimeter, resulting in a column added to the spatially joined dataframe.

## Workflow

-   Download data from given sources

| Data Source | Type | Link | What to Download |
|------------------|------------------|------------------|------------------|
| USGS GAP Project | Protected areas polygons (GAP status) | [USGS GAP](https://www.sciencebase.gov/catalog/item/6759abcfd34edfeb8710a004) | `PADUS4_1_State_CA_GDB_KMZ` |
| CAL FIRE Vegetation by Wildlife Habitat Relationships 2022 | California vegetation habitat raster | [FVEG22](https://www.fire.ca.gov/what-we-do/fire-resource-assessment-program/gis-mapping-and-data-analytics) | `Vegetation by Wildlife Habitat Relationships 2022` |
| Avian Knowledge Network | Bird observation survey data (point count & area search) | [AKN](https://avianknowledge.net) | `point count` and `area search` |
| Partners in flight | Avian species of interest | [PIF](https://pif.birdconservancy.org/avian-conservation-assessment-database-scores/)| `ACAD Regional your.date.xlsx |
-   Next run `R/spatial_join.R `

-   Next run `Analysis/indicator_analysis.qmd`

-   Next run `Analysis/Models/population_trends.qmd`

-   Next run `Analysis/Models/GLMM.qmd`

-   For figures you may run technical_memo_figures.qmd
