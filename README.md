# Avian GAP Biodiversity Analyses

This repository houses scripts, functions, and outputs from analyses of biodiversity by GAP Status and CAL FIRE habitat type, and of biodiversity trends in focal species across Avian Knowledge Network's sampling window. 

## Structure
### Analysis

Contains quarto document **indicator_analysis** with data-driven indicator species analysis and the resulting csv **focal_species_selection.csv** containing the final indicator species list for GAP and habitat type. The **models** subfolder contains all the quarto documents used to assess biodviersity with general linear mixed effect models.

### Outputs

Contains final figures resulting from analysis including tables from GLMM outputs

### R

Stores the functions used to complete analyses. 

**spatial_join.R**: Runs spatial join on avian data with USGS GAP status classifications and CALFIRE habitat type layers using transformed coordinates, producing a unified dataframe linking species observations to protection status and habitat type across California. Due to the initial granularity of the raster (30x30 meter cells) we aggregated the raster by a factor of 20.

**rich_gini**: Calculates rich-gini alpha biodiversity for each GAPxhabitat area in California's perimeter, resulting in a column added to the spatially joined dataframe. 

