library(tidyverse)
library(dplyr)
library(here)
library(readxl)

# Import PIF
pif <- read_excel(here("data", "ACAD Regional 2024.06.03 (1).xlsx"))

# Filter to California codes (5, 9, 15, 32, 33)
# BCR = Bird conservation regions
# pif_ca: dataset of regional concern birds (241 distinct species) for California regions
# Columns: 
# 
pif_ca <- pif %>% 
  filter(BCR == c(5, 9, 15, 32, 33))




