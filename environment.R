# ============================================================
# Reproducible package environment — BirdWatch Avian GAP Analysis
# ============================================================

# ==== Install packages ====
# Run this once, then comment out

# install.packages(c(
#   "tidyverse",
#   "sf",
#   "stars",
#   "terra",
#   "tidyterra",
#   "sfarrow",
#   "here",
#   "janitor",
#   "readxl",
#   "glmmTMB",
#   "broom.mixed",
#   "lmtest",
#   "spdep",
#   "indicspecies",
#   "gtsummary",
#   "gtExtras",
#   "kableExtra",
#   "tmap",
#   "paletteer",
#   "patchwork",
#   "showtext",
#   "webshot2", 
#   "marginaleffects",
#   "ggeffects",
# ))

# ==== Load packages ====

library(sf) # 1.0-24
library(stars) # 0.6-8
library(terra) # 1.8-60
library(tidyterra) # 0.7.2
library(sfarrow) # 0.4.1
library(tidyverse)# 2.0.0
library(here) # 1.0.2
library(janitor) # 2.2.1
library(readxl) # 1.4.5
library(glmmTMB) # 1.1.13
library(broom.mixed) # 0.2.9.6
library(lmtest) # 0.9-40
library(spdep) # 1.4-2
library(indicspecies) # 1.8.0
library(gt) # 1.2.0
library(gtsummary) # 2.5.0
library(gtExtras) # 0.6.1
library(kableExtra)# 1.4.0
library(tmap) # 4.2
library(paletteer) # 1.7.0
library(patchwork) # 1.3.2
library(showtext) # 0.9-7
library(webshot2) # 0.1.2
library(marginaleffects) # 0.32.0
library(ggeffects) # 2.3.2
