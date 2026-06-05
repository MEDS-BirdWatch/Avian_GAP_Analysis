# ============================================================
# Reproducible package environment — BirdWatch Avian GAP Analysis
# ============================================================

# === Install pacman ===
# install.packages("pacman")

# === Load packages ===
pacman::p_load(
  sf,              # 1.0-24
  stars,           # 0.6-8
  terra,           # 1.8-60
  tigris,          # 2.2.1
  tidyterra,       # 0.7.2
  sfarrow,         # 0.4.1
  arrow,           # 23.0.1.2
  tidyverse,       # 2.0.0
  here,            # 1.0.2
  janitor,         # 2.2.1
  readxl,          # 1.4.5
  glmmTMB,         # 1.1.13
  broom.mixed,     # 0.2.9.6
  lmtest,          # 0.9-40
  spdep,           # 1.4-2
  indicspecies,    # 1.8.0
  gt,              # 1.2.0
  gtsummary,       # 2.5.0
  gtExtras,        # 0.6.1
  kableExtra,      # 1.4.0
  tmap,            # 4.2
  paletteer,       # 1.7.0
  patchwork,       # 1.3.2
  showtext,        # 0.9-7
  webshot2,        # 0.1.2
  marginaleffects, # 0.32.0
  ggeffects        # 2.3.2
)

