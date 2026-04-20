# Script for initial data import, cleaning, and joining


#----------------------------- Load libraries ----------------------------------
library(tidyverse) # version 2.0.0
library(sf) #1.0-21
library(here) #1.0.1
library(janitor) #2.2.1
library(stars) #0.6-8
library(terra) #1.8-42
library(arcgislayers)
library(tigris) #2.2.1
library(arrow) #23.0.1.2
library(sfarrow) #0.4.1
#-------------------------------------------------------------------------------

#---------------------Create Processed data folder (if none exists)------------
ifelse(dir.exists(here("data","data_processed")),print("Directory: data_processed exists"), dir.create(here("data","data_processed")))

#-------------------------------------------------------------------------------  

#----------------------------------CAL FIRE-------------------------------------
# Source geodatabase from CAL FIRE
gdb_path <- here::here('data', 'ds1327', 'tiff', 'ds1327.tif')

# habitat_type <- arc_open("58f81510e4c14b1da3c64823302f5101")
# habitat_type<-  arc_raster(habitat_type)

# Create raster object for habitat type layer
habitat_type <- rast(gdb_path)

# Fill raster with habitat type data layer
activeCat(habitat_type, layer = 1) <- 'LIFEFORM'

# Aggregate the raster so that our cells are no longer 30x30
habitat_agregated <- terra::aggregate(habitat_type, fact = 20, 
                 fun = 'modal')

# Reclassify raster to regain the categories
lev_full <- levels(habitat_type)[[1]]

# Set factors for lifeform for reclassification matrix
lev_full$lifeform_code <- as.integer(factor(lev_full$LIFEFORM, 
                                            levels = c('CONIFER',
                                                       'HARDWOOD',
                                                       'HERBACEOUS',
                                                       'SHRUB',
                                                       'URBAN',
                                                       'WATER',
                                                       'AGRICULTURE',
                                                       'BARREN/OTHER')))

# Reclassify based on the matrix
habitat_simple <- classify(habitat_agregated, 
                           rcl = as.matrix(lev_full[, c("Value", 
                                                        "lifeform_code")]))

# Drop extreme values
habitat_simple[habitat_simple == 65535 | habitat_simple == 0] <- NA

# Reset levels
levels(habitat_simple) <- data.frame(value = 1:8, LIFEFORM = c('CONIFER',
                                                               'HARDWOOD',
                                                               'HERBACEOUS',
                                                               'SHRUB',
                                                               'URBAN',
                                                               'WATER',
                                                               'AGRICULTURE',
                                                               "BARREN/OTHER"))

# Convert to polygons 
habitat_poly <- as.polygons(habitat_simple) %>% 
  st_as_sf()

# Write st data to parquet
st_write_parquet(habitat_poly, here('data', 'data_processed','habitat_polygon.parquet'))
#--------------------------------------------------------------------------------

#-------------------------------California Shape-------------------------------

# CA shape for calculating gap status 5 (no gap) area 
ca <- states(cb = TRUE) %>% 
  filter(STUSPS == "CA") %>% 
  st_transform(crs(habitat_type))


#--------------------------------------------------------------------------------

#----------------------------------GAP Data-------------------------------------
# Read in GAP data as a vector 

gap <- st_read(here::here('data',
                          'PADUS4_1_State_CA_GDB_KMZ', 
                          'PADUS4_1_StateCA.gdb'),
               layer = "PADUS4_1Fee_State_CA",
               quiet = TRUE) %>% 
  clean_names() 

gap_clean <- gap %>% 
  dplyr::select(own_type, gap_sts, mang_type, Shape) %>% 
  st_transform(crs(habitat_type)) %>%  # transform first
  st_cast("MULTIPOLYGON", warn = FALSE) %>%  # convert curves
  st_make_valid() %>%  # then fix validity
  group_by(gap_sts) %>%
  summarise(geometry = st_union(Shape), 
            .groups = 'drop')
#-------------------------------------------------------------------------------

#-----------------------------Calculate Gap Status 5---------------------------
gap_5 <- st_difference(ca, st_union(gap_clean)) 

gap_5$gap_sts = '5'

gap_5 <- gap_5 %>% 
  select(gap_sts)

gap_clean <- bind_rows(gap_clean, gap_5)

# Write st data to parquet
st_write_parquet(gap_clean, here('data', 'data_processed','gap_polygon.parquet'))
#-------------------------------------------------------------------------------

#------------------------------AKN data-----------------------------------------
point_count <- read_csv(here::here('data', 'point_count.csv')) %>% 
  clean_names() %>% 
  mutate(survey_type = 'Point Count') %>% 
  mutate(survey_duration = case_when(
    protocol_code == 'FR50_T10' ~ 10,
    TRUE ~ 5
  ))

area_search <- read_csv(here::here('data', 'area_search.csv')) %>% 
  clean_names() %>% 
  mutate(survey_type = 'Area Search') 

point_area_geo <- full_join(area_search, point_count) %>% 
  st_as_sf(coords = c("decimal_longitude", "decimal_latitude"), crs = 4326) %>% 
  st_transform(st_crs(habitat_type))
#-------------------------------------------------------------------------------

#---------------------------Area Calculations-----------------------------------

area_intersection <- st_intersection(gap_clean, habitat_poly)

area_intersection <- area_intersection %>% 
  mutate(area = st_area(geometry)) %>% 
  st_drop_geometry() %>% 
  rename(habitat_type = LIFEFORM)
  

#------------------------------Spatial Join-------------------------------------
# Join objects 
birds_joined <- st_join(point_area_geo, gap_clean["gap_sts"])

# Match vector 
habitat_id <- terra::extract(habitat_type, vect(birds_joined), ID = FALSE)[,1]

birds_joined$habitat_type <- habitat_id

birds_joined <- left_join(birds_joined, area_intersection) %>% 
  filter(habitat_type != 'BARREN/OTHER')

# Remove demo and test codes

birds_joined <- birds_joined %>% 
  filter(!grepl("DEMO",project_code)&
           ! project_code %in% 'DUMM'&
           ! grepl("Dummy",project_name) &
           ! grepl("Test", project_name))

# Assign protected and unprotected
birds_joined <- birds_joined %>% 
  mutate(protection_sts = case_when(
    gap_sts %in% c(3,4,5) ~ 'unprotected',
    TRUE~ 'protected'
  )) %>% 
  # and add sample_effort column
  group_by(year_collected, protocol_code) %>% 
  # There are some negative survey_duration, so we take the absolute value
  mutate(sample_effort = abs(survey_duration) * n()) %>% 
  ungroup() %>% 
  filter(!is.na(sample_effort))

# Drop columns unrelated to analysis 
birds_joined <- birds_joined %>% 
  select(global_unique_identifier, study_area, protocol_code, observation_date, year_collected, month_collected, survey_duration, scientific_name, common_name, species_code, observation_count, survey_type, habitat_type, geometry, gap_sts, area, protection_sts, sample_effort)

# Write st data to parquet
st_write_parquet(birds_joined, here('data', 'data_processed','birds_joined.parquet'))
#-------------------------------------------------------------------------------

rm(list = ls())
