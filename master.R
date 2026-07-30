# Libraries ####
library(tidyr)
library(dplyr)
library(lubridate)
library(sf)
library(tmap)
library(here)
library(RColorBrewer) # col palette
library(shinyjs)
library(terra) # for buffers
library(mapview)
library(profmem)
library(bench)
library(purrr) # for map function to intersect each dataset
library(data.table)
library(fst)
library(ggplot2)
library(gt)
library(ggspatial)
library(patchwork)
library(tidyverse)
library(openxlsx)
library(zoo)

# Functions ####
source("scripts/functions.R") 

# Munic polygons, population...####
# Municipality polygons
com2 <- st_read(here("data/geo", "COMMUNE_FRMETDROM.shp")) # note : need to find date of these communes. Should be 2023-2024
# Lambert coordinate system: lat and long coordinates based on lambert 
lambert_conv <- st_read(here("data/geo", "shp-sim-france.shp"))
# extended lambert
lambert_conv_ext <- st_read(here("data/geo", "coordonnees_grille_safran_lambert-2-etendu.csv")) 
# corresp bdi_cod 2023 and insee code 2023
bdi_insee <- readxl::read_xlsx(here("data/geo/bdi_cod_insee_2023.xlsx"))
# population per 200m grid
# Opening data with vect: faster 
car <- vect(here("data/pop_communes/200m-carreaux-metropole", "car_m.dbf"))      # attributes
car_geo <- vect(here("data/pop_communes/200m-carreaux-metropole", "car_m.mif"))  # geometries

# Temperature data SAFRAN: 8km grid ####
temp_20_25<- read.csv2("data/donnees_sim_safran/QUOT_SIM2_previous-2020-202506.csv.gz")
temp_2010_2019 <- read.csv2("data/donnees_sim_safran/QUOT_SIM2_2010-2019.csv.gz")
## clean temperature data
source("scripts/clean_SAFRAN_temp_all_dep_2010_2019.R") 
source("scripts/clean_SAFRAN_temp_all_dep_2020_2025.R") 
## SAFRAN: corresponding polygons of 8*8km grid
grid <-  st_read(here("data/geo", "mailles_safran_complete_l93.gpkg"))

# Method 1:	Mean estimates weighted by the surface of the intersect grid x municipality polygon ####
## Intersection between the grid of temperature and the communes ####
# running time approx 6 HOURS 
source("scripts/polygon_munic_intersection/intersection_safran_grid_and_municipalities.R") 
# explanation: this gives the weight of each grid for the calculation of per-municipality variable (ex: temperature)
# open the result of this intersect
com2_temp_intersect_weights_all_fr <- readRDS("output/com2_temp_intersect_weights_all_fr.rds")

## For each municipality: calculate daily temp (mean, max, min), RH, wind speed####
# loop for temp per municipality per year (run time ~5mn/year)
source("scripts/polygon_munic_intersection/get_temp_per_municip_per_year_LOOP_201019_COM2.R")
source("scripts/polygon_munic_intersection/get_temp_per_municip_per_year_LOOP_202025_COM2.R")

# open final summarized results
temp_by_munic_2020_25 <- readRDS("output/temp_all_Fr_per_year/8km_munic_polygon_com2/2020_2025.rds")
temp_by_munic_2010_19 <- readRDS("output/temp_all_Fr_per_year/8km_munic_polygon_com2/2010_2019.rds")

# Method 2: Intersection between population centroid and different temp grid ####
# population 2021 for 2023 polygons
pop21insee <- read.csv2("data/pop_communes/donnees_communes_population_légales_2021_INSEE.csv") # ne contient pas Mayotte
## Population centroid ####
source("scripts/pop_centroid/pop_centroid.R")
# open the output
t4sf <-readRDS("output/centroid_pop/centroid_point_pop_municipality.RDS")  
## Grids ####
### 8km grid (SAFRAN, Météo France) ####
source("scripts/pop_centroid/pop_centroid_temp_8km_SAFRAN.R")
# open the output (example for 2020)
t20<-readRDS("output/temp_all_Fr_per_year/8km_pop_centroid/summary_8km_temp_summer2020.rds")

# From municipality polygons/temperature estimates to BDI_COD ####
source("scripts/code_insee_to_bdi_cod.R")
# open the outputs 
# For temperature based on method 1 (intersect municipality X 8 km grid)
temp_by_bdicod_2010_19_8km_polyg<- readRDS("output/temp_all_Fr_per_year/8km_munic_polygon_com2/2010_2019_bdi_cod.rds")
temp_by_bdicod_2020_25_8km_polyg<-readRDS("output/temp_all_Fr_per_year/8km_munic_polygon_com2/2020_2025_bdi_cod.rds")
# For temperature based on method 2 (estimates at the municipality centroid)
# open the output (example for 2020)
temp_by_bdicod_2010_19_8km_centr<- readRDS("output/temp_all_Fr_per_year/8km_pop_centroid/temp_2020_bdi_cod.rds") 
## Create BDI_COD polygons ####
source("scripts/BDI_COD_polygons.R")
# Open the output
bdi_cod<-readRDS("output/bdicod_2023_polyg.rds")

## Heatwave indicators in each BDI_COD ####
# based on the 8km pop° centroid data
### Data on heatwave thresholds ####
sacs<-readxl::read_excel("data/ibm_seuils_par_dpt.xlsx")
meteofr <- readxl::read_excel("data/2019_meteo_france_spicclean.xlsx")
### Identify days on which there is a heatwave ####
source("scripts/define_HW.R")
# open the output
temp_by_bdicod <-read.csv("output/temp_all_Fr_per_year/8km_pop_centroid/temp_by_bdicod13_23_HW_final.csv")


