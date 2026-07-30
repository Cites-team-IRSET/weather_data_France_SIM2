# centroid of pop° in each municipality ####
st_crs(t4sf)

# 8km grid ####
# removes grid outside the territory (CH, DE, BE)
grid_8km <- grid %>%
  st_transform(4326) %>%
  filter(pays==1 | nature== "Limite côtière" | nature== "Frontière internationale" | maille==1315) 
grid_8km <- grid_8km %>%
  select("maille", "pays",code_dept, code_reg, nature, geom)
mapview(grid_8km)
st_crs(grid_8km)

# eaxmple: zoom on Paris 
t <- grid_8km %>%
  filter_sf(ymin=48.77, ymax=48.94,
            xmin=2.22, xmax=2.45)
mapview(t)
# Join centroid and municipalities ####
t4sf_grid_8km <- st_join(t4sf, grid_8km,
                          left=TRUE, join = st_intersects)
tm_shape(grid_8km)+  tm_fill(col = "grey", alpha=1)
tm_shape(t4sf)+  tm_fill(col = "grey", alpha=1)
mapview(t4sf_grid_8km)

# find the municipalities for which no pop° centroid was assigned
t4sf_grid_8kmt<-t4sf_grid_8km%>%
  filter(is.na(maille))
t4sf_grid_8kmb<-t4sf_grid_8km
unmatched <- which(is.na(t4sf_grid_8kmb$maille))
# find nearest grid cell for those
nearest_ids <- st_nearest_feature(t4sf_grid_8kmb[unmatched, ], grid_8km)
# assign the attributes of the nearest grid
t4sf_grid_8kmb[unmatched, names(grid_8km)] <- grid_8km[nearest_ids, ] 
t4sf_grid_8kmb <- t4sf_grid_8kmb%>%
  select(-one_of("geom"))

# Temperature data for whole France ####
## 2010-2019 ####
### Loop by year ####
temp_years <- unique(temp_2010_2019_1_summer$year)
# Initialize empty list or sf
for (y in temp_years) {
  message("Processing year ", y)
  
  year_data <- temp_2010_2019_1_summer %>% filter(year == y)
  
  km8_y <- year_data %>%
    mutate(maille=num_maille)%>%
    select(maille,T_Q, date,T_Q, TSUP_H_Q, TINF_H_Q, FF_Q, HU_Q)
  
  temp_8km_per_munic_y <-t4sf_grid_8kmb  %>%
    left_join(km8_y) %>%
    select(INSEE_COM, NOM, date, maille,T_Q,T_Q, TSUP_H_Q, TINF_H_Q, FF_Q, HU_Q)
  
  rm(km8_y, year_data)
  
  # Put as data table to take less space 
  DT <- as.data.table(temp_8km_per_munic_y)
  
  # # store results in list
  # results[[as.character(y)]] <- DT
  # Save result to disk (CSV)
  outfile <- paste0("output/temp_all_Fr_per_year/8km/summary_8km_temp_summer", y, ".rds")
  saveRDS(DT, file = outfile)
  
  message("Saved year ", y)
  
  # make space after each year
  rm(temp_8km_per_munic_y, DT)
  gc()  # force garbage collection
  
}

## 2020-25 ####
### Loop by year ####
temp_years <- unique(temp_2020_2025_1_summer$year)
# Initialize empty list or sf
for (y in temp_years) {
  message("Processing year ", y)
  
  year_data <- temp_2020_2025_1_summer %>% filter(year == y)
  
  km8_y <- year_data %>%
    mutate(maille=num_maille)%>%
    select(maille,T_Q, date,T_Q, TSUP_H_Q, TINF_H_Q, FF_Q, HU_Q)
  
  temp_8km_per_munic_y <-t4sf_grid_8kmb  %>%
    left_join(km8_y) %>%
    select(INSEE_COM, NOM, date, maille,T_Q,T_Q, TSUP_H_Q, TINF_H_Q, FF_Q, HU_Q)
  
  rm(km8_y, year_data)
  
  # Put as data table to take less space 
  DT <- as.data.table(temp_8km_per_munic_y)
  
  # # store results in list
  # results[[as.character(y)]] <- DT
  # Save result to disk (CSV)
  outfile <- paste0("output/temp_all_Fr_per_year/8km/summary_8km_temp_summer", y, ".rds")
  saveRDS(DT, file = outfile)
  
  message("Saved year ", y)
  
  # make space after each year
  rm(temp_8km_per_munic_y, DT)
  gc()  # force garbage collection
  
}

