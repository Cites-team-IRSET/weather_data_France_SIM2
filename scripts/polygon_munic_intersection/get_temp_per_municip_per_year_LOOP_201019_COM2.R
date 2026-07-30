gc()
# loop by year
temp_years <- unique(temp_2010_2019_1_summer$year)
# Initialize empty list or sf
results <- list()

for (y in temp_years) {
  message("Processing year ", y)
  
  year_data <- temp_2010_2019_1_summer %>% filter(year == y)
  
  year_data_sf <- st_as_sf(year_data,
                           coords = c("LON_DG", "LAT_DG"),
                           crs = 4326)
  rm(year_data)

  # Join weights with all temperature observations
  temp_all_weighted <- year_data_sf %>%
    mutate(location_num = as.numeric(st_area(st_transform(geometry, 2154)))) %>%
    inner_join(
      st_drop_geometry(com2_temp_intersect_weights_all_fr) %>%
        select(num_maille, INSEE_COM, weight_fraction),
      by = "num_maille", relationship="many-to-many"
    )%>%
    st_transform(4326)
  
  temp_all_weighted <- as.data.frame(temp_all_weighted)%>%
    select(INSEE_COM, DATE, T_Q, TSUP_H_Q, TINF_H_Q, FF_Q, HU_Q, weight_fraction, num_maille)
  
  
  # Put as data table to take less space 
  DT <- as.data.table(temp_all_weighted)
  
  # Weighted summaries by INSEE_COM + DATE
  com2_temp_summary_dt <- DT[, .(
    temp_mean = weighted.mean(T_Q,        weight_fraction, na.rm = TRUE),
    temp_max  = weighted.mean(TSUP_H_Q,   weight_fraction, na.rm = TRUE),
    temp_min  = weighted.mean(TINF_H_Q,   weight_fraction, na.rm = TRUE),
    wind      = weighted.mean(FF_Q,       weight_fraction, na.rm = TRUE),
    hum       = weighted.mean(HU_Q,       weight_fraction, na.rm = TRUE)
  ), by = .(INSEE_COM, DATE)]
  
  
  
  # store results in list
  results[[as.character(y)]] <- com2_temp_summary_dt
  # Save result to disk (CSV)
  # outfile <- paste0("summary_", y, ".csv")
  # fwrite(com2_temp_summary_dt, file = here("output/temp_all_Fr_per_year", outfile))
  # message("Saved year ", y, " to ", outfile)
  
  # make space after each year
  rm(year_data_sf, temp_all_weighted, DT, com2_temp_summary_dt)
  gc()  # force garbage collection
  
}

result_all <- do.call(rbind, results)

saveRDS(result_all, here("output/temp_all_Fr_per_year/8km_munic_polygon_com2", "2010_2019.rds"))
result_all_2010_2019 <- readRDS(here("output/temp_all_Fr_per_year/8km_munic_polygon_com2", "2010_2019.rds"))


com2 <- com2 %>%
  select("INSEE_COM", "NOM", "geometry") %>%
  mutate(dept=substr(INSEE_COM, 1,2))%>%
  filter(
    grepl("[^0-9]", substr(INSEE_COM, 1, 2)) | # pour garder la corse, ie code INSEE_COM contenant lettre
      as.numeric(substr(INSEE_COM, 1, 2)) <= 95)

com2df <- as.data.frame(com2)
# add correct communes polygons
com2_temp_final <- merge(result_all_2010_2019, com2df, by = "INSEE_COM", all.x = TRUE)
# add region number
grid <-  st_read(here("data/metadata", "mailles_safran_complete_l93.gpkg"))
reg <- grid %>%
  mutate(dept=code_dept)%>%
  select(dept, code_reg) %>%
  st_drop_geometry()
reg_unique <- reg[!duplicated(reg$dept), ]
com2_temp_final <- merge(com2_temp_final, reg_unique, by = "dept", all.x = TRUE)

# com2_temp_final2 <- result_all %>%
#   left_join(com2_temp_summary_dt%>%
#               st_drop_geometry(),
#             by = "INSEE_COM") %>%
#   st_transform(4326)


saveRDS(com2_temp_final, here("output/temp_all_Fr_per_year/8km_munic_polygon_com2", "2010_2019_with_dep_reg_nb.rds"))
result_all_2010_2019 <- readRDS(here("output/temp_all_Fr_per_year/8km_munic_polygon_com2", "2010_2019_with_dep_reg_nb.rds"))


u<-com2_temp_final%>%
  filter(DATE==20100501)

# identify the communes absent of the final dataset
INSEE_COM_u<-as.data.frame(u)%>%
  select(INSEE_COM)
INSEE_COM_com2<-as.data.frame(com2)%>%
  select(INSEE_COM)
diff2 <- anti_join(INSEE_COM_com2,INSEE_COM_u)
# INSEE_COM 14715, 29155, 29083
com2_ins<-com2%>%
  filter(INSEE_COM==29155 |INSEE_COM==29083)
mapview(com2_ins)
# the islands of Ouessant and Ile-de-Sein are missing (both in Britanny Region)


