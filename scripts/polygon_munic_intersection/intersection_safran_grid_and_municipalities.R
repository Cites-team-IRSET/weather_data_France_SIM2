# Do the intersection between the grid of temperature and the communes only once ####
# com1 <- com1 %>%
#   select("insee", "nom", "geometry", "surf_ha") %>%
#   mutate(dept=substr(insee, 1,2))%>%
#   filter(
#     grepl("[^0-9]", substr(insee, 1, 2)) | 
#       as.numeric(substr(insee, 1, 2)) <= 95)
# filter(dept==56|dept==22|dept==29|dept==35)
com2 <- com2 %>%
  select("INSEE_COM", "NOM", "geometry") %>%
  mutate(dept=substr(INSEE_COM, 1,2))%>%
  filter(
    grepl("[^0-9]", substr(INSEE_COM, 1, 2)) | # keep Corsica, ie code INSEE contains one letter
      as.numeric(substr(INSEE_COM, 1, 2)) <= 95) # remove oversea french territories: we will use another temp. source for it

# mailles
# removes grid outside the territory (CH, DE, BE)
grid <- grid %>%
  st_transform(4326) %>%
  filter(pays==1 | nature== "Limite côtière" | nature== "Frontière internationale" | maille==1315) 
maille_all_fr <- grid %>%
  #  filter(code_reg==53) %>%
  select("maille", "pays",code_dept, code_reg, nature, geom)
mapview(maille_all_fr)

rm(grid)
 
# Open the data containing temperatures for one day (here 2022 01 01) for whole France
# temp_20_Bret_oneday_sf <- st_as_sf(temp_20_Bret_oneday,
temp_20_France_oneday_sf <- st_as_sf(temp_20_France_oneday,
                                     #temp_20_Bret_oneday_sf <- st_as_sf(temp_20_Bret_oneday_sf,
                                     coords = c("LON_DG", "LAT_DG"),
                                     crs = 4326)  # WGS84

rm(temp_20_France_oneday)

# tm_shape(maille_all_fr) + 
#   tm_fill(col = "grey", alpha=1) +
#   tm_shape(temp_20_France_oneday_sf) + 
#   tm_fill(col = "red", alpha=1) 


# Join the grid (SAFRAN with 8km² grid) with the data on temperature (Safran with points at the centroids of the 8km² grid)
temp_20_France_oneday_sf_maille <- st_join(maille_all_fr, temp_20_France_oneday_sf,
                                            left=TRUE, join = st_intersects) # left_join: keeps all variables from x dataset (temperatures!)
# force the exclusion of some mailles (international territory)
temp_20_France_oneday_sf_maille <- temp_20_France_oneday_sf_maille %>%
  mutate(num_maille=maille) %>%
  select(T_Q, TINF_H_Q, TSUP_H_Q, FF_Q, HU_Q, num_maille, geom )%>%
  filter(num_maille!=168 & num_maille!=940 & num_maille!=5246 & num_maille!=9651)
rm(maille_all_fr,t, temp_20_22_1, temp_20_France_oneday_sf, grid)

# intersection of the municipality polygons with the SAFRAN grid,
# sequentially for each dept: to do only once! (running time approx. 5-6hours)
#dept_codes <- unique(com1$dept)
dept_codes <- unique(com2$dept)
# Initialize empty list or sf
results <- list()

for (d in dept_codes) {
  message("Processing dept ", d)

  dep_data <- com2 %>% filter(dept == d)

  inter <- st_intersection(temp_20_France_oneday_sf_maille, dep_data)

  # store result in list
  results[[as.character(d)]] <- inter
  }

result_all <- do.call(rbind, results)

saveRDS(result_all, "output/com2_temp_intersect_list_by_dep.rds")

# calculate weight of each commune segment
# com1_temp_weights <- result_all %>%
#   mutate(location_num = as.numeric(st_area(st_transform(geom, 2154)))) %>%
#   group_by(insee) %>%
#   mutate(weight_fraction = location_num / sum(location_num, na.rm = TRUE)) %>%
#   ungroup() %>%
#     st_transform(4326)
# saveRDS(com1_temp_weights, "output/com1_temp_intersect_weights_all_fr.rds")
com2_temp_weights <- result_all %>%
  mutate(location_num = as.numeric(st_area(st_transform(geom, 2154)))) %>%
  group_by(INSEE_COM) %>%
  mutate(weight_fraction = location_num / sum(location_num, na.rm = TRUE)) %>%
  ungroup() %>%
  st_transform(4326)
saveRDS(com2_temp_weights, "output/com2_temp_intersect_weights_all_fr.rds")



# test with just one dept, Alsace ###
# com1_temp_weights  <- st_intersection(
#   temp_20_France_oneday_sf_maille ,
#   com1
# ) %>%
#   mutate(location_num = as.numeric(st_area(st_transform(geom, 2154)))) %>%
#   group_by(insee) %>%
#   mutate(weight_fraction = location_num / sum(location_num, na.rm = TRUE)) %>%
#   ungroup() %>%
#   # select(num_maille, insee, weight_fraction)%>%
#   st_transform(4326)
# com1_temp_weights 
# 
# 
# temp_20_Als_3days_sf <- st_as_sf(temp_20_Als_3days,
#                                  coords = c("LON_DG", "LAT_DG"),
#                                  crs = 4326)
# 
# # Join weights with all temperature observations
# temp_all_weighted <- temp_20_Als_3days_sf %>%
#   mutate(location_num = as.numeric(st_area(st_transform(geometry, 2154)))) %>%
#   inner_join(
#     st_drop_geometry(com1_temp_weights) %>%
#       select(num_maille, insee, weight_fraction),
#     by = "num_maille", relationship="many-to-many"
#   )%>%
#   st_transform(4326)
# 
# 
# com1_temp_summaryb <- temp_all_weighted %>%
#   mutate(location_num = as.numeric(st_area(st_transform(geometry, 2154)))) %>%
#   group_by(insee, DATE) %>%
#   summarise(temp_mean = weighted.mean(T_Q, weight_fraction, na.rm = TRUE)) %>%
#   ungroup()%>%
#   st_transform(4326)
# com1_temp_summaryb2 <- com1_temp_summaryb%>%
#   st_drop_geometry()
# com1df <- as.data.frame(com1)
# 
# # add correct communes polygons
# com1_temp_final <- com1_temp_summaryb2 %>%
#   left_join(com1df, by = "insee") 
# com1_temp_final2 <- com1 %>%
#   left_join(com1_temp_summaryb%>%
#               st_drop_geometry(),
#             by = "insee") %>%
#   st_transform(4326)
# write.csv2(com1_temp_final, "output/com1_temp_2020_bretagne.csv2")
# saveRDS(com1_temp_final, "output/com1_temp_2020_bretagne.rds")