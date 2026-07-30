# This codes finds the population centroid of each municipality in continental France
# Data ####
## Municipality polygons: opened as vector (faster) #### 
com2 <- vect(here("data/pop_communes", "COMMUNE_FRMETDROM.shp"))          # municipalities
com2 <- com2[, c("INSEE_COM", "NOM")]     # select columns

st_crs(car_geo)
# Combine geometry + attributes
car_all <- merge(car_geo, car, by = "idINSPIRE")  # adjust join column if different

# Spatial join: assign each population grid to a municipality ####
# Match CRS
car_all <- project(car_all, crs(com2))
rm(car, car_geo)

# Try with region Bretagne
# com2_bret <- terra::subset(com2, substr(com2$INSEE_COM, 1, 2) %in% c("56", "29", "22", "35"))
# grid_in_commune_bret <- intersect(car_all, com2_bret)
# grid_in_commune_sf_bret <- st_as_sf(grid_in_commune_bret)
# grid_in_commune_sfbret <- grid_in_commune_sf_bret %>%
#   group_by(INSEE_COM)%>%
#   filter(ind_c==max(ind_c))

# All continental Fr
grid_in_commune <- intersect(car_all, com2)
# Calculate the surface of each intersected grid*municipality
# reason: we are not interested in very small intersects: for instance if there is very
# high pop° at the grid cell of the bordering municipality, and a share of that grid is on the municipality 
# of interest, we are not interested in it!
grid_in_commune$area <- expanse(grid_in_commune)
grid_in_commune_sf <- st_as_sf(grid_in_commune)

# Select one population centroid for each municipality ####
# For each municipality, find the grid cell with the highest number of inhabitants
# For each commune, pick the most populated grid whose intersect area is at least 20,000 m²
grid_in_commune_sf <- grid_in_commune_sf %>%
  group_by(INSEE_COM)%>%
  mutate(n_inters=n(),
         rank_pop = rank(-ind_c, ties.method = "first"))

t4 <- grid_in_commune_sf  %>%
     group_by(INSEE_COM)%>%
    filter(area>=20000 | (INSEE_COM=="66223" & rank_pop=="1") | 
             (INSEE_COM=="33103" & rank_pop=="1")) 
# the communes  66223 and 33103 are very small: so we have to chose manually the intersect
# with the highest population

t4 <- t4  %>%
  group_by(INSEE_COM)%>%
  filter(ind_c==max(ind_c)) %>%
  filter(area==max(area))
duplicates <- t4[duplicated(t4$INSEE_COM) | duplicated(t4$INSEE_COM, fromLast = TRUE), ]
# no commune appears twice 

# Identify any commune without a population centroid
t6 <- as.data.frame(t4) %>%
  select(INSEE_COM) 

com2df<-as.data.frame(com2)
diff1 <- anti_join(com2df,t6)%>%
  filter(!grepl("^97", INSEE_COM))

# these are either: 
# - oversea territoires, 
# - population=1 or 0 (destroyed villages)
#- 2 islands (île Molene et île de Sein)
missing_insee <- diff1$INSEE_COM
# Subset the grid cells that intersect those communes
missing_grids <- grid_in_commune_sf %>%
  filter(INSEE_COM %in% missing_insee) %>%
  filter(rank_pop<100)
mapView(missing_grids)

# Get the population centroid for each municipality as a POINT (instead of polygon) ####
t4$centroid <- st_centroid(t4$geometry)
mapview(t4)
t4df <- as.data.frame(t4) %>%
  select(-one_of("geometry"))
t4sf <-st_as_sf(t4df)
t4sf <- t4sf %>%
  select("INSEE_COM", "NOM", "centroid")
saveRDS(t4sf, "output/centroid_pop/centroid_point_pop_municipality.RDS")  
mapview(t4sf)
