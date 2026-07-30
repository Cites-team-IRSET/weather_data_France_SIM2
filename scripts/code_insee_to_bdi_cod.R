# Goal: aggregated temperature/env variables that were calculated by municipality, ad PMSI_23 level
com2 <- com2%>% select(INSEE_COM, NOM, geometry)%>%
  filter(!grepl("^96", INSEE_COM) &!grepl("^97", INSEE_COM)& !grepl("^98", INSEE_COM))
bdi_insee <- bdi_insee %>%select(COM23_CODE, pmsi23_code)%>%
  rename(INSEE_COM=COM23_CODE, PMSI23_CODE=pmsi23_code)%>%
  filter(!grepl("^96", INSEE_COM) &!grepl("^97", INSEE_COM)& !grepl("^98", INSEE_COM))
bdi_insee<-unique(bdi_insee)
openxlsx::write.xlsx(bdi_insee, "data/carte_communes/bdi23_cod13_codinsee.xlsx")
pop21insee <- pop21insee %>% select(COM, PTOT)%>%
  rename(INSEE_COM=COM)%>%
  filter(!grepl("^96", INSEE_COM) &!grepl("^97", INSEE_COM)& !grepl("^98", INSEE_COM))

tbdi_insee <- bdi_insee%>%
  select(INSEE_COM)
tpop21insee <- pop21insee%>%
  select(INSEE_COM)
u<-anti_join(tpop21insee,tbdi_insee) # seules diff: marseille lyon paris et OM
u<-anti_join(tbdi_insee,tpop21insee)
dim(tpop21insee)
dim(tbdi_insee)
rm(tbdi_insee,tpop21insee,u)
bdi_insee_pop <- bdi_insee%>%
  left_join(pop21insee)%>%
  filter(!is.na(PTOT))

# 8km data ####
## temp calculated by municipality polygon ####
temp_by_munic_2020_25 <- readRDS("output/temp_all_Fr_per_year/8km_munic_polygon_com2/2020_2025.rds")
temp_by_munic_2010_19 <- readRDS("output/temp_all_Fr_per_year/8km_munic_polygon_com2/2010_2019.rds")

temp_by_munic_2020_25 <- temp_by_munic_2020_25%>%
  left_join(bdi_insee_pop)
temp_by_bdicod_2020_25<-temp_by_munic_2020_25%>%
  group_by(PMSI23_CODE, DATE)%>%
  summarise(temp_mean=weighted.mean(temp_mean,PTOT, na.rm=FALSE),
            temp_max=weighted.mean(temp_max,PTOT, na.rm=FALSE),
            temp_min=weighted.mean(temp_min,PTOT, na.rm=FALSE),
            wind=weighted.mean(wind,PTOT, na.rm=FALSE),
            hum=weighted.mean(hum,PTOT, na.rm=FALSE),
            .groups="drop")

temp_by_munic_2010_19 <- temp_by_munic_2010_19%>%
  left_join(bdi_insee_pop)
temp_by_bdicod_2010_19<-temp_by_munic_2010_19%>%
  group_by(PMSI23_CODE, DATE)%>%
  summarise(temp_mean=weighted.mean(temp_mean,PTOT, na.rm=FALSE),
            temp_max=weighted.mean(temp_max,PTOT, na.rm=FALSE),
            temp_min=weighted.mean(temp_min,PTOT, na.rm=FALSE),
            wind=weighted.mean(wind,PTOT, na.rm=FALSE),
            hum=weighted.mean(hum,PTOT, na.rm=FALSE),
            .groups="drop")

# save
saveRDS(temp_by_bdicod_2010_19, "output/temp_all_Fr_per_year/8km_munic_polygon_com2/2010_2019_bdi_cod.rds")
saveRDS(temp_by_bdicod_2020_25, "output/temp_all_Fr_per_year/8km_munic_polygon_com2/2020_2025_bdi_cod.rds")


## temp calculated by centroid of the population in each municipality ####
# loop by year
temp_years <- c(2020:2025) # same for 2010-2019

# Initialize empty list or sf
results <- list()
for (y in temp_years) {
  message("processing", y)
 temp_by_munic <- readRDS(paste0("output/temp_all_Fr_per_year/8km_pop_centroid/summary_8km_temp_summer", y, ".rds"))
 temp_by_munic <- temp_by_munic%>%
    left_join(bdi_insee_pop)
 
 temp_by_bdicod<-temp_by_munic%>%
   group_by(PMSI23_CODE, date)%>%
   summarise(temp_mean=weighted.mean(T_Q,PTOT, na.rm=FALSE),
             temp_max=weighted.mean(TSUP_H_Q,PTOT, na.rm=FALSE),
             temp_min=weighted.mean(TINF_H_Q,PTOT, na.rm=FALSE),
             wind=weighted.mean(FF_Q,PTOT, na.rm=FALSE),
             hum=weighted.mean(HU_Q,PTOT, na.rm=FALSE),
             .groups="drop")
 
 saveRDS(temp_by_bdicod, paste0("output/temp_all_Fr_per_year/8km_pop_centroid/temp_", y, "_bdi_cod.rds"))
 rm(temp_by_bdicod)
}

temp_by_munic_2010 <- temp_by_munic_2010%>%
  left_join(bdi_insee_pop)
temp_by_bdicod_2010<-temp_by_munic_2010%>%
  group_by(PMSI23_CODE, date)%>%
  summarise(temp_mean=weighted.mean(T_Q,PTOT, na.rm=FALSE),
            temp_max=weighted.mean(TSUP_H_Q,PTOT, na.rm=FALSE),
            temp_min=weighted.mean(TINF_H_Q,PTOT, na.rm=FALSE),
            wind=weighted.mean(FF_Q,PTOT, na.rm=FALSE),
            hum=weighted.mean(HU_Q,PTOT, na.rm=FALSE))

