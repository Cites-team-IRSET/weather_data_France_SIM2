# Temperature data ####
temp_20_25<- temp_20_25 %>%
  mutate(year=substr(DATE,1,4),
         month=substr(DATE,5,6),
         day=substr(DATE,7,8),
         date=as.Date(paste(year, month, day, sep="-"), format="%Y-%m-%d", useNA= "always")) %>%
    mutate(across(c(year, month, day,DATE,T_Q, TINF_H_Q, TSUP_H_Q, FF_Q, PRELIQ_Q, HU_Q),as.numeric))

  temp_20_25_1<- temp_20_25 %>%
  select("LAMBY", "LAMBX", "DATE", "year", "month", "day", "T_Q", "TINF_H_Q", "TSUP_H_Q", "FF_Q", "HU_Q", "date")
  # pick any additional weather variables you are interested in
  summary(temp_20_25$T_Q)  

## Get the lat and long coordinates based on lambert coordinates ##
lambert_conv <- lambert_conv %>%
  mutate(LAT_DG=lat_dg,
         LON_DG=lon_dg,
         LAMBX=lambx,
         LAMBY=lamby) %>%
  select(-one_of("lambx", "lamby", "lat_dg", "lon_dg"))

lambert_conv_ext<-lambert_conv_ext%>%
   mutate(LAMBX=LAMBX..hm.,
          LAMBY=LAMBY..hm.,
          LAT_DG=gsub(",", ".", LAT_DG),
          LON_DG=gsub(",", ".", LON_DG),
          across(c(LAMBX, LAMBY, LAT_DG, LON_DG), as.numeric)) %>%
   select(-one_of("LAMBX..hm.", "LAMBY..hm."))
 write.csv2(lambert_conv_ext, here("output", "lambert_conv_ext_clean.csv"))
# merge both lambert files
 lambert_both <- lambert_conv_ext %>%
   left_join(lambert_conv)  
 
 temp_20_25_1 <- temp_20_25_1 %>%
   left_join(lambert_both)%>%
   filter(!is.na(num_maille))# remove maille outside France (CH DE BE)

 #rm(lambert_conv,lambert_conv_ext,lambert_both, temp_20_25, temp_20_25)
 temp_2020_2025_1_summer <- temp_20_25_1 %>%
   filter(month<10 & month >4)

# ## Only Britanny to begin ####
# temp_20_25_Bret <- temp_20_25_1 %>%
#   filter(num_dep==35 | num_dep==22| num_dep==29| num_dep==56)
#  temp_20_Bret <- temp_20_25_Bret %>%
#    filter(year==2020)
#  temp_20_Bret_oneday <- temp_20_25_Bret %>%
#    filter(DATE==20220101)
#  temp_20_Bret_3days <- temp_20_25_Bret %>%
#    filter(DATE==20200101 | DATE==20200102 | DATE==20200103)
# ### Hottest day of summer 2022 ####
# summer2022_hottest_day <- temp_20_25_Bret %>%
#   filter(DATE==20220718) %>%
#   group_by(geometry) %>%
#   summarise(T_Q,
#             LON_DG = unique(LON_DG),
#             LAT_DG = unique(LAT_DG))
# # only Alsace
#  temp_20_25_Als <- temp_20_25_1 %>%
#    filter(num_dep==67 | num_dep==68)
#  temp_20_Als_oneday <- temp_20_25_Als %>%
#    filter(DATE==20200101)
#  temp_20_Als_3days <- temp_20_25_Als %>%
#    filter(DATE==20200101 | DATE==20200102 | DATE==20200103)

 
 
## Whole France but only one day of 2020 ####
temp_20_France_oneday <- temp_20_25_1 %>%
  filter(DATE==20220101)

 