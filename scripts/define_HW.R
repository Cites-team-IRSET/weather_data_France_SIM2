# defining heatwaves 
# thresholds ####
## SACS : IBMmin, IBMmax ####
# 3d moving average of min and max temp for each department 
# based on 99.5 perc of rolling avg temp June-August 1973-2022 at the department level

sacs<-sacs%>% 
  mutate(dpt=ifelse(nchar(dpt)<2, paste0("0",dpt),dpt))

## Meteo France Spic 99.5 perc, Sdeb 97.5perc, Sint 95perc ####
# based on Tmean. Reference 1981-2010. Values for each department
# HW: if tmean>p99.5 (Spic), tmean>97.5 (Sdeb) for the surrounding days. 
# HW interruption: if Temp<p97.5 (Sdeb) for at least 3 consec. days or if Temp<p95 (Sint) for at least 1 day
# Spic : seuil de pic de chaleur= percentile 99,5 de la température moyenne journalière du département sur la période 1981-2010.
# Sdeb : seuil indiquant les dates de début et de fin de l’épisode= percentile 97,5 de la température moyenne journalière du département sur la période 1981-2010.
# Sint : seuil d’interruption : correspondant au percentile 95 de la température moyenne journalière du département sur la période 1981-2010.
meteofr<-meteofr %>%
  mutate(dpt=ifelse(nchar(dpt)<2, paste0("0",dpt),dpt))

# loop by year
temp_years <- c(2013:2023)

# Initialize empty list or sf
results <- list()
for (y in temp_years) {
  message("processing", y)
  temp_by_bdicod <- readRDS(paste0("output/temp_all_Fr_per_year/8km_pop_centroid/temp_", y, "_bdi_cod.rds"))
  
  temp_by_bdicodb <- temp_by_bdicod %>%
    select(PMSI23_CODE, date, temp_max, temp_min, temp_mean, hum)%>%
    group_by(PMSI23_CODE)%>%
    mutate(dpt=substring(PMSI23_CODE,1,2))%>%
    left_join(sacs, by="dpt")%>%
           # SACS heatwave indicator
    mutate(temp_min02=rollmean(temp_min, k = 3, fill = NA, align = "right"),
           temp_max02=rollmean(temp_max, k = 3, fill = NA, align = "right"),
           SACS_day = ifelse(temp_min02 >= IBM_min & temp_max02 >= IBM_max,1,0),
           # streak_sacs = sequence(rle(SACS_day)$lengths) * SACS_day,
           HW_SACS_raw  = ifelse((lead(SACS_day,1)==1 | lead(SACS_day,2)==1| 
                                lead(SACS_day,0)==1),1,0),
           HW_SACS = fuse_episodes(HW_SACS_raw, max_gap = 3),
           # Heat index 
           temp_mean_F= (temp_mean * 9/5) + 32,
           # If temp mean >=27 or RH>=40, HI takes 0 (non interpretable)
           HI = ifelse(temp_mean >= 27 & hum >= 40,
                       -42.379 + 2.04901523*temp_mean_F + 10.14333127*hum - 
                         0.22475541 * temp_mean_F*hum - 6.83783E-3 * temp_mean_F^2 -
                         5.481717E-2* hum^2+ 1.22874E-3*  temp_mean_F^2* hum + 
                         8.5282E-4 *temp_mean_F*hum^2 - 
                         1.99E-6 *  temp_mean_F^2*hum^2,
                       0))%>%
    # Meteo France heatwave indicator : spic, sdev, sint
    left_join(meteofr, by="dpt")%>%
    mutate(spic=ifelse(temp_mean>P99.5_tmean,1,0),
           sdeb=ifelse(temp_mean>P97.5_tmean,1,0),
           sint=ifelse(temp_mean<P95_tmean,1,0))%>%
    ungroup()
  
  # Define Meteo France heatwave events 
  temp_by_bdicodc <- as.data.table(temp_by_bdicodb)
  temp_by_bdicodc[, MF_day := 0]
  
  temp_by_bdicodc[, MF_day := {
    hw <- integer(.N)
    i <- 1
    
    while (i <= .N) {
      # HW: tmean> Spic
      if (spic[i]) {
        
        # extend HW period before Spic (if previous days tmean>Sdeb)
        start <- i
        j <- i - 1
        while (j >= 1 && sdeb[j]) {start <- j
        j <- j - 1}
        
        # extend HW period after Spic
        end <- i
        j <- i + 1
        below_sdeb_count <- 0
        
        while (j <= .N) {
          # stop if tmean < Sint
          if (sint[j]) break
          if (!sdeb[j]) { below_sdeb_count <- below_sdeb_count + 1} 
          else {below_sdeb_count <- 0
          end <- j}
          # stop if tmean for 3 consecutive < Sdeb
          if (below_sdeb_count >= 3) break
          j <- j + 1
        }
        
        # HW indicator
        idx_hw <- start:end
        idx_hw <- idx_hw[spic[idx_hw] | sdeb[idx_hw]]
        
        hw[idx_hw] <- 1
        
        i <- end + 1
      } else { i <- i + 1}
    }
    hw
  }, by = PMSI23_CODE]
  
  temp_by_bdicodd<-temp_by_bdicodc%>%
    group_by(PMSI23_CODE)%>%
    mutate(HW_MF=ifelse((lag(MF_day,0)==1 & lag(MF_day,1)==1 & lag(MF_day,2)==1) |
                      (lag(MF_day,0)==1 & lag(MF_day,1)==1 & lead(MF_day,1)==1) |
                      (lag(MF_day,0)==1 & lead(MF_day,1)==1 & lead(MF_day,2)==1),1,0),
           HW_MFf=fuse_episodes_if(hw=HW_MF, Sint=sint,max_gap=3))%>%
    ungroup()%>%
    select(PMSI23_CODE, dpt,date, temp_max, temp_min, temp_mean, hum,
           temp_min02,temp_max02,IBM_min,IBM_max,
           SACS_day,HW_SACS_raw,HW_SACS,MF_day,HW_MF,HW_MFf,HI)
  
  # store results
  results[[as.character(y)]] <- temp_by_bdicodd
  
}

# Combine all years together
temp_by_bdicod13_23<-results %>%
  imap_dfr(~ mutate(.x, year = .y))

# HW based on chosen percentiles: P90, P95, P97.5 based on temperature distribution between summers 2013-23: all years together
temp_by_bdicod13_23<-temp_by_bdicod13_23%>%
  group_by(dpt)%>%
  mutate(p90=as.numeric(quantile(temp_mean,p=0.90)),
         p95=as.numeric(quantile(temp_mean,p=0.95)),
         p97.5=as.numeric(quantile(temp_mean,p=0.975)),
         p99.5=as.numeric(quantile(temp_mean,p=0.995)))%>%
  ungroup()

temp_by_bdicod13_23<-temp_by_bdicod13_23%>%
  group_by(PMSI23_CODE, substr(date,1,4))%>%
  mutate(HW_90p=ifelse(temp_mean>p90,1,0),
         HW_95p=ifelse(temp_mean>p95,1,0),
         HW_97.5p=ifelse(temp_mean>p97.5,1,0),
         HW_99.5p=ifelse(temp_mean>p99.5,1,0),

         # include HW that last at least 2 or at least 3 days
         HW_90p_2plus=ifelse((lag(HW_90p,0)==1 & lag(HW_90p,1)==1) |
                               (lag(HW_90p,0)==1 & lead(HW_90p,1)==1),1,0),
         HW_90p_3plus=ifelse((lag(HW_90p,0)==1 & lag(HW_90p,1)==1 & lag(HW_90p,2)==1 ) |
                               (lag(HW_90p,0)==1 & lead(HW_90p,1)==1 & lead(HW_90p,2)==1 )|
                               (lag(HW_90p,0)==1 & lead(HW_90p,1)==1 & lag(HW_90p,1)==1 ),1,0),

         HW_95p_2plus=ifelse((lag(HW_95p,0)==1 & lag(HW_95p,1)==1) |
                               (lag(HW_95p,0)==1 & lead(HW_95p,1)==1),1,0),
         HW_95p_3plus=ifelse((lag(HW_95p,0)==1 & lag(HW_95p,1)==1 & lag(HW_95p,2)==1 ) |
                               (lag(HW_95p,0)==1 & lead(HW_95p,1)==1 & lead(HW_95p,2)==1 )|
                               (lag(HW_95p,0)==1 & lead(HW_95p,1)==1 & lag(HW_95p,1)==1 ),1,0),

         HW_97.5p_2plus=ifelse((lag(HW_97.5p,0)==1 & lag(HW_97.5p,1)==1) |
                                 (lag(HW_97.5p,0)==1 & lead(HW_97.5p,1)==1),1,0),
         HW_97.5p_3plus=ifelse((lag(HW_97.5p,0)==1 & lag(HW_97.5p,1)==1 & lag(HW_97.5p,2)==1 ) |
                                 (lag(HW_97.5p,0)==1 & lead(HW_97.5p,1)==1 & lead(HW_97.5p,2)==1 )|
                                 (lag(HW_97.5p,0)==1 & lead(HW_97.5p,1)==1 & lag(HW_97.5p,1)==1 ),1,0),

         HW_99.5p_2plus=ifelse((lag(HW_99.5p,0)==1 & lag(HW_99.5p,1)==1) |
                                 (lag(HW_99.5p,0)==1 & lead(HW_99.5p,1)==1),1,0),
         HW_99.5p_3plus=ifelse((lag(HW_99.5p,0)==1 & lag(HW_99.5p,1)==1 & lag(HW_99.5p,2)==1 ) |
                                 (lag(HW_99.5p,0)==1 & lead(HW_99.5p,1)==1 & lead(HW_99.5p,2)==1 )|
                                 (lag(HW_99.5p,0)==1 & lead(HW_99.5p,1)==1 & lag(HW_99.5p,1)==1 ),1,0),
         # Combine HW separated by less than 3 days
         HW_90p_2plusf = fuse_episodes(HW_90p_2plus, max_gap = 3),
         HW_90p_3plusf = fuse_episodes(HW_90p_3plus, max_gap = 3),
         HW_95p_2plusf = fuse_episodes(HW_95p_2plus, max_gap = 3),
         HW_95p_3plusf = fuse_episodes(HW_95p_3plus, max_gap = 3),
         HW_97.5p_2plusf = fuse_episodes(HW_97.5p_2plus, max_gap = 3),
         HW_97.5p_3plusf = fuse_episodes(HW_97.5p_3plus, max_gap = 3),
         HW_99.5p_2plusf = fuse_episodes(HW_99.5p_2plus, max_gap = 3),
         HW_99.5p_3plusf = fuse_episodes(HW_99.5p_3plus, max_gap = 3)
  )%>%
  ungroup()

# summary table of the yearly number of heatwave-days in over all the PMSI codes
t<-temp_by_bdicod13_23%>%
  group_by(year=substr(date,1,4))%>%
  summarise(HW_SACS=sum(HW_SACS, na.rm=TRUE),
            HW_MFf=sum(HW_MFf, na.rm=TRUE),
            HW_90p_2plusf=sum(HW_90p_2plusf, na.rm=TRUE),
            HW_90p_3plusf=sum(HW_90p_3plusf, na.rm=TRUE),
            HW_95p_2plusf=sum(HW_95p_2plusf, na.rm=TRUE),
            HW_95p_3plusf=sum(HW_95p_3plusf, na.rm=TRUE),
            HW_97.5p_2plusf=sum(HW_97.5p_2plusf, na.rm=TRUE),
            HW_97.5p_3plusf=sum(HW_97.5p_3plusf, na.rm=TRUE),
            HW_99.5p_2plusf=sum(HW_99.5p_2plusf, na.rm=TRUE),
            HW_99.5p_3plusf=sum(HW_99.5p_3plusf, na.rm=TRUE))
View(t)
write.xlsx(t, "output/temp_all_Fr_per_year/8km_pop_centroid/nb_HW_per_bdi_cod_13_23.xlsx")
temp_by_bdicod13_23<-temp_by_bdicod13_23%>%
  select(-one_of("temp_min02","temp_max02","SACS_day", "HW_SACS_raw", 
                 "MF_day", "HW_MF", 
                 "substr(date, 1, 4)", "IBM_min", "IBM_max",
                 "p90", "p95", "p97.5","p99.5",  "HW_90p", "HW_95p", "HW_97.5p", "HW_99.5p",
                 "dpt", "year"))
write.xlsx(temp_by_bdicod13_23, "output/temp_all_Fr_per_year/8km_pop_centroid/temp_by_bdicod13_23_HW_final.xlsx")
fwrite(temp_by_bdicod13_23, "output/temp_all_Fr_per_year/8km_pop_centroid/temp_by_bdicod13_23_HW_final.csv")

t4<-mytemp_by_bdicod13_23%>%
  group_by(year=substr(date,1,4))%>%
  summarise(HW_SACS=sum(HW_SAC, na.rm=TRUE),
            HW_MFf=sum(HW_MFf, na.rm=TRUE),
            HW_90p_2plusf=sum(HW_90p_2plusf, na.rm=TRUE),
            HW_90p_3plusf=sum(HW_90p_3plusf, na.rm=TRUE),
            HW_95p_2plusf=sum(HW_95p_2plusf, na.rm=TRUE),
            HW_95p_3plusf=sum(HW_95p_3plusf, na.rm=TRUE),
            HW_97.5p_2plusf=sum(HW_97.5p_2plusf, na.rm=TRUE),
            HW_97.5p_3plusf=sum(HW_97.5p_3plusf, na.rm=TRUE),
            HW_99.5p_2plusf=sum(HW_99.5p_2plusf, na.rm=TRUE),
            HW_99.5p_3plusf=sum(HW_99.5p_3plusf, na.rm=TRUE))
View(t)

u<-read.csv("output/temp_all_Fr_per_year/8km_pop_centroid/temp_by_bdicod13_23_HW_final.csv")
u<-u%>%
  group_by(PMSI23_CODE,substr(date, 1,4))%>%
  mutate(HW_MFf_l01=ifelse((HW_MFf==1 | lag(HW_MFf)==1),1,0),
         HW_SACS=ifelse((HW_SACS==1 |lag (HW_SACS)==1),1,0),
        HW_90p_2plus=ifelse((HW_90p_2plus==1 |lag (HW_90p_2plus)==1),1,0),
        HW_90p_3plus=ifelse((HW_90p_3plus==1 |lag (HW_90p_3plus)==1),1,0),
        HW_95p_2plus=ifelse((HW_95p_2plus==1 |lag (HW_95p_2plus)==1),1,0),
        HW_95p_3plus=ifelse((HW_95p_3plus==1 |lag (HW_95p_3plus)==1),1,0),
        HW_97.5p_2plus=ifelse((HW_97.5p_2plus==1 |lag (HW_97.5p_2plus)==1),1,0),
        HW_97.5p_3plus=ifelse((HW_97.5p_3plus==1 |lag (HW_97.5p_3plus)==1),1,0),
        HW_99.5p_2plus=ifelse((HW_99.5p_2plus==1 |lag (HW_99.5p_2plus)==1),1,0),
        HW_99.5p_3plus=ifelse((HW_99.5p_3plus==1 |lag (HW_99.5p_3plus)==1),1,0)
)