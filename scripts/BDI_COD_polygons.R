# Geography of the BDI_COD (PMSI23_CODE): aggregate polygon of the INSEE codes part of the BDI_COD ####
com2b <- com2%>%
  left_join(bdi_insee_pop)

str(com2b)
bdi_cod <- com2b %>%
  group_by(PMSI23_CODE)%>%
  summarise(geometry=st_union(geometry))
head(bdi_cod)
mapview(bdi_cod)

saveRDS(bdi_cod, "output/bdicod_2023_polyg.rds")
