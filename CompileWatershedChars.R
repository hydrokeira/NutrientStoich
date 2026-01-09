#calculate KG Climate for each site, merge in land use
require(dplyr)
require(kgc)
require(tidyr)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/NutrientRegimes")

monthly_results<-read.csv("WRTDS_Outputs_Clean_01082026.csv")

sites_unique<-monthly_results %>%
  select(LTER, Stream_Name, Latitude, Longitude) %>%
  distinct()

#format data to pull using KGC
coord_data<-data.frame(sites_unique, rndCoord.lon=RoundCoordinates(sites_unique$Longitude), 
                       rndCoord.lat=RoundCoordinates(sites_unique$Latitude))

#get KG for each site
data <- data.frame(coord_data,ClimateZ=LookupCZ(coord_data))

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

KG_name<-read.csv("KG_Clim_Name.csv")

data<-merge(data, KG_name, by="ClimateZ")

monthly_climate<-data[,c(2:5,9)]

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/NutrientRegimes")

lulc<-read.csv("LULC_TempVar_GLC1000m.csv")

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")
LC_simple<-read.csv("LULC_Class_Simple.csv")
colnames(LC_simple)[1]<-"LandClass"

lulc<-left_join(lulc, LC_simple)

lulc_avg <- lulc %>%
  dplyr::filter(Simple_Class %in% c("Impervious", "Cropland")) %>%
  dplyr::group_by(Stream_Name, Simple_Class) %>%
  dplyr::summarise(avg_proportion = mean(prop, na.rm = T)) %>%
  pivot_wider(names_from = Simple_Class, values_from = avg_proportion)

monthly_climate_LULC<-left_join(monthly_climate, lulc_avg)

monthly_climate_LULC$Cropland<-as.numeric(monthly_climate_LULC$Cropland)
monthly_climate_LULC$Impervious<-as.numeric(monthly_climate_LULC$Impervious)

monthly_climate_LULC$Cropland[is.na(monthly_climate_LULC$Cropland)] <- 0
monthly_climate_LULC$Impervious[is.na(monthly_climate_LULC$Impervious)] <- 0

monthly_climate_LULC <- monthly_climate_LULC %>%
  mutate(impacted_class = case_when(
    Impervious >= 0.03 & Cropland < 0.65 ~ "urban",
    Impervious >= 0.03 & Cropland >= 0.65 ~ "agricultural & urban",
    Impervious < 0.03 & Cropland >= 0.65 ~ "agricultural",
    .default = "less-impacted"
  ))

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/NutrientRegimes")

write.csv(monthly_climate_LULC, "Env_Data_AllSites.csv")
