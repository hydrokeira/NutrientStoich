#calculate KG Climate for each site, merge in land use
require(dplyr)
require(kgc)
require(tidyr)
require(ggplot2)
require(ggpubr)

setwd("/Users/keirajohnson/Library/CloudStorage/Box-Box/Keira_Johnson/SiSyn/NutrientRegimes")

monthly_results<-read.csv("WRTDS_Outputs_Clean_01082026.csv")

sites_unique<-monthly_results %>%
  select(LTER, Stream_Name, Latitude, Longitude) %>%
  distinct()

#format data to pull using KGC
coord_data<-data.frame(sites_unique, rndCoord.lon=RoundCoordinates(sites_unique$Longitude), 
                       rndCoord.lat=RoundCoordinates(sites_unique$Latitude))

#get KG for each site
data <- data.frame(coord_data,ClimateZ=LookupCZ(coord_data))

setwd("/Users/keirajohnson/Library/CloudStorage/Box-Box/Keira_Johnson/SiSyn")

KG_name<-read.csv("KG_Clim_Name.csv")

data<-merge(data, KG_name, by="ClimateZ")

monthly_climate<-data[,c(2:5,9)]

setwd("/Users/keirajohnson/Library/CloudStorage/Box-Box/Keira_Johnson/SiSyn/NutrientRegimes")

lulc<-read.csv("LULC_TempVar_GLC1000m.csv")

setwd("/Users/keirajohnson/Library/CloudStorage/Box-Box/Keira_Johnson/SiSyn")
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

setwd("/Users/keirajohnson/Library/CloudStorage/Box-Box/Keira_Johnson/SiSyn/NutrientRegimes")

write.csv(monthly_climate_LULC, "Env_Data_AllSites.csv")

monthly_results<-read.csv("WRTDS_Outputs_Clean_01082026.csv")

p1<-monthly_results %>%
  left_join(monthly_climate_LULC) %>%
  mutate(chemical=factor(chemical, levels=c("N", "P", "DSi"))) %>%
  filter(FNConc_mgL < 1E4) %>%
  ggplot(aes(x=cut_width(Cropland, 0.05, boundary = 0), FNConc_mgL))+
  annotate("rect",
           xmin = 14, xmax = Inf,
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.2) +
  geom_boxplot(outliers = F, fill=NA)+facet_wrap(~chemical, scales="free_y", nrow=3)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size = 20))+
  labs(x="Proportion Cropland", y="Concentration")+ggtitle("Agricultural Threshold (≥ 0.65)")

p2<-monthly_results %>%
  left_join(monthly_climate_LULC) %>%
  mutate(chemical=factor(chemical, levels=c("N", "P", "DSi"))) %>%
  ggplot(aes(x=cut_width(Impervious, 0.01, boundary = 0), FNConc_mgL))+
  annotate("rect",
           xmin = 3, xmax = Inf,
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.2) +
  geom_boxplot(outliers = F, fill=NA)+facet_wrap(~chemical, scales="free_y", nrow=3)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size = 20))+
  labs(x="Proportion Impervious", y="Concentration")+ggtitle("Urban Threshold (≥ 0.03)")

pdf("LandUseThresholds.pdf", width = 15, height = 9)
ggarrange(p1, p2)
dev.off()


