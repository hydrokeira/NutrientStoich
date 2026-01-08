### clean WRTDS monthly output file for all analysis
require(readxl)
require(dplyr)
require(tidyverse)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/NutrientRegimes")

output<-read.csv("Full_Results_WRTDS_kalman_monthly.csv")

output <- output %>%
  dplyr::mutate(chemical=case_when(
    chemical=="NO3"~"N",
    chemical=="NOx"~"N",
    .default = chemical
  )) %>%
  filter(chemical %in% c("DSi", "N", "P"))

unique(output$chemical)

QLog<-read_xlsx("Site_Reference_Table_01082026.xlsx")

#extract columns you need
QLog<-QLog[,c("Stream_Name", "Latitude", "Longitude")]

output<-left_join(output, QLog)

southern_hemi<-subset(output, output$Latitude < 0)
southern_hemi_sites<-unique(southern_hemi$Stream_Name)

southern_hemi_sites

output_normalized <- output %>%
  mutate(
    Month = if_else(
      Stream_Name %in% southern_hemi,
      ((Month + 6 - 1) %% 12) + 1,  # shift by 6 months, wrapping 1–12
      Month
    )
  )

unique(output_normalized$Stream_Name)

#remove sites without continuous flow
remove_site<-c("SADDLE STREAM 007", "MARTINELLI")

output_normalized<-output_normalized[!output_normalized$Stream_Name %in% remove_site,]

output_normalized<-output_normalized[!output_normalized$LTER=="MCM",]

num_solutes<-output_normalized %>%
  group_by(Stream_Name) %>%
  summarise(num_chem=n_distinct(chemical))

good_sites<-num_solutes %>%
  filter(num_chem==3)

output_normalized_complete<-output_normalized %>%
  filter(Stream_Name %in% good_sites$Stream_Name)

unique(output_normalized_complete$Stream_Name)

output_normalized_complete %>%
  group_by(LTER) %>%
  summarise(num_sites=n_distinct(Stream_Name))

output_normalized_complete<-output_normalized_complete[complete.cases(output_normalized_complete$FNConc_uM),]

test_complete_years<-output_normalized_complete %>%
  group_by(Stream_Name, chemical, Year) %>%
  summarise(num_obs=n_distinct(Month)) %>%
  filter(num_obs==12) %>%
  mutate(unique=paste(Stream_Name, chemical, Year))

output_normalized_complete_months<-output_normalized_complete %>%
  mutate(unique=paste(Stream_Name, chemical, Year)) %>%
  filter(unique %in% test_complete_years$unique)

write.csv(output_normalized_complete_months, "WRTDS_Outputs_Clean_01082026.csv")

