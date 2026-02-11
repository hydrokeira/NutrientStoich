require(ggtern)
require(cetcolor)
require(dplyr)
require(ggplot2)

#### read in and data cleaning steps - this sets up for all solutes ####

#set wd
setwd("/Users/keirajohnson/Library/CloudStorage/Box-Box/Keira_Johnson/SiSyn/NutrientRegimes")

monthly_results<-read.csv("WRTDS_Outputs_Clean_01082026.csv")

monthly_results_wide <- monthly_results %>%
  select(Stream_Name, chemical, FNConc_mgL, Month) %>%
  pivot_wider(names_from = chemical, values_from = FNConc_mgL, 
              values_fn = function(x) mean(x, na.rm = TRUE))

month_clusters<-read.csv("ClusterStreams_allSolutes.csv")

month_clusters<-month_clusters %>%
  mutate(Cluster = case_when(
    Cluster==1~1,
    Cluster==4~2,
    Cluster==2~3,
    Cluster==3~4,
    Cluster==5~5
  ))

monthly_results_clusters<-left_join(monthly_results, month_clusters[,c(2,3,16)])

monthly_stoich_cluster_wide<-monthly_results %>%
  select(Stream_Name, chemical, FNConc_uM, Month) %>%
  pivot_wider(names_from = chemical, values_from = FNConc_uM, values_fn = function(x) mean(x, na.rm = T))  %>%
  left_join(month_clusters[,c(2,3,16)])

#### calculate depletion states ####

threshold = 0.2

monthly_stoich_class_16<-monthly_stoich_cluster_wide %>%
  mutate(
    total = DSi/16 + N/16 + P,
    DSi_frac = (DSi/16) / total,
    N_frac   = (N/16) / total,
    P_frac   = P / total
  ) %>%
  mutate(
    zone = case_when(
      DSi_frac < threshold & N_frac < threshold ~ "Si-N depleted",
      DSi_frac < threshold & P_frac < threshold ~ "Si-P depleted",
      N_frac < threshold & P_frac < threshold ~ "N-P depleted",
      DSi_frac < threshold ~ "Si depleted",
      N_frac < threshold ~ "N depleted",
      P_frac < threshold ~ "P depleted",
      TRUE ~ "Balanced"
    )
  ) %>%
  mutate(Si_molar=16)

#### calculate depletion states ####

threshold = 0.2

#### for 16 ####
monthly_stoich_class_16<-monthly_stoich_cluster_wide %>%
  mutate(
    total = DSi/16 + N/16 + P,
    DSi_frac = (DSi/16) / total,
    N_frac   = (N/16) / total,
    P_frac   = P / total
  ) %>%
  mutate(
    zone = case_when(
      DSi_frac < threshold & N_frac < threshold ~ "Si-N depleted",
      DSi_frac < threshold & P_frac < threshold ~ "Si-P depleted",
      N_frac < threshold & P_frac < threshold ~ "N-P depleted",
      DSi_frac < threshold ~ "Si depleted",
      N_frac < threshold ~ "N depleted",
      P_frac < threshold ~ "P depleted",
      TRUE ~ "Balanced"
    )
  ) %>%
  mutate(Si_molar=16)

#### for 20 ####
monthly_stoich_class_20<-monthly_stoich_cluster_wide %>%
  mutate(
    total = DSi/20 + N/16 + P,
    DSi_frac = (DSi/20) / total,
    N_frac   = (N/16) / total,
    P_frac   = P / total
  ) %>%
  mutate(
    zone = case_when(
      DSi_frac < threshold & N_frac < threshold ~ "Si-N depleted",
      DSi_frac < threshold & P_frac < threshold ~ "Si-P depleted",
      N_frac < threshold & P_frac < threshold ~ "N-P depleted",
      DSi_frac < threshold ~ "Si depleted",
      N_frac < threshold ~ "N depleted",
      P_frac < threshold ~ "P depleted",
      TRUE ~ "Balanced"
    )
  ) %>%
  mutate(Si_molar=20)

#### for 40 ####
monthly_stoich_class_40<-monthly_stoich_cluster_wide %>%
  mutate(
    total = DSi/40 + N/16 + P,
    DSi_frac = (DSi/40) / total,
    N_frac   = (N/16) / total,
    P_frac   = P / total
  ) %>%
  mutate(
    zone = case_when(
      DSi_frac < threshold & N_frac < threshold ~ "Si-N depleted",
      DSi_frac < threshold & P_frac < threshold ~ "Si-P depleted",
      N_frac < threshold & P_frac < threshold ~ "N-P depleted",
      DSi_frac < threshold ~ "Si depleted",
      N_frac < threshold ~ "N depleted",
      P_frac < threshold ~ "P depleted",
      TRUE ~ "Balanced"
    )
  ) %>%
  mutate(Si_molar=40)

Si_molar_test<-bind_rows(monthly_stoich_class_16, monthly_stoich_class_20, monthly_stoich_class_40)

k1<-Si_molar_test %>%
  dplyr::filter(chemical=="P") %>%
  dplyr::mutate(zone=factor(zone, levels=c("Balanced", "N depleted", "P depleted", "Si depleted",
                                           "N-P depleted", "Si-N depleted", "Si-P depleted"))) %>%
  ggplot(aes(x=zone, fill=as.factor(Si_molar), group=Si_molar))+
  geom_bar(position="dodge")+theme_classic()+
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x="", y="Number of Site-Month\nObservations", fill="Molar Ratio\nof Si")+
  scale_fill_manual(values = c("#a8dbd9", "#4f90a6", "#2a5674"))

p1<-monthly_stoich_class_16 %>%
  dplyr::left_join(month_clusters) %>%
  dplyr::filter(chemical=="P") %>%
  ggtern(aes(x=DSi/16, y=N/16, z=P))+
  geom_point(alpha=0.2, col="#a8dbd9")+
  scale_color_manual(values = cet_pal(12, "c4s"))+theme_bw()+
  geom_Tline(Tintercept = .20)+geom_Rline(Rintercept = .20)+geom_Lline(Lintercept = .20)+
  theme(text = element_text(size = 20), legend.position = "null")+
  labs(col="Month", x="DSi", y="N", z="P")
p1

p2<-monthly_stoich_class_16 %>%
  dplyr::left_join(month_clusters) %>%
  dplyr::filter(chemical=="P") %>%
  ggtern(aes(x=DSi/20, y=N/16, z=P))+
  geom_point(alpha=0.2, col="#4f90a6")+
  scale_color_manual(values = cet_pal(12, "c4s"))+theme_bw()+
  geom_Tline(Tintercept = .20)+geom_Rline(Rintercept = .20)+geom_Lline(Lintercept = .20)+
  theme(text = element_text(size = 20), legend.position = "null")+
  labs(col="Month", x="DSi", y="N", z="P")
p2
p3<-monthly_stoich_class_16 %>%
  dplyr::left_join(month_clusters) %>%
  dplyr::filter(chemical=="P") %>%
  ggtern(aes(x=DSi/40, y=N/16, z=P))+
  geom_point(alpha=0.2, col="#2a5674")+
  scale_color_manual(values = cet_pal(12, "c4s"))+theme_bw()+
  geom_Tline(Tintercept = .20)+geom_Rline(Rintercept = .20)+geom_Lline(Lintercept = .20)+
  theme(text = element_text(size = 20), legend.position = "null")+
  labs(col="Month", x="DSi", y="N", z="P")
p3
set1<-ggarrange(p1, p2, p3, nrow = 1)

pdf("Si_Molar_Test.pdf", width = 12, height = 8)

ggarrange(set1, k1, nrow=2, heights = c(0.5, 0.65))

dev.off()

Si_molar_test <- Si_molar_test %>%
  mutate(obs_id = paste(Stream_Name, Month, chemical, sep = "_"))

zone_wide <- Si_molar_test %>%
  filter(chemical == "P") %>%
  select(obs_id, Si_molar, zone) %>%
  pivot_wider(names_from = Si_molar,
              values_from = zone,
              names_prefix = "Si_")

transitions_changed <- zone_wide %>%
  filter(Si_16 != Si_20)

trans_16_20 <- transitions_changed %>%
  count(Si_16, Si_20, name = "Freq")

p1<-ggplot(trans_16_20,
       aes(axis1 = Si_16, axis2 = Si_20, y = Freq)) +
  geom_alluvium(width = 1/4) +
  geom_stratum(width = 1/4, fill = "grey90", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Si 16", "Si 20"),
                   expand = c(.1, .1)) +
  labs(x = "Si Molar Ratio",
       y = "Number of Observations") +
  theme_classic() +
  theme(text = element_text(size = 18))+
  scale_y_continuous(limits = c(0,1500))

transitions_changed <- zone_wide %>%
  filter(Si_16 != Si_40)

trans_16_40 <- transitions_changed %>%
  count(Si_16, Si_40, name = "Freq")

p2<-ggplot(trans_16_40,
       aes(axis1 = Si_16, axis2 = Si_40, y = Freq)) +
  geom_alluvium(width = 1/4) +
  geom_stratum(width = 1/4, fill = "grey90", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Si 16", "Si 40"),
                   expand = c(.1, .1)) +
  labs(x = "Si Molar Ratio",
       y = "Number of Observations") +
  theme_classic() +
  theme(text = element_text(size = 18))+
  scale_y_continuous(limits = c(0,1500))

p2

pdf("Shifts_Depletion_SiMolar.pdf", width = 16, height = 10)
ggarrange(p1,p2)
dev.off()

