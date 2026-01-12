#set wd
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/NutrientRegimes")

monthly_results<-read.csv("WRTDS_Outputs_Clean_01082026.csv")

monthly_results_wide <- monthly_results %>%
  select(Stream_Name, chemical, FNConc_mgL, Month) %>%
  pivot_wider(names_from = chemical, values_from = FNConc_mgL, 
              values_fn = function(x) mean(x, na.rm = TRUE))

month_clusters<-read.csv("ClusterStreams_allSolutes.csv")

month_clusters<-month_clusters %>%
  dplyr::mutate(Cluster = case_when(
    Cluster==1~1,
    Cluster==4~2,
    Cluster==2~3,
    Cluster==3~4,
    Cluster==5~5
  )) %>%
  dplyr::select(Stream_Name, chemical, Cluster) %>%
  pivot_wider(names_from = chemical, values_from = Cluster) %>%
  dplyr::mutate(sync_class = case_when(
    DSi == N & N == P ~ "fully coherent",
    DSi == N ~ "Si-N",
    DSi == P ~ "Si-P",
    N == P ~ "N-P",
    TRUE ~ "non-coherent"
  ))

monthly_results_clusters<-left_join(monthly_results, month_clusters[,c(1,5)])

monthly_stoich_cluster_wide<-monthly_results %>%
  select(Stream_Name, chemical, FNConc_uM, Month) %>%
  pivot_wider(names_from = chemical, values_from = FNConc_uM, values_fn = function(x) mean(x, na.rm = T))  %>%
  left_join(month_clusters[,c(1,5)])

threshold = 0.2

monthly_stoich_class<-monthly_stoich_cluster_wide %>%
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
  )

zones<-monthly_stoich_class %>%
  dplyr::group_by(Stream_Name) %>%
  dplyr::summarise(num_zones=n_distinct(zone))

spatial_data<-read.csv("Env_Data_AllSites.csv")

spatial_data<-spatial_data[!duplicated(spatial_data$Stream_Name),]

p1<-monthly_stoich_cluster_wide %>%
  left_join(zones) %>%
  mutate(zone_class=case_when(
    num_zones == 1~"static",
    num_zones > 1~"dynamic"
  )) %>%
  mutate(zone_class=factor(zone_class, levels=c("static", "dynamic"))) %>%
  left_join(month_clusters[,c(1,5)]) %>%
  left_join(spatial_data[,c(3,9)]) %>%
  mutate(impacted_class=factor(impacted_class, levels=c("agricultural & urban", "agricultural", "urban", "less-impacted"))) %>%
  arrange(Stream_Name, Month) %>%
  mutate(sync_class=case_when(
    sync_class=="perfect"~"fully coherent",
    sync_class=="asynchronous"~"non-coherent",
    .default = sync_class
  )) %>%
  mutate(sync_class=factor(sync_class, levels=c("fully coherent", "DSi-N", "DSi-P", "N-P", "non-coherent"))) %>%
  filter(!is.na(impacted_class)) %>%
  ggtern(aes(x=DSi/16, y=N/16, z=P))+
  geom_point(aes(col=impacted_class))+
  #geom_path(aes(col=impacted_class, group=Stream_Name))+
  theme_bw()+
  geom_Tline(Tintercept = .20)+geom_Rline(Rintercept = .20)+geom_Lline(Lintercept = .20)+
  theme(text = element_text(size = 20), legend.position = "none")+labs(col="Impacted Class", x="DSi", y="N", z="P")+
  scale_color_manual(values = c("darkorange3","goldenrod3", "tan", "palegreen4"))+
  facet_wrap(~zone_class)+
  guides(color = guide_legend(reverse=T))

p1

p2<-monthly_stoich_cluster_wide %>%
  left_join(zones) %>%
  mutate(zone_class=case_when(
    num_zones == 1~"static",
    num_zones > 1~"dynamic"
  )) %>%
  mutate(zone_class=factor(zone_class, levels=c("static", "dynamic"))) %>%
  left_join(month_clusters[,c(1,5)]) %>%
  left_join(spatial_data[,c(3,9)]) %>%
  mutate(impacted_class=factor(impacted_class, levels=c("agricultural & urban", "agricultural", "urban", "less-impacted"))) %>%
  mutate(sync_class=factor(sync_class, levels=c("fully coherent", "Si-N", "Si-P", "N-P", "non-coherent"))) %>%
  filter(!is.na(impacted_class)) %>%
  ggplot(aes(y=sync_class))+geom_bar(aes(fill=impacted_class), position = "fill")+
  facet_wrap(~zone_class)+
  theme_classic()+scale_y_discrete(limits=rev)+
  scale_fill_manual(values = c("darkorange3","goldenrod3", "tan", "palegreen4"))+
  labs(y="", x="Proportion of sites", fill="")+
  theme(text = element_text(size = 20), legend.position = "top")+
  guides(fill = guide_legend(reverse=T))

pdf("LULC_Ternary_Updated01122026.pdf", width=13, height = 6)

p1

dev.off()

pdf("LULC_Ternary_Barplot_Updated01122026.pdf", width=13, height = 4)

p2

dev.off()

#for calculating proporitons

monthly_stoich_cluster_wide %>%
  left_join(zones) %>%
  mutate(zone_class=case_when(
    num_zones == 1~"static",
    num_zones > 1~"dynamic"
  )) %>%
  mutate(zone_class=factor(zone_class, levels=c("static", "dynamic"))) %>%
  left_join(month_clusters[,c(1,5)]) %>%
  left_join(spatial_data[,c(3,9)]) %>%
  filter(zone_class=="dynamic") %>%
  group_by(sync_class, impacted_class) %>%
  tally()

monthly_stoich_cluster_wide %>%
  left_join(zones) %>%
  mutate(zone_class=case_when(
    num_zones == 1~"static",
    num_zones > 1~"dynamic"
  )) %>%
  mutate(zone_class=factor(zone_class, levels=c("static", "dynamic"))) %>%
  left_join(month_clusters[,c(1,5)]) %>%
  left_join(spatial_data[,c(3,9)]) %>%
  filter(zone_class=="dynamic") %>%
  group_by(sync_class) %>%
  tally()
