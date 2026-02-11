#set wd
require(dplyr)
require(tidyr)
require(tidytext)
require(ggplot2)

setwd("/Users/keirajohnson/Library/CloudStorage/Box-Box/Keira_Johnson/SiSyn/NutrientRegimes")

#read in monthly results
monthly_results<-read.csv("WRTDS_Outputs_Clean_01082026.csv")

month_clusters<-read.csv("ClusterStreams_allSolutes.csv")

month_clusters<-month_clusters %>%
  dplyr::mutate(Cluster = case_when(
    Cluster==1~1,
    Cluster==4~2,
    Cluster==2~3,
    Cluster==3~4,
    Cluster==5~5
  )) %>%
  dplyr::mutate(Cluster = case_when(
    Cluster==1~"ESp",
    Cluster==2~"Sp",
    Cluster==3~"Su",
    Cluster==4~"Fa",
    Cluster==5~"A"
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

colnames(month_clusters)<-c("Stream_Name", "cluster_DSi", "cluster_N", "cluster_P","sync_class")

monthly_stoich_cluster_wide<-monthly_results %>%
  select(Stream_Name, chemical, FNConc_uM, Month) %>%
  pivot_wider(names_from = chemical, values_from = FNConc_uM, values_fn = function(x) mean(x, na.rm = T))  %>%
  left_join(month_clusters)

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


zones_count<-monthly_stoich_class %>%
  group_by(Stream_Name) %>%
  summarise(num_zones=n_distinct(zone))

monthly_stoich_class<-left_join(monthly_stoich_class, zones_count)

monthly_stoich_class <- monthly_stoich_class %>%
  mutate(stoich_state = case_when(
    num_zones==1~"static",
    .default = "dynamic"
  ),
  sSi=DSi/16, sN=N/16, sP=P)

monthly_stoich_class %>%
  filter(!duplicated(Stream_Name)) %>%
  group_by(stoich_state, sync_class) %>%
  tally()

monthly_stoich_class <- monthly_stoich_class %>%
  mutate(scenario=case_when(
    stoich_state=="static" & zone=="Balanced"~"panel_b",
    stoich_state=="static" & zone!="Balanced"~"panel_a",
    stoich_state=="dynamic" & sync_class=="fully coherent"~"panel_d",
    TRUE ~ "panel_c"
  ),
  regime_set = paste(cluster_N, "-", cluster_DSi, "-", cluster_P))

spatial_data<-read.csv("Env_Data_AllSites.csv")

regime_sets_panels<-monthly_stoich_class %>%
  filter(!duplicated(Stream_Name)) %>%
  left_join(spatial_data[,c(3,9)]) %>%
  filter(!duplicated(Stream_Name)) 
  
regime_sets_panels_count<-monthly_stoich_class %>%
    filter(!duplicated(Stream_Name)) %>%
    dplyr::group_by(scenario, regime_set) %>%
    tally() %>%
    ungroup() %>%
    dplyr::group_by(scenario) %>%
    arrange(-n) %>%
    mutate(num_sites=sum(n), prop=cumsum(n)/num_sites) %>%
    dplyr::group_by(scenario)

c25 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)



p2<-regime_sets_panels %>%
    left_join(spatial_data[,c(2,3)]) %>%
    left_join(regime_sets_panels_count) %>%
    filter(!is.na(n)) %>%
    filter(scenario %in% c("panel_a", "panel_c")) %>%
    mutate(regime_set = reorder_within(regime_set, -n, scenario)) %>%
    ggplot(aes(regime_set, fill=LTER))+geom_bar(stat = "count",col="black")+
    facet_wrap(~scenario, scales = "free_x", space="free_x")+theme_classic()+
    scale_x_reordered() +
    theme(text = element_text(size = 20), axis.text.x = element_text(size = 15, angle = 90, vjust=0.5),
          legend.position = "null")+
    labs(y="Number of Sites", x="Regime Set (N - Si - P)", fill="Observation Network")+
    scale_fill_manual(values = c25)

p1<-regime_sets_panels %>%
  left_join(spatial_data[,c(3,9)]) %>%
  mutate(impacted_class=factor(impacted_class, levels=c("less-impacted", "urban", "agricultural", "agricultural & urban"))) %>%
  left_join(regime_sets_panels_count) %>%
  filter(!is.na(n)) %>%
  filter(scenario %in% c("panel_a", "panel_c")) %>%
  mutate(regime_set = reorder_within(regime_set, -n, scenario)) %>%
  ggplot(aes(regime_set, fill=impacted_class))+geom_bar(stat = "count",col="black")+
  facet_wrap(~scenario, scales = "free_x", space="free_x")+theme_classic()+
  scale_x_reordered() +
  theme(text = element_text(size = 20), axis.text.x = element_blank(),
        legend.position = "null")+
  labs(y="Number of Sites", x="", fill="Observation Network")+
  scale_fill_manual(values = c("palegreen4", "tan", "goldenrod", "darkorange3"))

pdf("Panels_Sets_LULC_LTER_Distributions.pdf", width = 16, height = 10)

ggarrange(p1, p2, align = "v", nrow = 2, heights = c(0.45, 0.7))

dev.off()

monthly_stoich_class_panela_c <- monthly_stoich_class %>%
  filter(scenario == "panel_a" |
           scenario == "panel_c")

unique(monthly_stoich_class_fig10$Stream_Name)

monthly_stoich_class_fig10 <- monthly_stoich_class %>%
  filter(scenario == "panel_a" & regime_set %in% c("3 - 3 - 3", "3 - 3 - 2", "5 - 2 - 1") |
           scenario == "panel_c" & regime_set %in% c("3 - 2 - 1", "3 - 2 - 5", "5 - 2 - 1"))

monthly_stoich_class_fig10 %>%
  left_join(spatial_data[,c(3,9)]) %>%
  mutate(impacted_class=factor(impacted_class, levels=c("less-impacted", "urban", "agricultural", "agricultural & urban"))) %>%
  left_join(regime_sets_panels_count) %>%
  filter(!is.na(n)) %>%
  filter(scenario %in% c("panel_a", "panel_c")) %>%
  mutate(regime_set = reorder_within(regime_set, -n, scenario)) %>%
  ggplot(aes(regime_set, fill=impacted_class))+geom_bar(stat = "count")+
  facet_wrap(~scenario, scales = "free_x", nrow = 2)+theme_classic()+
  scale_x_reordered() +
  scale_fill_manual(values = c("palegreen4", "tan", "goldenrod", "darkorange3"))+
  theme(text = element_text(size = 20))+
  labs(y="Number of Sites", x="Regime Set (N - Si - P)")

monthly_results_norm <- monthly_results %>%
  dplyr::group_by(Stream_Name, chemical) %>%
  dplyr::mutate(norm_conc=scale(FNConc_mgL))

#aggregate by month and stream to get one value for every month and stream pair
month_norm_agg<-aggregate(norm_conc ~ Month+Stream_Name+chemical, data = monthly_results_norm, FUN=mean)

monthly_stoich_class_fig10_long <- monthly_stoich_class_fig10 %>%
  dplyr::left_join(month_norm_agg)

colnames(monthly_stoich_class_fig10_long)[23]<-"norm_value"

pdf("Panels_Sets_Regimes.pdf", width = 9, height = 11)

monthly_stoich_class_fig10_long %>%
  mutate(chemical=factor(chemical, levels=c("N", "DSi", "P"))) %>%
  mutate(regime_set=factor(regime_set, 
                           levels=c("3 - 3 - 3","3 - 2 - 1", "3 - 3 - 2", "3 - 2 - 5", "5 - 2 - 1"))) %>%
  ggplot(aes(Month, norm_value))+
  geom_smooth(aes(group=chemical, col=chemical, lty=chemical), se=F) + facet_wrap(~regime_set, nrow = 3)+
  theme_classic()+scale_color_manual(values = c("grey35","grey75", "black"))+
  scale_linetype_manual(values = c("dashed", "solid", "dotted"))+
  theme(text = element_text(size = 20), legend.position = "bottom")+
  scale_x_continuous(breaks = seq(1,12,1), labels = seq(1,12,1))
  
dev.off()

monthly_stoich_class_long <- monthly_stoich_class %>%
  pivot_longer(cols = 17:19, values_to = "scaled_conc", names_to = "solute")

sites<-unique(monthly_stoich_class_long$Stream_Name)

pdf("All_Sites_RRNorm_Conc.pdf", width = 8, height = 6)

for (i in 1:length(sites)) {
  
  print(i)
  
  one_site<-monthly_stoich_class_long %>%
    filter(Stream_Name==sites[i])
  
  p1<-one_site %>%
    mutate(solute=factor(solute, levels=c("sN", "sSi", "sP"))) %>%
    ggplot(aes(Month, scaled_conc, col=solute))+geom_line(aes(group=solute), size=1)+
    theme_classic(base_size = 20)+
    ggtitle(paste(sites[i]), 
            subtitle = paste0(one_site$stoich_state[1], "; ", one_site$sync_class[1]))+
    scale_x_continuous(labels = seq(1,12,1), breaks = seq(1,12,1))+labs(x="Month", y="RR Norm Concentration")+
    scale_color_manual(values = c("grey35", "grey75", "black"))
  
  print(p1)
  
}

dev.off()

panel_a <-190
panel_c<-2
panel_b<-15
panel_d<-120

pdf("Dynamic_StoichClass_Sites_Concentrations.pdf", width = 12, height = 8)

for (i in 1:nrow(many_zones)) {
  
  print(i)
  
  one_site<-monthly_stoich_class_dynamic %>%
    filter(Stream_Name==many_zones$Stream_Name[i])
  
  subtitle_text<-c(paste0("Clusters: ", "Si", "-", one_site[1,6], ", ", 
                          "N", "-", one_site[1,7], ", ",
                          "P", "-", one_site[1,8]))
  
  one_site<-one_site %>%
    pivot_longer(cols = c(3:5), names_to = "solute", values_to = "concentration") %>%
    pivot_longer(cols = c(6:8), names_to = "solute_frac", values_to = "fraction")
  
  p1<-ggplot(one_site, aes(Month, concentration, col=solute))+geom_line()+
    theme_classic()+ggtitle(many_zones$Stream_Name[i], subtitle = subtitle_text)+
    theme(text = element_text(size = 20))+labs(x="Month", y="Concentration (uM)",
                                               col="Solute")
  
  p2<-ggplot(one_site, aes(Month, fraction, col=solute_frac))+geom_line()+
    theme_classic()+geom_hline(yintercept = 0.2)+
    theme(text = element_text(size = 20))+labs(x="Month", y="Concentration Fraction",
                                               col="Solute")
  
  p3<-ggarrange(p1, p2, nrow = 2, widths = c(0.7, 0.55))
  
  print(p3)
  
}

dev.off()

stream_names<-unique(monthly_stoich_class$Stream_Name)

pdf("AllSites_Concentrations.pdf", width = 10, height = 8)

for (i in 1:length(stream_names)) {
  
  print(i)
  
  one_site<-monthly_stoich_class %>%
    filter(Stream_Name==stream_names[i])
  
  one_site<-one_site %>%
    pivot_longer(cols = c(3:5), names_to = "solute", values_to = "concentration")
  
  p1<-ggplot(one_site, aes(Month, concentration, col=solute))+geom_line()+
    theme_classic()+ggtitle(stream_names[i])+
    theme(text = element_text(size = 20))+labs(x="Month", y="Concentration (uM)",
                                               col="Solute")
  
  print(p1)
  
}

dev.off()

