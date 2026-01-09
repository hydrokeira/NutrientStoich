require(ggtern)
require(cetcolor)

#### read in and data cleaning steps - this sets up for all solutes ####

#set wd
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/NutrientRegimes")

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

####plot example of one stream - Mississippi river, colored by Month
monthly_stoich_cluster_wide %>%
  filter(chemical=="P" & Stream_Name == "Mississippi River near St. Francisville") %>%
  ggtern(aes(x=DSi/16, y=N/16, z=P, col=as.factor(Month)))+geom_point()+
  scale_color_manual(values = cet_pal(12, "c4s"))+theme_bw()+
  geom_Tline(Tintercept = .20)+geom_Rline(Rintercept = .20)+geom_Lline(Lintercept = .20)+
  theme(text = element_text(size = 20))+labs(col="Month", x="DSi", y="N", z="P")

monthly_stoich_cluster_wide %>%
  filter(chemical=="P" & Stream_Name == "Mississippi River near St. Francisville") %>%
  ggtern(aes(x=DSi/16, y=N/16, z=P))+geom_path(col="red")+
  scale_color_manual(values = cet_pal(12, "c4s"))+theme_bw()+
  geom_Tline(Tintercept = .20)+geom_Rline(Rintercept = .20)+geom_Lline(Lintercept = .20)+
  theme(text = element_text(size = 20))+labs(col="Month", x="DSi", y="N", z="P")

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

annual_stoich<-monthly_stoich_cluster_wide %>%
  dplyr::mutate(
    total = DSi/16 + N/16 + P,
    DSi_frac = (DSi/16) / total,
    N_frac   = (N/16) / total,
    P_frac   = P / total
  ) %>%
  dplyr::filter(chemical=="P") %>%
  dplyr::group_by(Stream_Name) %>%
  dplyr::summarise(mean_DSi=mean(DSi_frac), mean_N=mean(N_frac), mean_P=mean(P_frac)) %>%
  dplyr::mutate(
    annual_zone = case_when(
      mean_DSi < threshold & mean_N < threshold ~ "Si-N depleted",
      mean_DSi < threshold & mean_P < threshold ~ "Si-P depleted",
      mean_N < threshold & mean_P < threshold ~ "N-P depleted",
      mean_DSi < threshold ~ "Si depleted",
      mean_N < threshold ~ "N depleted",
      mean_P < threshold ~ "P depleted",
      TRUE ~ "Balanced"
    )
  )

p3<-monthly_stoich_class %>%
  dplyr::filter(chemical=="P") %>%
  dplyr::left_join(annual_stoich) %>%
  pivot_longer(cols = c(12,16), names_to = "scale", values_to = "depletion_state") %>%
  dplyr::mutate(scale=case_when(
    scale=="zone"~"monthly",
    scale=="annual_zone"~"annual",
  )) %>%
  dplyr::mutate(depletion_state=factor(depletion_state, levels=c("Balanced", "N depleted", "P depleted", "Si depleted",
                                                          "N-P depleted", "Si-N depleted", "Si-P depleted"))) %>%
  dplyr::group_by(depletion_state, scale) %>%
  ggplot(aes(y=depletion_state, fill=scale))+
  geom_bar(position = "dodge", col="black")+theme_classic()+
  theme(text = element_text(size = 20), legend.position = "bottom")+
  scale_fill_manual(values = c("black", "white"))+scale_y_discrete(limits=rev)+
  labs(y="", x="Number of Site-Month Observations", fill="")+
  guides(fill = guide_legend(reverse=T))

p3

#calculate proportion of each class
monthly_stoich_class %>%
  dplyr::filter(chemical=="P") %>%
  dplyr::left_join(annual_stoich) %>%
  pivot_longer(cols = c(12,16), names_to = "scale", values_to = "depletion_state") %>%
  dplyr::mutate(scale=case_when(
    scale=="zone"~"monthly",
    scale=="annual_zone"~"annual",
  )) %>%
  dplyr::mutate(depletion_state=factor(depletion_state, levels=c("Balanced", "N depleted", "P depleted", "Si depleted",
                                                          "N-P depleted", "Si-N depleted", "Si-P depleted"))) %>%
  dplyr::group_by(depletion_state, scale) %>%
  dplyr::filter(scale=="monthly") %>%
  dplyr::summarise(prop=(n()/3924)*100)

p4<-monthly_stoich_class %>%
  dplyr::filter(chemical=="P") %>%
  dplyr::left_join(annual_stoich) %>%
  pivot_longer(cols = c(12,16), names_to = "scale", values_to = "depletion_state") %>%
  dplyr::mutate(scale=case_when(
    scale=="zone"~"monthly",
    scale=="annual_zone"~"annual",
  )) %>%
  dplyr::mutate(depletion_state=factor(depletion_state, levels=c("Balanced", "N depleted", "P depleted", "Si depleted",
                                                          "N-P depleted", "Si-N depleted", "Si-P depleted"))) %>%
  dplyr::group_by(depletion_state, scale) %>%
  dplyr::summarise(num_sites=n_distinct(Stream_Name)) %>%
  ggplot(aes(y=depletion_state, x=num_sites, fill=scale))+
  geom_bar(position = "dodge", col="black", stat = "identity")+
  theme_classic()+
  theme(text = element_text(size = 20), legend.position = "bottom")+
  scale_fill_manual(values = c("black", "white"))+scale_y_discrete(limits=rev)+
  labs(y="", x="Number of Sites", fill="")+
  guides(fill = guide_legend(reverse=T))

p4

p1<-monthly_stoich_cluster_wide %>%
  dplyr::group_by(Stream_Name) %>%
  dplyr::summarise(mean_DSi=mean(DSi), mean_N=mean(N), mean_P=mean(P)) %>%
  ggtern(aes(x=mean_DSi/16, y=mean_N/16, z=mean_P))+
  geom_point()+
  theme_bw()+
  geom_Tline(Tintercept = .20)+geom_Rline(Rintercept = .20)+geom_Lline(Lintercept = .20)+
  theme(text = element_text(size = 20), legend.position = "bottom")+
  labs(x="DSi", y="N", z="P")

p1

p2<-monthly_stoich_cluster_wide %>%
  dplyr::left_join(month_clusters) %>%
  dplyr::filter(chemical=="P") %>%
  ggtern(aes(x=DSi/16, y=N/16, z=P, col=as.factor(Month)))+
  geom_point()+
  scale_color_manual(values = cet_pal(12, "c4s"))+theme_bw()+
  geom_Tline(Tintercept = .20)+geom_Rline(Rintercept = .20)+geom_Lline(Lintercept = .20)+
  theme(text = element_text(size = 20), legend.position = "null")+
  labs(col="Month", x="DSi", y="N", z="P")
  

p2

set1<-ggarrange(p1, p2)

set2<-ggarrange(p3, p4, align = "h")

pdf("Annual_Monthly_Ternary_Updated01092026.pdf", width = 12.5, height = 12.5)

ggarrange(set1, set2, nrow = 2, heights = c(0.7, 0.55))

dev.off()

p5<-monthly_stoich_class %>%
  dplyr::group_by(zone, Month) %>%
  tally(name="Month_count") %>%
  left_join(overall_count) %>%
  mutate(prop=Month_count/n) %>%
  group_by(zone) %>%
  mutate(prop_scaled = prop / max(prop, na.rm = TRUE)) %>%
  mutate(zone=factor(zone, levels=c("Si-P depleted", "N depleted", "Si depleted", "N-P depleted",
                                    "Balanced", "Si-N depleted", "P depleted"))) %>%
  ggplot(aes(Month, zone, fill = prop_scaled)) +
  geom_tile() +
  scale_fill_gradient(
    low = "grey80", high = "grey15",
    name = "Scaled\nproportion"
  ) +
  theme_classic()+
  theme(text = element_text(size = 20))+
  scale_x_continuous(breaks = seq(1,12,1), labels = seq(1,12,1))+
  scale_y_discrete(limits=rev)+labs(y="")

p5

p6<-monthly_stoich_class %>%
  dplyr::filter(chemical=="P") %>%
  dplyr::mutate(zone=factor(zone, levels=c("Balanced", "N depleted", "P depleted", "Si depleted",
                                                                 "N-P depleted", "Si-N depleted", "Si-P depleted"))) %>%
  ggplot(aes(x=zone))+
  geom_bar(stat = "count", fill="black")+theme_classic()+
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x="", y="Number of Site-Month Observations", fill="")+
  guides(fill = guide_legend(reverse=T))

p6

#### now divide into static vs dynamic ####

overall_count<-monthly_stoich_class %>%
  group_by(zone) %>%
  tally()

zones<-monthly_stoich_class %>%
  dplyr::group_by(Stream_Name) %>%
  dplyr::summarise(num_zones=n_distinct(zone))

table(zones$num_zones)

many_zones<-monthly_stoich_class %>%
  dplyr::group_by(Stream_Name) %>%
  dplyr::summarise(num_zones=n_distinct(zone)) %>%
  dplyr::filter(num_zones > 1)

one_zone<-monthly_stoich_class %>%
  dplyr::group_by(Stream_Name) %>%
  dplyr::summarise(num_zones=n_distinct(zone)) %>%
  dplyr::filter(num_zones==1)

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
  ),
  sync_cluster=case_when(
    sync_class=="fully coherent"~DSi,
    sync_class=="Si-N"~DSi,
    sync_class=="Si-P"~DSi,
    sync_class=="N-P"~N,
    sync_class=="non-coherent"~NA,
  ))

p1<-monthly_stoich_cluster_wide %>%
  left_join(zones) %>%
  dplyr::mutate(zone_class=case_when(
    num_zones==1~"static",
    .default = "dynamic"
  )) %>%
  dplyr::mutate(zone_class=factor(zone_class, levels=c("static", "dynamic"))) %>%
  dplyr::arrange(Stream_Name, Month) %>%
  ggtern(aes(x=DSi/16, y=N/16, z=P))+
  geom_path(aes(group=Stream_Name), col="black", alpha=0.5)+theme_classic()+
  geom_Tline(Tintercept = .20)+geom_Rline(Rintercept = .20)+geom_Lline(Lintercept = .20)+
  theme(text = element_text(size = 20), legend.position = "null")+
  labs(col="Cluster", x="DSi", y="N", z="P")+facet_wrap(~zone_class)

p1

p2<-monthly_stoich_class %>%
  dplyr::filter(chemical=="P") %>%
  dplyr::left_join(zones) %>%
  dplyr::mutate(zone=factor(zone, levels=c("Balanced", "N depleted", "P depleted", "Si depleted",
                                    "N-P depleted", "Si-N depleted", "Si-P depleted"))) %>%
  dplyr::mutate(zone_class=case_when(
    num_zones==1~"static",
    .default = "dynamic"
  )) %>%
  dplyr::mutate(zone_class=factor(zone_class, levels=c("dynamic", "static"))) %>%
  ggplot(aes(y=zone, fill=zone_class))+
  geom_bar(position = "dodge", col="black")+
  theme_classic()+
  theme(text = element_text(size = 20), legend.position = "right")+
  scale_fill_manual(values = c("black", "white"))+scale_y_discrete(limits=rev)+
  labs(y="", x="Number of Site-Month Observations", fill="")+
  guides(fill = guide_legend(reverse=T))

p2

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
  ),
  sync_cluster=case_when(
    sync_class=="fully coherent"~DSi,
    sync_class=="Si-N"~DSi,
    sync_class=="Si-P"~DSi,
    sync_class=="N-P"~N,
    sync_class=="non-coherent"~NA,
  ))

p3<-month_clusters %>%
  dplyr::left_join(zones) %>%
  dplyr::mutate(zone_class=case_when(
    num_zones == 1~"static",
    num_zones > 1~"dynamic"
  )) %>%
  mutate(sync_class=factor(sync_class, levels=c("non-coherent", "N-P", "Si-P", "Si-N", "fully coherent"))) %>%
  ggplot(aes(y=1))+geom_bar(aes(y=zone_class, fill=sync_class), position = "fill")+
  scale_fill_manual(values = c("grey70", "dodgerblue", "dodgerblue3", "dodgerblue4", "black"), na.value = "grey")+
  theme_classic()+
  theme(text = element_text(size = 20),
        axis.ticks.y = element_blank())+
  labs(y="", x="Proportion of Sites", fill="Coherence")+
  guides(fill = guide_legend(reverse=T))

p3

bottom_set<-ggarrange(p2, p3, heights = c(0.4, 0.3), align = "v", nrow = 2)

pdf("Dynamic_Static_DepletionState.pdf", width = 10, height = 12)

ggarrange(p1, bottom_set, nrow=2, heights = c(0.5, 0.6))

dev.off()