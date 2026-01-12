require(dplyr)
require(ggplot2)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/NutrientRegimes")

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


(table(month_clusters$sync_class)/327)*100

p1<-month_clusters %>%
  mutate(sync_class=factor(sync_class, levels=c("non-coherent", "N-P", "Si-P", "Si-N", "fully coherent"))) %>%
  ggplot(aes(y=1))+geom_bar(aes(fill=sync_class), position = "fill")+
  scale_fill_manual(values = c("grey70", "dodgerblue", "dodgerblue3", "dodgerblue4", "black"), na.value = "grey")+
  theme_classic()+
  theme(text = element_text(size = 20),
        axis.ticks.y = element_blank(), axis.text.y = element_blank())+
  labs(y="", x="", fill="Coherence Class")+
  guides(fill = guide_legend(reverse=T))

p1

p2<-month_clusters %>%
  dplyr::filter(!is.na(sync_cluster)) %>%
  mutate(sync_class=factor(sync_class, levels=c("non-coherent", "N-P", "Si-P", "Si-N", "fully coherent"))) %>%
  mutate(sync_cluster=factor(sync_cluster, levels=c(5,4,3,2,1))) %>%
  ggplot(aes(y=sync_class))+geom_bar(aes(fill=as.factor(sync_cluster)), position = "fill")+
  scale_fill_manual(values = c("tan4", "#CA562C", "#EDBB8A", "#B4C8A8", "#008080"))+
  theme_classic()+
  theme(text = element_text(size = 20))+
  labs(x="", y="", fill="Cluster") +
  guides(fill = guide_legend(reverse=T))

p2

spatial_data<-read.csv("Env_Data_AllSites.csv")

p3<-month_clusters %>%
  left_join(spatial_data[,c(3,9)]) %>%
  mutate(sync_class=factor(sync_class, levels=c("non-coherent", "N-P", "Si-P", "Si-N", "fully coherent"))) %>%
  mutate(impacted_class=factor(impacted_class, levels=c("agricultural & urban", "agricultural", "urban", "less-impacted"))) %>%
  ggplot(aes(y=sync_class))+geom_bar(aes(fill=impacted_class), position = "fill")+
  theme_classic()+
  theme(text = element_text(size = 20))+
  labs(x="Proportion of Sites", y="", fill="Impacted Class")+
  scale_fill_manual(values = c("darkorange3", "goldenrod3", "tan", "palegreen4"))+
  guides(fill = guide_legend(reverse=T))

p3

pdf("Coherence_Cluster_LULC_Updated01092026.pdf", width = 10, height = 8)

ggarrange(p1, p2, p3, nrow = 3, align = "v")

dev.off()

month_clusters %>%
  left_join(spatial_data[,c(3,9)]) %>%
  group_by(sync_class, impacted_class) %>%
  tally()
