require(dplyr)
require(ggplot2)
require(rcartocolor)
require(ggpubr)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/NutrientRegimes")

solutes_clusters<-read.csv("AllSolutes_ClusterNumbers.csv")
solutes_clusters<-solutes_clusters[,-c(1)]

spatial_data<-read.csv("Env_Data_AllSites.csv")
spatial_data<-spatial_data[,-c(1)]
spatial_data<-spatial_data[!duplicated(spatial_data$Stream_Name),]

sync_true<-left_join(solutes_clusters, spatial_data)

sync_true$Name<-factor(sync_true$Name,
                                    levels = c("Arid",
                                               "Semi-Arid",
                                               "Mediterranean",
                                               "Humid Tropical",
                                               "Humid Subtropical",
                                               "Humid Temperate",
                                               "Humid Continental",
                                               "Subarctic"))

sync_true$impacted_class<-factor(sync_true$impacted_class, 
                                 levels = c("less-impacted", "urban", "agricultural","agricultural & urban"))

sync_true <- sync_true %>%
  pivot_longer(cols=2:4, values_to = "value", names_to = "variable")

sync_true <- sync_true %>%
  mutate(value=case_when(
    value==1~1,
    value==4~2,
    value==2~3,
    value==3~4,
    value==5~5
  ))

k1<-sync_true %>%
  mutate(sync_cluster=factor(value, levels=c(5,4,3,2,1))) %>%
  ggplot(aes(y=Name, fill=as.factor(value)))+geom_bar(position="fill")+theme_classic()+
  labs(x="", y="")+
  theme(text=element_text(size=20),
        legend.position = "null")+
  facet_wrap(~variable, nrow = 1)+
  scale_fill_manual(values = c("tan4", "#CA562C", "#EDBB8A", "#B4C8A8", "#008080"))+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(labels = c(0,0.5,1), breaks = c(0,0.5,1))

k1

k3<-sync_true %>%
  mutate(value=factor(value, levels=c(5,4,3,2,1))) %>%
  ggplot(aes(y=impacted_class, fill=as.factor(value)))+geom_bar(position="fill")+theme_classic()+
  labs(y="", x="Proportion", fill="Cluster")+
  theme(text=element_text(size=20), legend.position = "bottom")+
  facet_wrap(~variable, nrow = 1)+
  scale_fill_manual(values = c("tan4", "#CA562C", "#EDBB8A", "#B4C8A8", "#008080"))+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(labels = c(0,0.5,1), breaks = c(0,0.5,1))+
  guides(fill = guide_legend(reverse=T))

k3


pdf("Climate_LandUse.pdf", width = 12, height = 7)

ggarrange(k1, k3, align = "v", nrow = 2, heights = c(0.5, 0.4))

dev.off()
