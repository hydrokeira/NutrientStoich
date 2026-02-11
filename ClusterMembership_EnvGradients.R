require(dplyr)
require(ggplot2)
require(rcartocolor)
require(ggpubr)
require(tidyr)

setwd("/Users/keirajohnson/Library/CloudStorage/Box-Box/Keira_Johnson/SiSyn/NutrientRegimes")

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
  mutate(value=factor(value, levels=c(5,4,3,2,1))) %>%
  mutate(variable=factor(variable, levels=c("N", "DSi", "P"))) %>%
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
  mutate(variable=factor(variable, levels=c("N", "DSi", "P"))) %>%
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

p1<-spatial_data %>%
  mutate(impacted_class=factor(impacted_class, levels = c("less-impacted", "urban", "agricultural","agricultural & urban"))) %>%
  ggplot(aes(y=impacted_class))+
  geom_bar(stat = "count", fill="black")+theme_classic()+
  scale_y_discrete(limits=rev)+
  theme(text=element_text(size=20))+labs(y="", x="Number of Sites")
p1

p2<-spatial_data %>%
  mutate(Name=factor(Name, levels = c("Arid",
                                      "Semi-Arid",
                                      "Mediterranean",
                                      "Humid Tropical",
                                      "Humid Subtropical",
                                      "Humid Temperate",
                                      "Humid Continental",
                                      "Subarctic"))) %>%
  ggplot(aes(y=Name))+
  geom_bar(stat = "count", fill="black")+theme_classic()+
  scale_y_discrete(limits=rev)+
  theme(text=element_text(size=20))+labs(y="", x="Number of Sites")

p2

pdf("Site_Env_Distributions.pdf", width = 11.5, height = 4)

ggarrange(p2, p1, align = "h", nrow = 1)

dev.off()

table(spatial_data$Name)

p3<-spatial_data %>%
  mutate(Name=factor(Name, levels = c("Arid",
                                      "Semi-Arid",
                                      "Mediterranean",
                                      "Humid Tropical",
                                      "Humid Subtropical",
                                      "Humid Temperate",
                                      "Humid Continental",
                                      "Subarctic"))) %>%
  mutate(impacted_class=factor(impacted_class, levels = c("less-impacted", "urban", "agricultural","agricultural & urban"))) %>%
  ggplot(aes(y=Name, fill=impacted_class))+
  geom_bar(position = "fill")+theme_classic()+
  scale_y_discrete(limits=rev)+
  theme(text=element_text(size=20))+labs(y="", x="Proportion of Sites", fill="Impacted Class")+
  scale_fill_manual(values = c("palegreen4", "tan", "goldenrod3", "darkorange3"))
p3
p4<-spatial_data %>%
  mutate(Name=factor(Name, levels = c("Arid",
                                      "Semi-Arid",
                                      "Mediterranean",
                                      "Humid Tropical",
                                      "Humid Subtropical",
                                      "Humid Temperate",
                                      "Humid Continental",
                                      "Subarctic"))) %>%
  mutate(impacted_class=factor(impacted_class, levels = c("less-impacted", "urban", "agricultural","agricultural & urban"))) %>%
  ggplot(aes(y=impacted_class, fill=Name))+
  geom_bar(position = "fill")+theme_classic()+
  scale_y_discrete(limits=rev)+
  theme(text=element_text(size=20))+labs(y="", x="Proportion of Sites", fill="Climate Zone")+
  scale_fill_manual(values = carto_pal(8, "Safe"))
p4

pdf("LULC_Climate_Interaction.pdf", width = 10, height = 7)

ggarrange(p3, p4, nrow = 2, align = "v", heights = c(0.6, 0.35))

dev.off()

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

p5<-spatial_data %>%
  mutate(Name=factor(Name, levels = c("Arid",
                                      "Semi-Arid",
                                      "Mediterranean",
                                      "Humid Tropical",
                                      "Humid Subtropical",
                                      "Humid Temperate",
                                      "Humid Continental",
                                      "Subarctic"))) %>%
  mutate(impacted_class=factor(impacted_class, levels = c("less-impacted", "urban", "agricultural","agricultural & urban"))) %>%
  ggplot(aes(y=LTER))+
  geom_bar(stat = "count", fill="black")+theme_classic()+
  scale_y_discrete(limits=rev)+
  theme(text=element_text(size=20))+labs(y="", x="Number of Sites")
p5

p6<-spatial_data %>%
  mutate(Name=factor(Name, levels = c("Arid",
                                      "Semi-Arid",
                                      "Mediterranean",
                                      "Humid Tropical",
                                      "Humid Subtropical",
                                      "Humid Temperate",
                                      "Humid Continental",
                                      "Subarctic"))) %>%
  mutate(impacted_class=factor(impacted_class, levels = c("less-impacted", "urban", "agricultural","agricultural & urban"))) %>%
  ggplot(aes(y=Name, fill=LTER))+
  geom_bar(position = "fill")+theme_classic()+
  scale_y_discrete(limits=rev)+
  theme(text=element_text(size=20), legend.position = "null")+labs(y="", x="Proportion of Sites", fill="Observation Network")+
  scale_fill_manual(values = c25)
p6

p7<-spatial_data %>%
  mutate(Name=factor(Name, levels = c("Arid",
                                      "Semi-Arid",
                                      "Mediterranean",
                                      "Humid Tropical",
                                      "Humid Subtropical",
                                      "Humid Temperate",
                                      "Humid Continental",
                                      "Subarctic"))) %>%
  mutate(impacted_class=factor(impacted_class, levels = c("less-impacted", "urban", "agricultural","agricultural & urban"))) %>%
  ggplot(aes(y=impacted_class, fill=LTER))+
  geom_bar(position = "fill")+theme_classic()+
  scale_y_discrete(limits=rev)+
  theme(text=element_text(size=20))+labs(y="", x="Proportion of Sites", fill="Observation Network")+
  scale_fill_manual(values = c25)
p7

pdf("LTER_Dist_Climate_LULC.pdf", width = 15, height = 14)

ggarrange(p5, p6, p7, nrow = 3, align = "v", heights = c(0.8, 0.5, 0.35))

dev.off()

spatial_data<-read.csv("Env_Data_AllSites.csv")

chem<-read.csv("WRTDS_Outputs_Clean_01082026.csv")

pdf("LULC_Nutrient_Conc.pdf", width = 12, height = 5)

chem %>%
  group_by(Stream_Name, chemical) %>%
  summarise(mean_conc=mean(FNConc_mgL)) %>%
  filter(mean_conc < 100) %>%
  left_join(spatial_data[,c(3,9)]) %>%
  mutate(impacted_class=factor(impacted_class, levels=c("less-impacted", "urban", "agricultural",
                                                        "agricultural & urban"))) %>%
  ggplot(aes(impacted_class, mean_conc))+geom_violin(scale = "width", fill="black")+
  facet_wrap(~chemical, scales="free_y")+
  theme_classic()+theme(text = element_text(size = 20), axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x="", y="Mean Concentration (mg/L)")

dev.off()

