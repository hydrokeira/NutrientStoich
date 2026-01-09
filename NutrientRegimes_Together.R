### cluster time series into nutrient regimes
require(data.table)
require(dtw)
require(dtwclust)

#### fordtwclust#### for N, Si, and P together (includes NO3 and NOx) ####

monthly_results<-read.csv("WRTDS_Outputs_Clean_01082026.csv")

monthly_results_norm <- monthly_results %>%
  dplyr::group_by(Stream_Name, chemical) %>%
  dplyr::mutate(norm_conc=scale(FNConc_mgL))

#aggregate by month and stream to get one value for every month and stream pair
month_norm_agg<-aggregate(norm_conc ~ Month+Stream_Name+chemical, data = monthly_results_norm, FUN=mean)

colnames(month_norm_agg)[4]<-"norm_conc"

month_conc<-month_norm_agg

#cast (make wide) to get in proper format for DTW, should be 1-12 columns and sites as rows
month_cast<-dcast(month_conc, formula = Month~Stream_Name+chemical, value.var = "norm_conc")
month_norm<-month_cast
month_norm_t<-as.data.frame(t(month_norm[2:ncol(month_norm)]))

#### Cluster Validity Indices ####
#set up index "best" avlues - i.e. for "Sil" CVI, higher values (max) is better
index<-c("Sil","D","COP","DB","DBstar","CH","SF")
cvi_ideal<-c("max","max","min","min","min","max","max")
cvi_index<-data.frame(index, cvi_ideal)

#test CVIs for 2-15 clusters
clust.dba <- tsclust(month_norm_t, type="partitional", centroid = "dba", distance = "dtw",
                     window.size=1L, k=2L:10L, seed = 8)

#put CVI output into list
cvi_df<-lapply(clust.dba, cvi)

#put into df and melt
cluster_stats<-do.call(rbind, cvi_df)
cluster_stats_melt<-melt(cluster_stats)

#change column to reflect number of cluster (currently starts at 1, need to start at 2)
cluster_stats_melt$Var1<-cluster_stats_melt$Var1+1

#rename column
colnames(cluster_stats_melt)[2]<-"index"

#merge index df with CVI df
cluster_stats_melt<-merge(cluster_stats_melt, cvi_index, by="index")
colnames(cluster_stats_melt)[2]<-"number_of_clusters"
cluster_stats_melt$cvi_goal<-paste(cluster_stats_melt$index, "-", cluster_stats_melt$cvi_ideal)

pdf("CVI_facetplot.pdf", width = 10, height = 5)

#plot to evaluate CVIs
ggplot(cluster_stats_melt, aes(number_of_clusters, value))+geom_line()+
  facet_wrap(~cvi_goal, scales = "free", nrow = 2)+
  theme_classic()+theme(text = element_text(size = 20))+
  labs(x="Number of Clusters", y="CVI value")


dev.off()

#### now we DTW with the decided number of clusters, some basic visualization ####
#cluster
clust.dba <- tsclust(month_norm_t, type="partitional", centroid = "dba", distance = "dtw",
                     window.size=1, k=5L, seed = 8)

clust.dba

#plot the lines and centroids
plot(clust.dba, type = "sc")

#plot just the centroids
plot(clust.dba, type = "centroid")

#pull out centroids, save as csv
centroids_list<-clust.dba@centroids
centroids_df<-as.data.frame(do.call(rbind, centroids_list))
centroids_df$cluster<-seq(1,5)
write.csv(centroids_df, "AverageClusterCentroids_allSolutes.csv")

#pull out individual lines
mydata<-clust.dba@datalist

#convert to df
data_df<-t(do.call(cbind, mydata))

month_clusters<-as.data.frame(cbind(data_df, clust.dba@cluster))
colnames(month_clusters)<-c(paste(seq(1:12)), "Cluster")
month_clusters<-rownames_to_column(month_clusters, "Site")

month_clusters<-month_clusters %>% 
  tidyr::extract(Site, into = c("Stream_Name", "chemical"), "(.*)_([^_]+)$")

month_clusters_solute_sum <- month_clusters %>%
  dplyr::group_by(Cluster, chemical) %>%
  dplyr::tally()

ggplot(month_clusters_solute_sum, aes(Cluster, n, fill=chemical))+geom_bar(stat = "identity")

write.csv(month_clusters, "ClusterStreams_allSolutes.csv")

month_clusters_wide <- month_clusters[,c(1,2,15)] %>%
  pivot_wider(names_from = chemical, values_from = Cluster)

month_clusters_wide<-month_clusters_wide[complete.cases(month_clusters_wide),]

write.csv(month_clusters_wide, "AllSolutes_ClusterNumbers.csv")

#### plot data for Figure 3 ####
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn/NutrientRegimes")

solutes_clusters<-read.csv("AllSolutes_ClusterNumbers.csv")

solutes_clusters_long<-solutes_clusters %>%
  pivot_longer(cols = c(DSi, N, P), names_to = "solute", values_to = "average_cluster")

all_clusters<-read.csv("ClusterStreams_allSolutes.csv")

all_clusters_melt<-melt(all_clusters, id.vars=c("X", "Stream_Name", "chemical", "Cluster"))

all_clusters_melt$unique<-paste(all_clusters_melt$Stream_Name, all_clusters_melt$chemical)

all_clusters_melt <- all_clusters_melt %>%
  mutate(Cluster=case_when(
    Cluster==1~1,
    Cluster==4~2,
    Cluster==2~3,
    Cluster==3~4,
    Cluster==5~5
  ))

p1<-ggplot(all_clusters_melt, aes(variable, value, col=chemical))+
  geom_line(aes(group=unique), alpha=0.6, size=0.8)+
  facet_wrap(~Cluster)+theme_classic()+scale_color_manual(values=c("grey75", "grey35", "black"))+
  theme(text = element_text(size = 20), legend.position = "null")+
  scale_x_discrete(labels=seq(1,12,1))+
  labs(y="Normalized Concentration", x="Month", col="")

p1

solutes_clusters_melt<-melt(solutes_clusters[2:5], id.vars = c("Stream_Name"))

solutes_clusters_melt <- solutes_clusters_melt %>%
  mutate(value=case_when(
    value==1~1,
    value==4~2,
    value==2~3,
    value==3~4,
    value==5~5
  ))

p2<-ggplot(solutes_clusters_melt, aes(as.character(value)))+
  geom_bar(stat="count", aes(fill=variable), alpha=0.8)+
  theme_classic()+scale_fill_manual(values = c("grey75", "grey35", "black"))+
  theme(text = element_text(size=20), legend.position = "top")+
  labs(x="Cluster", y="Count", fill="Solute")

p2

