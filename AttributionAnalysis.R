library(dplyr)
library(purrr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(ggtern)

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

month_clusters<-month_clusters %>%
  select(Stream_Name, chemical, Cluster) %>%
  pivot_wider(values_from = Cluster, names_from = chemical)

colnames(month_clusters)<-c("Stream_Name", "cluster_DSi", "cluster_N", "cluster_P")

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

many_zones<-monthly_stoich_class %>%
  dplyr::group_by(Stream_Name) %>%
  dplyr::summarise(num_zones=n_distinct(zone)) %>%
  filter(num_zones > 1)

monthly_stoich_class_dynamic<-monthly_stoich_class %>%
  filter(Stream_Name %in% many_zones$Stream_Name)
  
detect_threshold_crossings <- function(df, threshold = 0.2) {
  
  lag_circ <- function(x) dplyr::lag(x, default = dplyr::last(x))
  
  df %>%
    group_by(Stream_Name) %>%
    arrange(Month) %>%
    mutate(
      DSi_lag = lag_circ(DSi_frac),
      N_lag   = lag_circ(N_frac),
      P_lag   = lag_circ(P_frac),
      
      cross_DSi = (DSi_lag >= threshold & DSi_frac < threshold) |
        (DSi_lag <= threshold & DSi_frac > threshold),
      
      cross_N   = (N_lag >= threshold & N_frac < threshold) |
        (N_lag <= threshold & N_frac > threshold),
      
      cross_P   = (P_lag >= threshold & P_frac < threshold) |
        (P_lag <= threshold & P_frac > threshold),
      
      # Direction flags
      dir_DSi = case_when(
        DSi_lag >= threshold & DSi_frac < threshold ~ "into depletion",
        DSi_lag <= threshold & DSi_frac > threshold ~ "out of depletion",
        TRUE ~ NA_character_
      ),
      dir_N = case_when(
        N_lag >= threshold & N_frac < threshold ~ "into depletion",
        N_lag <= threshold & N_frac > threshold ~ "out of depletion",
        TRUE ~ NA_character_
      ),
      dir_P = case_when(
        P_lag >= threshold & P_frac < threshold ~ "into depletion",
        P_lag <= threshold & P_frac > threshold ~ "out of depletion",
        TRUE ~ NA_character_
      )
    ) %>%
    pivot_longer(
      cols = starts_with("cross_"),
      names_to = "solute",
      values_to = "crossed"
    ) %>%
    filter(crossed) %>%
    mutate(
      solute = sub("cross_", "", solute),
      direction = case_when(
        solute == "DSi" ~ dir_DSi,
        solute == "N" ~ dir_N,
        solute == "P" ~ dir_P
      )
    ) %>%
    select(Stream_Name, Month, solute, direction)
}


simple_month_solute<-detect_threshold_crossings(monthly_stoich_class_dynamic)

monthly_stoich_class_dynamic<-monthly_stoich_class_dynamic %>%
  mutate(sDSi = DSi/16, sN = N/16, sP = P/1)

df_list<-list()

for (i in 1:nrow(simple_month_solute)) {
  
  print(i)
  
  prev_month <- ifelse(simple_month_solute$Month[i] == 1, 12,
                       simple_month_solute$Month[i] - 1)
  
    one_site<-monthly_stoich_class_dynamic %>%
      filter(Stream_Name==simple_month_solute$Stream_Name[i] & 
               Month %in% c(simple_month_solute$Month[i], prev_month))
  
  prev_scaled<-one_site[1,14:16]
  curr_scaled<-one_site[2,14:16]
  
  names(prev_scaled) <- names(curr_scaled) <- c("DSi", "N", "P")
  
  delta_s <- curr_scaled - prev_scaled
  
  total_change <- sum(abs(delta_s))
  
  pct_contrib <- abs(delta_s) / total_change * 100
  
  df<-tibble(
    Solute = names(delta_s),
    delta_scaled = as.numeric(delta_s),
    driver_DSi = pct_contrib$DSi,
    driver_N = pct_contrib$N,
    driver_P = pct_contrib$P
  )
  
  df$Stream_Name<-simple_month_solute$Stream_Name[i]
  df$Month<-simple_month_solute$Month[i]
  df$Changing_Solute<-simple_month_solute$solute[i]
  df$direction<-simple_month_solute$direction[i]
  
  df_list[[i]]<-df
  
}

change_attribution<-do.call(bind_rows, df_list)

change_attribution_filtered<-change_attribution %>%
  group_by(Stream_Name, Month, Changing_Solute) %>%
  filter(Solute==Changing_Solute)

change_attribution_filtered %>%
  group_by(Changing_Solute) %>%
  tally()

change_attribution_filtered_long<-change_attribution_filtered %>%
  pivot_longer(cols = c(3:5), names_to = "solute_percent", values_to = "percent_value")

change_attribution_filtered_long$percent_value<-as.numeric(change_attribution_filtered_long$percent_value)

#set heights proportional to number of shifting events, for N 90/144 (6.25 in if P is 10 in long)

pdf("Attribution_Total_N_Updated01122025.pdf", width = 4, height = 6.25)

change_attribution_filtered_long %>%
  left_join(change_attribution_filtered[,c(4,6:8)]) %>%
  dplyr::mutate(solute_percent=factor(solute_percent, levels=c("driver_P", "driver_DSi", "driver_N")),
         Changing_Solute=factor(Changing_Solute, levels=c("N", "DSi", "P"))) %>%
  dplyr::filter(Changing_Solute=="N") %>%
  dplyr::group_by(Changing_Solute) %>%
  dplyr::mutate(
    switching_events = paste0(Stream_Name, Month),
    switching_events = reorder_within(switching_events, driver_N, Changing_Solute)
  ) %>%
  ggplot(aes(x = percent_value, y = switching_events, fill = solute_percent, col=solute_percent)) +
  geom_bar(stat = "identity") +
  scale_y_reordered() +
  theme_classic() +
  theme(
    axis.text.y = element_blank(),
    text = element_text(size = 20),
    legend.position = "null"
  ) +
  labs(x = "Percent Attribution", y = "", fill="Solute Driver") +
  scale_color_manual(values = c("black", "grey75", "grey35"))+
  scale_fill_manual(values = c("black", "grey75", "grey35"))

dev.off()

pdf("Attribution_OverallTotal_N_Updated01122026.pdf", width = 4, height = 1.5)

change_attribution_filtered_long %>%
  filter(Changing_Solute=="N") %>%
  dplyr::group_by(solute_percent, Changing_Solute) %>%
  dplyr::summarise(mean_attribution=mean(percent_value)) %>%
  mutate(solute_percent=factor(solute_percent, levels=c("driver_P", "driver_DSi", "driver_N")),
         Changing_Solute=factor(Changing_Solute, levels=c("N", "DSi", "P"))) %>%
  ggplot(aes(y=Changing_Solute, x=mean_attribution, fill=solute_percent))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("black", "grey75", "grey35"))+
  theme_classic()+
  theme(legend.position = "null", axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        text = element_text(size = 20))+
  labs(x="", y="")

dev.off()

#for Si 114/144

pdf("Attribution_Total_Si_Updated01122025.pdf", width = 4, height = 8)

change_attribution_filtered_long %>%
  left_join(change_attribution_filtered[,c(4,6:8)]) %>%
  dplyr::mutate(solute_percent=factor(solute_percent, levels=c("driver_P", "driver_DSi", "driver_N")),
                Changing_Solute=factor(Changing_Solute, levels=c("N", "DSi", "P"))) %>%
  dplyr::filter(Changing_Solute=="DSi") %>%
  dplyr::group_by(Changing_Solute) %>%
  dplyr::mutate(
    switching_events = paste0(Stream_Name, Month),
    switching_events = reorder_within(switching_events, driver_N, Changing_Solute)
  ) %>%
  ggplot(aes(x = percent_value, y = switching_events, fill = solute_percent, col=solute_percent)) +
  geom_bar(stat = "identity") +
  scale_y_reordered() +
  theme_classic() +
  theme(
    axis.text.y = element_blank(),
    text = element_text(size = 20),
    legend.position = "null"
  ) +
  labs(x = "Percent Attribution", y = "", fill="Solute Driver") +
  scale_color_manual(values = c("black", "grey75", "grey35"))+
  scale_fill_manual(values = c("black", "grey75", "grey35"))

dev.off()

pdf("Attribution_OverallTotal_Si_Updated01122026.pdf", width = 4, height = 1.5)

change_attribution_filtered_long %>%
  filter(Changing_Solute=="DSi") %>%
  dplyr::group_by(solute_percent, Changing_Solute) %>%
  dplyr::summarise(mean_attribution=mean(percent_value)) %>%
  mutate(solute_percent=factor(solute_percent, levels=c("driver_P", "driver_DSi", "driver_N")),
         Changing_Solute=factor(Changing_Solute, levels=c("N", "DSi", "P"))) %>%
  ggplot(aes(y=Changing_Solute, x=mean_attribution, fill=solute_percent))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("black", "grey75", "grey35"))+
  theme_classic()+
  theme(legend.position = "null", axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        text = element_text(size = 20))+
  labs(x="", y="")

dev.off()

pdf("Attribution_Total_P_Updated01122025.pdf", width = 4, height = 10)

change_attribution_filtered_long %>%
  left_join(change_attribution_filtered[,c(4,6:8)]) %>%
  dplyr::mutate(solute_percent=factor(solute_percent, levels=c("driver_P", "driver_DSi", "driver_N")),
                Changing_Solute=factor(Changing_Solute, levels=c("N", "DSi", "P"))) %>%
  dplyr::filter(Changing_Solute=="P") %>%
  dplyr::group_by(Changing_Solute) %>%
  dplyr::mutate(
    switching_events = paste0(Stream_Name, Month),
    switching_events = reorder_within(switching_events, driver_N, Changing_Solute)
  ) %>%
  ggplot(aes(x = percent_value, y = switching_events, fill = solute_percent, col=solute_percent)) +
  geom_bar(stat = "identity") +
  scale_y_reordered() +
  theme_classic() +
  theme(
    axis.text.y = element_blank(),
    text = element_text(size = 20),
    legend.position = "null"
  ) +
  labs(x = "Percent Attribution", y = "", fill="Solute Driver") +
  scale_color_manual(values = c("black", "grey75", "grey35"))+
  scale_fill_manual(values = c("black", "grey75", "grey35"))

dev.off()

pdf("Attribution_OverallTotal_P_Updated01122026.pdf", width = 4, height = 1.5)

change_attribution_filtered_long %>%
  filter(Changing_Solute=="P") %>%
  dplyr::group_by(solute_percent, Changing_Solute) %>%
  dplyr::summarise(mean_attribution=mean(percent_value)) %>%
  mutate(solute_percent=factor(solute_percent, levels=c("driver_P", "driver_DSi", "driver_N")),
         Changing_Solute=factor(Changing_Solute, levels=c("N", "DSi", "P"))) %>%
  ggplot(aes(y=Changing_Solute, x=mean_attribution, fill=solute_percent))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("black", "grey75", "grey35"))+
  theme_classic()+
  theme(legend.position = "null", axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        text = element_text(size = 20))+
  labs(x="", y="")

dev.off()


change_attribution_filtered_long %>%
  left_join(change_attribution_filtered[,c(4,6:8)]) %>%
  mutate(solute_percent=factor(solute_percent, levels=c("driver_N", "driver_DSi", "driver_P"))) %>%
  group_by(Changing_Solute) %>%
  mutate(
    switching_events = paste0(Stream_Name, Month),
    switching_events = reorder_within(switching_events, driver_N, Changing_Solute)
  ) %>%
  ggplot(aes(x = percent_value, y = switching_events, fill = solute_percent)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_y_reordered() +
  theme_classic() +
  theme(
    axis.text.y = element_blank(),
    text = element_text(size = 20)
  ) +
  labs(x = "Percent Attribution", y = "", fill="Solute Driver") +
  scale_fill_manual(values = c("grey35", "grey75", "black")) +
  facet_wrap(~Changing_Solute+Month, scales = "free_y", nrow = 3)

pdf("Attribution_DepletionState_Updated01122026.pdf", width = 15, height = 10)

change_attribution_filtered_long %>%
  left_join(change_attribution_filtered[,c(4,6:8)]) %>%
  mutate(solute_percent=factor(solute_percent, levels=c("driver_P", "driver_DSi", "driver_N"))) %>%
  mutate(Changing_Solute=factor(Changing_Solute, levels=c("N", "DSi", "P"))) %>%
  group_by(Changing_Solute) %>%
  mutate(
    switching_events = paste0(Stream_Name, Month),
    switching_events = reorder_within(switching_events, driver_N, Changing_Solute)
  ) %>%
  ggplot(aes(x = percent_value, y = switching_events, fill = solute_percent, col=solute_percent)) +
  geom_bar(stat = "identity") +
  scale_y_reordered() +
  theme_classic() +
  theme(
    axis.text.y = element_blank(),
    text = element_text(size = 20)
  ) +
  labs(x = "Percent Attribution", y = "", fill="Solute Driver", col="Solute Driver") +
  scale_fill_manual(values = c("black", "grey75", "grey35")) +
  scale_color_manual(values = c("black", "grey75", "grey35")) +
  facet_wrap(~direction+Changing_Solute, scales = "free_y", nrow = 2)

dev.off()

change_attribution_filtered_long %>%
  dplyr::group_by(Changing_Solute, solute_percent, direction) %>%
  dplyr::summarise(mean_val=mean(percent_value))

month_clusters<-read.csv("ClusterStreams_allSolutes.csv")

month_clusters<-month_clusters %>%
  mutate(Cluster = case_when(
    Cluster==1~1,
    Cluster==4~2,
    Cluster==2~3,
    Cluster==3~4,
    Cluster==5~5
  )) %>%
  select(Stream_Name, chemical, Cluster) %>%
  pivot_wider(names_from = chemical, values_from = Cluster) %>%
  mutate(sync_class = case_when(
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

month_clusters_long<-month_clusters %>%
  pivot_longer(cols = c(2:4), names_to = "Changing_Solute", values_to = "Cluster")

change_attribution_filtered_long_sync<-left_join(change_attribution_filtered_long, month_clusters[,c(1,5)])

p1<-change_attribution_filtered_long_sync %>%
  left_join(change_attribution_filtered[,c(4,6:8)]) %>%
  mutate(solute_percent=factor(solute_percent, levels=c("driver_N", "driver_DSi", "driver_P"))) %>%
  mutate(sync_class=factor(sync_class, levels=c("non-coherent", "N-P", "Si-P", "Si-N", "fully coherent"))) %>%
  dplyr::group_by(Changing_Solute) %>%
  mutate(
    switching_events = paste0(Stream_Name, Month),
    switching_events = reorder_within(switching_events, driver_N, Changing_Solute)
  ) %>%
  ggplot(aes(x = percent_value, y = switching_events, fill = solute_percent)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_y_reordered() +
  theme_classic() +
  theme(
    axis.text.y = element_blank(),
    text = element_text(size = 20),
    legend.position = "null"
  ) +
  labs(x = "Percent Attribution", y = "", fill="Solute Driver") +
  scale_fill_manual(values = c("grey35", "grey75", "black")) +
  facet_wrap(~Changing_Solute+sync_class, scales = "free_y", nrow=3)

p1

plot_df <- change_attribution_filtered_long_sync %>%
  left_join(change_attribution_filtered[,c(4,6:8)]) %>%
  mutate(solute_percent=factor(solute_percent,
                               levels=c("driver_N","driver_DSi","driver_P"))) %>%
  dplyr::group_by(sync_class, Solute, solute_percent) %>%
  dplyr::summarise(
    n_events = n(),                     # number of switching events
    mean_attr = mean(percent_value),    # mean attribution
    seg_height = n_events * (mean_attr/100),  # weighted segment height
    .groups="drop"
  )

p1<-plot_df %>%
  mutate(sync_class=factor(sync_class, levels=c("non-coherent", "N-P", "Si-P", "Si-N", "fully coherent"))) %>%
  mutate(Solute=factor(Solute, levels=c("N", "DSi", "P"))) %>%
  ggplot(aes(y = sync_class,
           x = seg_height,
           fill=Solute)) +
  geom_col(position = "stack") +
  facet_wrap(~ Solute, labeller = as_labeller(nutrient_names)) +
  scale_fill_manual(values = c("grey35", "grey75", "black")) +
  theme_classic() +
  ylab("") +
  xlab("")+theme(text = element_text(size = 20), legend.position = "null")
p1


change_attribution_filtered_long_sync %>%
  left_join(change_attribution_filtered[,c(4,6:8)]) %>%
  mutate(solute_percent=factor(solute_percent, levels=c("driver_N", "driver_DSi", "driver_P"))) %>%
  dplyr::group_by(Changing_Solute) %>%
  mutate(
    switching_events = paste0(Stream_Name, Month),
    switching_events = reorder_within(switching_events, driver_N, Changing_Solute)
  ) %>%
  dplyr::group_by(Changing_Solute, sync_class, solute_percent) %>%
  dplyr::summarise(mean_val=mean(percent_value))

spatial_data<-read.csv("Env_Data_AllSites.csv")

spatial_data<-spatial_data[!duplicated(spatial_data$Stream_Name),]

change_attribution_filtered_long %>%
  left_join(change_attribution_filtered[,c(4,6:8)]) %>%
  left_join(spatial_data[,c(3,9)]) %>%
  filter(!is.na(impacted_class)) %>%
  mutate(solute_percent=factor(solute_percent, levels=c("driver_P", "driver_DSi", "driver_N"))) %>%
  #filter(impacted_class %in% c("ag", "non-impacted")) %>%
  group_by(Changing_Solute) %>%
  mutate(
    switching_events = paste0(Stream_Name, Month),
    switching_events = reorder_within(switching_events, driver_N, Changing_Solute)
  ) %>%
  ggplot(aes(x = percent_value, y = switching_events, fill = solute_percent)) +
  geom_bar(stat = "identity") +
  scale_y_reordered() +
  theme_classic() +
  theme(
    axis.text.y = element_blank(),
    text = element_text(size = 20)
  ) +
  labs(x = "Percent Attribution", y = "", fill="Solute Driver") +
  scale_fill_manual(values = c("black", "grey75", "grey35")) +
  facet_wrap(~Changing_Solute+impacted_class, scales = "free_y", nrow = 3)+
  guides(fill = guide_legend(reverse=T))

plot_df <- change_attribution_filtered_long_sync %>%
  left_join(change_attribution_filtered[,c(4,6:8)]) %>%
  left_join(spatial_data[,c(3,9)]) %>%
  mutate(solute_percent=factor(solute_percent,
                               levels=c("driver_N","driver_DSi","driver_P"))) %>%
  dplyr::group_by(impacted_class, Solute, solute_percent) %>%
  dplyr::summarise(
    n_events = n(),                     # number of switching events
    mean_attr = mean(percent_value),    # mean attribution
    seg_height = n_events * (mean_attr/100),  # weighted segment height
    .groups="drop"
  )

p2<-plot_df %>%
  mutate(impacted_class=factor(impacted_class, levels=c("agricultural & urban", "agricultural", "urban", "less-impacted"))) %>%
  mutate(Solute=factor(Solute, levels=c("N", "DSi", "P"))) %>%
  ggplot(aes(y = impacted_class,
             x = n_events,
             fill=Solute)) +
  geom_col(position = "stack") +
  facet_wrap(~ Solute, labeller = as_labeller(nutrient_names)) +
  scale_fill_manual(values = c("grey35", "grey75", "black")) +
  theme_classic() +
  ylab("") +
  xlab("Number of State Shifts")+theme(text = element_text(size = 20), legend.position = "null")
p2

pdf("StateShifts_Coherence_LULC.pdf", width = 12.5, height = 6)

ggarrange(p1, p2, nrow=2, align = "v")

dev.off()
