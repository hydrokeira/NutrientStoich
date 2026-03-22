require(ggtern)
require(cetcolor)
require(dplyr)
require(ggplot2)

load("TernaryPlotInputData.RData")

#### calculate depletion states ####

threshold = 0.2

#this code here classifies each observation into its stoichiometric state
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

#this plots it using ggtern
monthly_stoich_cluster_wide %>%
  dplyr::filter(chemical=="P") %>% #this ensures that youre only getting one point per stream
  ggtern(aes(x=DSi/16, y=N/16, z=P, col=as.factor(Month)))+
  geom_point()+
  scale_color_manual(values = cet_pal(12, "c4s"))+theme_bw()+
  geom_Tline(Tintercept = .20)+geom_Rline(Rintercept = .20)+geom_Lline(Lintercept = .20)+
  theme(text = element_text(size = 20), legend.position = "null")+
  labs(col="Month", x="DSi", y="N", z="P")
