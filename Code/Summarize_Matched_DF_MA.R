
library(dplyr)
library(tidyr)


heatwave_days <- matched_df %>%
  filter(heatwave==1)

heatwave_day_grp <- heatwave_days %>%
  summarize(n=n(), 
            nzcta=n_distinct(zip),
            PMAD=sum(PMAD),
            PMAD_primary=sum(PMAD_primary),
            PMAD_primsec=sum(PMAD_primsec),
            SMI=sum(SMI),
            SMI_primary=sum(SMI_primary),
            SMI_primsec=sum(SMI_primsec),
            MDP=sum(MDP),
            MDP_primary=sum(MDP_primary),
            MDP_primsec=sum(MDP_primsec),
            SUIA=sum(SUIA),
            SUIA_primary=sum(SUIA_primary),
            SUIA_primsec=sum(SUIA_primsec),
            SUIT=sum(SUIT),
            SUIT_primary=sum(SUIT_primary),
            SUIT_primsec=sum(SUIT_primsec),
            SUB=sum(SUB),
            SUB_primary=sum(SUB_primary),
            SUB_primsec=sum(SUB_primsec))

heatwave_days_matched <- matched_df %>%
  filter(status=='case')

heatwave_day_match_grp <- heatwave_days_matched %>%
  summarize(n=n(), 
            nzcta=n_distinct(zip),
            PMAD=sum(PMAD),
            PMAD_primary=sum(PMAD_primary),
            PMAD_primsec=sum(PMAD_primsec),
            SMI=sum(SMI),
            SMI_primary=sum(SMI_primary),
            SMI_primsec=sum(SMI_primsec),
            MDP=sum(MDP),
            MDP_primary=sum(MDP_primary),
            MDP_primsec=sum(MDP_primsec),
            SUIA=sum(SUIA),
            SUIA_primary=sum(SUIA_primary),
            SUIA_primsec=sum(SUIA_primsec),
            SUIT=sum(SUIT),
            SUIT_primary=sum(SUIT_primary),
            SUIT_primsec=sum(SUIT_primsec),
            SUB=sum(SUB),
            SUB_primary=sum(SUB_primary),
            SUB_primsec=sum(SUB_primsec))

control_days_matched <- matched_df %>%
  filter(status=='control')

control_days_match_grp <- control_days_matched %>%
  summarize(n=n(), 
            nzcta=n_distinct(zip),
            PMAD=sum(PMAD),
            PMAD_primary=sum(PMAD_primary),
            PMAD_primsec=sum(PMAD_primsec),
            SMI=sum(SMI),
            SMI_primary=sum(SMI_primary),
            SMI_primsec=sum(SMI_primsec),
            MDP=sum(MDP),
            MDP_primary=sum(MDP_primary),
            MDP_primsec=sum(MDP_primsec),
            SUIA=sum(SUIA),
            SUIA_primary=sum(SUIA_primary),
            SUIA_primsec=sum(SUIA_primsec),
            SUIT=sum(SUIT),
            SUIT_primary=sum(SUIT_primary),
            SUIT_primsec=sum(SUIT_primsec),
            SUB=sum(SUB),
            SUB_primary=sum(SUB_primary),
            SUB_primsec=sum(SUB_primsec))

matched_df_summary <- rbind(control_days_match_grp, heatwave_day_match_grp, heatwave_day_grp)

matched_df_summary <- matched_df_summary %>% 
  mutate(ID = 1:n()) 

matched_long <- matched_df_summary %>%
  pivot_longer(cols=-ID)

matched_wide <- matched_long %>%
  pivot_wider(names_from=ID)

colnames(matched_wide)[2] <- "Matched Non-Heatwave Days"
colnames(matched_wide)[3] <- "Heatwave & Lag Days"
colnames(matched_wide)[4] <- "Heatwave Days"

write.csv(matched_wide, "Results/Matched_Df_Summary_Lag3_LowIntensity.csv")

black <- read.csv("Results/Matched_Df_Summary_Lag3_Black.csv")%>%
  mutate(Group = "Black")
white <- read.csv("Results/Matched_Df_Summary_Lag3_White.csv")%>%
  mutate(Group = "White")
hispanic <- read.csv("Results/Matched_Df_Summary_Lag3_Hispanic.csv")%>%
  mutate(Group = "Hispanic")
other <- read.csv("Results/Matched_Df_Summary_Lag3_Other.csv")%>%
  mutate(Group = "Other")
age <- read.csv("Results/Matched_Df_Summary_Lag3_Age.csv")%>%
  mutate(Group = "Age >= 35")
medicaid <- read.csv("Results/Matched_Df_Summary_Lag3_Medicaid.csv")%>%
  mutate(Group = "Medicaid")
private <- read.csv("Results/Matched_Df_Summary_Lag3_PrivateIns.csv")%>%
  mutate(Group = "Private Ins")
selfpay <- read.csv("Results/Matched_Df_Summary_Lag3_SelfPay.csv")%>%
  mutate(Group = "Self-Pay")
otherins <- read.csv("Results/Matched_Df_Summary_Lag3_OtherIns.csv")%>%
  mutate(Group = "Other Ins")
agelow <- read.csv("Results/Matched_Df_Summary_Lag3_AgeUnder35.csv")%>%
  mutate(Group = "Age < 35")
nonhisp <- read.csv("Results/Matched_Df_Summary_Lag3_NonHispanic.csv")%>%
  mutate(Group = "Non-Hispanic")

grptable <- rbind(age, agelow, black, hispanic, nonhisp, white, other, medicaid, private, selfpay, otherins)
write.csv(grptable, "Results/individual_subgroup_matched.csv")

grptable_wide <- grptable %>%
  dplyr::select(-X)%>%
  pivot_wider(names_from=name, values_from = 2:4)

write.csv(grptable_wide, "Results/individual_subgroup_matched_Lag3.csv")

grptable_long <- pivot_longer(grptable_wide, cols = c(Matched.Non.Heatwave.Days, Heatwave))

# Regional

urban <- read.csv("Results/Matched_Df_Summary_Lag3_Urban.csv")%>%
  mutate(Group = "Urban")
suburban <- read.csv("Results/Matched_Df_Summary_Lag3_Suburban.csv")%>%
  mutate(Group = "Suburban")
rural <- read.csv("Results/Matched_Df_Summary_Lag3_Rural.csv")%>%
  mutate(Group = "Rural")
western <- read.csv("Results/Matched_Df_Summary_Lag3_Western.csv")%>%
  mutate(Group = "Western")
coastal <- read.csv("Results/Matched_Df_Summary_Lag3_Coastal.csv")%>%
  mutate(Group = "Coastal")
Piedmont <- read.csv("Results/Matched_Df_Summary_Lag3_Piedmont.csv")%>%
  mutate(Group = "Piedmont")
iceincome <- read.csv("Results/Matched_Df_Summary_Lag3_ICEIncome.csv")%>%
  mutate(Group = "ICE Income Below Median")
icerace <- read.csv("Results/Matched_Df_Summary_Lag3_ICERace.csv")%>%
  mutate(Group = "ICE Race Below Median")
abvincome <- read.csv("Results/Matched_Df_Summary_Lag3_ICEIncomeAbove.csv")%>%
  mutate(Group = "ICE Income Above Median")
abvrace <- read.csv("Results/Matched_Df_Summary_Lag3_ICERaceAbove.csv")%>%
  mutate(Group = "ICE Race Above Median")

grptable <- rbind(urban, suburban, rural, western, coastal, Piedmont, iceincome, abvincome, icerace, abvrace)

grptable_wide <- grptable %>%
  dplyr::select(-X)%>%
  pivot_wider(names_from=name, values_from = 2:4)

write.csv(grptable_wide, "Results/Matched DFs/regional_subgroup_matched_Lag3.csv")

low <- read.csv("Results/Matched_Df_Summary_Lag3_LowIntensity.csv")%>%
  mutate(Group = "Low Intensity")
moderate <- read.csv("Results/Matched_Df_Summary_Lag3_ModerateIntensity.csv")%>%
  mutate(Group = "Moderate Intensity")
high <- read.csv("Results/Matched_Df_Summary_Lag3_HighIntensity.csv")%>%
  mutate(Group = "High Intensity")

grptable <- rbind(low, moderate, high)

grptable_wide <- grptable %>%
  dplyr::select(-X)%>%
  pivot_wider(names_from=name, values_from = 2:4)

write.csv(grptable_wide, "Results/Matched DFs/hwintensity_matched_Lag3.csv")

indiv <- read.csv("Results/individual_subgroup_matched_Lag3.csv")
region <- read.csv("Results/Matched DFs/regional_subgroup_matched_Lag3.csv")
hw <- read.csv("Results/Matched DFs/hwintensity_matched_Lag3.csv")

tab <- rbind(indiv, region, hw)

tab <- tab %>%
  dplyr::select(-X)

write.csv(tab, "Results/Matched_DF_Summary_w_HW_Lag3.csv")





