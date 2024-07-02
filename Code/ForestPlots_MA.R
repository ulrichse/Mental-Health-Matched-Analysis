
# Create forest plots of cumulative lags and also create forest plots with multiple columns for different outcomes

library(tidyr)
library(forestploter)
library(dplyr)
library(grid)
#### PLOTS ####

full_cumlag <- read.csv("Results/Lag7/Cumulative_Lag7_Full.csv")
other_cumlag <- read.csv("Results/Lag7/Cumulative_Lag7_Other.csv")
medicaid_cumlag <- read.csv("Results/Lag7/Cumulative_Lag7_Medicaid.csv")
commercial_cumlag <- read.csv("Results/Lag7/Cumulative_Lag7_PrivateIns.csv")
age_cumlag <- read.csv("Results/Lag7/Cumulative_Lag7_Age.csv")
black_cumlag <- read.csv("Results/Lag7/Cumulative_Lag7_Black.csv")
white_cumlag <- read.csv("Results/Lag7/Cumulative_Lag7_White.csv")
hispanic_cumlag <- read.csv("Results/Lag7/Cumulative_Lag7_Hispanic.csv")
nonhisp_cumlag <- read.csv("Results/Lag7/Cumulative_Lag7_NonHispanic.csv")
selfpay_cumlag <- read.csv("Results/Lag7/Cumulative_Lag7_SelfPay.csv")
agelow_cumlag <- read.csv("Results/Lag7/Cumulative_Lag7_AgeUnder35.csv")
otherins_cumlag <- read.csv("Results/Lag7/Cumulative_Lag7_OtherIns.csv")

urban <- read.csv("Results/Lag7/Cumulative_Lag7_Urban.csv")
suburban <- read.csv("Results/Lag7/Cumulative_Lag7_Suburban.csv")
rural <- read.csv("Results/Lag7/Cumulative_Lag7_Rural.csv")
western <- read.csv("Results/Lag7/Cumulative_Lag7_Western.csv")
coastal <- read.csv("Results/Lag7/Cumulative_Lag7_Coastal.csv")
piedmont <- read.csv("Results/Lag7/Cumulative_Lag7_Piedmont.csv")
icerace <- read.csv("Results/Lag7/Cumulative_Lag7_ICERace.csv")
raceabv <- read.csv("Results/Lag7/Cumulative_Lag7_ICERaceAbove.csv")
iceincome <- read.csv("Results/Lag7/Cumulative_Lag7_ICEIncome.csv")
incabove <- read.csv("Results/Lag7/Cumulative_Lag7_ICEIncomeAbove.csv")

forest_data <- rbind(full_cumlag, age_cumlag, agelow_cumlag, black_cumlag, white_cumlag,  other_cumlag,  medicaid_cumlag, commercial_cumlag, selfpay_cumlag, otherins_cumlag,
                     urban, suburban, rural, western, coastal, piedmont, icerace, raceabv, iceincome, incabove)

forest_data <- forest_data %>%
  dplyr::select(-X, -estmean)%>%
  distinct(over_rr, over_rr_se, over_rr_high, over_rr_low, outcome, Subgroup)%>%
  rename(Outcome=outcome)

outcomes <- c("PMAD_primsec", "MDP_primsec", "SMI_primsec", "SUIT_primsec", "SUB_primsec")

forest_data <- forest_data %>%
  filter(Outcome %in% outcomes)

#### Plot with multiple outcomes ####

forest_pivot <- pivot_wider(forest_data, names_from = Outcome, values_from = c('over_rr', 'over_rr_se', 'over_rr_high', 'over_rr_low'))

write.csv(forest_pivot, "forest_pivot2.csv")
dt <- read.csv("forest_pivot2.csv")

# Add two blank columns for CI
dt$`PMAD P/S` <- paste(rep(" ", 20), collapse = " ")
dt$`MDP P/S` <- paste(rep(" ", 20), collapse = " ")
dt$`SMI P/S` <- paste(rep(" ", 20), collapse = " ")
dt$`SUI-T P/S` <- paste(rep(" ", 20), collapse = " ")
dt$`SUB P/S` <- paste(rep(" ", 20), collapse = " ")

# Generate point estimation and 95% CI. Paste two CIs together and separate by line break.
dt$`PMAD RR (95% CI)` <- ifelse(is.na(dt$over_rr_se_PMAD_primsec), "",
                                sprintf("%.2f (%.2f to %.2f)",
                                        dt$over_rr_PMAD_primsec, dt$over_rr_low_PMAD_primsec, dt$over_rr_high_PMAD_primsec))

dt$`MDP RR (95% CI)` <- ifelse(is.na(dt$over_rr_se_MDP_primsec), "",
                               sprintf("%.2f (%.2f to %.2f)",
                                       dt$over_rr_MDP_primsec, dt$over_rr_low_MDP_primsec, dt$over_rr_high_MDP_primsec))

dt$`SMI RR (95% CI)` <- ifelse(is.na(dt$over_rr_se_SMI_primsec), "",
                               sprintf("%.2f (%.2f to %.2f)",
                                       dt$over_rr_SMI_primsec, dt$over_rr_low_SMI_primsec, dt$over_rr_high_SMI_primsec))

dt$`SUIT RR (95% CI)` <- ifelse(is.na(dt$over_rr_se_SUIT_primsec), "",
                                sprintf("%.2f (%.2f to %.2f)",
                                        dt$over_rr_SUIT_primsec, dt$over_rr_low_SUIT_primsec, dt$over_rr_high_SUIT_primsec))

dt$`SUB RR (95% CI)` <- ifelse(is.na(dt$over_rr_se_SUB_primsec), "",
                                sprintf("%.2f (%.2f to %.2f)",
                                        dt$over_rr_SUB_primsec, dt$over_rr_low_SUB_primsec, dt$over_rr_high_SUB_primsec))

est = list(dt$over_rr_PMAD_primsec,
           dt$over_rr_MDP_primsec,
           dt$over_rr_SMI_primsec,
           dt$over_rr_SUIT_primsec,
           dt$over_rr_SUB_primsec)
lower = list(dt$over_rr_low_PMAD_primsec,
             dt$over_rr_low_MDP_primsec,
             dt$over_rr_low_SMI_primsec,
             dt$over_rr_low_SUIT_primsec,
             dt$over_rr_low_SUB_primsec) 
upper = list(dt$over_rr_high_PMAD_primsec,
             dt$over_rr_high_MDP_primsec,
             dt$over_rr_high_SMI_primsec,
             dt$over_rr_high_SUIT_primsec,
             dt$over_rr_high_SUB_primsec)

dt <- dt %>%
  dplyr::select(Category,
                Subgroup, 
                `PMAD RR (95% CI)`, 
                'PMAD P/S',
                `MDP RR (95% CI)`, 
                'MDP P/S',
                `SMI RR (95% CI)`,
                'SMI P/S',
                `SUIT RR (95% CI)`,
                'SUI-T P/S',
                `SUB RR (95% CI)`,
                'SUB P/S')

tm <- forest_theme(core=list(fg_params=list(hjust = 1, x = 0.9),
                             bg_params=list(fill = c("#f6eff7", "#d0d1e6", "#a6bddb", "#67a9cf"))),
                   colhead=list(fg_params=list(hjust=0.5, x=0.5)))

tm <- forest_theme(base_size = 10,
                   footnote_gp = gpar(fontsize = 8),
                   footnote_parse = TRUE)
p <- forest(dt,
            est = est,
            lower = lower, 
            upper = upper,
            ci_column = c(4, 6, 8, 10, 12),
            ticks_at = c(1),
            ref_line = 1,
            title = "Cumulative Lag0 to Lag7, Primary/Secondary Outcomes",
            theme = tm,
            footnote = "Missing confidence intervals indicate category was dropped from 
            the model for the outcome due to insufficient sample size.")
           

plot(p)

dt <- dt %>%
  mutate(`SMI RR (95% CI)` = ifelse(`SMI RR (95% CI)` == " ", "N/A", `SMI RR (95% CI)`))
dt <- read.csv("dt_fp.csv")



# Set-up theme
tm <- forest_theme(base_size = 10)
refline_lty = "solid",
ci_pch = c(15, 18),
ci_col = c("#377eb8", "#4daf4a"),
footnote_gp = gpar(col = "blue"),
legend_name = "Group",
legend_value = c("Trt 1", "Trt 2"),
vertline_lty = c("dashed", "dotted"),
vertline_col = c("#d6604d", "#bababa"),
# Table cell padding, width 4 and heights 3
core = list(padding = unit(c(4, 3), "mm")))


## Plot of one group with multiple outcomes

dt <- read.csv("Results/Cumulative_Lag5_Lag7_Full.csv")%>%
  dplyr::select(-X, -estmean, -Subgroup)%>%
  distinct(over_rr, over_rr_se, over_rr_high, over_rr_low, outcome)%>%
  rename(Outcome=outcome)

outcomes <- c("PMAD P-S", "SMI P-S", "MDP P-S", "SUIT P-S", "SUB P-S")

dt <- dt %>%
  filter(Outcome %in% outcomes)

dt$`RR (95% CI)` <- ifelse(is.na(dt$over_rr_se), "",
                             sprintf("%.2f (%.2f to %.2f)",
                                     dt$over_rr, dt$over_rr_low, dt$over_rr_high))
  
dt <- dt %>%
  dplyr::select('Outcome', 'RR (95% CI)', 'over_rr', 'over_rr_low', 'over_rr_high', 'over_rr_se')
  
  dt$` ` <- paste(rep(" ", 40), collapse = " ")
  est = dt$over_rr
  lower = dt$over_rr_low
  upper = dt$over_rr_high
  sizes = dt$over_rr_se
  
  dt <- dt %>%
    dplyr::select(-over_rr, -over_rr_high, -over_rr_low, -over_rr_se)
  
  tm <- forest_theme(base_size = 12,
                     title_gp = gpar(fontsize=12))
  
  p <- forest(dt,
              est = est,
              lower = lower, 
              upper = upper,
              sizes = sizes,
              ci_column = 3,
              ref_line = 1,
              xlim = c(0.75, 1.5),
              ticks_at = c(0.75, 1),
              title = "Cumulative Lag5 to Lag7, All ED Visits (May-September, 2011-2019)",
              theme = tm)
  
  plot(p)  # Replace with your plot code
 

  ## Forest plot for multiple lags (not cumulative)
  
  library(grid)
  library(forestploter)
  library(data.table)
  library(tidyr)
  
  forest_data <- read.csv("Results/Individual_Lag0_Lag7_Full.csv")
  
  outcomes <- c("PMAD_PrimSec", "MDP_PrimSec", "SMI_PrimSec", "SUIT_PrimSec", "SUB_PrimSec")
  
  forest_data <- forest_data %>%
    filter(Pred_Model %in% outcomes)%>%
    dplyr::select(Pred_Model, Type, lag0, lag1, lag2, lag3, lag4, lag5, lag6, lag7)
  
  forest_data <- pivot_wider(forest_data, names_from = Type, values_from = c("lag0", "lag1", "lag2", "lag3", "lag4", "lag5", "lag6", "lag7"))
  
  dt <- forest_data
  
  dt$`    lag0` <- paste(rep(" ", 20), collapse = " ")
  dt$` lag1` <- paste(rep(" ", 20), collapse = " ")
  dt$`    lag2` <- paste(rep(" ", 20), collapse = " ")
  dt$` lag3` <- paste(rep(" ", 20), collapse = " ")
  dt$`    lag4` <- paste(rep(" ", 20), collapse = " ")
  dt$` lag5` <- paste(rep(" ", 20), collapse = " ")
  dt$`    lag6` <- paste(rep(" ", 20), collapse = " ")
  dt$` lag7` <- paste(rep(" ", 20), collapse = " ")
  
  dt$'lag0 95% CI' <- paste(sprintf("%.1f (%.1f, %.1f)", dt$lag0_matRRfit, dt$lag0_matRRlow, dt$lag0_matRRhigh))
  dt$'lag1 95% CI' <- paste(sprintf("%.1f (%.1f, %.1f)", dt$lag1_matRRfit, dt$lag1_matRRlow, dt$lag1_matRRhigh))
  dt$'lag2 95% CI' <- paste(sprintf("%.1f (%.1f, %.1f)", dt$lag2_matRRfit, dt$lag2_matRRlow, dt$lag2_matRRhigh))
  dt$'lag3 95% CI' <- paste(sprintf("%.1f (%.1f, %.1f)", dt$lag3_matRRfit, dt$lag3_matRRlow, dt$lag3_matRRhigh))
  dt$'lag4 95% CI' <- paste(sprintf("%.1f (%.1f, %.1f)", dt$lag4_matRRfit, dt$lag4_matRRlow, dt$lag4_matRRhigh))
  dt$'lag5 95% CI' <- paste(sprintf("%.1f (%.1f, %.1f)", dt$lag5_matRRfit, dt$lag5_matRRlow, dt$lag5_matRRhigh))
  dt$'lag6 95% CI' <- paste(sprintf("%.1f (%.1f, %.1f)", dt$lag6_matRRfit, dt$lag6_matRRlow, dt$lag6_matRRhigh))
  dt$'lag7 95% CI' <- paste(sprintf("%.1f (%.1f, %.1f)", dt$lag7_matRRfit, dt$lag7_matRRlow, dt$lag7_matRRhigh))
  
  est = list(dt$lag0_matRRfit,
             dt$lag1_matRRfit,
             dt$lag2_matRRfit,
             dt$lag3_matRRfit,
             dt$lag4_matRRfit,
             dt$lag5_matRRfit,
             dt$lag6_matRRfit,
             dt$lag7_matRRfit)
  lower = list(dt$lag0_matRRlow,
               dt$lag1_matRRlow,
               dt$lag2_matRRlow,
               dt$lag3_matRRlow,
               dt$lag4_matRRlow,
               dt$lag5_matRRlow,
               dt$lag6_matRRlow,
               dt$lag7_matRRlow)
  upper = list(dt$lag0_matRRhigh,
               dt$lag1_matRRhigh,
               dt$lag2_matRRhigh,
               dt$lag3_matRRhigh,
               dt$lag4_matRRhigh,
               dt$lag5_matRRhigh,
               dt$lag6_matRRhigh,
               dt$lag7_matRRhigh)
  
  #dt <- dt %>%
  # dplyr::select(-c(2:25))
  
  dt <- dt %>%
    dplyr::select(Pred_Model, '    lag0', 'lag0 95% CI', ' lag1', 'lag1 95% CI', '    lag2', 'lag2 95% CI', ' lag3', 'lag3 95% CI',
                  '    lag4', 'lag4 95% CI', ' lag5', 'lag5 95% CI', '    lag6', 'lag6 95% CI', ' lag7', 'lag7 95% CI')
  
  tm <- forest_theme(base_size = 10,
                     refline_lty = "solid",
                     ci_pch = c(15, 18),
                     ci_col = c("#377eb8", "#4daf4a"),
                     footnote_gp = gpar(col = "blue"),
                     legend_name = "Group",
                     legend_value = c("Trt 1", "Trt 2"),
                     vertline_lty = c("dashed", "dotted"),
                     vertline_col = c("#d6604d", "#bababa"),
                     # Table cell padding, width 4 and heights 3
                     core = list(padding = unit(c(4, 3), "mm")))
  
  p <- forest(dt,
              est = est,
              lower = lower, 
              upper = upper,
              ci_column = c(2, 4, 6, 8, 10, 12, 14, 16),
              ref_line = 1,
              ticks_at = c(1),
              title = "All ED Visits, May-September (2011 to 2019)",
              theme = tm
  )
  
  plot(p)  # Replace with your plot code
  
  }

 



### Multiple lags the long way

forest_data <- read.csv("Lago_Lag7_FP.csv")

forest_data2 <- pivot_longer(forest_data, cols = c("lag0", "lag1", "lag2", "lag3", "lag4", "lag5", "lag6", "lag7"), names_to = "Lag")
forest_data2 <- pivot_wider(forest_data2, names_from = c("Type", "Outcome"), values_from = value)

dt <- forest_data2

dt$`    PMAD` <- paste(rep(" ", 20), collapse = " ")
dt$` SMI` <- paste(rep(" ", 20), collapse = " ")
dt$`    MDP` <- paste(rep(" ", 20), collapse = " ")
dt$` SUI-T` <- paste(rep(" ", 20), collapse = " ")
dt$`    SUB` <- paste(rep(" ", 20), collapse = " ")


dt$'PMAD 95% CI' <- paste(sprintf("%.1f (%.1f, %.1f)", dt$'matRRfit_PMAD P-S', dt$'matRRlow_PMAD P-S', dt$'matRRhigh_PMAD P-S'))
dt$'SMI 95% CI' <- paste(sprintf("%.1f (%.1f, %.1f)", dt$'matRRfit_SMI P-S', dt$'matRRlow_SMI P-S', dt$'matRRhigh_SMI P-S'))
dt$'MDP 95% CI' <- paste(sprintf("%.1f (%.1f, %.1f)", dt$'matRRfit_MDP P-S', dt$'matRRlow_MDP P-S', dt$'matRRhigh_MDP P-S'))
dt$'SUI-T 95% CI' <- paste(sprintf("%.1f (%.1f, %.1f)", dt$'matRRfit_SMI P-S', dt$'matRRlow_SMI P-S', dt$'matRRhigh_SMI P-S'))
dt$'SUB 95% CI' <- paste(sprintf("%.1f (%.1f, %.1f)", dt$'matRRfit_SUB P-S', dt$'matRRlow_SUB P-S', dt$'matRRhigh_SUB P-S'))

est = list(dt$'matRRfit_PMAD P-S',
           dt$'matRRfit_SMI P-S',
           dt$'matRRfit_MDP P-S',
           dt$'matRRfit_SUIT P-S',
           dt$'matRRfit_SUB P-S')
   
lower =list(dt$'matRRlow_PMAD P-S',
            dt$'matRRlow_SMI P-S',
            dt$'matRRlow_MDP P-S',
            dt$'matRRlow_SUIT P-S',
            dt$'matRRlow_SUB P-S')

upper = list(dt$'matRRhigh_PMAD P-S',
             dt$'matRRhigh_SMI P-S',
             dt$'matRRhigh_MDP P-S',
             dt$'matRRhigh_SUIT P-S',
             dt$'matRRhigh_SUB P-S')

dt <- dt %>%
  dplyr::select(Lag, '    PMAD', 'PMAD 95% CI', ' SMI', 'SMI 95% CI', '    MDP', 'MDP 95% CI', ' SUI-T', 'SUI-T 95% CI',
                '    SUB', 'SUB 95% CI')

tm <- forest_theme(base_size = 10),
                   refline_lty = "solid",
                   ci_pch = c(15, 18),
                   footnote_gp = gpar(col = "blue"),
                   vertline_lty = c("dashed", "dotted"),
                   vertline_col = c("#d6604d", "#bababa"),
                   # Table cell padding, width 4 and heights 3
                   core = list(padding = unit(c(4, 3), "mm")))

p <- forest(dt,
            est = est,
            lower = lower, 
            upper = upper,
            ci_column = c(2, 4, 6, 8, 10),
            ref_line = 1,
            ticks_at = c(1),
            title = "Daily Lags, All ED Visits (May-Sept 2011-2019)",
            theme = tm
)

plot(p)

## HW Intensity plots

low7 <- read.csv("Results/Lag7/Cumulative_Lag7_LowIntensity.csv")%>%
  mutate(Lag="Lag0 to Lag7")%>%
  mutate(Intensity="Low")
mod7 <- read.csv("Results/Lag7/Cumulative_Lag7_ModerateIntensity.csv")%>%
  mutate(Lag="Lag0 to Lag7")%>%
  mutate(Intensity="Moderate")
high7 <- read.csv("Results/Lag7/Cumulative_Lag7_HighIntensity.csv")%>%
  mutate(Lag="Lag0 to Lag7")%>%
  mutate(Intensity="High")
low3 <- read.csv("Results/Lag3/Cumulative_Lag3_LowIntensity.csv")%>%
  mutate(Lag="Lag0 to Lag3")%>%
  mutate(Intensity="Low")
mod3 <- read.csv("Results/Lag3/Cumulative_Lag3_ModerateIntensity.csv")%>%
  mutate(Lag="Lag0 to Lag3")%>%
  mutate(Intensity="Moderate")
high3 <- read.csv("Results/Lag3/Cumulative_Lag3_HighIntensity.csv")%>%
  mutate(Lag="Lag0 to Lag3")%>%
  mutate(Intensity="High")

forest_data <- rbind(low7, mod7, high7, low3, mod3, high3)

forest_data <- forest_data %>%
  dplyr::select(-X, -estmean)%>%
  rename(Outcome=outcome)

outcomes <- c("PMAD_primsec", "MDP_primsec", "SMI_primsec", "SUIT_primsec", "SUB_primsec")

forest_data <- forest_data %>%
  filter(Outcome %in% outcomes)%>%
  dplyr::select(-Subgroup)

forest_data <- distinct(forest_data)

forest_data2 <- pivot_wider(forest_data, names_from = Intensity, values_from = c(over_rr, over_rr_se, over_rr_high, over_rr_low))

write.csv(forest_data2, "hwintensityforest.csv")

dt <- read.csv("hwintensityforest.csv")

dt$`    Low Intensity` <- paste(rep(" ", 20), collapse = " ")
dt$` Moderate Intensity` <- paste(rep(" ", 20), collapse = " ")
dt$`    High Intensity` <- paste(rep(" ", 20), collapse = " ")

dt$'Low Intensity 95% CI' <- paste(sprintf("%.2f (%.2f, %.2f)", dt$'over_rr_Low', dt$'over_rr_low_Low', dt$'over_rr_high_Low'))
dt$'Moderate Intensity 95% CI' <- paste(sprintf("%.2f (%.2f, %.2f)", dt$'over_rr_Moderate', dt$'over_rr_low_Moderate', dt$'over_rr_high_Moderate'))
dt$'High Intensity 95% CI' <- paste(sprintf("%.2f (%.2f, %.2f)", dt$'over_rr_High', dt$'over_rr_low_High', dt$'over_rr_high_High'))

est = list(dt$over_rr_Low,
           dt$over_rr_Moderate,
           dt$over_rr_High)

lower = list(dt$over_rr_low_Low,
             dt$over_rr_low_Moderate,
             dt$over_rr_low_High)

upper = list(dt$over_rr_high_Low,
             dt$over_rr_high_Moderate,
             dt$over_rr_high_High)

dt <- dt %>%
  dplyr::select(Lag, Outcome, '    Low Intensity', 'Low Intensity 95% CI', ' Moderate Intensity', 'Moderate Intensity 95% CI', '    High Intensity', 'High Intensity 95% CI')

tm <- forest_theme(base_size = 10)

p <- forest(dt,
            est = est,
            lower = lower, 
            upper = upper,
            ci_column = c(3,5,7),
            ref_line = 1,
            ticks_at = c(1),
            title = "Heatwave Intensity, All ED Visits (May-Sept 2011-2019)",
            theme = tm
)

plot(p)

#### Alt HW ###
above95_7 <- read.csv("Results/Lag7/Cumulative_Lag7_Full_Above95th.csv")%>%
  mutate('Heatwave Definition'="3-Days Above 95%",
         'Lag'='Lag0 to Lag7')
hi7 <- read.csv("Results/Lag7/Cumulative_Lag7_Full_HeatIndex.csv")%>%
  mutate('Heatwave Definition'="Heat Index",
         'Lag'='Lag0 to Lag7')
above95_3 <- read.csv("Results/Lag3/Cumulative_Lag3_Full_Above95th.csv")%>%
  mutate('Heatwave Definition'="3-Days Above 95%",
         'Lag'='Lag0 to Lag3')
hi3 <- read.csv("Results/Lag3/Cumulative_Lag3_Full_HeatIndex.csv")%>%
  mutate('Heatwave Definition'="Heat Index",
         'Lag'='Lag0 to Lag3')

forest_data <- rbind(above95_7, hi7, above95_3, hi3)

forest_data <- forest_data %>%
  dplyr::select(-X, -estmean)%>%
  rename(Outcome=outcome)

outcomes <- c("PMAD_primsec", "MDP_primsec", "SMI_primsec", "SUIT_primsec", "SUB_primsec")

forest_data <- forest_data %>%
  filter(Outcome %in% outcomes)%>%
  dplyr::select(-Subgroup)

forest_data <- distinct(forest_data)

forest_data2 <- pivot_wider(forest_data, names_from = c('Heatwave Definition'), values_from = c(over_rr, over_rr_se, over_rr_high, over_rr_low))

write.csv(forest_data2, "althwforest.csv")

dt <- read.csv("althwforest.csv")

dt$`    3-Day 95% HW` <- paste(rep(" ", 20), collapse = " ")
dt$` Heat Index` <- paste(rep(" ", 20), collapse = " ")

dt$'3-Day 95% HW 95% CI' <- paste(sprintf("%.2f (%.2f, %.2f)", dt$'over_rr_3.Days.Above.95.', dt$'over_rr_low_3.Days.Above.95.', dt$'over_rr_high_3.Days.Above.95.'))
dt$'Heat Index 95% CI' <- paste(sprintf("%.2f (%.2f, %.2f)", dt$'over_rr_Heat.Index', dt$'over_rr_low_Heat.Index', dt$'over_rr_high_Heat.Index'))

est = list(dt$'over_rr_3.Days.Above.95.',
           dt$'over_rr_Heat.Index')

lower = list(dt$'over_rr_low_3.Days.Above.95.',
             dt$'over_rr_low_Heat.Index')

upper = list(dt$'over_rr_high_3.Days.Above.95.',
             dt$'over_rr_high_Heat.Index')

dt <- dt %>%
  dplyr::select(Lag, Outcome, '    3-Day 95% HW', '3-Day 95% HW 95% CI', ' Heat Index', 'Heat Index 95% CI')

tm <- forest_theme(base_size = 10),
refline_lty = "solid",
ci_pch = c(15, 18),
footnote_gp = gpar(col = "blue"),
vertline_lty = c("dashed", "dotted"),
vertline_col = c("#d6604d", "#bababa"),
# Table cell padding, width 4 and heights 3
core = list(padding = unit(c(4, 3), "mm")))

p <- forest(dt,
            est = est,
            lower = lower, 
            upper = upper,
            ci_column = c(3,5),
            ref_line = 1,
            ticks_at = c(1),
            title = "Alternative HW Definitions, All ED Visits (May-Sept 2011-2019)",
            theme = tm
)

plot(p)







