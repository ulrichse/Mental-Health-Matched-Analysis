# Create forest plots of cumulative lags and also create forest plots with multiple columns for different outcomes

library(tidyr)
library(forestploter)
library(dplyr)
library(grid)
#### PLOTS ####

#### Read in data files; you can include more subgroups but I am just doing one for an example

full <- read.csv("Results/Lag7/Cumulative_Lag7_Full.csv")
black <- read.csv("Results/Lag7/Cumulative_Lag7_Black.csv")

forest_data <- rbind(full, black)

forest_data <- forest_data %>%
  dplyr::select(-X, -estmean)%>%
  distinct(over_rr, over_rr_se, over_rr_high, over_rr_low, outcome, Subgroup)%>%
  rename(Outcome=outcome)

outcomes <- c("PMAD", "MDP", "SMI", "SUIT", "SUB")

forest_data <- forest_data %>%
  filter(Outcome %in% outcomes)

#### Plot with multiple outcomes ####

dt <- pivot_wider(forest_data, names_from = Outcome, values_from = c('over_rr', 'over_rr_se', 'over_rr_high', 'over_rr_low'))

# Add two blank columns for CI
dt$`PMAD` <- paste(rep(" ", 20), collapse = " ")
dt$`MDP` <- paste(rep(" ", 20), collapse = " ")
dt$`SMI` <- paste(rep(" ", 20), collapse = " ")
dt$`SUI-T` <- paste(rep(" ", 20), collapse = " ")
dt$`SUB` <- paste(rep(" ", 20), collapse = " ")

# Generate point estimation and 95% CI. Paste two CIs together and separate by line break.
dt$`PMAD RR (95% CI)` <- ifelse(is.na(dt$over_rr_se_PMAD), "",
                                sprintf("%.2f (%.2f to %.2f)",
                                        dt$over_rr_PMAD, dt$over_rr_low_PMAD, dt$over_rr_high_PMAD))

dt$`MDP RR (95% CI)` <- ifelse(is.na(dt$over_rr_se_MDP), "",
                               sprintf("%.2f (%.2f to %.2f)",
                                       dt$over_rr_MDP, dt$over_rr_low_MDP, dt$over_rr_high_MDP))

dt$`SMI RR (95% CI)` <- ifelse(is.na(dt$over_rr_se_SMI), "",
                               sprintf("%.2f (%.2f to %.2f)",
                                       dt$over_rr_SMI, dt$over_rr_low_SMI, dt$over_rr_high_SMI))

dt$`SUIT RR (95% CI)` <- ifelse(is.na(dt$over_rr_se_SUIT), "",
                                sprintf("%.2f (%.2f to %.2f)",
                                        dt$over_rr_SUIT, dt$over_rr_low_SUIT, dt$over_rr_high_SUIT))

dt$`SUB RR (95% CI)` <- ifelse(is.na(dt$over_rr_se_SUB), "",
                               sprintf("%.2f (%.2f to %.2f)",
                                       dt$over_rr_SUB, dt$over_rr_low_SUB, dt$over_rr_high_SUB))

est = list(dt$over_rr_PMAD,
           dt$over_rr_MDP,
           dt$over_rr_SMI,
           dt$over_rr_SUIT,
           dt$over_rr_SUB)
lower = list(dt$over_rr_low_PMAD,
             dt$over_rr_low_MDP,
             dt$over_rr_low_SMI,
             dt$over_rr_low_SUIT,
             dt$over_rr_low_SUB) 
upper = list(dt$over_rr_high_PMAD,
             dt$over_rr_high_MDP,
             dt$over_rr_high_SMI,
             dt$over_rr_high_SUIT,
             dt$over_rr_high_SUB)

dt <- dt %>%
  dplyr::select(Subgroup, 
                `PMAD RR (95% CI)`, 
                'PMAD',
                `MDP RR (95% CI)`, 
                'MDP',
                `SMI RR (95% CI)`,
                'SMI',
                `SUIT RR (95% CI)`,
                'SUI-T',
                `SUB RR (95% CI)`,
                'SUB')

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
            ci_column = c(3, 5, 7, 9, 10),
            ticks_at = c(1),
            ref_line = 1,
            title = "Cumulative Lag0 to Lag7, Primary/Secondary Outcomes",
            theme = tm)
plot(p)


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
            theme = tm)
plot(p)  # Replace with your plot code


