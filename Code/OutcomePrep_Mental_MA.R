## This code pulls selected maternal mental health outcomes from ED delivery data ("deliveries.sas7bdat") using ICD-10 codes. 

## Call packages and set working directory 

library(tableone)
library(gmodels)
library(dplyr)
library(lubridate)
library(arrow)
library(data.table)
library(dplyr)
library(geepack)
library(haven)
library(lubridate)
library(lme4)
setwd("~/RStudio/Matched Mental Health/")

## Read in data files

# Hospital delivery data for North Carolina

df_wrong <- read_parquet("Data/nc_pregnancy_2011_2019.parquet")

#### AGE ####
df_wrong$Age <- as.factor(ifelse(df_wrong$agey < 20, '18-19', 
                               ifelse(df_wrong$agey<25, '20-24',
                                      ifelse(df_wrong$agey <30, '25-29',
                                             ifelse(df_wrong$agey<35,'30-34',
                                                    ifelse(df_wrong$agey<40, '35-39',
                                                           ifelse(df_wrong$agey <= 44, '40+', 0)))))))
df_wrong$Age <- factor(df_wrong$Age)

df_wrong$Race <- fifelse(df_wrong$Race == "5", "White",
                       fifelse(df_wrong$Race == "3", "Black",
                               fifelse(df_wrong$Race %in% c("2", "1", "4", "6"), "Other",
                                       "Unknown")))
df_wrong$Race <- as.factor(df_wrong$Race)
table(df_wrong$Race)

df_wrong$Ethnicity <- fifelse(df_wrong$ethnicity == "1", "Not Hispanic",
                              fifelse(df_wrong$ethnicity == "2", "Hispanic",
                                      "Unknown"))
df_wrong$Ethnicity <- factor(df_wrong$Ethnicity)
table(df_wrong$Ethnicity)


df_wrong$Age <- as.factor(ifelse(df_wrong$agey < 20, '18-19', 
                                 ifelse(df_wrong$agey<25, '20-24',
                                        ifelse(df_wrong$agey <30, '25-29',
                                               ifelse(df_wrong$agey<35,'30-34',
                                                      ifelse(df_wrong$agey<40, '35-39',
                                                             ifelse(df_wrong$agey <= 44, '40+', 0)))))))
df_wrong$Age <- factor(df_wrong$Age)
table(df_wrong$Age)

df_wrong$Insurance <- as.factor(ifelse(df_wrong$insurance == "1", "Medicaid",
                                       ifelse(df_wrong$insurance == "0", "Private Ins",
                                              ifelse(df_wrong$insurance == "3", "Other gov't", 
                                                     ifelse(df_wrong$insurance == "4",
                                                            "Self-Pay", "Other/Unknown")))))

df_wrong$Insurance <- factor(df_wrong$Insurance)
table(df_wrong$Insurance)

table(df_wrong$PMAD)
table(df_wrong$PMAD_Primary)
table(df_wrong$PMAD_PrimSec)
table(df_wrong$SMI)
table(df_wrong$SMI_Primary)
table(df_wrong$SMI_PrimSec)
table(df_wrong$MDP)
table(df_wrong$MDP_Primary)
table(df_wrong$MDP_PrimSec)
table(df_wrong$suicide_attempt)
table(df_wrong$suicide_attempt_primary)
table(df_wrong$suicide_attempt_PrimSec)
table(df_wrong$suicide_thought)
table(df_wrong$suicide_thought_primary)
table(df_wrong$suicide_thought_PrimSec)
table(df_wrong$SubAbuse)
table(df_wrong$Sub_Primary)
table(df_wrong$Sub_PrimSec)



tabdat <- df_wrong %>%
  dplyr::select(Date, 
                ptzip,
                Age, 
                Race, 
                Ethnicity, 
                Insurance,
                PMAD, 
                MDP, 
                SMI, 
                suicide_attempt, 
                suicide_thought, 
                SubAbuse, 
                PMAD_Primary, 
                MDP_Primary, 
                SMI_Primary, 
                suicide_attempt_primary, 
                suicide_thought_primary, 
                Sub_Primary,
                PMAD_PrimSec, 
                MDP_PrimSec, 
                SMI_PrimSec, 
                suicide_attempt_PrimSec, 
                suicide_thought_PrimSec, 
                Sub_PrimSec
                )

write_parquet(tabdat, "Data/Outcomes_Mental.parquet")

#### Create table one ####

library(dplyr)
library(tableone)

# Ensure all categorical variables have the same levels in each subset
tabdat <- read_parquet("Data/Outcomes_Mental.parquet") %>%
  mutate(Age = factor(Age, levels = unique(tabdat$Age)),
         Race = factor(Race, levels = unique(tabdat$Race)),
         Ethnicity = factor(Ethnicity, levels = unique(tabdat$Ethnicity)),
         Insurance = factor(Insurance, levels = unique(tabdat$Insurance)),
         Year = year(Date))

# Subset the data
MDP <- subset(tabdat, MDP == '1')
PMAD <- subset(tabdat, PMAD == '1')
SMI <- subset(tabdat, SMI == '1')
SUICT <- subset(tabdat, suicide_thought == '1')
SUICA <- subset(tabdat, suicide_attempt == '1')
SUB <- subset(tabdat, SubAbuse == '1')

# Define categorical variables
catVars <- c("Year", "Age", "Race", "Ethnicity", "Insurance")

# Create table one for each subset and the full data
full_tab <- CreateTableOne(vars = catVars, data = tabdat, factorVars = catVars)
mdp_tab <- CreateTableOne(vars = catVars, data = MDP, factorVars = catVars)
smi_tab <- CreateTableOne(vars = catVars, data = SMI, factorVars = catVars)
pmad_tab <- CreateTableOne(vars = catVars, data = PMAD, factorVars = catVars)
suict_tab <- CreateTableOne(vars = catVars, data = SUICT, factorVars = catVars)
suica_tab <- CreateTableOne(vars = catVars, data = SUICA, factorVars = catVars)
sub_tab <- CreateTableOne(vars = catVars, data = SUB, factorVars = catVars)

# Print tables
full_tabmat <- print(full_tab, showAllLevels = TRUE, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd = FALSE)
mdp_tabmat <- print(mdp_tab, showAllLevels = TRUE, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd = FALSE)
smi_tabmat <- print(smi_tab, showAllLevels = TRUE, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd = FALSE)
pmad_tabmat <- print(pmad_tab, showAllLevels = TRUE, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd = FALSE)
suict_tabmat <- print(suict_tab, showAllLevels = TRUE, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd = FALSE)
suica_tabmat <- print(suica_tab, showAllLevels = TRUE, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd = FALSE)
sub_tabmat <- print(sub_tab, showAllLevels = TRUE, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd = FALSE)



# Ensure all tables have the same number of rows
if (nrow(full_tabmat) == nrow(mdp_tabmat) && nrow(full_tabmat) == nrow(smi_tabmat) &&
    nrow(full_tabmat) == nrow(pmad_tabmat) && nrow(full_tabmat) == nrow(suict_tabmat) && nrow(full_tabmat) == nrow(suica_tabmat) && nrow(full_tabmat) == nrow(sub_tabmat)) {
  mental_tabmat <- cbind(full_tabmat, mdp_tabmat, smi_tabmat, pmad_tabmat, suict_tabmat, suica_tabmat, sub_tabmat)
} else {
  stop("Number of rows in matrices do not match")
}

mental_tabmat <- cbind(full_tabmat, mdp_tabmat, smi_tabmat, pmad_tabmat, suict_tabmat, suica_tabmat, sub_tabmat)

mental_tabmat <- mental_tabmat %>%
  as.data.frame()

colnames(mental_tabmat)[2] <- "Full"
colnames(mental_tabmat)[3] <- "level.1"
colnames(mental_tabmat)[4] <- "MDP"
colnames(mental_tabmat)[5] <- "level.2"
colnames(mental_tabmat)[6] <- "SMI"
colnames(mental_tabmat)[7] <- "level.3"
colnames(mental_tabmat)[8] <- "PMAD"
colnames(mental_tabmat)[9] <- "level.4"
colnames(mental_tabmat)[10] <- "SUICT"
colnames(mental_tabmat)[11] <- "level.5"
colnames(mental_tabmat)[12] <- "SUICA"
colnames(mental_tabmat)[13] <- "level.6"
colnames(mental_tabmat)[14] <- "SUB"

mental_tabmat <- mental_tabmat %>%
  dplyr::select(level, Full, MDP, SMI, PMAD, SUICT, SUICA, SUB)

write.csv(mental_tabmat, file = "Results/TableOne_Mental.csv")

# Primary Outcomees 

# Subset the data
MDP <- subset(tabdat, MDP_Primary == '1')
PMAD <- subset(tabdat, PMAD_Primary == '1')
SMI <- subset(tabdat, SMI_Primary == '1')
SUICT <- subset(tabdat, suicide_thought_primary == '1')
SUB <- subset(tabdat, Sub_Primary == '1')

# Define categorical variables
catVars <- c("Year", "Age", "Race", "Ethnicity", "Insurance", "ICE_Income_Stratum", "ICE_Race_Stratum", "RUCA_Cat","geo_region")

# Create table one for each subset and the full data
full_tab <- CreateTableOne(vars = catVars, data = tabdat, factorVars = catVars)
mdp_tab <- CreateTableOne(vars = catVars, data = MDP, factorVars = catVars)
smi_tab <- CreateTableOne(vars = catVars, data = SMI, factorVars = catVars)
pmad_tab <- CreateTableOne(vars = catVars, data = PMAD, factorVars = catVars)
suict_tab <- CreateTableOne(vars = catVars, data = SUICT, factorVars = catVars)
sub_tab <- CreateTableOne(vars = catVars, data = SUB, factorVars = catVars)

# Print tables
full_tabmat <- print(full_tab, showAllLevels = TRUE, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd = FALSE)
mdp_tabmat <- print(mdp_tab, showAllLevels = TRUE, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd = FALSE)
smi_tabmat <- print(smi_tab, showAllLevels = TRUE, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd = FALSE)
pmad_tabmat <- print(pmad_tab, showAllLevels = TRUE, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd = FALSE)
suict_tabmat <- print(suict_tab, showAllLevels = TRUE, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd = FALSE)
sub_tabmat <- print(sub_tab, showAllLevels = TRUE, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd = FALSE)



# Ensure all tables have the same number of rows
if (nrow(full_tabmat) == nrow(mdp_tabmat) && nrow(full_tabmat) == nrow(smi_tabmat) &&
    nrow(full_tabmat) == nrow(pmad_tabmat) && nrow(full_tabmat) == nrow(suict_tabmat) && nrow(full_tabmat) == nrow(suica_tabmat) && nrow(full_tabmat) == nrow(sub_tabmat)) {
  mental_tabmat <- cbind(full_tabmat, mdp_tabmat, smi_tabmat, pmad_tabmat, suict_tabmat, suica_tabmat, sub_tabmat)
} else {
  stop("Number of rows in matrices do not match")
}

mental_tabmat <- cbind(full_tabmat, mdp_tabmat, smi_tabmat, pmad_tabmat, suict_tabmat, sub_tabmat)

mental_tabmat <- mental_tabmat %>%
  as.data.frame()

colnames(mental_tabmat)[2] <- "Full"
colnames(mental_tabmat)[3] <- "level.1"
colnames(mental_tabmat)[4] <- "MDP_Primary"
colnames(mental_tabmat)[5] <- "level.2"
colnames(mental_tabmat)[6] <- "SMI_Primary"
colnames(mental_tabmat)[7] <- "level.3"
colnames(mental_tabmat)[8] <- "PMAD_Primary"
colnames(mental_tabmat)[9] <- "level.4"
colnames(mental_tabmat)[10] <- "SUICT_Primary"
colnames(mental_tabmat)[11] <- "level.5"
colnames(mental_tabmat)[12] <- "SUB_Primary"

mental_tabmat <- mental_tabmat %>%
  dplyr::select(level, MDP_Primary, SMI_Primary, PMAD_Primary, SUICT_Primary, SUB_Primary)

write.csv(mental_tabmat, file = "Results/TableOne_Mental_Primary.csv")

# Primary/Secondary Outcomes 

# Subset the data
MDP <- subset(tabdat, MDP_PrimSec == '1')
PMAD <- subset(tabdat, PMAD_PrimSec == '1')
SMI <- subset(tabdat, SMI_PrimSec == '1')
SUICT <- subset(tabdat, suicide_thought_PrimSec == '1')
SUB <- subset(tabdat, Sub_PrimSec == '1')

# Define categorical variables
catVars <- c("Year", "Age", "Race", "Ethnicity", "Insurance")

# Create table one for each subset and the full data
full_tab <- CreateTableOne(vars = catVars, data = tabdat, factorVars = catVars)
mdp_tab <- CreateTableOne(vars = catVars, data = MDP, factorVars = catVars)
smi_tab <- CreateTableOne(vars = catVars, data = SMI, factorVars = catVars)
pmad_tab <- CreateTableOne(vars = catVars, data = PMAD, factorVars = catVars)
suict_tab <- CreateTableOne(vars = catVars, data = SUICT, factorVars = catVars)
sub_tab <- CreateTableOne(vars = catVars, data = SUB, factorVars = catVars)

# Print tables
full_tabmat <- print(full_tab, showAllLevels = TRUE, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd = FALSE)
mdp_tabmat <- print(mdp_tab, showAllLevels = TRUE, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd = FALSE)
smi_tabmat <- print(smi_tab, showAllLevels = TRUE, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd = FALSE)
pmad_tabmat <- print(pmad_tab, showAllLevels = TRUE, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd = FALSE)
suict_tabmat <- print(suict_tab, showAllLevels = TRUE, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd = FALSE)
sub_tabmat <- print(sub_tab, showAllLevels = TRUE, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd = FALSE)

# Ensure all tables have the same number of rows
if (nrow(full_tabmat) == nrow(mdp_tabmat) && nrow(full_tabmat) == nrow(smi_tabmat) &&
    nrow(full_tabmat) == nrow(pmad_tabmat) && nrow(full_tabmat) == nrow(suict_tabmat) && nrow(full_tabmat) == nrow(sub_tabmat)) {
  mental_tabmat <- cbind(full_tabmat, mdp_tabmat, smi_tabmat, pmad_tabmat, suict_tabmat, sub_tabmat)
} else {
  stop("Number of rows in matrices do not match")
}

mental_tabmat <- cbind(full_tabmat, mdp_tabmat, smi_tabmat, pmad_tabmat, suict_tabmat, sub_tabmat)

mental_tabmat <- mental_tabmat %>%
  as.data.frame()

colnames(mental_tabmat)[2] <- "Full"
colnames(mental_tabmat)[3] <- "level.1"
colnames(mental_tabmat)[4] <- "MDP_PrimSec"
colnames(mental_tabmat)[5] <- "level.2"
colnames(mental_tabmat)[6] <- "SMI_PrimSec"
colnames(mental_tabmat)[7] <- "level.3"
colnames(mental_tabmat)[8] <- "PMAD_PrimSec"
colnames(mental_tabmat)[9] <- "level.4"
colnames(mental_tabmat)[10] <- "SUICT_PrimSec"
colnames(mental_tabmat)[11] <- "level.5"
colnames(mental_tabmat)[12] <- "SUB_PrimSec"

mental_tabmat <- mental_tabmat %>%
  dplyr::select(level, MDP_PrimSec, SMI_PrimSec, PMAD_PrimSec, SUICT_PrimSec, SUB_PrimSec)

write.csv(mental_tabmat, file = "Results/TableOne_Mental_PrimSec.csv")


global <- read.csv("Results/TableOne_Mental.csv")
primary <- read.csv("Results/TableOne_Mental_Primary.csv")%>%
  dplyr::select(-level)
primsec <- read.csv("Results/TableOne_Mental_PrimSec.csv")%>%
  dplyr::select(-level)

table <- cbind(global, primary, primsec)

table <- table %>%
  dplyr::select(level, Full, PMAD, PMAD_Primary, PMAD_PrimSec, MDP, MDP_Primary, MDP_PrimSec, SMI, SMI_Primary, SMI_PrimSec, SUICT, SUICT_Primary, SUICT_PrimSec, SUB, SUB_Primary, SUB_PrimSec, SUICA)

write.csv(table, "Results/TableOne_Mental_Outcomes.csv")

#### Combine with outcomes ####

regionvars <- read.csv("Regional_Vars.csv")
cw <- read.csv("Data/Crosswalk_NC.csv")

tabdat <- tabdat %>%
  rename(ZIP=ptzip)

tabdat <- tabdat %>%
  left_join(cw, by=c('ZIP'))

tabdat <- tabdat %>%
  rename(zip=ZIP)

tabdat <- tabdat %>%
  left_join(ice, by=c('zip'))

table(tabdat$geo_region)

