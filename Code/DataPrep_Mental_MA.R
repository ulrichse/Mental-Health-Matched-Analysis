
## Use this code to create Heatwave_Metrics_v2.parquet and Outcomes_Mental_v2.parquet  
## This code will create a data frame consisting of daily observations at the ZCTA level for 
## the selected time period, exposure metric (heatwaves) and health outcome(s) (births, PTB, hypertension etc.)

# The should have the following columns after running this code: 

## "zip": county zip code
## "date": date of each day
## "year": year for each day
## "doy": day of year, used to find similar non-heatwave days in other years
## "dow": day of week
## "heatwave": an indicator variable for heatwave exposure, "heatwave" is 1 
##          if a day is exposed to storm and 0 for non-heatwave exposed days (or
##          other binary exposure of interest)
## "SMI, MDP, PMAD, SUB", etc. : outcomes included in delivery data 


# Read in packages and set working directory

library(dplyr)
library(arrow)
library(lubridate)
library(data.table)
library(haven)
library(sjPlot)
library(MASS)

setwd("~/Matched Mental Health")
getwd()

#### DATA PREP ####

#HEATWAVES
## Read in Heatwave_Metrics.parquet file and restrict to warm months (May to September) of 2016 to 2019
## This file contains daily, ZCTA-level heatwave data. We use the Heatwave column in this analysis, which 
## indicates whether a day within a ZCTA was a heatwave or not (1/0), based on EHF calculations. 
## Further documentation of EHF heatwave data is available here: https://github.com/wertisml/Heatwave
## The 'Zip' column in the Heatwave_Metrics.parquet file represent ZCTAs, so we do not need to crosswalk. 

data <- read_parquet("Data/Heatwave_Metrics.parquet")%>%
  setDT()%>%
  filter(Zip >= 27006 & Zip <= 28909)%>%  # Filter for NC zip codes
  rename(date=Date, # Rename date column to lowercase
         heatwave=Heatwave, # Select Heatwave column as our heatwave variable
         zip=Zip)%>% # Rename zip code column to lowercase
  mutate(year=year(date),
         doy=yday(date),
         dow=wday(date))%>%
  dplyr::select(date, heatwave, year, doy, dow, zip)%>% # Select variables of interest
  mutate(heatwave=as.integer(heatwave)) %>% # Set as an integer just in case
  filter(year(date) >= 2011 & year(date) <= 2019)%>% # Filter for years of interest
  filter(month(date) >= 5 & month(date) <= 9) # Filter for months of interest

#Filter out heatwaves with lag periods outside of May-September (heatwaves occurring during the last week of September)
data <- data %>% # Create sept_heatwave variable that =1 if heatwave occurs during September 23-30
  mutate(sept_heatwave=ifelse(heatwave==1 & date %in% c("2016-09-23","2016-09-24","2016-09-25", "2016-09-26", "2016-09-27", "2016-09-28", "2016-09-29", "2016-09-30", 
                                                        "2017-09-23","2017-09-24","2017-09-25", "2017-09-26", "2017-09-27", "2017-09-28", "2017-09-29", "2017-09-30", 
                                                        "2018-09-23","2018-09-24","2018-09-25", "2018-09-26", "2018-09-27", "2018-09-28", "2018-09-29", "2018-09-30", 
                                                        "2019-09-23","2019-09-24","2019-09-25", "2019-09-26", "2019-09-27", "2019-09-28", "2019-09-29", "2019-09-30"), 1, 0))

data <- data %>%
  mutate(heatwave=ifelse(heatwave==1 & sept_heatwave==0, 1, 0)) %>% # Remove late September heatwaves from the dataset
  dplyr::select(-sept_heatwave)

write_parquet(data, "Data/Heatwave_Metrics_v2.parquet")

#DELIVERIES
## deliveries.parquet is individual delivery data with date and zip code. 
## Includes 0/1 indicators for GDM, HDP, PTB, SMI, PMAD, MDP, SMM21, and SUB for each delivery. 
## We will restrict deliveries to May to September of 2016 to 2019. 
## ptzip is the patient's zip code of residence. We will need to crosswalk to ZCTA values. 
## We will aggregate the deliveries to create daily, ZCTA-level counts for births and outcomes.

deliv <- read_parquet("Data/Outcomes_Mental.parquet")%>% # Read in deliveries file
  setDT()%>%
  mutate(date=as.Date(Date), # Create date column
         zip=as.numeric(ptzip))%>% # This is not in ZCTA form so we will have to crosswalk it below
  filter(zip >= 27006 & zip <= 28909)%>% # Filter for years of interest
  dplyr::select(zip, 
                date, 
                SMI, 
                MDP, 
                PMAD, 
                suicide_attempt, 
                suicide_thought, 
                SubAbuse, 
                SMI_Primary, 
                MDP_Primary, 
                PMAD_Primary, 
                suicide_attempt_primary, 
                suicide_thought_primary, 
                Sub_Primary, 
                SMI_PrimSec, 
                MDP_PrimSec, 
                PMAD_PrimSec, 
                suicide_attempt_PrimSec, 
                suicide_thought_PrimSec, 
                Sub_PrimSec, 
                Race, 
                Age, 
                Insurance, 
                Ethnicity) # Filter for variables of interest 

# Read in crosswalk file to convert from zip code to ZCTA
cw <- read.csv("Data/Crosswalk_NC.csv")%>%
  setDT()%>%
  rename(zip=ZIP)

# Crosswalk the delivery data 
deliv <- deliv %>%
  left_join(cw, by=c('zip'))%>%
  dplyr::select(-zip, ZIP_TYPE)%>%
  rename(zip=ZCTA) #The 'zip' variable in the deliv dataframe will now represent ZCTAs, not zip codes. There should be ~804 unique ZCTAs. 

write_parquet(deliv,"Data/Outcomes_Mental_v2.parquet")

# Create shapefile for mapping

data <- read_parquet("Data/Outcomes_Mental_v2.parquet")

outcomes <- data %>%
  group_by(zip)%>%
  summarize(across(where(is.integer), sum, na.rm = TRUE))
            
heat <- read_parquet("Data/Heatwave_Metrics.parquet")

heatgrp <- heat %>%
  filter(Zip >= 27006 & Zip <= 28909)%>%
  filter(year(Date) >= 2011 & year(Date) <= 2019)%>% # Filter for years of interest
  filter(month(Date) >= 5 & month(Date) <= 9) %>%
  group_by(Zip)%>%
  summarize(heatwave=sum(Heatwave),
            low_intensity=sum(low_intensity),
            moderate_intensity=sum(moderate_intensity),
            high_intensity=sum(high_intensity),
            Severe_Heatwaves=sum(Severe_Heatwaves),
            Extreme_Heatwaves=sum(Extreme_Heatwaves))

mapdat <- heatgrp %>%
  rename(zip=Zip)%>%
  left_join(outcomes, by=c("zip"))

shp <- read_sf("Data/shp/tl_2020_us_zcta510.shp")%>%
  rename(zip=ZCTA5CE10)%>%
  filter(zip >= 27006 & zip <= 28909)%>%
  mutate(zip=as.numeric(zip))

shp <- shp %>%
  left_join(mapdat, by=c('zip'))

write_sf(shp, "Data/shp/MapData_2011_2019.shp")


















