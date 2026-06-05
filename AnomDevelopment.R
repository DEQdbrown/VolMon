###                                                                          ###
### This query pulls 10 years of data from AWQMS and calculates the 1st and  ###
### 99th percentile for each parameter. This is used to determine outliers   ###
### during the audit period. Since the script pulls 10 years of data it      ###
### takes close to an hour to run, so be prepared to wait. This pull only    ###
### needs to be run once a year.                                             ###
###                                                                          ###
### This script was adapted from Travis Pritchard's IR duplicate script.     ###
### Run line 13 if you have never installed the AWQMSdata package before.    ###
### Run line 14 periodically to capture updates to the AWQMSdata pacakage.   ###
###                                                                          ###

#install.packages("devtools")
#devtools::install_github("TravisPritchardODEQ/AWQMSdata",dependencies = TRUE, force = TRUE, upgrade = FALSE)

### Load tools and packages necessary for this script
library(tidyverse)
library(AWQMSdata)
library(lubridate)
library(readxl)
library(openxlsx)

### Disable scientific notation
options(scipen = 999999)

### Set working directory
setwd("//deqlab1/Assessment/AWQMS/Validation")

### Set the data window by changing these dates
End_Date <- Sys.Date()
Start_Date <- End_Date - years(10)

### Pull in list of normalized units, parameterUIDs, and common names
NormUnits <- read.xlsx("//deqlab1/Assessment/AWQMS/Validation/NormalizedUnits.xlsx")

### Load convert_units function
source("https://raw.githubusercontent.com/DEQdbrown/AWQMS_Audit/main/FUNCTION_convert_units.R")

### Pull all data from AWQMS for the last 10 years
All_Proj_Data <- AWQMS_Data(startdate = Start_Date, enddate = End_Date, filterQC = FALSE) %>% 
  mutate(SampleStartDate = as.Date(SampleStartDate, format = "%Y-%m-%d")) 

### Filter out Unknown Projects, non-detect results, and QA samples
Filt_Proj_Data <- All_Proj_Data %>%
  filter(!str_detect(Result_Operator, '<'), # remove non-detects
         !is.na(Result_Unit), # remove NAs from unit column
         Result_status != 'Rejected', # remove rejected data
         !str_detect(Activity_Type, 'Blank|Spike')) %>% # remove blanks and matrix spikes
  mutate(Result_Unit = str_replace(Result_Unit, "/L", "/l"),
         Result_Unit = str_replace(Result_Unit, "mL", "ml"))
  
# This snippet isn't necessary to run, but can be helpful if needed. It is more informative if lines 43-48 have been run 
# Count_Param <- All_Proj_Data %>%
#   group_by(SampleMedia, chr_uid, Char_Name, Unit_UID) %>% # group data by these four columns
#   summarise(count = n(), .groups = 'drop') # provide a count of instances of each group
  
### Check for new parameters
Param_Check <- Filt_Proj_Data %>%
  left_join(NormUnits, c('SampleMedia', 'chr_uid', 'Char_Name', 'Result_Unit', 'Unit_UID')) %>% # joins data with the NormUnits file
  distinct(SampleMedia, chr_uid, ParamUID, Char_Name, CASNumber, Unit_UID, Result_Unit, 
           Pref_Unit_UID, Preferred_Unit) %>% # Finds the unique combinations of these nine columns
  filter(is.na(ParamUID)) # removes non-NAs from the ParamUID column leaving only new parameters without ParamUIDs

write.xlsx(Param_Check, str_glue("AnyNewParametersNeeded_{End_Date}.xlsx"))
stop("Review needed")
# Check this file for any new parameters that need a ParamUID, preferred units or conversion.
# If new parameters are found, update NormUnits file and reload by running line 34 again before moving on with this script


### Check to see if additional conversions are needed
Conv_Check <- Filt_Proj_Data %>%
  left_join(NormUnits, c('SampleMedia', 'chr_uid', 'Char_Name', 'Result_Unit', 'Unit_UID')) %>% # joins data with the NormUnits file
  distinct(SampleMedia, ParamUID, Char_Name, Unit_UID, Pref_Unit_UID) %>% # Finds the unique combinations of these five columns
  mutate(Same = if_else(Unit_UID == Pref_Unit_UID, 'Yep', 'Nope')) %>% # creates a column indicating if the unit columns match
  filter(Same != 'Yep') # removes rows where the unit columns match leaving only pairs where conversion might be needed
  
write.xlsx(Conv_Check, str_glue("AnyNewConversionsNeeded_{End_Date}.xlsx"))
stop("Review needed")
# Check this file for any conversions not already captured in the script
# If any new conversions are needed, including one to one conversions, update the code below before moving on with this script

### Convert data to preferred units and filter out leachate and influent
Converted_Data <- Filt_Proj_Data %>%
  left_join(NormUnits, c('SampleMedia', 'chr_uid', 'Char_Name', 'Result_Unit', 'Unit_UID')) %>% # join with normalized units file
  filter(is.na(SampleSubmedia) | !str_detect(SampleSubmedia, 'Leachate|Influent')) %>%
  convert_units(unit_col = 'Unit_UID', pref_unit_col = 'Pref_Unit_UID', 
                result_col = 'Result_Numeric') %>% # convert_units is the function listed above
  relocate(c(Conv_Result, Preferred_Unit, Pref_Unit_UID), 
           .before = ResultCondName) %>% # move joined or created results columns closer to AWQMS result columns
  filter(!is.na(Result_Unit)) %>% # remove NAs from Result_Unit column
  filter(Statistical_Base != "Delta" | is.na(Statistical_Base)) # filters out Delta and NA values from the statistical base column
   
LeachInflu_Data <- Filt_Proj_Data %>%
  left_join(NormUnits, c('SampleMedia', 'chr_uid', 'Char_Name', 'Result_Unit', 'Unit_UID')) %>% 
  filter(str_detect(SampleSubmedia, 'Leachate|Influent')) %>%
  convert_units(unit_col = 'Unit_UID', pref_unit_col = 'Pref_Unit_UID', 
                result_col = 'Result_Numeric') %>% 
  relocate(c(Conv_Result, Preferred_Unit, Pref_Unit_UID), 
           .before = ResultCondName) %>% 
  filter(!is.na(Result_Unit)) %>% 
  filter(Statistical_Base != "Delta" | is.na(Statistical_Base)) 

### Calculate percentiles for a majority of data and leachate/influent data separately
MajorityPerc <- Converted_Data %>%
  mutate(Char_Speciation = as.character(Char_Speciation),
         Sample_Fraction = as.character(Sample_Fraction),
         Statistical_Base = as.character(Statistical_Base)) %>% # ensures that these three columns are characters
  group_by(SampleMedia, ParamUID, Char_Speciation, Sample_Fraction, 
           Statistical_Base, Preferred_Unit) %>% # groups data by these six columns
  summarise(
    p01 = quantile(Conv_Result, probs = 0.01, na.rm = TRUE), 
    p99 = quantile(Conv_Result, probs = 0.99, na.rm = TRUE)) %>% # calculates the 1st and 99th percentiles
  mutate(SubMedia = if_else(SampleMedia == 'Water',"NonLeach",NA))

LeachPerc <- LeachInflu_Data %>%
  mutate(Char_Speciation = as.character(Char_Speciation),
         Sample_Fraction = as.character(Sample_Fraction),
         Statistical_Base = as.character(Statistical_Base)) %>% 
  group_by(SampleMedia, ParamUID, Char_Speciation, Sample_Fraction, 
           Statistical_Base, Preferred_Unit) %>% 
  summarise(
    p01 = quantile(Conv_Result, probs = 0.01, na.rm = TRUE), 
    p99 = quantile(Conv_Result, probs = 0.99, na.rm = TRUE)) %>%
  mutate(SubMedia = if_else(SampleMedia == 'Water',"Leach",NA))

### Combine Data and write the file to Excel
Percentiles <- bind_rows(MajorityPerc, LeachPerc)

write.xlsx(Percentiles, str_glue("OutlierPercentiles_{End_Date}.xlsx"))
