###                                                                          ###
### This query pulls 20 years of data from AWQMS and calculates the 1st,     ###
### 5th, 95th and 99th percentile for each parameter. These are used to      ###
### determine anomalies during grab data review. This script only needs to   ###
### be run once every five years, so check the date on the most recent file  ###
### before running again.                                                    ###
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
library(odbc)
library(RODBC)

### Disable scientific notation
options(scipen = 999999)

### Set working directory
setwd("//deqlab1/Assessment/AWQMS/Validation")

### Set the data window by changing these dates
End_Date <- Sys.Date()
Start_Date <- End_Date - years(20)

### Pull in list of normalized units, parameterUIDs, and common names
NormUnits <- read.xlsx("//deqlab1/Assessment/AWQMS/Validation/NormalizedUnits.xlsx")

### Load convert_units function
source("https://raw.githubusercontent.com/DEQdbrown/AWQMS_Audit/main/FUNCTION_convert_units.R")

### Pull all data from AWQMS for the last 10 years
All_Proj_Data <- AWQMS_Data(startdate = Start_Date, enddate = End_Date, project = 'Surface Water Ambient Monitoring', filterQC = FALSE) %>% 
  mutate(SampleStartDate = as.Date(SampleStartDate, format = "%Y-%m-%d")) 


### Filter out Unknown Projects, non-detect results, and QA samples
Filt_Proj_Data <- All_Proj_Data %>%
  filter(!str_detect(Result_Operator, '<'), # remove non-detects
         !is.na(Result_Unit), # remove NAs from unit column
         Result_status != 'Rejected', # remove rejected data
         !str_detect(Activity_Type, 'Blank|Spike')) %>% # remove blanks and matrix spikes
  mutate(Result_Unit = str_replace(Result_Unit, "/L", "/l"),
         Result_Unit = str_replace(Result_Unit, "mL", "ml")) |>
  filter(!MLocID %in% c('10332-ORDEQ', '10339-ORDEQ', '10344-ORDEQ', "10350-ORDEQ", # this removes the Willamette mainstem and Columbia sites from the calculation
                        '10352-ORDEQ', '10355-ORDEQ', '10359-ORDEQ', '10555-ORDEQ', # large rivers have slightly different water chemistry, so including them may
                        '10611-ORDEQ', '10616-ORDEQ'))                              # misrepresent the conditions likely to occur in VolMon size streams.

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
   
### Calculate percentiles for a majority of data and leachate/influent data separately
Percentiles <- Converted_Data %>%
  mutate(
    Char_Speciation = case_when(
      is.na(Char_Speciation) ~ "None",
      Char_Speciation == "None" ~ "None",
      TRUE ~ as.character(Char_Speciation)),
    Sample_Fraction = case_when(
      str_detect(Char_Name, 'Dissolved') & is.na(Sample_Fraction) ~ "Dissolved",
      is.na(Sample_Fraction) ~ "None",
      TRUE ~ as.character(Sample_Fraction)),
    Statistical_Base = as.character(Statistical_Base)) %>% # ensures that these three columns are characters
  group_by(SampleMedia, Char_Name, Char_Speciation, Sample_Fraction, 
           Statistical_Base, Preferred_Unit) %>% # groups data by these six columns
  summarise(
    amb01L = quantile(Conv_Result, probs = 0.01, na.rm = TRUE),
    amb05L = quantile(Conv_Result, probs = 0.05, na.rm = TRUE),
    amb95U = quantile(Conv_Result, probs = 0.95, na.rm = TRUE),
    amb99U = quantile(Conv_Result, probs = 0.99, na.rm = TRUE)) # calculates the 1st, 5th, 95th and 99th percentiles
 
# ### Combine Data and write the file to Excel
# Percentiles <- bind_rows(MajorityPerc, LeachPerc)

write.xlsx(Percentiles, str_glue("VolMon_AnomPercentiles_{End_Date}.xlsx"))
