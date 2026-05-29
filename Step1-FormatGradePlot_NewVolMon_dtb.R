
#If AWQMS Package needs to be updated 
#install.packages("devtools")
#devtools::install_github("TravisPritchardODEQ/AWQMSdata",dependencies = TRUE, force = TRUE, upgrade = FALSE)



### This script takes the volmon grab data template that has been updated to match the example data set. 
library(tidyverse)
library(lubridate)
library(RODBC) 
library(odbc) 
library(readxl)
library(data.table)
library(fuzzyjoin)
library(ggplot2)

# load functions 
source("https://raw.githubusercontent.com/DEQdbrown/VolMon/refs/heads/main/Format_Template_NewVolMon_dtb.R")
source("https://raw.githubusercontent.com/DEQdbrown/VolMon/refs/heads/main/Auto_Grade_NewVolMon_dtb.R")
###source("Plots.R") ## Removed for now, we don't typically need plots. KM 10.2023
source("https://raw.githubusercontent.com/DEQdbrown/VolMon/refs/heads/main/Database_tables_NewVolMon_dtb.R")

# set file path for vol data working copy template and enter submission ID

###### Set Working Directory, working path,  and Submission ID ###########
setwd("//deqlab1/Vol_Data/Powder/2024/grab/R") ##### change this each time!!!!!!!!!!!!!!

data_path <- "//deqlab1/Vol_Data/Powder/2024/grab/WorkingCopy_WQM-Grab_2024_dtb.xlsx"

sub_id <- 305

# bring in the data tab of the working copy 
data <- read_excel(data_path,sheet = "Results") |>
  rename_with(~ str_remove_all(.x, "[\\^\\*]")) #These lines remove the ^ and * from the new template's column headers, so the script will run correctly

data <- data %>%
  set_names(make.names(colnames(data))) %>% 
  filter(!is.na(Monitoring.Location.ID)) %>%
  #mutate(row_ID = 1:n()) %>% #adds a row number
  mutate(Activity.Start.Time = strftime(Activity.Start.Time, format= "%H:%M:%S", tz = "UTC"), #had to add UTC to get the time correct - will this harm anything? 
                  DT = paste(Activity.Start.Date,Activity.Start.Time),
                  DateTime = ymd_hms(DT))

# Pulling in current Characteristic and Methods tables from the VolMon Database
VM2.sql <- odbcConnect("VolMon2")
chars <- sqlFetch(VM2.sql,"dbo.tlu_Characteristic")
methods <- sqlFetch(VM2.sql, "dbo.tlu_Method") |>
  filter(!is.na(Code))
odbcClose(VM2.sql)

# bring in the data tab of the working copy 
# project <- read_excel(data_path, sheet = "ProjectInfo", skip =5) %>%
#   rename(LOQ = 'Limit of Quantitation', Low_QC = 'Low Level QC limit') %>% 
#   filter(!is.na(CharID))

# Create the ProjectInfo tab using set information for field parameters and E. coli. Other parameters will be read in from Results tab
projectinfo <- data |>
  distinct(Characteristic.Name, .keep_all = TRUE) |> 
  mutate(Characteristic.Name = if_else(Characteristic.Name == "Dissolved Oxygen, Saturation", 
                                       "Dissolved oxygen saturation", Characteristic.Name)) |> # If other AWQMS names don't match the VolMon Database add them here. Updating the SQL table with negate the need for this.
  left_join(chars, by = c("Characteristic.Name" = 'Characteristic')) |>
  left_join(methods, by = c("Result.Analytical.Method.ID" = "Code")) |>
  rename(LOQ = 'Reporting.Limit.Value', FieldOrLab = 'Activity.Type', 
         "MethodShortName" = 'Result.Analytical.Method.ID', 'MethodSpeciation' = 'Method.Speciation',
         'Analytical.Organization' = 'Laboratory.Name') |>
  mutate(Low_QC = NA_real_,
         FieldOrLab = case_when(
           str_detect(FieldOrLab, 'Field') ~ "Field",
           TRUE ~ "Lab"),
         Result.Unit = case_when(
           is.na(Result.Unit) & CharIDText == 'ph'~ 'S.U.',
           TRUE ~ Result.Unit),
         LOQ = case_when(
           is.na(LOQ) & CharIDText == 'ec'  & FieldOrLab == 'Lab'   ~ 0.01,
           is.na(LOQ) & CharIDText == 'do'  & FieldOrLab == 'Field' ~ 0.01,
           is.na(LOQ) & CharIDText == 'dos' & FieldOrLab == 'Field' ~ 5,
           is.na(LOQ) & CharIDText == 'ph'  & FieldOrLab == 'Field' ~ 0.1,
           is.na(LOQ) & CharIDText == 't'   & FieldOrLab == 'Field' ~ -5,
           is.na(LOQ) & CharIDText == 'tb'  & FieldOrLab == 'Field' ~ 1,
           is.na(LOQ) & CharIDText == 'sc'  & FieldOrLab == 'Field' ~ 0,
           TRUE ~ LOQ),
         Low_QC = case_when(
           is.na(Low_QC) & CharIDText == 'dos' & FieldOrLab == 'Field' ~ 10,
           is.na(Low_QC) & CharIDText == 'tb'  & FieldOrLab == 'Field' ~ 20,
           TRUE ~ Low_QC),
         MethodShortName = case_when(
           is.na(MethodShortName) & CharIDText == 'ec'  & FieldOrLab == 'Lab'   ~ 'C24',
           is.na(MethodShortName) & CharIDText == 'do'  & FieldOrLab == 'Field' ~ 'NFM6.2.1-LUM',
           is.na(MethodShortName) & CharIDText == 'dos' & FieldOrLab == 'Field' ~ 'NFM6.2.1-LUM',
           is.na(MethodShortName) & CharIDText == 'ph'  & FieldOrLab == 'Field' ~ '150.1',
           is.na(MethodShortName) & CharIDText == 't'   & FieldOrLab == 'Field' ~ '170.1',
           is.na(MethodShortName) & CharIDText == 'tb'  & FieldOrLab == 'Field' ~ '180.1',
           is.na(MethodShortName) & CharIDText == 'sc'  & FieldOrLab == 'Field' ~ '120.1',
           TRUE ~ MethodShortName)
         ) |>
  select(CharID, CharIDText, Characteristic.Name, MethodShortName, Result.Unit, 
         MethodSpeciation, FieldOrLab, Analytical.Organization, LOQ, Low_QC)
  
#Function to format the data - must confirm that the QC checks are zero - 
format_data(data)
# function to add auto grade and anomalies 
AutoGrade_Anom(res)
# Generate data summaries and plots. # KM 10.2023- removed for now. We don't typically use plots ## 
###Plots_QCSum(final_DQL)
# creates database tables 
DB_tables(final_DQL)

# saves the project for LAM to upload to VolMon2
save.image(file = str_glue("Step1_{sub_id}.RData")) # DTB - added the sub_id to file name 10.2024
