## L. Merrick 
## 4/24/2020
## Rewrite of scripts originally written by Steve Hanson

library(tidyverse)
library(lubridate)
library(RODBC) 
library(odbc)
library(readxl)
library(data.table)

### connect to VOLMON2 ###
format_data <- function(data){
### pulls tables from SQL VolMon2 db - this requires an ODBC connection to VOLMON2 on DEQLEAD-LIMS
VM2.sql <- odbcConnect("VolMon2") 
sub <- sqlFetch(VM2.sql,"dbo.t_Submission") 
chars <- sqlFetch(VM2.sql,"dbo.tlu_Characteristic")
type <- sqlFetch(VM2.sql,"dbo.tlu_Type")
odbcClose(VM2.sql)

### restructure the data template ####
#pull out comments first 
# {act_comments <- data %>% 
#    select(row_ID, Result.Comment) %>% # the activity comment was part of the old template that had a comment for the sample event
#    filter(!is.na(Result.Comment))
# 
# res_comments <- data %>% 
#   select(row_ID, Result.Comment) %>% 
#   filter(!is.na(Result.Comment)) %>%
#   mutate(CharIDText = Result.Comment)

# This column is sometimes emitted from the template - this will need to be added if missing
# res_qual <- data %>% 
#    select(row_ID,Result.Qualifier) %>%
#    filter(!is.na(Result.Qualifier)) %>%
#    mutate(CharIDText = Result.Qualifier)
# 
# res_ACC <- data %>% 
#   select(row_ID,Accuracy.DQL) %>%
#   filter(!is.na(Accuracy.DQL)) %>%
#   mutate(CharIDText = as.character(Accuracy.DQL))
# 
# res_PREC <- data %>% 
#   select(row_ID,Precision.DQL) %>%
#   filter(!is.na(Precision.DQL)) %>%
#   mutate(CharIDText = as.character(Precision.DQL))
# 
# res_DQL <- data %>% 
#   select(row_ID,Result.DQL) %>%
#   filter(!is.na(Result.DQL)) %>%
#   mutate(CharIDText = as.character(Result.DQL))

{res <- data %>%
       select(Monitoring.Location.ID,DateTime,Bottom.Depth.Height,Bottom.Depth.Height.Unit,Activity.Media.Name,
              Sample.Collection.Method,Result.Qualifier,Result.Value,Result.Value.Type,Characteristic.Name,Activity.Type,Result.Comment) %>%
       filter(!is.na(Result.Value)) %>%
       mutate(sample_type = ifelse(Activity.Type %in% c("Quality Control Sample-Field Replicate","Quality Control Field Replicate Measurement/Observation"),"dup","sample")) %>%
       #rename(FieldOrLab = 'Activity.Type') |>     
       mutate(Characteristic.Name = if_else(Characteristic.Name == "Dissolved Oxygen, Saturation",
                                            "Dissolved oxygen saturation", Characteristic.Name),
              samplers= NA_character_) |>
              # FieldOrLab = case_when(
              #   str_detect(FieldOrLab, 'Field') ~ "Field",
              #   TRUE ~ "Lab")) %>%
       #left_join(res_comments, by = c('row_ID','CharIDText')) %>% 
       #left_join(res_qual, by = c('row_ID','CharIDText')) %>% # Add depth 
       #left_join(res_ACC, by = c('row_ID','CharIDText')) %>%
       #left_join(res_PREC, by = c('row_ID','CharIDText')) %>%
       #left_join(res_DQL, by = c('row_ID','CharIDText')) %>%
       #left_join(act_comments, by = 'row_ID') %>%
       left_join(projectinfo, by = c("Characteristic.Name" = 'Characteristic.Name')) %>% # I had to update template
       mutate(subid = sub_id,
              act_type = case_when(sample_type == "dup" & FieldOrLab %like% 'Field'~ 158, 
                                   sample_type == "sample" & FieldOrLab %like% 'Field'~ 149,
                                   sample_type == "dup" & FieldOrLab %like% 'Lab'~ 313,
                                   sample_type == "sample" & FieldOrLab %like% 'Lab'~ 233)) %>%  #I looked over all values in this field in the VolMon DB. I think this should cover it. 
       left_join(type, by = c('act_type' = 'TypeID')) %>% 
       left_join(sub, by = c('subid' = 'SubID')) %>%
       rename("LASAR_ID" = "Monitoring.Location.ID") %>%
       mutate(Date4Id = strftime(DateTime, format = '%Y%m%d%H%M',tz = 'UTC'), #keeping UTC in these for now
              Date4group = strftime(DateTime, format = '%Y%m%d',tz = 'UTC'), 
              act_id = paste(subid,Date4Id,LASAR_ID,TypeIDText,sep = "-"), # fix date format
              act_group = case_when(DupBatchType == 449  ~ paste(subid,Date4group,samplers,sep = "-"), # Duplicates are done once a day by a sampling crew- multiple crews on one day
                                    DupBatchType == 450  ~ paste(subid,samplers,sep = "-"),# Duplicates done by a sampler at regular frequency, but not daily
                                    DupBatchType == 448 ~ paste(subid,Date4group,sep = "-"), # Duplicates batches are once a day without additional groupings
                                    TRUE ~ 'ERROR'), # Duplicates batches are once a day without additional groupings
              result_id = paste(act_id,CharIDText, sep = "-"),
              sub_char = paste(sub_id, CharIDText, sep = "-"),
              actgrp_char = paste(act_group, CharIDText, sep = "-"),
              act_group_stn = paste(act_group, LASAR_ID, sep = "-")) %>% 
   select(-TypeFilter,-LastChangeDate, -DEQuse, -MonitoringLocationRequired, -AnalyticalMethodRequired, -SubRevNarrative, -OriginalDate) #sjh# remove columns that shouldn't be needed


res$Result.Value <- as.numeric(as.character(res$Result.Value)) # Makes sure that results are numeric, not character. KM 7.10.2024

#only run this if needed - there are too many columns to make this useful
#write.csv(res, "res.csv") 

#### QC checks for char and duplicates#### 
### These will likely need to  be corrected in the WorkingFile 
### batching error 
bad_batch <- res %>%
             filter(act_group == 'ERROR')
             
# check for redundant results (same location, sample date/time, QC type, and results)
redundant_result <-res %>% 
              group_by(act_group_stn,LASAR_ID,DateTime,CharIDText,sample_type,Result.Value) %>%   
              mutate(dup_res = ifelse(n() > 1, 1, 0)) %>%
              ungroup() %>% 
              filter(dup_res == 1) 

# check for duplicate location, sample date/time, QC type, with different results 
redundant_act_char <- res %>% # can we call this RedundantActRt? # I wonder if these can be flagged with anomalies?
                  group_by(act_group_stn,LASAR_ID,DateTime,CharIDText,sample_type) %>% 
                  mutate(dup_res = ifelse(n() > 1, 1, 0)) %>%
                  ungroup() %>% 
                  filter(dup_res == 1) 

# verify charIDtext in characteristic table  - should be zero  
name_check <- res %>% 
              left_join(chars, by = 'CharIDText') %>% 
              filter(is.na('CharID')) }

.GlobalEnv$res <- res
.GlobalEnv$bad_batch <- bad_batch
.GlobalEnv$redundant_result <- redundant_result
.GlobalEnv$redundant_act_char <- redundant_act_char
.GlobalEnv$name_check <- name_check}

## STUFF STEVE ADDED # LAM I don't understand this 
# may want to have something to see if there are data fields missing from the project info if this becomes automated.
#charsfx <- c("_r", "_qual", "d_r", "d_qual", "_PREC", "_ACC", "_DQL", "_m", "_cmnt") #sjh # all the characteristic suffixes
#AllRsltFlds <- paste0(rep(project$CharID, each = length(charsfx)), charsfx) # Create list of result fields based on characteristic list from project info sheet
#NoPrjDatFlds <- names(data)[-which(names(data) %in% AllRsltFlds)] # identified all the field names from Excel data tab not expected from project info char list
#for (i in charsfx) { # My apologies for the for loop
#MissingProjectFields <- NoPrjDatFlds[endsWith(NoPrjDatFlds,i)]} # I think this should give a vector of result fields in data worksheet but not defined in project worksheet.


            
