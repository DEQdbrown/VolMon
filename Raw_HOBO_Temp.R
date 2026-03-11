###############################################################################
# This script processes HOBO logger data and writes to a VolMon template      #
# for submission to AWQMS. Adjust paths below for different years. Small      #
# adjustments will need to be made if reading in .csv files instead of .xlsx  #
# files. Check the comments for where to make the changes (lines 38, 44).     #
###############################################################################

## Load packages
library(openxlsx)
library(readxl)
library(tidyverse)
library(lutz)
library(hms)

###############################################################################
# File paths and folder setup                                                 #
###############################################################################

# Path to the template workbook
wb_path <- "//deqlab1/Vol_Data/North Fork John Day/2023_MF_submitted_May2024/TestTestCopy_Cont_Vol_MFIMW_2023_NFJDWC_Final.xlsx"
wb <- loadWorkbook(wb_path, isUnzipped = FALSE)

# Folder with logger Excel files
folder <- "//deqlab1/Vol_Data/North Fork John Day/2023_MF_submitted_May2024/CSV to Template/CSV/CSV"

# Desired output folder for _w_results version
output_folder <- "//deqlab1/Vol_Data/North Fork John Day/2023_MF_submitted_May2024/CSV to Template/CSV"
org <- "MIDJOHNDAY_WC"

###############################################################################
# Read and process logger files                                               #
# You may need to comment/uncomment out lines 39/40 and 46/47 for xlsx/csv    #
###############################################################################

# Change working directory to the folder of csv files
setwd(folder)

# Use this section of code to load an entire folder of files
#raw_files <- list.files(path = folder, pattern = "\\.xlsx$", full.names = TRUE) # Use this line if using xlsx files
raw_files <- list.files(path = folder, pattern = "\\.csv$", full.names = TRUE) # Use this line if using csv files
skip_log <- c()

# This 
stop("Answer the prompt after running the next two lines of code, then run the remaining code.")
header_row <- as.integer(
  readline(prompt = "Which row contains the column names? Enter 1 or 2: "))

skip_rows <- header_row - 1

raw_data <- map_dfr(raw_files, function(file) {
  message("Reading: ", basename(file))
  tryCatch({
    #data <- suppressWarnings(read_excel(file, skip = skip_rows)) # Use this line if using xlsx files
    data <- suppressWarnings(read_csv(file, skip = skip_rows, locale = locale(encoding = "Windows-1252"))) # Use this line if using csv files
    filename <- tools::file_path_sans_ext(basename(file))
    data <- data %>% mutate(equipID = filename)
    
    # Skip if no Date or Temp column
    has_date <- any(str_detect(names(data), "Date"))
    has_temp <- any(str_detect(names(data), "Temp"))
    
    if (!has_date | !has_temp) {
      message("⚠️ Skipping file (missing Date or Temp): ", basename(file))
      skip_log <<- c(skip_log, filename)
      return(NULL)
    }
    
    return(data)
  }, error = function(e) {
    message("❌ Error reading file ", basename(file), ": ", e$message)
    skip_log <<- c(skip_log, tools::file_path_sans_ext(basename(file)))
    return(NULL)
  })
})

if (length(skip_log) > 0) {
  write_lines(skip_log, file.path(output_folder, "skipped_serials.txt"))
  message("✔ Skipped serial numbers written to skipped_serials.txt")
}

###############################################################################
# Process deployment sheet                                                    #
###############################################################################

deploy <- readWorkbook(wb, sheet = "Deployment") %>%
  rename(equipID = "Equipment.ID.#", MLocID = "Monitoring.Location.ID",
         startdate = "Deployment.Start.Date", enddate = "Deployment.End.Date",
         starttime = "Deployment.Start.Time", endtime = "Deployment.End.Time") %>%
  mutate(equipID = as.numeric(equipID),
         startdate = as.Date(startdate, origin = "1899-12-30"),
         #starttime = trimws(as.character('Deployment.Start.Time')),
         #starttime = na_if(starttime, ""),
         starttime = as.numeric(starttime),
         starttime = as_hms(starttime * 86400),
         startdate = ymd_hms(paste(startdate, starttime)),
         enddate = as.Date(enddate, origin = "1899-12-30"),
         #endtime = trimws(as.character('Deployment.End.Time')),
         #endtime = na_if(endtime, ""),
         endtime = as.numeric(endtime),
         endtime = as_hms(endtime * 86400),
         enddate = ymd_hms(paste(enddate, endtime))) %>%
  select(-starttime, -endtime)

###############################################################################
# Format HOBO temperature logger data                                         #
###############################################################################

## Create lists of the temp and datetime columns based on the column names in your data
temp_columns <- names(raw_data)[grep("^Temp", names(raw_data))] 
datetime_columns <- names(raw_data)[grep("^Date", names(raw_data))] 

data <- raw_data %>%
  rename(DateTime1 = all_of(datetime_columns[1]),
         Temp1 = all_of(temp_columns[1])) %>%
  reduce(temp_columns[-1], .init = ., # Iterate through all the temp columns and combine them into Temp1 
         ~ mutate(.x, Temp1 = if_else(is.na(Temp1) & !is.na(.data[[.y]]), .data[[.y]], Temp1))) %>%
  reduce(datetime_columns[-1], .init = ., # Iterate through all the datetime columns and combine them into DateTime1
         ~ mutate(.x, DateTime1 = if_else(is.na(DateTime1) & !is.na(.data[[.y]]), .data[[.y]], DateTime1))) %>%
  select(DateTime1, Temp1, equipID) %>% # Drop everything besides what's listed in the select statement
  mutate(DateTime1 = str_trim(DateTime1), # trims off extra spaces from DateTime column and formats into datetime
         DateTime1 = mdy_hms(DateTime1), 
         DateTime1 = if_else( # this if_else adds a second to any readings taken at midnight
           format(DateTime1, "%H:%M:%S") == "00:00:00",
           DateTime1 + seconds(1),
           DateTime1),
         DateTime1 = ymd_hms(DateTime1),
         equipID = as.double(equipID)) %>%
  filter(!is.na(Temp1)) # removes the blank rows from end of each dataset

###############################################################################
# Join deployment info and format for results                                 #
###############################################################################

## Match deployments to results based on equipment and date
dat <- data %>%
  mutate(equipID = as.double(equipID)) %>% # format equipID as double
  group_by(equipID) %>%
  summarise(startdate = min(DateTime1, na.rm = TRUE), # identify the first and last reading for each deployment
            enddate = max(DateTime1, na.rm = TRUE)) %>%
  left_join(deploy, by = "equipID") %>% # connect results with deployment data
  #filter(startdate.x >= startdate.y, enddate.x <= enddate.y) %>% # filter out any rows where start and end dates from results don't line up with a deployment
  select(equipID, MLocID, Characteristic.Name)

## Create dataframe that matches the Results tab in the template
results <- data %>%
  left_join(dat, by = "equipID") %>%
  mutate("Activity Start Date" = as.Date(DateTime1), # add necessary columns to fill out the Results tab
         "Activity Start Time" = format(as.POSIXct(DateTime1), format = "%H:%M:%S"),
         is_dst = dst(DateTime1), # check for correct time zone based on date
         "Activity Start/End Time Zone" = if_else(is_dst == TRUE, "PST", "PDT"),
         "Result Unit" = "deg C",
         "Result Status ID" = "Validated") %>%
  select(-DateTime1, -is_dst) %>% # Drop columns not on the Results tab
  relocate(c(equipID, Characteristic.Name, Temp1), .before = "Result Unit") %>%
  rename("Monitoring Location ID" = MLocID, "Equipment ID  #" = equipID,
         "Characteristic Name" = Characteristic.Name, "Result Value" = Temp1)

###############################################################################
# Write to workbook and save output                                           #
###############################################################################

writeData(wb, "Results", results)
wb_basename <- tools::file_path_sans_ext(basename(wb_path))
output_file <- file.path(output_folder, paste0(wb_basename, "_w_results.xlsx"))
saveWorkbook(wb, file = output_file, overwrite = TRUE)
