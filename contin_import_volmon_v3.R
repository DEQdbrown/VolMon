contin_import_volmon_v3 <- function (file, sheets = c("Organization_Details", "Projects", 
                           "Monitoring_Locations", "Deployment", #"QC_Equipment", 
                           "Results", "PrePost", "Audit_Data")) 
{
     options(scipen = 999)
     sheet_check <- sheets %in% c("Organization_Details", "Projects", 
                                  "Monitoring_Locations", "Deployment", #"QC_Equipment", 
                                  "Results", "PrePost", "Audit_Data")
     if (any(!sheet_check)) {
          stop(paste0("The following are not acceptable input values for variable 'sheets': ", 
                      sheets[!sheet_check]))
     }
     org_import <- NA
     projects_import <- NA
     locations_import <- NA
     deployment_import <- NA
     equipment_import <- NA
     results_import <- NA
     prepost_import <- NA
     audit_import <- NA
     if ("Organization_Details" %in% sheets) {
          org_import <- read_excel(file,sheet = "Organization Details", range = "B6:C19",
                                   col_names = FALSE) |>
            setNames(c("key","value")) |>
            mutate(across(everything(), as.character))
     }
     if ("Projects" %in% sheets) {
          projects_import <- read_excel(file,sheet = "Projects") |>
            rename_with(~ str_remove_all(.x, "[\\^\\*]")) |> #These lines remove the ^ and * from the new template's column headers, so the script will run correctly
            mutate(across(everything(), as.character))
     }
     if ("Monitoring_Locations" %in% sheets) {
          locations_import <- read_excel(file,sheet = "Monitoring_Locations") |>
            rename_with(~ str_remove_all(.x, "[\\^\\*?]")|> str_trim()) |>
            mutate(`Source Map Scale` = NA, Reachcode = NA, Measure = NA, LLID = NA, 
                   `River Mile` = NA, `Permanent Identifier` = NA) |>
            mutate(`Date Established` = as.Date(`Date Established`, format = "%Y-%m-%d"),
                   across(c(Latitude, Longitude,`Source Map Scale`, Reachcode, 
                            Measure, LLID, `River Mile`), as.numeric),
                   across(-c(Latitude, Longitude,`Date Established`, `Source Map Scale`, 
                             Reachcode, Measure, LLID, `River Mile`), as.character))
     }
     if ("Deployment" %in% sheets) {
          deployment_import <- read_excel(file,sheet = "Deployment") |>
            rename_with(~ str_remove_all(.x, "[\\^\\*#]") |> str_trim()) |>
                        mutate(`Sample Depth` = as.numeric(`Sample Depth`),
                   across(c(`Deployment Start Date`,`Deployment End Date`), \(x) as.Date (x, format = "%Y-%m-%d")),
                   across(c(`Deployment Start Time`,`Deployment End Time`), hms::as_hms),
                   `Equipment ID` = as.numeric(`Equipment ID`),
                   across(-c(`Sample Depth`, `Equipment ID`, `Deployment Start Date`,`Deployment End Date`,
                             `Deployment Start Time`,`Deployment End Time`), as.character))
     }
     # if ("QC_Equipment" %in% sheets) {
     #      equipment_import <- read_excel(file, sheet = "QC_Equipment") |>
     #        rename_with(~ str_remove_all(.x, "[\\^\\*?]")|> str_trim()) |>
     #        mutate(across(everything(), as.character))
     # }
     if ("Results" %in% sheets) {
          results_import <- read_excel(file,sheet = "Results") |>
            rename_with(~ str_remove_all(.x, "[\\^\\*#]") |> str_trim()) |>
            mutate(`Activity Start Date` = as.Date(`Activity Start Date`, format = "%Y-%m-%d"),
                   `Activity Start Time` = hms::as_hms(`Activity Start Time`),
                   across(c(`Equipment ID`, `Result Value`), as.numeric),
                   across(-c(`Activity Start Date`, `Activity Start Time`, `Equipment ID`, 
                             `Result Value`), as.character))
     }
     if ("PrePost" %in% sheets) {
          prepost_import <- read_excel(file,sheet = "PrePost") |>
            rename_with(~ str_remove_all(.x, "[\\^\\*#]") |> str_trim()) |>
            mutate(`Activity Date` = as.Date(`Activity Date`, format = "%Y-%m-%d"),
                   `Activity Time` = hms::as_hms(`Activity Time`),
                   across(c(`Equipment ID`, `Equipment Result Value`, `Reference Result Value`, 
                            `Reference ID`), as.numeric),
                   across(c(`Characteristic Name`, `Equipment Result Unit`, 
                            `Reference Result Unit`), as.character))
     }
     if ("Audit_Data" %in% sheets) {
          audit_import <- read_excel(file,sheet = "Audit_Data") |>
            rename_with(~ str_remove_all(.x, "[\\^\\*#]") |> str_trim()) |>
            mutate(across(c(`Activity Start Date`, `Activity End Date`), \(x) as.Date (x, format = "%Y-%m-%d")),
                   across(c(`Activity Start Time`, `Activity End Time`), hms::as_hms),
                   across(c(`Equipment ID`, `Result Value`), as.numeric),
                   across(-c(`Activity Start Date`, `Activity End Date`,`Activity Start Time`,
                             `Activity End Time`, `Equipment ID`, `Result Value`), as.character))
     }
     template_sheets <- list(Organization_Details = as.data.frame(org_import), 
                             Projects = as.data.frame(projects_import), 
                             Monitoring_Locations = as.data.frame(locations_import), 
                             Deployment = as.data.frame(deployment_import), 
                             #QC_Equipment = as.data.frame(equipment_import),
                             Results = as.data.frame(results_import), 
                             PrePost = as.data.frame(prepost_import), 
                             Audit_Data = as.data.frame(audit_import))
     
     template_sheets <- lapply(template_sheets, function(x) {
       names(x) <- make.names(names(x))
       x
     })
     
     return(template_sheets)
}


