### Based off of continuous export version 3 ###

contin_export_volmon_v3 <- function (file, org, projects, mloc, deployment, results, prepost, 
                                     audits, sumstats = NULL, equipment = NULL) 
{
# This if statement populates the new equipment tab, if we start wanting the QC_equipment tab this might need an update   
  if (is.null(equipment)) { 
          equipment <- deployment %>% dplyr::left_join(projects) %>%
            dplyr::mutate(Equipment.Type = "Probe/Sensor",
                          Equipment.Name = Equipment.ID, 
                          Model.Number = as.character(NA), 
                          Serial.Number = as.character(NA), 
                          Comments = as.character(NA), 
                          Quality.Assurance.Plan = Approved.QAPP.Indicator, 
                          Continuous.Monitoring = "Yes") |>
            select(c(Equipment.Type, Equipment.ID, Equipment.Name, Model.Number,
                     Serial.Number, Comments, Quality.Assurance.Plan, Continuous.Monitoring))
     }
     
# These lines swap the . between column header words for a space
     names(projects) <- gsub("\\.", " ", names(projects))
     names(mloc) <- gsub("\\.", " ", names(mloc))
     names(deployment) <- gsub("\\.", " ", names(deployment))
     names(equipment) <- gsub("\\.", " ", names(equipment))
     names(results) <- gsub("\\.", " ", names(results))
     names(prepost) <- gsub("\\.", " ", names(prepost))
     names(audits) <- gsub("\\.", " ", names(audits))

# This section looks for sumstats and creates a workbook without the tab if it's missing. If not then it creates the tab
     if (is.null(sumstats)) {
          xlsx_list <- list(org, projects, mloc, deployment, equipment, 
                            results, prepost, audits)
          names(xlsx_list) <- c("Organization Details", "Projects", 
                                "Monitoring_Locations", "Deployment", "New_Equipment", 
                                "Results", "PrePost", "Audit_Data")
          openxlsx::write.xlsx(xlsx_list, 
                               file = file, 
                               colWidths = "auto", 
                               firstRow = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), 
                               rowNames = rep(FALSE, length(xlsx_list)),
                               borders = "rows",
                               headerStyle = openxlsx::createStyle(fgFill = "#000000", 
                                                                   halign = "LEFT", 
                                                                   textDecoration = "Bold", 
                                                                   wrapText = TRUE, 
                                                                   border = "Bottom", 
                                                                   fontColour = "white", 
                                                                   fontName = "Arial", 
                                                                   fontSize = 10))
     }
     else {
          xlsx_list <- list(org, projects, mloc, deployment, equipment, 
                            results, prepost, audits, sumstats)
          names(xlsx_list) <- c("Organization Details", "Projects", 
                                "Monitoring_Locations", "Deployment", "New_Equipment", 
                                "Results", "PrePost", "Audit_Data", "AWQMS_Sum_Stats")
          openxlsx::write.xlsx(xlsx_list, 
                               file = file, 
                               colWidths = "auto", 
                               firstRow = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), 
                               rowNames = rep(FALSE, length(xlsx_list)), 
                               borders = "rows",
                               headerStyle = openxlsx::createStyle(fgFill = "#000000", 
                                                                   halign = "LEFT", 
                                                                   textDecoration = "Bold", 
                                                                   wrapText = TRUE, 
                                                                   border = "Bottom", 
                                                                   fontColour = "white", 
                                                                   fontName = "Arial", 
                                                                   fontSize = 10))
     }
 
# These lines actually create the workbook and format the column headers for import to AWQMS    
     wb <- openxlsx::loadWorkbook(file = file, isUnzipped = FALSE)
     openxlsx::modifyBaseFont(wb, fontSize = 10, fontName = "Arial")
     for (sheet_name in names(xlsx_list[2:length(xlsx_list)])) {
          openxlsx::setColWidths(wb, 
                                 sheet = sheet_name, 
                                 cols = c(1:ncol(xlsx_list[[sheet_name]])), 
                                 widths = "auto")
          openxlsx::addStyle(wb, 
                             sheet = sheet_name, 
                             rows = c(1:nrow(xlsx_list[[sheet_name]]) + 1), 
                             cols = c(1:ncol(xlsx_list[[sheet_name]])), 
                             stack = TRUE, 
                             gridExpand = TRUE, 
                             style = openxlsx::createStyle(valign = "top",
                                                           fontName = "Arial", 
                                                           fontSize = 10))
     }

     openxlsx::setColWidths(wb, 
                            sheet = "Organization Details", 
                            cols = c(2:3), 
                            widths = c(20, 100))
     openxlsx::addStyle(wb, 
                        sheet = "Organization Details", 
                        rows = c(6:19), 
                        cols = c(2:3), 
                        stack = TRUE, 
                        gridExpand = TRUE, 
                        style = openxlsx::createStyle(wrapText = TRUE, 
                                                      valign = "top", 
                                                      fontName = "Arial", 
                                                      fontSize = 9))
     openxlsx::addStyle(wb, 
                        sheet = "Organization Details", 
                        rows = c(6:19), 
                        cols = 2, 
                        stack = TRUE, 
                        style = openxlsx::createStyle(textDecoration = "bold", 
                                                     fontName = "Arial", 
                                                     fontSize = 9))
     openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
}

