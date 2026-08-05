contin_import_volmon_v2 <- function (file, sheets = c("Organization_Details", "Projects", 
                           "Monitoring_Locations", "Deployment", "New_Equipment", "Results", 
                           "PrePost", "Audit_Data")) 
{
     options(scipen = 999)
     sheet_check <- sheets %in% c("Organization_Details", "Projects", 
                                  "Monitoring_Locations", "Deployment", "New_Equipment", 
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
          org_import <- readxl::read_excel(file, sheet = "Organization Details", 
                                           range = "B6:C19", col_types = c("text", "text"), 
                                           col_names = FALSE)
          colnames(org_import) <- c("key", "value")
     }
     if ("Projects" %in% sheets) {
          projects_col_types <- c("text", "text", "text", "text", 
                                  "text", "text")
          projects_col_names <- make.names(cols_projects_volmon())
          projects_import <- readxl::read_excel(file, sheet = "Projects", 
                                                col_types = projects_col_types)
          colnames(projects_import) <- projects_col_names
          projects_import <- projects_import[rowSums(is.na(projects_import)) != 
                                                  ncol(projects_import), ]
     }
     if ("Monitoring_Locations" %in% sheets) {
          locations_col_types <- c("text", "text", "text", "numeric", 
                                   "numeric", "text", "text", "text", "text", "text", 
                                   "text", "text", "text", "text", "date", "text", 
                                   "text", "text", "text", "text", "text","text","text","text","text","text","text","text")
          locations_col_names <- make.names(mloc_col_names_volmon())
          locations_import <- readxl::read_excel(file, sheet = "Monitoring_Locations", 
                                                 range = cellranger::cell_cols(1:28), col_types = locations_col_types)
          colnames(locations_import) <- locations_col_names
          locations_import <- locations_import[rowSums(is.na(locations_import)) != 
                                                    ncol(locations_import), ]
     }
     if ("Deployment" %in% sheets) {
          deployment_col_types <- c("text", "text", "text", "date", "date", "date","date",
                                    "text", "text", "text", "text")
          deployment_col_names <- make.names(cols_deploy_volmon())
          deployment_import <- readxl::read_excel(file, sheet = "Deployment", 
                                                  col_types = deployment_col_types)
          colnames(deployment_import) <- deployment_col_names
          deployment_import <- deployment_import[rowSums(is.na(deployment_import)) != 
                                                      ncol(deployment_import), ]
     }
     if ("Results" %in% sheets) {
          results_col_types <- c("text", "date", "date", "text", 
                                 "text", "text", "numeric", "text", "text")
          results_col_names <- make.names(odeqcdr::cols_results())
          results_import <- readxl::read_excel(file, sheet = "Results", 
                                               range = cellranger::cell_cols(1:9), col_types = results_col_types)
          colnames(results_import) <- results_col_names
          results_import <- results_import[rowSums(is.na(results_import)) != 
                                                ncol(results_import), ]
     }
     if ("PrePost" %in% sheets) {
          prepost_col_types <- c("text", "text", "date", "date", 
                                 "numeric", "text", "numeric", "text","text")
          prepost_col_names <- make.names(cols_prepost_volmon())
          prepost_import <- readxl::read_excel(file, sheet = "PrePost", 
                                               col_types = prepost_col_types)
          colnames(prepost_import) <- prepost_col_names
          prepost_import <- prepost_import[rowSums(is.na(prepost_import)) != 
                                                ncol(prepost_import), ]
     }
     if ("Audit_Data" %in% sheets) {
          audit_col_types <- c("text", "date", "date","date","date", "text", "text", "text", 
                               "text", "text", "text", "numeric", "text", "text", "text", "text", 
                               "text", "text", "text")
          audit_col_names <- make.names(cols_audit_volmon())
          audit_import <- readxl::read_excel(file, sheet = "Audit_Data", 
                                             range = cellranger::cell_cols(1:19), col_types = audit_col_types)
          colnames(audit_import) <- audit_col_names
          audit_import <- audit_import[rowSums(is.na(audit_import)) != 
                                            ncol(audit_import), ]
     }
     template_sheets <- list(Organization_Details = as.data.frame(org_import), 
                             Projects = as.data.frame(projects_import), Monitoring_Locations = as.data.frame(locations_import), 
                             Deployment = as.data.frame(deployment_import), Results = as.data.frame(results_import), 
                             PrePost = as.data.frame(prepost_import), Audit_Data = as.data.frame(audit_import))
     return(template_sheets)
}


