update_deploy_volmon <- function (deploy, from = 2, to = 3) 
{
     if (from == 2 & to == 3) {
          deploy.to <- deploy %>% dplyr::mutate(Deployment.Start.Date.Time = Deployment.Start.Date, 
                                                Deployment.End.Date.Time = Deployment.End.Date, 
                                                Deployment.Start.End.Time.Zone = format(Deployment.Start.Date, 
                                                                                        format = "%Z"), Media = Sample.Media, Media.Subdivision = Sample.Sub.Media, 
                                                Project.ID = as.character(NA), Alternate.Project.ID.1 = as.character(NA), 
                                                Alternate.Project.ID.2 = as.character(NA), Frequency.in.min = as.numeric(NA), 
                                                Depth.in.m = dplyr::case_when( is.na(Sample.Depth.Unit) ~ NA_real_,
                                                                               Sample.Depth.Unit == "ft" ~ as.numeric(Sample.Depth) * 0.3048,
                                                                               Sample.Depth.Unit == "in" ~ as.numeric(Sample.Depth) * 0.0254, 
                                                                               Sample.Depth.Unit == "dm" ~ as.numeric(Sample.Depth) * 0.01, 
                                                                               Sample.Depth.Unit ==  "cm" ~ as.numeric(Sample.Depth) * 0.1, 
                                                                               Sample.Depth.Unit == "m" ~ as.numeric(Sample.Depth))) %>% 
               dplyr::select(dplyr::any_of(make.names(odeqcdr::cols_deploy(ver = to))))
          warning("Project.ID set to NA. Please update.")
     }
     if (from == 3 & to == 2) {
          deploy.to <- deploy %>% dplyr::mutate(Deployment.Start.Date = Deployment.Start.Date.Time, 
                                                Deployment.End.Date = Deployment.End.Date.Time, 
                                                Characteristic.Name = as.character(NA), Sample.Media = Media, 
                                                Sample.Sub.Media = Media.Subdivision, Sample.Depth = Depth.in.m, 
                                                Sample.Depth.Unit = dplyr::if_else(is.na(Depth.in.m), 
                                                                                   as.character(NA), "m")) %>% dplyr::select(dplyr::any_of(make.names(odeqcdr::cols_deploy(ver = to))))
          warning("Characteristic.Name set to NA. Please update.")
     }
     return(deploy.to)
}
