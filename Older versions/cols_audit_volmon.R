cols_audit_volmon<- function () 
{
     audit_col_names <- c("Monitoring Location ID", 
                          "Activity Start Date", "Activity Start Time", "Activity End Date", 
                          "Activity End Time", "Activity Start End Time Zone", 
                          "Activity Type", "Activity ID", "Equipment ID", "Sample Collection Method", 
                          "Characteristic Name", "Result Value", "Result Unit", 
                          "Result Analytical Method ID", "Result Analytical Method Context", 
                          "Result Value Type", "Result Status ID", "Result Measure Qualifier", 
                          "Result Comment", "precDQL","rDQL")
     return(audit_col_names)
}
