### This script captures the edits determined during the Shiny review of a   ###
### VolMon continuous dataset. This script should be saved locally with the  ###
### working copy of the dataset, so that any additional changes can easily   ###
### be added and read into the main script. Make sure to leave notes that    ###
### can easily be understood by another data steward who might pick this up  ###
### at a later date.                                                         ###

# Version 1.0 - 08/20/2026

## Load package for making edits
library(odeqcdr)

## Bring the data from the main script into the function and make edits
apply_logger_review <- function(df4.results, df3.audits.dql){

## Result Data Review
result_updates <- tibble::tribble(
  ~rows, ~DQL, ~comment,

## Replace these notes and row calls with your information, but keep the format
#List of loggers
  c(1:3474,15503:18979,22621:26095,29740:33217), "B", "No post or pre audit, downgraded to B data"
)

df5.results <- df4.results

for (i in seq_len(nrow(result_updates))) {
  df5.results <- odeqcdr::dql_update(
    df5.results,
    rows    = result_updates$rows[[i]],
    DQL     = result_updates$DQL[i],
    comment = result_updates$comment[i]
  )
}

## Audit Data Review
audit_updates <- tibble::tribble(
  ~rows, ~DQL, ~comment,
  
#All loggers
  c(4,19,22,52,58,64,67,73), "A", "Original audit associated with result collected before/after logger deployed/retrieved"
)

df4.audits.dql <- df3.audits.dql

for (i in seq_len(nrow(audit_updates))) {
  
  df4.audits.dql <- odeqcdr::dql_update(
    df4.audits.dql,
    rows    = audit_updates$rows[[i]],
    DQL     = audit_updates$DQL[i],
    comment = audit_updates$comment[i]
  )
}

## Return both updated data frames
list(df5.results = df5.results,
     df4.audits.dql = df4.audits.dql)
}

