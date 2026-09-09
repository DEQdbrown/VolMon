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
#40515, 41284, 41285, 40517
  c(1:3474,15503:18979,22621:26095,29740:33217), "B", "No post or pre audit, downgraded to B data",
  c(3475:3752,4936:6451,8215:14836, 18980:21956,26096:29074,33218:36194,40338:43312,44479:50484,51597:57600), "A", "A audit, A data",
  c(3753:4935), "C", "Logger appears to be out of water",
  c(6452:6700,7120:8214,14837:15502,21957:22620,29075:29739,36195:36858,43313:43977,43978:44478,50485:51094,51095:51596,57601:57639 ), "C", "Before or after logger deployed",
    
#40528,40529,40529,41923,41287,41290,41292,41293,41296,41360
  c(65165:68597,72284:75813,79401:82926,107873:111399,114990:118424), "B", "No post or pre audit, downgraded to B data",
  c(86520:92977,93639:100194,100755:107312,122613:124049,125548:128707), "B", "B audit, B data or no audit on one end",
  c(68598:71624,75814:78838,82927:85853,111400:114431,118425:121590), "A", "A audit, A data",
  c(124050:125547), "C", "Logger appears to be out of water",

#41297,42188,40526
  c(129224:132704,144064:147031), "B", "No post or pre audit, downgraded to B data",
  c(132705:135679,136846:139783,147032:149958), "A", "A audit, A data",
  c(150615:156506,157730:163623,164843:170735,171963:177857), "A", "A audit, A data",
  
  
  c(139784:142848), "B", "B Audit, B data",
  c(135680:136342,136343:136845,142849:143459,143460:144063,149959:150004,150005:150614,156507:157121), "C", "Before or after logger deployed",
  c(157122:157729,163624:164238,164239:164842,170736:171357,171358:171962,177858:178476 ), "C", "Before or after logger deployed"
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

