library(data.table)
library(tidyverse)

loc <- "data/working/combine_by_department/NIH"
nih_contract <- fread(file.path(loc, "nih_usa-spending_contracts_2016_resflag.csv"))

nih_contract_summary <- data.frame(agency = "NIH", type = "contract", transaction_count = NA, res_transaction_count = NA, award_count = NA, 
                                   res_award_count = NA, obligation_total = NA, res_obligation_total = NA)

nih_contract_summary[1,"transaction_count"] <- nrow(nih_contract)
nih_contract_summary[1,"res_transaction_count"] <- nrow(filter(nih_contract, resflag == TRUE))
nih_contract_summary[1,"award_count"] <- length(unique(nih_contract$award_id_piid))
nih_contract_summary[1,"res_award_count"] <- length(unique(filter(nih_contract, resflag == TRUE)$award_id_piid))
nih_contract_summary[1,"obligation_total"] <- sum(nih_contract$federal_action_obligation)
nih_contract_summary[1,"res_obligation_total"] <- sum(filter(nih_contract, resflag == TRUE)$federal_action_obligation)

nih_grant <- fread(file.path(loc, "nih_usa-spending_grants_2016_matchflag.csv"))
#only run this if there are V1 column(s):
#nih_grant <- nih_grant[,3:78]
nih_grant_summary <- data.frame(agency = "NIH", type = "grant", transaction_count = NA, res_transaction_count = NA, award_count = NA, 
                                   res_award_count = NA, obligation_total = NA, res_obligation_total = NA)

nih_grant_summary[1,"transaction_count"] <- nrow(nih_grant)
nih_grant_summary[1,"res_transaction_count"] <- nrow(filter(nih_grant, resflag == TRUE))
nih_grant_summary[1,"award_count"] <- length(unique(nih_grant$award_id_fain))
nih_grant_summary[1,"res_award_count"] <- length(unique(filter(nih_grant, resflag == TRUE)$award_id_fain))
nih_grant_summary[1,"obligation_total"] <- sum(nih_grant$federal_action_obligation)
nih_grant_summary[1,"res_obligation_total"] <- sum(filter(nih_grant, resflag == TRUE)$federal_action_obligation)

nih_summary <- rbind(nih_contract_summary, nih_grant_summary)
