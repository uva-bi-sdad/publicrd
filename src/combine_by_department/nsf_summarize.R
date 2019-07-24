library(data.table)
library(tidyverse)

loc <- "data/working/combine_by_department/NSF"
nsf_contract <- fread(file.path(loc, "nsf_usa-spending_contracts_2016_resflag.csv"))

nsf_contract_summary <- data.frame(agency = "NSF", type = "contract", transaction_count = NA, res_transaction_count = NA, award_count = NA, 
                                   res_award_count = NA, obligation_total = NA, res_obligation_total = NA)

nsf_contract_summary[1,"transaction_count"] <- nrow(nsf_contract)
nsf_contract_summary[1,"res_transaction_count"] <- nrow(filter(nsf_contract, resflag == TRUE))
nsf_contract_summary[1,"award_count"] <- length(unique(nsf_contract$award_id_piid))
nsf_contract_summary[1,"res_award_count"] <- length(unique(filter(nsf_contract, resflag == TRUE)$award_id_piid))
nsf_contract_summary[1,"obligation_total"] <- sum(nsf_contract$federal_action_obligation)
nsf_contract_summary[1,"res_obligation_total"] <- sum(filter(nsf_contract, resflag == TRUE)$federal_action_obligation)

nsf_grant <- fread(file.path(loc, "nsf_usa-spending_grants_2016_matchflag.csv"))
#only run this if there are V1 column(s):
nsf_grant <- nsf_grant[,3:76]
nsf_grant_summary <- data.frame(agency = "NSF", type = "grant", transaction_count = NA, res_transaction_count = NA, award_count = NA, 
                                res_award_count = NA, obligation_total = NA, res_obligation_total = NA)

nsf_grant_summary[1,"transaction_count"] <- nrow(nsf_grant)
nsf_grant_summary[1,"res_transaction_count"] <- nrow(filter(nsf_grant, resflag == TRUE))
nsf_grant_summary[1,"award_count"] <- length(unique(nsf_grant$award_id_fain))
nsf_grant_summary[1,"res_award_count"] <- length(unique(filter(nsf_grant, resflag == TRUE)$award_id_fain))
nsf_grant_summary[1,"obligation_total"] <- sum(nsf_grant$federal_action_obligation)
nsf_grant_summary[1,"res_obligation_total"] <- sum(filter(nsf_grant, resflag == TRUE)$federal_action_obligation)

nsf_summary <- rbind(nsf_contract_summary, nsf_grant_summary)

write.csv(nsf_summary, file.path(loc, "nsf_summary.csv"))
