library(data.table)
library(tidyverse)

loc <- "data/working/combine_by_department/NASA"
nasa_contract <- fread(file.path(loc, "nasa_usa-spending_contracts_2016_resflag.csv"))

nasa_contract_summary <- data.frame(agency = "NASA", type = "contract", transaction_count = NA, res_transaction_count = NA, award_count = NA, 
                                   res_award_count = NA, obligation_total = NA, res_obligation_total = NA)

nasa_contract_summary[1,"transaction_count"] <- nrow(nasa_contract)
nasa_contract_summary[1,"res_transaction_count"] <- nrow(filter(nasa_contract, resflag == TRUE))
nasa_contract_summary[1,"award_count"] <- length(unique(nasa_contract$award_id_piid))
nasa_contract_summary[1,"res_award_count"] <- length(unique(filter(nasa_contract, resflag == TRUE)$award_id_piid))
nasa_contract_summary[1,"obligation_total"] <- sum(nasa_contract$federal_action_obligation)
nasa_contract_summary[1,"res_obligation_total"] <- sum(filter(nasa_contract, resflag == TRUE)$federal_action_obligation)

nasa_grant <- fread(file.path(loc, "nasa_usa-spending_grants_2016_matchflag.csv"))
#only run this if there are V1 column(s):
nasa_grant <- nasa_grant[,3:76]
nasa_grant_summary <- data.frame(agency = "NASA", type = "grant", transaction_count = NA, res_transaction_count = NA, award_count = NA, 
                                res_award_count = NA, obligation_total = NA, res_obligation_total = NA)

nasa_grant_summary[1,"transaction_count"] <- nrow(nasa_grant)
nasa_grant_summary[1,"res_transaction_count"] <- nrow(filter(nasa_grant, resflag == TRUE))
nasa_grant_summary[1,"award_count"] <- length(unique(nasa_grant$award_id_fain))
nasa_grant_summary[1,"res_award_count"] <- length(unique(filter(nasa_grant, resflag == TRUE)$award_id_fain))
nasa_grant_summary[1,"obligation_total"] <- sum(nasa_grant$federal_action_obligation)
nasa_grant_summary[1,"res_obligation_total"] <- sum(filter(nasa_grant, resflag == TRUE)$federal_action_obligation)

nasa_summary <- rbind(nasa_contract_summary, nasa_grant_summary)

write.csv(nasa_summary, file.path(loc, "nasa_summary.csv"))
