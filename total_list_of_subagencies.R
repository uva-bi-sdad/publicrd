library(data.table)
library(tidyverse)

#How many subagencies exist within valid S&E?

loc <- "data/working/combined_usa-spending"

#combined grant files:
grant <- fread(file.path(loc, "usa-spending_grants_2016_matchflag.csv"))

#combined contract files:
contract <- fread(file.path(loc, "usa-spending_contracts_2016_resflag_new.csv"))

#benchmarks for higher ed:
bench_loc <- "data/original"
bench <- fread(file.path(bench_loc, "fy16_highered_obligation_benchmark.csv"))

#CFDA codes:
cfda <- fread("data/working/cfda_isSNE_initial.csv")

##For this round, we are ONLY comparing to higher ed data. However, the way recipient
##types are noted in the actual USA Spending data is super weird, but it's easy to down-
##load data that is only for higher ed. So here we are getting all the award IDs
##for higher ed contracts/grants in FY16.

he_loc <- "data/original"
g_highered <- fread(file.path(he_loc, "2016-data_multiple_usa-spending_transaction_grants-only_highered-only.csv"))
c_highered <- fread(file.path(he_loc, "2016-data_multiple_usa-spending_transaction_contracts-only_highered-only.csv"))
g_h_id <- unique(g_highered$award_id_fain)
c_h_id <- unique(c_highered$award_id_piid)

grant_h <- filter(grant, award_id_fain %chin% g_h_id)
contract_h <- filter(contract, award_id_piid %chin% c_h_id)

#putting in a column with the clean name we actually care about
rename <- function(x){if(x == "DEPARTMENT OF DEFENSE (DOD)"){
  "DOD"
} else if(x == "DEPARTMENT OF HEALTH AND HUMAN SERVICES (HHS)"){
  "NIH"
} else if(x == "DEPARTMENT OF AGRICULTURE (USDA)"){
  "USDA"
} else if(x == "NATIONAL AERONAUTICS AND SPACE ADMINISTRATION (NASA)"){
  "NASA"
} else if(x == "DEPARTMENT OF ENERGY (DOE)"){
  "DOE"
} else if(x == "NATIONAL SCIENCE FOUNDATION (NSF)"){
  "NSF"
} else{
  "error"
}
}

grant_h$agency <- sapply(grant_h$awarding_agency_name, rename)   
contract_h$agency <- sapply(contract_h$awarding_agency_name, rename)

grant_h %>% group_by(awarding_agency_name, agency) %>% summarise(count = n())
contract_h %>% group_by(awarding_agency_name, agency) %>% summarise(count = n())

##checking--how much "negative" money is going into each agency (unknown if NCSES tracks this)
grant_h %>% 
  filter(federal_action_obligation < 0) %>% 
  group_by(awarding_agency_name, agency) %>% 
  summarise(total = sum(federal_action_obligation))
contract_h %>% 
  filter(federal_action_obligation < 0) %>% 
  group_by(awarding_agency_name, agency) %>% 
  summarise(total = sum(federal_action_obligation))

###identifying SnE in grants by CFDA codes

cfda_sne <- filter(cfda, isSnE == 1)$code

grant_h$sne_cfda <- grant_h$cfda_number %in% cfda_sne

grant_h %>% group_by(sne_cfda) %>% summarise(count = n())
grant_h %>% group_by(sne_cfda, resflag) %>% summarise(count = n())

grant_h$resflag <- grant_h$sne_cfda

grant_h %>% group_by(sne_cfda, fedmatch, resflag) %>% summarise(count = n())

###What sub-agencies give grants

grant_h %>% group_by(sne_cfda, resflag)

unique_subagencies <- grant_h %>% 
  filter(resflag == TRUE) %>%
  select(awarding_agency_name, awarding_sub_agency_name) %>%
  unique()

