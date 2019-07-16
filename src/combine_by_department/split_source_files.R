library(data.table)
library(tidyverse)

#####Dividing USA Spending contracts into 6 agencies

loc <- "data/working"
usa_contract_2016 <- fread(file.path(loc, "2016-data_multiple_usa-spending_transaction_prime_contracts-only_select-fields.csv"))


contract_nih <- filter(usa_contract_2016, awarding_sub_agency_name == "NATIONAL INSTITUTES OF HEALTH")
contract_usda <- filter(usa_contract_2016, awarding_agency_name == "DEPARTMENT OF AGRICULTURE (USDA)")
contract_dod <- filter(usa_contract_2016, awarding_agency_name == "DEPARTMENT OF DEFENSE (DOD)")
contract_nsf <- filter(usa_contract_2016, awarding_agency_name == "NATIONAL SCIENCE FOUNDATION (NSF)")
contract_nasa <- filter(usa_contract_2016, awarding_agency_name == "NATIONAL AERONAUTICS AND SPACE ADMINISTRATION (NASA)")
contract_doe <- filter(usa_contract_2016, awarding_agency_name == "DEPARTMENT OF ENERGY (DOE)")


#####Dividing USA Spending grants into 6 agencies

loc <- "data/original"
usa_grant_2016 <- fread(file.path(loc, "2016-data_multiple_usa-spending_transaction_prime_grants-only.csv"))

grant_nih <- filter(usa_grant_2016, awarding_sub_agency_name == "NATIONAL INSTITUTES OF HEALTH")
grant_usda <- filter(usa_grant_2016, awarding_agency_name == "DEPARTMENT OF AGRICULTURE (USDA)")
grant_dod <- filter(usa_grant_2016, awarding_agency_name == "DEPARTMENT OF DEFENSE (DOD)")
grant_nsf <- filter(usa_grant_2016, awarding_agency_name == "NATIONAL SCIENCE FOUNDATION (NSF)")
grant_nasa <- filter(usa_grant_2016, awarding_agency_name == "NATIONAL AERONAUTICS AND SPACE ADMINISTRATION (NASA)")
grant_doe <- filter(usa_grant_2016, awarding_agency_name == "DEPARTMENT OF ENERGY (DOE)")


####Writing out USA Spending data

loc_nih <- "data/working/combine_by_department/NIH"
write.csv(grant_nih, file.path(loc_nih, "nih_usa-spending_grants_2016.csv"))
write.csv(contract_nih, file.path(loc_nih, "nih_usa-spending_contracts_2016.csv"))

loc_usda <- "data/working/combine_by_department/USDA"
write.csv(grant_usda, file.path(loc_usda, "usda_usa-spending_grants_2016.csv"))
write.csv(contract_usda, file.path(loc_usda, "usda_usa-spending_contracts_2016.csv"))

loc_nsf <- "data/working/combine_by_department/NSF"
write.csv(grant_nsf, file.path(loc_nsf, "nsf_usa-spending_grants_2016.csv"))
write.csv(contract_nsf, file.path(loc_nsf, "nsf_usa-spending_contracts_2016.csv"))

loc_dod <- "data/working/combine_by_department/DOD"
write.csv(grant_dod, file.path(loc_dod, "dod_usa-spending_grants_2016.csv"))
write.csv(contract_dod, file.path(loc_dod, "dod_usa-spending_contracts_2016.csv"))

loc_nasa <- "data/working/combine_by_department/NASA"
write.csv(grant_nasa, file.path(loc_nasa, "nasa_usa-spending_grants_2016.csv"))
write.csv(contract_nasa, file.path(loc_nasa, "nasa_usa-spending_contracts_2016.csv"))

loc_doe <- "data/working/combine_by_department/DOE"
write.csv(grant_doe, file.path(loc_doe, "doe_usa-spending_grants_2016.csv"))
write.csv(contract_doe, file.path(loc_doe, "doe_usa-spending_contracts_2016.csv"))


###Reading in FedReporter for FY2014-FY2016

loc_fed <- "data/working"
fed_rep_2014 <- fread(file.path(loc_fed, "2014-fed_reporter_fixed_cfda.csv"))
fed_rep_2015 <- fread(file.path(loc_fed, "2015-fed_reporter_fixed_cfda.csv"))
fed_rep_2016 <- fread(file.path(loc_fed, "2016-fed_reporter_fixed_cfda.csv"))

##checking to make sure columns match between sources
ncol(fed_rep_2014) == ncol(fed_rep_2015) & ncol(fed_rep_2014) == ncol(fed_rep_2016)
sum(colnames(fed_rep_2014) == colnames(fed_rep_2015))/ncol(fed_rep_2015)
sum(colnames(fed_rep_2015) == colnames(fed_rep_2016))/ncol(fed_rep_2016)

sum(sapply(fed_rep_2014, class) == sapply(fed_rep_2015, class))/ncol(fed_rep_2014)
sum(sapply(fed_rep_2015, class) == sapply(fed_rep_2016, class))/ncol(fed_rep_2015)

fed_rep_2014$CONGRESSIONAL_DISTRICT <- as.integer(fed_rep_2014$CONGRESSIONAL_DISTRICT)

fed_rep <- rbind(fed_rep_2014, fed_rep_2015, fed_rep_2016)

fed_nih <- filter(fed_rep, AGENCY == "NIH")
fed_dod <- filter(fed_rep, DEPARTMENT == "DOD")
fed_nasa <- filter(fed_rep, DEPARTMENT == "NASA")
fed_nsf <- filter(fed_rep, DEPARTMENT == "NSF")
fed_usda <- filter(fed_rep, DEPARTMENT == "USDA")


loc_nih <- "data/working/combine_by_department/NIH"
write.csv(fed_nih, file.path(loc_nih, "nih_fed-reporter_2016.csv"))

loc_usda <- "data/working/combine_by_department/USDA"
write.csv(fed_usda, file.path(loc_usda, "usda_fed-reporter_2016.csv"))

loc_nsf <- "data/working/combine_by_department/NSF"
write.csv(fed_nsf, file.path(loc_nsf, "nsf_fed-reporter_2016.csv"))

loc_dod <- "data/working/combine_by_department/DOD"
write.csv(fed_dod, file.path(loc_dod, "dod_fed-reporter_2016.csv"))

loc_nasa <- "data/working/combine_by_department/NASA"
write.csv(fed_nasa, file.path(loc_nasa, "nasa_fed-reporter_2016.csv"))
