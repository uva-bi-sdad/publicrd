###combining USASpending contract and grant files

library(data.table)
library(lubridate)
library(tidyverse)

loc <- file.path("data/original")

prime_grant <- fread(file.path(loc, "2016-data_multiple_usa-spending_transaction_prime_grants-only.csv"))
prime_contract <- fread(file.path(loc, "2016-data_multiple_usa-spending_transaction_prime_contracts-only.csv"))

ncol(prime_contract) == ncol(prime_grant)
ncol(prime_grant)
ncol(prime_contract)

##what columns are shared between grant and contract files?
#how many are shared (39)
sum(colnames(prime_contract) %chin% colnames(prime_grant))
#what are the shared columns?
shared <- colnames(prime_contract) %chin% colnames(prime_grant)
shared_cols <- colnames(prime_contract)[shared]
contract_only <- !colnames(prime_contract) %chin% colnames(prime_grant)
contract_only_cols <- colnames(prime_contract)[contract_only]
grant_only <- !colnames(prime_grant) %chin% colnames(prime_contract)
grant_only_cols <- colnames(prime_grant)[grant_only]

##print out list of columns in each group
paste(as.character(shared_cols),collapse=", ",sep="")
paste(as.character(grant_only_cols),collapse=", ",sep="")
paste(as.character(contract_only_cols),collapse=", ",sep="")

##I went and decided what fields might be relevant; now narrowing down that
##massive contracts file to just what we might care about:

contract_col_keep <- c(shared_cols, "award_id_piid", "federally_funded_research_and_development_corp",
                       "educational_institution", "organizational_type", "cage_code", "product_or_service_code",
                       "product_or_service_code_description", "dod_claimant_program_code", "dod_claimant_program_description",
                       "naics_code", "naics_description", "dod_acquisition_program_code", 
                       "dod_acquisition_program_description", "research_code", "research",
                       "major_program", "foundation", "nonprofit_organization",
                       "other_not_for_profit_organization", "private_university_or_college",
                       "state_controlled_institution_of_higher_learning", "1862_land_grant_college",
                       "1890_land_grant_college", "1994_land_grant_college", 
                       "historically_black_college", "tribal_college", "school_of_forestry",
                       "veterinary_college")

prime_contract_trim <- select(prime_contract, contract_col_keep)

###writing this out
loc <- file.path("data/working")
write.csv(prime_contract_trim, file.path(loc, "2016-data_multiple_usa-spending_transaction_prime_contracts-only_select-fields.csv"))

###how much contract money comes from our six agencies?

usa_contract_6ag <- filter(prime_contract_trim, awarding_agency_name %in% c("DEPARTMENT OF DEFENSE (DOD)", "DEPARTMENT OF ENERGY (DOE)",
   "NATIONAL AERONAUTICS AND SPACE ADMINISTRATION (NASA)", "DEPARTMENT OF AGRICULTURE (USDA)", 
   "NATIONAL SCIENCE FOUNDATION (NSF)") | awarding_sub_agency_name == "NATIONAL INSTITUTES OF HEALTH")

ggplot(usa_contract_6ag) + geom_bar(aes(x = awarding_agency_name, y = federal_action_obligation), stat = "sum")

usa_contract_6ag %>% group_by(awarding_agency_name) %>% summarise(count = n(), 
  total = sum(federal_action_obligation)) %>% arrange(desc(total)) %>% ggplot()+
  geom_col(aes(x=awarding_agency_name, y=total))+coord_flip()

usa_contract_6ag %>% group_by(awarding_agency_name) %>% summarise(count = n(), 
  total = sum(federal_action_obligation)) %>% arrange(desc(total)) %>% ggplot()+
  geom_col(aes(x=awarding_agency_name, y=count))+coord_flip()

usa_grant_6ag <- filter(prime_grant, awarding_agency_name %in% c("DEPARTMENT OF DEFENSE (DOD)", 
               "DEPARTMENT OF ENERGY (DOE)", "NATIONAL AERONAUTICS AND SPACE ADMINISTRATION (NASA)", 
               "DEPARTMENT OF AGRICULTURE (USDA)", "NATIONAL SCIENCE FOUNDATION (NSF)") | 
                 awarding_sub_agency_name == "NATIONAL INSTITUTES OF HEALTH")

usa_c_s_6ag <- usa_contract_6ag %>% group_by(awarding_agency_name) %>% summarise(count = n(), 
  total = sum(federal_action_obligation), count = n())
usa_c_s_6ag$type <- "contract"

usa_g_s_6ag <- usa_grant_6ag %>% group_by(awarding_agency_name) %>% summarise(count = n(), 
     total = sum(federal_action_obligation), count = n())
usa_g_s_6ag$type <- "grant"


usa_6ag_sum <- rbind(usa_c_s_6ag, usa_g_s_6ag)
ggplot(usa_6ag_sum)+geom_bar(aes(x=awarding_agency_name, y=count), stat = "sum") + coord_flip()
ggplot(usa_6ag_sum)+geom_bar(aes(x=awarding_agency_name, y=total), stat = "sum") + coord_flip()

ggplot(usa_6ag_sum)+geom_bar(aes(x=awarding_agency_name, y=count, fill = type), stat = "sum", 
                             position = "fill") + coord_flip()+
                              ggtitle("Count of grants and contracts as proportion of all obligations")
ggplot(usa_6ag_sum)+geom_bar(aes(x=awarding_agency_name, y=total, fill = type), stat = "sum", 
                             position = "fill") + coord_flip() + 
                            ggtitle("Total amount of grants and contracts as proportion of all obligations")

sum(usa_6ag_sum[usa_6ag_sum$awarding_agency_name == "NATIONAL AERONAUTICS AND SPACE ADMINISTRATION (NASA)","total"])

###investigating that research field in the contracts data--not very useful, only marks 3 specific kinds of research
sum(is.na(usa_contract_6ag$research))/nrow(usa_contract_6ag$research)
table(usa_contract_6ag$research_code)
###investigating "product and service code" fields
##this could be REALLY USEFUL: 
product_service <- usa_contract_6ag %>% group_by(awarding_agency_name, product_or_service_code, product_or_service_code_description) %>%
  summarise(count = n()) %>% arrange(desc(count))

product_service_nsf <- usa_contract_6ag %>% filter(awarding_agency_name == "NATIONAL SCIENCE FOUNDATION (NSF)") %>% group_by(product_or_service_code, product_or_service_code_description) %>%
  summarise(count = n()) %>% arrange(desc(count))

product_service_nasa <- usa_contract_6ag %>% filter(awarding_agency_name == "NATIONAL AERONAUTICS AND SPACE ADMINISTRATION (NASA)") %>% group_by(product_or_service_code, product_or_service_code_description) %>%
  summarise(count = n()) %>% arrange(desc(count))

