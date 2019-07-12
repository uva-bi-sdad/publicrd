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

contract_col_keep <- c(shared_cols, "federally_funded_research_and_development_corp",
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

