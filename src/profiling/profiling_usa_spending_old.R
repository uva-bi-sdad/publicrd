library(R.utils)
library(tidyverse)
library(data.table)
library(dplyr)
library(stringr)
options(scipen=999)

usa_spending_transaction <- read_csv("data/original/2016-data_multiple_usa-spending_transaction_prime.csv")

##Read in Glossaries

country_glossary <- read_csv("glossary/country_glossary.csv")
state_zip_glossary <- read_csv("glossary/state_zip_glossary.csv")
agency_glossary <- read_csv("glossary/agency_glossary.csv")

##Include relevant variables:

usa_spending_transaction <- usa_spending_transaction %>% select(award_id_fain, modification_number, award_id_uri, 
                                   
                                   federal_action_obligation, non_federal_funding_amount, total_funding_amount, 
                                   
                                   action_date, period_of_performance_start_date, period_of_performance_current_end_date,
                                   
                                   awarding_agency_code, awarding_agency_name, 
                                   awarding_sub_agency_code, awarding_sub_agency_name,
                                   awarding_office_code, awarding_office_name, 
                                   funding_agency_code, funding_agency_name, 
                                   funding_sub_agency_code, funding_sub_agency_name,
                                   funding_office_code, funding_office_name,
                                   
                                   recipient_duns, recipient_name, recipient_parent_duns, recipient_parent_name,
                                   
                                   recipient_country_code, recipient_country_name, 
                                   recipient_address_line_1, recipient_address_line_2,
                                   recipient_city_code, recipient_city_name, 
                                   recipient_state_code, recipient_state_name, recipient_zip_code, 
                                   
                                   primary_place_of_performance_country_code, primary_place_of_performance_country_name, 
                                   primary_place_of_performance_code, primary_place_of_performance_city_name, 
                                   primary_place_of_performance_state_name, primary_place_of_performance_zip_4, 
                                   
                                   cfda_number, cfda_title, 
                                   
                                   assistance_type_code, assistance_type_description, award_description, 
                                   record_type_code, record_type_description, last_modified_date
                                   )

##Creating Data Frame

field_profile_usa_spending <- data.frame(field = as.character(colnames(usa_spending_transaction)), 
                              completeness = as.numeric(NA),
                              validity = as.numeric(NA))

#field_profile_usa_spending$completeness <- mutate(field_profile_usa_spending, is.na(usa_spending_transaction))

##~~~~~~~~~~~~~~~~COMPLETENESS~~~~~~~~~~~~~~~~

##Creating completeness function

#Function to calculate proportion of N/A responses in dataset
na_count <- function(x){
  (sum(is.na(x)) + sum(x == "", na.rm = TRUE))/nrow(usa_spending_transaction)
}

#Function to calculate % complete
percent_complete <- function(x){
  (1-na_count(x))*100
}

#Update field_profile with completeness
field_profile_usa_spending$completeness <- apply(usa_spending_transaction, MARGIN = 2, percent_complete)


##~~~~~~~~~~~~~~~~VALIDITY~~~~~~~~~~~~~~~~

#Not complete, but decent information on how to analyze this database here:
#https://www.fpds.gov/downloads/FAADS/Grants_Data_Dictionary_(Draft).pdf
#https://datalab.usaspending.gov/assets/analyst-guide-1-2.pdf
#https://fas.org/sgp/crs/misc/R44027.pdf

#Function to input a filtering function and a database column to filter, 

#step 1:
#
#output percentage of valid entries in column:

#Function to update field profile:

place_in_field_profile <- function(percentage, database_profiling){
  
  
  
  
  
}

colnames(usa_spending_transaction)


##award_id_fain
fain_length <- nchar(usa_spending_transaction$award_id_fain)
hist(fain_length, breaks = 100)


usa_spending_transaction[which(nchar(usa_spending_transaction) == 13), "recipient_name"]

subagency_list <- usa_spending_transaction[match(unique(usa_spending_transaction$awarding_sub_agency_name), usa_spending_transaction$awarding_sub_agency_name),]
subagency_list_final <- select(subagency_list, award_id_fain, modification_number, 
                         awarding_agency_code, awarding_agency_name, 
                         awarding_sub_agency_code, awarding_sub_agency_name,
                         awarding_office_code, awarding_office_name)

subagency_list_final <- subagency_list_final %>% arrange(awarding_agency_name)

##modification_number

##award_id_uri

##federal_action_obligation

federal_action_obligation_valid <- function(database, column){
  
  valid_entries <- database[which(grepl("^[0-1]$", database[[column]]) == FALSE &
                                  grepl("^[0-9-]+?\\.?[0-9]+$", database[[column]]) == TRUE |
                                  grepl("^-[0-9-]+?\\.?[0-9]+$", database[[column]]) == TRUE |
                                  grepl("^[2-9-]", database[[column]]) == TRUE), column]
  
  percent_valid <- nrow(valid_entries)/nrow(database)*100
  column_number <- match(column, colnames(database))
  
  field_profile_usa_spending[column_number, "validity"] <- percent_valid
  
  assign('field_profile_usa_spending', field_profile_usa_spending, envir=.GlobalEnv)
  
}

federal_action_obligation_valid(usa_spending_transaction, "federal_action_obligation")


federal_action_valid <- usa_spending_transaction[which(grepl("[0-1]", usa_spending_transaction$federal_action_obligation) == FALSE &
                                                       grepl("^[0-9-]+?\\.?[0-9]+$", usa_spending_transaction$federal_action_obligation) == TRUE |
                                                       grepl("^-[0-9-]+?\\.?[0-9]+$", usa_spending_transaction$federal_action_obligation) == TRUE |
                                                       grepl("^[1-9-]", usa_spending_transaction$federal_action_obligation) == TRUE), "federal_action_obligation"]
field_profile_usa_spending[4,"validity"] <- (nrow(federal_action_valid)/nrow(usa_spending_transaction))*100


##non_federal_funding_amount

non_federal_funding_valid <- usa_spending_transaction[which(grepl("^[0-9-]+?\\.?[0-9]+$", usa_spending_transaction$non_federal_funding_amount) == TRUE |
                                                            grepl("^-[0-9-]+?\\.?[0-9]+$", usa_spending_transaction$non_federal_funding_amount) == TRUE |
                                                            grepl("^[0-9-]", usa_spending_transaction$non_federal_funding_amount) == TRUE), "non_federal_funding_amount"]
field_profile_usa_spending[5,"validity"] <- (nrow(non_federal_funding_valid)/nrow(usa_spending_transaction))*100

##total_funding_amount

total_funding_valid <- usa_spending_transaction[which(grepl("[0-1]", usa_spending_transaction$total_funding_amount) == FALSE &
                                                      grepl("^[0-9-]+?\\.?[0-9]+$", usa_spending_transaction$total_funding_amount) == TRUE |
                                                      grepl("^-[0-9-]+?\\.?[0-9]+$", usa_spending_transaction$total_funding_amount) == TRUE |
                                                      grepl("^[1-9-]", usa_spending_transaction$total_funding_amount) == TRUE), "total_funding_amount"]
field_profile_usa_spending[6,"validity"] <- (nrow(federal_action_valid)/nrow(usa_spending_transaction))*100

##action_date

action_date_valid <- usa_spending_transaction[which(grepl("^201[5-6]-[0-1][0-9]-[0-3][0-9]", usa_spending_transaction$action_date) == TRUE), "action_date"]
field_profile_usa_spending[7,"validity"] <- (nrow(action_date_valid)/nrow(usa_spending_transaction))*100

##period_of_performance_start_date

performance_start_date_valid <- usa_spending_transaction[which(grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]", 
                                                                     usa_spending_transaction$period_of_performance_start_date) == TRUE), "period_of_performance_start_date"]

field_profile_usa_spending[8,"validity"] <- (nrow(performance_start_date_valid)/nrow(usa_spending_transaction))*100


##period_of_performance_current_end_date

performance_end_date_valid <- usa_spending_transaction[which(grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]", 
                                                                   usa_spending_transaction$period_of_performance_current_end_date) == TRUE), "period_of_performance_current_end_date"]

field_profile_usa_spending[9,"validity"] <- (nrow(performance_end_date_valid)/nrow(usa_spending_transaction))*100

##awarding_agency_code

#Use modified version of agency_glossary:
#USA spending uses CGAC codes rather than Department IDs ("012" instead of "1200"), so we will modify the glossary:




##awarding_agency_name

##awarding_sub_agency_code

##awarding_sub_agency_name

##awarding_office_code

##awarding_office_name

##funding_agency_code

##funding_agency_name

##funding_sub_agency_code

##funding_sub_agency_name

##funding_office_code

##funding_office_name

##recipient_duns

recipient_duns_valid <- usa_spending_transaction[which(grepl("^[0-9]{9}$", usa_spending_transaction$recipient_duns) == TRUE), "recipient_duns"]
field_profile_usa_spending[22,"validity"] <- (nrow(recipient_duns_valid)/nrow(usa_spending_transaction))*100

##recipient_name

recipient_name_length <- nchar(usa_spending_transaction$recipient_name)
hist(recipient_name_length, breaks = 100)
#Huge spike at ~45 chars, examine:
recipient_name_long <- usa_spending_transaction[which(recipient_name_length >= 45), "recipient_name"]
#~18000 names that are exactly that length. Some are valid, but vast majority are cut off
#(e.g. "WESTERN KENTUCKY UNIVERSITY RESEARCH FOUNDATI")
#I'll mark these as invalid as a reminder for when we try to join these institutions to another database,
#though it is still useful information.
recipient_name_cut <- usa_spending_transaction[which(recipient_name_length == 45), "recipient_name"]

#Screen for numbers
recipient_name_odd <- usa_spending_transaction[which(grepl("[^a-zA-Z. /(),&'360-]", usa_spending_transaction$recipient_name) == TRUE), "recipient_name"]
#Several use "`" as an apostrophe, which might cause problems with matching. 
#Most with numbers not "360" are listing a zip code in the field, which is not valid.

recipient_name_invalid <- usa_spending_transaction[which(grepl("[^a-zA-Z. /(),&'360-]", usa_spending_transaction$recipient_name) == TRUE | 
                                                         recipient_name_length == 45), "recipient_name"]
field_profile_usa_spending[23,"validity"] <- ((nrow(usa_spending_transaction)-nrow(recipient_name_invalid))/nrow(usa_spending_transaction))*100

##recipient_parent_duns

recipient_parent_duns_valid <- usa_spending_transaction[which(grepl("^[0-9]{9}$", usa_spending_transaction$recipient_parent_duns) == TRUE), "recipient_parent_duns"]
field_profile_usa_spending[24,"validity"] <- (nrow(recipient_parent_duns_valid)/nrow(usa_spending_transaction))*100

##recipient_parent_name



##recipient_country_code & recipient_country_name

#Use country_glossary to verify if names and codes of countries are actual names with the correct corresponding code:

country_glossary$country_name <- toupper(country_glossary$country_name)

country_frame <- select(usa_spending_transaction, recipient_country_name, recipient_country_code)
colnames(country_frame)[colnames(country_frame)=="recipient_country_name"] <- "country_name"
colnames(country_frame)[colnames(country_frame)=="recipient_country_code"] <- "country_code"
country_frame <- left_join(country_frame, country_glossary)

#Created new data frame with three columns: country_name, country_code and country_abb. 
#country_name has been matched against the glossary, so if the recipient_country_name exists,
#then the glossary will match it with a country_abb. 

#If recipient_country_name is not valid, then an N/A will appear in country_abb.

recipient_country_name_valid <- country_frame[which(is.na(country_frame$country_name) == FALSE & is.na(country_frame$country_abb) == FALSE), "country_name"]
field_profile_usa_spending[27,"validity"] <- (nrow(recipient_country_name_valid)/nrow(usa_spending_transaction))*100

#Check if recipient_country_code and country_abb match:

country_frame$matches <- mapply(grepl, paste0('\\b', country_frame$country_code, '\\b'), country_frame$country_abb)

recipient_country_code_valid <- country_frame[which(country_frame$matches == TRUE & is.na(country_frame$country_abb) == FALSE), "country_name"]
field_profile_usa_spending[26,"validity"] <- (nrow(recipient_country_code_valid)/nrow(usa_spending_transaction))*100


##recipient_address_line_1

##recipient_address_line_2

##recipient_city_code

##recipient_city_name


##recipient_state_code, recipient_state_name & recipient_zip_code

state_zip_glossary$state_name <- toupper(state_zip_glossary$state_name)

state_frame <- select(usa_spending_transaction, recipient_state_name, recipient_state_code, recipient_zip_code)
colnames(state_frame)[colnames(state_frame)=="recipient_state_name"] <- "state_name"
colnames(state_frame)[colnames(state_frame)=="recipient_state_code"] <- "state_code"
colnames(state_frame)[colnames(state_frame)=="recipient_zip_code"] <- "zip_code"

state_frame <- left_join(state_frame, state_zip_glossary)

#If state name exists, then corresponding state_abbr will exist:

recipient_state_name_valid <- state_frame[which(is.na(state_frame$state_name) == FALSE & is.na(state_frame$state_abbr) == FALSE), "state_name"]
field_profile_usa_spending[33,"validity"] <- (nrow(recipient_state_name_valid)/nrow(usa_spending_transaction))*100

#Check if recipient_state_code and state_abbr match:

state_frame$matches_state_code <- mapply(grepl, paste0('\\b', state_frame$state_code, '\\b'), state_frame$state_abbr)

recipient_state_code_valid <- state_frame[which(state_frame$matches_state_code == TRUE & is.na(state_frame$state_abbr) == FALSE), "state_name"]
field_profile_usa_spending[32,"validity"] <- (nrow(recipient_state_code_valid)/nrow(usa_spending_transaction))*100

#Check if recipient_zip_code is within proper range

state_frame$three_digit_zip <- substr(state_frame$zip_code, start = 1, stop = 3)

check_zip_range <- function(given_zip){
  
  checked_zip <- ifelse(state_frame$zip_code_min <= given_zip & state_frame$zip_code_max >= given_zip, TRUE, 
                        
                        ifelse(state_frame$zip_code_min_extra <= given_zip & state_frame$zip_code_max_extra >= given_zip, TRUE, FALSE)
                        
                        )
  
  return(checked_zip)    
  
}

state_frame$matches_zip_code <- check_zip_range(state_frame$three_digit_zip)

recipient_zip_code_valid <- state_frame[which(state_frame$matches_zip_code == TRUE & is.na(state_frame$zip_code) == FALSE), "state_name"]
field_profile_usa_spending[34,"validity"] <- (nrow(recipient_state_code_valid)/nrow(usa_spending_transaction))*100


##primary_place_of_performance_country_code & primary_place_of_performance_country_name

country_glossary$country_name <- toupper(country_glossary$country_name)

country_frame_performance <- select(usa_spending_transaction, primary_place_of_performance_country_name, primary_place_of_performance_country_code)
colnames(country_frame_performance)[colnames(country_frame_performance)=="primary_place_of_performance_country_name"] <- "country_name"
colnames(country_frame_performance)[colnames(country_frame_performance)=="primary_place_of_performance_country_code"] <- "country_code"
country_frame_performance <- left_join(country_frame_performance, country_glossary)

primary_place_country_name_valid <- country_frame_performance[which(is.na(country_frame_performance$country_name) == FALSE & 
                                                                    is.na(country_frame_performance$country_abb) == FALSE), "country_name"]
field_profile_usa_spending[36,"validity"] <- (nrow(recipient_country_name_valid)/nrow(usa_spending_transaction))*100

country_frame_performance$matches <- mapply(grepl, paste0('\\b', country_frame_performance$country_code, '\\b'), country_frame_performance$country_abb)

primary_place_country_code_valid <- country_frame_performance[which(country_frame_performance$matches == TRUE & 
                                                                    is.na(country_frame_performance$country_abb) == FALSE), "country_name"]
field_profile_usa_spending[35,"validity"] <- (nrow(recipient_country_code_valid)/nrow(usa_spending_transaction))*100

##primary_place_of_performance_code

##primary_place_of_performance_city_name

##primary_place_of_performance_state_name & primary_place_of_performance_zip_4

state_zip_glossary$state_name <- toupper(state_zip_glossary$state_name)

state_frame_performance <- select(usa_spending_transaction, primary_place_of_performance_state_name, primary_place_of_performance_zip_4)
colnames(state_frame_performance)[colnames(state_frame_performance)=="primary_place_of_performance_state_name"] <- "state_name"
colnames(state_frame_performance)[colnames(state_frame_performance)=="primary_place_of_performance_zip_4"] <- "zip_code"

state_frame_performance <- left_join(state_frame_performance, state_zip_glossary)

#If state name exists, then corresponding state_abbr will exist:

primary_place_of_performance_state_name_valid <- state_frame_performance[which(is.na(state_frame_performance$state_name) == FALSE & 
                                                                               is.na(state_frame_performance$state_abbr) == FALSE), "state_name"]
field_profile_usa_spending[39,"validity"] <- (nrow(primary_place_of_performance_state_name_valid)/nrow(usa_spending_transaction))*100

#Check if primary_place_of_performance_zip_4 is within proper range

state_frame_performance$three_digit_zip <- substr(state_frame_performance$zip_code, start = 1, stop = 3)

check_zip_range <- function(given_zip){
  
  checked_zip <- ifelse(state_frame_performance$zip_code_min <= given_zip & state_frame_performance$zip_code_max >= given_zip, TRUE, 
                        
                        ifelse(state_frame_performance$zip_code_min_extra <= given_zip & state_frame_performance$zip_code_max_extra >= given_zip, TRUE, FALSE)
                        
  )
  
  return(checked_zip)    
  
}

state_frame_performance$matches_zip_code <- check_zip_range(state_frame_performance$three_digit_zip)

primary_place_of_performance_zip_4_valid <- state_frame_performance[which(state_frame_performance$matches_zip_code == TRUE & 
                                                                           is.na(state_frame_performance$zip_code) == FALSE), "state_name"]
field_profile_usa_spending[40,"validity"] <- (nrow(primary_place_of_performance_zip_4_valid)/nrow(usa_spending_transaction))*100


##cfda_number

cfda_number_valid <- usa_spending_transaction[which(usa_spending_transaction$cfda_number != 000 & usa_spending_transaction$cfda_number != 999 & 
                                                    grepl("[0-9]{2}\\.[0-9]{3}$", usa_spending_transaction$cfda_number) == TRUE), "cfda_number"]

field_profile_usa_spending[41,"validity"] <- (nrow(cfda_number_valid)/nrow(usa_spending_transaction))*100

cfda_number_invalid <- usa_spending_transaction[which(usa_spending_transaction$cfda_number == 000 | usa_spending_transaction$cfda_number == 999 | 
                                                      grepl("[0-9]{2}\\.[0-9]{3}$", usa_spending_transaction$cfda_number) == FALSE), "cfda_number"]

##cfda_title



##assistance_type_code
#02 Block Grant
#03 Formula Grant
#04 Project Grant
#05 Cooperative Agreement

assistance_type_code_valid <- usa_spending_transaction[which(grepl("^[0]{1}[2-5]{1}$", usa_spending_transaction$assistance_type_code) == TRUE), "assistance_type_code"]
field_profile_usa_spending[43,"validity"] <- (nrow(assistance_type_code_valid)/nrow(usa_spending_transaction))*100

#assistance_type_description

##award_description

##record_type_code
#Record type may be ‘Individual’ or ‘Aggregate’. Aggregate reporting is for payments to individuals identifiable by a geographical unit. 
#Individual reporting consists of “action-by-action” transactions. Aggregate is marked as 1. Individual is marked as 2.
#Because we are looking at transactions, all of these should be equal to 2:

record_type_code_valid <- usa_spending_transaction[which(grepl("^[2]{1}$", usa_spending_transaction$record_type_code) == TRUE), "record_type_code"]
field_profile_usa_spending[46,"validity"] <- (nrow(record_type_code_valid)/nrow(usa_spending_transaction))*100

##record_type_description

record_type_description_valid <- usa_spending_transaction[which(grepl("^[2]{1}$", usa_spending_transaction$record_type_code) == TRUE), "record_type_code"]

##last_modified_date

last_modified_date_valid <- usa_spending_transaction[which(grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]", 
                                                                   usa_spending_transaction$last_modified_date) == TRUE), "last_modified_date"]

field_profile_usa_spending[48,"validity"] <- (nrow(last_modified_date_valid)/nrow(usa_spending_transaction))*100




