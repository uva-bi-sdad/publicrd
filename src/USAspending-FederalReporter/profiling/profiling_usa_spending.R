library(R.utils)
library(tidyverse)
library(data.table)
library(dplyr)
library(stringr)
options(scipen=999)

usa_spending_transaction <- read_csv("data/original/2016-data_multiple_usa-spending_transaction_prime_grants-only.csv")

##Read in Glossaries

glossary_country <- read_csv("glossary/country_glossary.csv")
glossary_state_zip <- read_csv("glossary/state_zip_glossary.csv")
glossary_agency <- read_csv("glossary/agency_glossary.csv")

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



#output valid_entries of valid entries in column:

#Function to update field profile:

#1field_profile_loader <- function(percentage, database_profiling){
  
  
  
  
  
#}




##award_id_fain
#hist(fain_length, breaks = 100)

#subagency_list <- usa_spending_transaction[match(
#unique(usa_spending_transaction$awarding_sub_agency_name), usa_spending_transaction$awarding_sub_agency_name),]
#subagency_list_final <- select(subagency_list, award_id_fain, modification_number, 
                               #awarding_agency_code, awarding_agency_name, 
                               #awarding_sub_agency_code, awarding_sub_agency_name,
                               #awarding_office_code, awarding_office_name)

#subagency_list_final <- subagency_list_final %>% arrange(awarding_agency_name)

##modification_number

v_modification_number <- function(database, column){
  
  valid_entries <- database[which(str_detect(database[[column]], "^[0-9A-Z]+$") == TRUE), column]
  
  percent_valid <- nrow(valid_entries)/nrow(database)*100
  column_number <- match(column, colnames(database))
  
  field_profile_usa_spending[column_number, "validity"] <- percent_valid
  
  assign('field_profile_usa_spending', field_profile_usa_spending, envir=.GlobalEnv)
  
}

v_modification_number(usa_spending_transaction, "modification_number")

##award_id_uri



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##federal_action_obligation & total_funding_amount & non_federal_funding_amount

#Function covers all positive and negative numbers with or without decimals except for 0 or 1:

v_funding_amount <- function(database, column){
  
  valid_entries <- database[which(str_detect(database[[column]], "^[0-1]$") == FALSE &
                                  str_detect(database[[column]], "^[0-9-]+?\\.?[0-9]+$") == TRUE |
                                  str_detect(database[[column]], "^-[0-9-]+?\\.?[0-9]+$") == TRUE |
                                  str_detect(database[[column]], "^[2-9-]") == TRUE), column]
  
  percent_valid <- nrow(valid_entries)/nrow(database)*100
  column_number <- match(column, colnames(database))
  
  field_profile_usa_spending[column_number, "validity"] <- percent_valid
  
  assign('field_profile_usa_spending', field_profile_usa_spending, envir=.GlobalEnv)
  
}

#Same as normal function but allows 0 or 1 as a valid response:

v_non_federal_funding_amount <- function(database, column){
  
  valid_entries <- database[which(str_detect(database[[column]], "^[0-9-]+?\\.?[0-9]+$") == TRUE |
                                  str_detect(database[[column]], "^-[0-9-]+?\\.?[0-9]+$") == TRUE |
                                  str_detect(database[[column]], "^[0-9-]") == TRUE), column]
  
  percent_valid <- nrow(valid_entries)/nrow(database)*100
  column_number <- match(column, colnames(database))
  
  field_profile_usa_spending[column_number, "validity"] <- percent_valid
  
  assign('field_profile_usa_spending', field_profile_usa_spending, envir=.GlobalEnv)
  
}

v_funding_amount(usa_spending_transaction, "federal_action_obligation")
v_funding_amount(usa_spending_transaction, "total_funding_amount")
v_non_federal_funding_amount(usa_spending_transaction, "non_federal_funding_amount")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##action_date & period_of_performance_start_date & period_of_performance_current_end_date

#This date checker is function that accepts format for 20th and 21st century. The column action_date is limited to only 2015 and 2016

v_date <- function(database, column){
  
  valid_entries <- database[which(str_detect(database[[column]], "^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]") == TRUE), column]
  
  percent_valid <- nrow(valid_entries)/nrow(database)*100
  column_number <- match(column, colnames(database))
  
  field_profile_usa_spending[column_number, "validity"] <- percent_valid
  
  assign('field_profile_usa_spending', field_profile_usa_spending, envir=.GlobalEnv)
  
}

v_action_date <- function(database, column){
  
  valid_entries <- database[which(str_detect(database[[column]], "^201[5-6]-[0-1][0-9]-[0-3][0-9]") == TRUE), column]
  
  percent_valid <- nrow(valid_entries)/nrow(database)*100
  column_number <- match(column, colnames(database))
  
  field_profile_usa_spending[column_number, "validity"] <- percent_valid
  
  assign('field_profile_usa_spending', field_profile_usa_spending, envir=.GlobalEnv)
  
}

v_action_date(usa_spending_transaction, "action_date")
v_date(usa_spending_transaction, "period_of_performance_start_date")
v_date(usa_spending_transaction, "period_of_performance_current_end_date")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##Formatting Fixes

##awarding_agency_code & awarding_agency_name

#Use modified version of glossary_agency and isolate relevant columns from USAspending into separate frame_awarding_agency:

glossary_awarding_agency <- unique(glossary_agency %>% select(DEPARTMENT_ID, DEPARTMENT_NAME))
frame_awarding_agency <- usa_spending_transaction %>% select(awarding_agency_code, awarding_agency_name)

#USA spending uses CGAC codes rather than Department IDs ("012" instead of "1200"), so we will modify the glossary to match:
#List of CGAC codes by agency here: https://obamawhitehouse.archives.gov/sites/default/files/omb/assets/a11_current_year/app_c.pdf

cgac_converter <- function(department_id){
  
  twodigit_department_id <-ifelse(str_detect(department_id, "[0]{2}$") == TRUE, substr(department_id, start = 1, stop = 2), department_id)
  fixed_department_id <- ifelse(str_detect(twodigit_department_id, "^[0-9]{2}$") == TRUE,
                                paste0("0", twodigit_department_id), twodigit_department_id)
  
  return(fixed_department_id)
  
}

glossary_awarding_agency$DEPARTMENT_ID <- cgac_converter(glossary_awarding_agency$DEPARTMENT_ID)

#USAspending has department abbreviations in parentheses, so we need to remove these to match against glossary:

parentheses_remover <- function(department_name){
  
  remove_parentheses <- ifelse(str_detect(department_name, "\\(") == TRUE, gsub(" \\([A-Z]+\\)", "", department_name), department_name)
  
  return(remove_parentheses)
  
}

frame_awarding_agency$awarding_agency_name <- parentheses_remover(frame_awarding_agency$awarding_agency_name)

#Create agency_frame to join the glossary and test for matches:

frame_awarding_agency <- left_join(frame_awarding_agency, glossary_awarding_agency, by = c("awarding_agency_code" = "DEPARTMENT_ID"))
frame_awarding_agency$matches <- mapply(grepl, paste0('\\b', frame_awarding_agency$awarding_agency_name, '\\b'), frame_awarding_agency$DEPARTMENT_NAME)

#If there are any missing codes or N/A in DEPARTMENT_NAME then the CGAC code was invalid:

awarding_agency_code_valid <- frame_awarding_agency[which(
  is.na(frame_awarding_agency$awarding_agency_code) == FALSE & 
  is.na(frame_awarding_agency$DEPARTMENT_NAME) == FALSE), "awarding_agency_code"]
field_profile_usa_spending[10,"validity"] <- (nrow(awarding_agency_code_valid)/nrow(usa_spending_transaction))*100

#If awarding_agency_name and DEPARTMENT_NAME do not match, then the name was invalid:

awarding_agency_name_valid <- frame_awarding_agency[which(
  frame_awarding_agency$matches == TRUE & 
  is.na(frame_awarding_agency$awarding_agency_name) == FALSE), "awarding_agency_name"]
field_profile_usa_spending[11,"validity"] <- (nrow(awarding_agency_name_valid)/nrow(usa_spending_transaction))*100


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##Function to test agency codes and names

##awarding_sub_agency_code & awarding_sub_agency_name

#Same process as the previous two:

glossary_awarding_sub_agency <- glossary_agency %>% select(AGENCY_CODE, AGENCY_NAME)
glossary_awarding_sub_agency <- unique(glossary_awarding_sub_agency)

#Create sub_agency_frame

frame_awarding_sub_agency <- usa_spending_transaction %>% select(awarding_sub_agency_code, awarding_sub_agency_name)

#Fix "U.S." versus "US" discrepancy:

period_remover <- function(sub_agency_name){
  
  remove_period <- ifelse(str_detect(sub_agency_name, "U.S.") == TRUE, gsub("U\\.S\\.", "US", sub_agency_name), sub_agency_name)
  
  return(remove_period)
  
}

frame_awarding_sub_agency$awarding_sub_agency_name <- period_remover(frame_awarding_sub_agency$awarding_sub_agency_name)

#Join sub_agency_frame with glossary and test for matches:

frame_awarding_sub_agency <- left_join(frame_awarding_sub_agency, glossary_awarding_sub_agency, by = c("awarding_sub_agency_code" = "AGENCY_CODE"))
frame_awarding_sub_agency$matches <- mapply(grepl, paste0('\\b', frame_awarding_sub_agency$awarding_sub_agency_name, '\\b'), frame_awarding_sub_agency$AGENCY_NAME)

#If there are any missing codes or N/A in DEPARTMENT_NAME then the CGAC code was invalid:

awarding_sub_agency_code_valid <- frame_awarding_sub_agency[which(
  is.na(frame_awarding_sub_agency$awarding_sub_agency_code) == FALSE & 
  is.na(frame_awarding_sub_agency$AGENCY_NAME) == FALSE), "awarding_sub_agency_code"]
field_profile_usa_spending[12,"validity"] <- (nrow(awarding_sub_agency_code_valid)/nrow(usa_spending_transaction))*100

#If awarding_agency_name and DEPARTMENT_NAME do not match, then the name was invalid:

awarding_sub_agency_name_valid <- frame_awarding_sub_agency[which(
  frame_awarding_sub_agency$matches == TRUE & 
  is.na(frame_awarding_sub_agency$awarding_sub_agency_name) == FALSE), "awarding_sub_agency_name"]
field_profile_usa_spending[13,"validity"] <- (nrow(awarding_sub_agency_name_valid)/nrow(usa_spending_transaction))*100


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##funding_agency_code & funding_agency_name:

#Use modified version of glossary_agency and isolate relevant columns from USAspending into separate frame_awarding_agency:

glossary_funding_agency <- glossary_agency %>% select(DEPARTMENT_ID, DEPARTMENT_NAME)
glossary_funding_agency <- unique(glossary_funding_agency)
frame_funding_agency <- usa_spending_transaction %>% select(funding_agency_code, funding_agency_name)

#USA spending uses CGAC codes rather than Department IDs ("012" instead of "1200"), so we will modify the glossary to match:
#List of CGAC codes by agency here: https://obamawhitehouse.archives.gov/sites/default/files/omb/assets/a11_current_year/app_c.pdf

cgac_converter <- function(department_id){
  
  twodigit_department_id <-ifelse(str_detect(department_id, "[0]{2}$") == TRUE, substr(department_id, start = 1, stop = 2), department_id)
  fixed_department_id <- ifelse(str_detect(twodigit_department_id, "^[0-9]{2}$") == TRUE,
                                paste0("0", twodigit_department_id), twodigit_department_id)
  
  return(fixed_department_id)
  
}

glossary_funding_agency$DEPARTMENT_ID <- cgac_converter(glossary_funding_agency$DEPARTMENT_ID)

#USAspending has department abbreviations in parentheses, so we need to remove these to match against glossary:

parentheses_remover <- function(department_name){
  
  remove_parentheses <- ifelse(str_detect(department_name, "\\(") == TRUE, gsub(" \\([A-Z]+\\)", "", department_name), department_name)
  
  return(remove_parentheses)
  
}

frame_funding_agency$funding_agency_name <- parentheses_remover(frame_funding_agency$funding_agency_name)

#Create agency_frame to join the glossary and test for matches:

frame_funding_agency <- left_join(frame_funding_agency, glossary_funding_agency, by = c("funding_agency_code" = "DEPARTMENT_ID"))
frame_funding_agency$matches <- mapply(grepl, paste0('\\b', frame_funding_agency$funding_agency_name, '\\b'), frame_funding_agency$DEPARTMENT_NAME)

#If there are any missing codes or N/A in DEPARTMENT_NAME then the CGAC code was invalid:

funding_agency_code_valid <- frame_funding_agency[which(
  is.na(frame_funding_agency$funding_agency_code) == FALSE & 
  is.na(frame_funding_agency$DEPARTMENT_NAME) == FALSE), "funding_agency_code"]
field_profile_usa_spending[16,"validity"] <- (nrow(funding_agency_code_valid)/nrow(usa_spending_transaction))*100

#If awarding_agency_name and DEPARTMENT_NAME do not match, then the name was invalid:

funding_agency_name_valid <- frame_funding_agency[which(frame_funding_agency$matches == TRUE & 
                                                          is.na(frame_funding_agency$funding_agency_name) == FALSE), "funding_agency_name"]
field_profile_usa_spending[17,"validity"] <- (nrow(funding_agency_name_valid)/nrow(usa_spending_transaction))*100

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##funding_sub_agency_code & funding_sub_agency_name

#Same process as the previous two:

glossary_funding_sub_agency <- glossary_agency %>% select(AGENCY_CODE, AGENCY_NAME)
glossary_funding_sub_agency <- unique(glossary_funding_sub_agency)

#Create sub_agency_frame

frame_funding_sub_agency <- usa_spending_transaction %>% select(funding_sub_agency_code, funding_sub_agency_name)
frame_funding_sub_agency$funding_sub_agency_code <- as.character(frame_funding_sub_agency$funding_sub_agency_code)

#Fix "U.S." versus "US" discrepancy:

period_remover <- function(sub_agency_name){
  
  remove_period <- ifelse(str_detect(sub_agency_name, "U.S.") == TRUE, gsub("U\\.S\\.", "US", sub_agency_name), sub_agency_name)
  
  return(remove_period)
  
}

frame_funding_sub_agency$funding_sub_agency_name <- period_remover(frame_funding_sub_agency$funding_sub_agency_name)

#Join sub_agency_frame with glossary and test for matches:

frame_funding_sub_agency <- left_join(frame_funding_sub_agency, glossary_funding_sub_agency, by = c("funding_sub_agency_code" = "AGENCY_CODE"))
frame_funding_sub_agency$matches <- mapply(grepl, paste0('\\b', frame_funding_sub_agency$funding_sub_agency_name, '\\b'), frame_funding_sub_agency$AGENCY_NAME)

#If there are any missing codes or N/A in AGENCY_CODE then the code was invalid:

funding_sub_agency_code_valid <- frame_funding_sub_agency[which(
  is.na(frame_funding_sub_agency$funding_sub_agency_code) == FALSE & 
  is.na(frame_funding_sub_agency$AGENCY_NAME) == FALSE), "funding_sub_agency_code"]
field_profile_usa_spending[18,"validity"] <- (nrow(funding_sub_agency_code_valid)/nrow(usa_spending_transaction))*100

#If awarding_agency_name and DEPARTMENT_NAME do not match, then the name was invalid:

funding_sub_agency_name_valid <- frame_funding_sub_agency[which(
  frame_funding_sub_agency$matches == TRUE & 
  is.na(frame_funding_sub_agency$funding_sub_agency_name) == FALSE), "funding_sub_agency_name"]
field_profile_usa_spending[19,"validity"] <- (nrow(funding_sub_agency_name_valid)/nrow(usa_spending_transaction))*100

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##awarding_office_code & funding_office_code

v_office_code <- function(database, column){
  
  valid_entries <- database[which(str_detect(database[[column]], "^[A-Z0-9]+$") == TRUE), column]
  
  percent_valid <- nrow(valid_entries)/nrow(database)*100
  column_number <- match(column, colnames(database))
  
  field_profile_usa_spending[column_number, "validity"] <- percent_valid
  
  assign('field_profile_usa_spending', field_profile_usa_spending, envir=.GlobalEnv)
  
}

v_office_code(usa_spending_transaction, "awarding_office_code")
v_office_code(usa_spending_transaction, "funding_office_code")

##awarding_office_name & funding_office_name

v_office_name <- function(database, column){
  
  valid_entries <- database[which(str_detect(database[[column]], "^[a-zA-Z ]+$") == TRUE), column]
  
  percent_valid <- nrow(valid_entries)/nrow(database)*100
  column_number <- match(column, colnames(database))
  
  field_profile_usa_spending[column_number, "validity"] <- percent_valid
  
  assign('field_profile_usa_spending', field_profile_usa_spending, envir=.GlobalEnv)
  
}

v_office_name(usa_spending_transaction, "awarding_office_name")
v_office_name(usa_spending_transaction, "funding_office_name")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##recipient_duns & recipient_parent_duns

#DUNS checker:

v_duns_number <- function(database, column){
  
  valid_entries <- database[which(str_detect(database[[column]], "^[0-9]{9}$") == TRUE), column]
  
  percent_valid <- nrow(valid_entries)/nrow(database)*100
  column_number <- match(column, colnames(database))
  
  field_profile_usa_spending[column_number, "validity"] <- percent_valid
  
  assign('field_profile_usa_spending', field_profile_usa_spending, envir=.GlobalEnv)
  
}

v_duns_number(usa_spending_transaction, "recipient_duns")
v_duns_number(usa_spending_transaction, "recipient_parent_duns")


##recipient_name

recipient_name_length <- nchar(usa_spending_transaction$recipient_name)
#Huge spike at ~45 chars, examine:
recipient_name_long <- usa_spending_transaction[which(recipient_name_length >= 45), "recipient_name"]
#~18000 names that are exactly that length. Some are valid, but vast majority are cut off (e.g. "WESTERN KENTUCKY UNIVERSITY RESEARCH FOUNDATI")
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

##recipient_parent_name

recipient_parent_name_length <- nchar(usa_spending_transaction$recipient_parent_name)
#Several at 20 chars
recipient_parent_name_test <- usa_spending_transaction[which(recipient_parent_name_length == 20), "recipient_parent_name"]
#No obvious cutoff points, everything looks fine.
#Screen for numbers
recipient_parent_name_odd <- usa_spending_transaction[which(
  grepl("[^a-zA-Z. /(),&'360-]", usa_spending_transaction$recipient_parent_name) == TRUE), "recipient_parent_name"]
#None of these odd ones actually look invalid either.

field_profile_usa_spending[25,"validity"] <- field_profile_usa_spending[25, "completeness"]

##recipient_country_code & recipient_country_name

#Use glossary_country to verify if names and codes of countries are actual names with the correct corresponding code:

glossary_country$country_name <- toupper(glossary_country$country_name)

frame_country <- select(usa_spending_transaction, recipient_country_name, recipient_country_code)
colnames(frame_country)[colnames(frame_country)=="recipient_country_name"] <- "country_name"
colnames(frame_country)[colnames(frame_country)=="recipient_country_code"] <- "country_code"
frame_country <- left_join(frame_country, glossary_country)

#Created new data frame with three columns: country_name, country_code and country_abb. 
#country_name has been matched against the glossary, so if the recipient_country_name exists,
#then the glossary will match it with a country_abb. 

#If recipient_country_name is not valid, then an N/A will appear in country_abb.

recipient_country_name_valid <- frame_country[which(is.na(frame_country$country_name) == FALSE & is.na(frame_country$country_abb) == FALSE), "country_name"]
field_profile_usa_spending[27,"validity"] <- (nrow(recipient_country_name_valid)/nrow(usa_spending_transaction))*100

#Check if recipient_country_code and country_abb match:

frame_country$matches <- mapply(grepl, paste0('\\b', frame_country$country_code, '\\b'), frame_country$country_abb)

recipient_country_code_valid <- frame_country[which(frame_country$matches == TRUE & is.na(frame_country$country_abb) == FALSE), "country_name"]
field_profile_usa_spending[26,"validity"] <- (nrow(recipient_country_code_valid)/nrow(usa_spending_transaction))*100

##recipient_address_line_1

#Start with number or P.O. box, then move onto letters

v_address <- function(database, column){
  
  valid_entries <- database[which(str_detect(database[[column]], "^[0-9A-]+[ ][0-9a-zA-Z -\\.\\/;]+$") == TRUE |
                                  str_detect(database[[column]], "^[O-P \\.]+BOX[ ][0-9]+") == TRUE |
                                  str_detect(database[[column]], "^POST[ ]OFFICE[ ]BOX[ ][0-9]+") == TRUE |
                                  str_detect(database[[column]], "^BOX[ ][0-9]+") == TRUE |
                                  str_detect(database[[column]], "^CAMPUS[ ]BOX[ ][0-9]+") == TRUE), column]
  
  percent_valid <- nrow(valid_entries)/nrow(database)*100
  column_number <- match(column, colnames(database))
  
  field_profile_usa_spending[column_number, "validity"] <- percent_valid
  
  assign('field_profile_usa_spending', field_profile_usa_spending, envir=.GlobalEnv)
  
}



v_address(usa_spending_transaction, "recipient_address_line_1")
v_address(usa_spending_transaction, "recipient_address_line_2")


##recipient_address_line_2

##recipient_city_code

#FIPS codes. Must be a 5 digit number:

v_city_code <- function(database, column){
  
  valid_entries <- database[which(str_detect(database[[column]], "^[0-9]{5}$") == TRUE), column]
  
  percent_valid <- nrow(valid_entries)/nrow(database)*100
  column_number <- match(column, colnames(database))
  
  field_profile_usa_spending[column_number, "validity"] <- percent_valid
  
  assign('field_profile_usa_spending', field_profile_usa_spending, envir=.GlobalEnv)
  
}

v_city_code(usa_spending_transaction, "recipient_city_code")

##recipient_city_name & primary_place_of_performance_city_name

v_city_name <- function(database, column){
  
  valid_entries <- database[which(str_detect(database[[column]], "^[a-zA-Z -\\.]+$") == TRUE), column]
  
  percent_valid <- nrow(valid_entries)/nrow(database)*100
  column_number <- match(column, colnames(database))
  
  field_profile_usa_spending[column_number, "validity"] <- percent_valid
  
  assign('field_profile_usa_spending', field_profile_usa_spending, envir=.GlobalEnv)
  
}

v_city_name(usa_spending_transaction, "recipient_city_name")
v_city_name(usa_spending_transaction, "primary_place_of_performance_city_name")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##recipient_state_code, recipient_state_name & recipient_zip_code

glossary_state_zip$state_name <- toupper(glossary_state_zip$state_name)

frame_state <- select(usa_spending_transaction, recipient_state_name, recipient_state_code, recipient_zip_code)
colnames(frame_state)[colnames(frame_state)=="recipient_state_name"] <- "state_name"
colnames(frame_state)[colnames(frame_state)=="recipient_state_code"] <- "state_code"
colnames(frame_state)[colnames(frame_state)=="recipient_zip_code"] <- "zip_code"

frame_state <- left_join(frame_state, glossary_state_zip)

#If state name exists, then corresponding state_abbr will exist:

recipient_state_name_valid <- frame_state[which(is.na(frame_state$state_name) == FALSE & is.na(frame_state$state_abbr) == FALSE), "state_name"]
field_profile_usa_spending[33,"validity"] <- (nrow(recipient_state_name_valid)/nrow(usa_spending_transaction))*100

#Check if recipient_state_code and state_abbr match:

frame_state$matches_state_code <- mapply(grepl, paste0('\\b', frame_state$state_code, '\\b'), frame_state$state_abbr)

recipient_state_code_valid <- frame_state[which(frame_state$matches_state_code == TRUE & is.na(frame_state$state_abbr) == FALSE), "state_name"]
field_profile_usa_spending[32,"validity"] <- (nrow(recipient_state_code_valid)/nrow(usa_spending_transaction))*100

#Check if recipient_zip_code is within proper range

frame_state$three_digit_zip <- substr(frame_state$zip_code, start = 1, stop = 3)

check_zip_range <- function(given_zip){
  
  checked_zip <- ifelse(frame_state$zip_code_min <= given_zip & frame_state$zip_code_max >= given_zip, TRUE, 
                        
                        ifelse(frame_state$zip_code_min_extra <= given_zip & frame_state$zip_code_max_extra >= given_zip, TRUE, FALSE)
                        
  )
  
  return(checked_zip)    
  
}

frame_state$matches_zip_code <- check_zip_range(frame_state$three_digit_zip)

recipient_zip_code_valid <- frame_state[which(frame_state$matches_zip_code == TRUE & is.na(frame_state$zip_code) == FALSE), "state_name"]
field_profile_usa_spending[34,"validity"] <- (nrow(recipient_state_code_valid)/nrow(usa_spending_transaction))*100

##primary_place_of_performance_country_code & primary_place_of_performance_country_name

glossary_country$country_name <- toupper(glossary_country$country_name)

frame_country_performance <- select(usa_spending_transaction, primary_place_of_performance_country_name, primary_place_of_performance_country_code)
colnames(frame_country_performance)[colnames(frame_country_performance)=="primary_place_of_performance_country_name"] <- "country_name"
colnames(frame_country_performance)[colnames(frame_country_performance)=="primary_place_of_performance_country_code"] <- "country_code"
frame_country_performance <- left_join(frame_country_performance, glossary_country)

primary_place_country_name_valid <- frame_country_performance[which(is.na(frame_country_performance$country_name) == FALSE & 
                                                                      is.na(frame_country_performance$country_abb) == FALSE), "country_name"]
field_profile_usa_spending[36,"validity"] <- (nrow(recipient_country_name_valid)/nrow(usa_spending_transaction))*100

frame_country_performance$matches <- mapply(grepl, paste0('\\b', frame_country_performance$country_code, '\\b'), frame_country_performance$country_abb)

primary_place_country_code_valid <- frame_country_performance[which(frame_country_performance$matches == TRUE & 
                                                                      is.na(frame_country_performance$country_abb) == FALSE), "country_name"]
field_profile_usa_spending[35,"validity"] <- (nrow(recipient_country_code_valid)/nrow(usa_spending_transaction))*100

##primary_place_of_performance_code

#Looks like it is the city FIPS code, so five numbers. Also could be "00FORGN"

v_primary_place_of_performance_code <- function(database, column){
  
  valid_entries <- database[which(str_detect(database[[column]], "^[0-9]{7}$") == TRUE | 
                                  str_detect(database[[column]], "00FORGN") == TRUE  ), column]
  
  percent_valid <- nrow(valid_entries)/nrow(database)*100
  column_number <- match(column, colnames(database))
  
  field_profile_usa_spending[column_number, "validity"] <- percent_valid
  
  assign('field_profile_usa_spending', field_profile_usa_spending, envir=.GlobalEnv)
  
}

v_primary_place_of_performance_code(usa_spending_transaction, "primary_place_of_performance_code")


##primary_place_of_performance_state_name & primary_place_of_performance_zip_4

glossary_state_zip$state_name <- toupper(glossary_state_zip$state_name)

frame_state_performance <- select(usa_spending_transaction, primary_place_of_performance_state_name, primary_place_of_performance_zip_4)
colnames(frame_state_performance)[colnames(frame_state_performance)=="primary_place_of_performance_state_name"] <- "state_name"
colnames(frame_state_performance)[colnames(frame_state_performance)=="primary_place_of_performance_zip_4"] <- "zip_code"

frame_state_performance <- left_join(frame_state_performance, glossary_state_zip)

#If state name exists, then corresponding state_abbr will exist:

primary_place_of_performance_state_name_valid <- frame_state_performance[which(is.na(frame_state_performance$state_name) == FALSE & 
                                                                                 is.na(frame_state_performance$state_abbr) == FALSE), "state_name"]
field_profile_usa_spending[39,"validity"] <- (nrow(primary_place_of_performance_state_name_valid)/nrow(usa_spending_transaction))*100

#Check if primary_place_of_performance_zip_4 is within proper range

frame_state_performance$three_digit_zip <- substr(frame_state_performance$zip_code, start = 1, stop = 3)

check_zip_range <- function(given_zip){
  
  checked_zip <- ifelse(frame_state_performance$zip_code_min <= given_zip & frame_state_performance$zip_code_max >= given_zip, TRUE, 
                        
                        ifelse(frame_state_performance$zip_code_min_extra <= given_zip & frame_state_performance$zip_code_max_extra >= given_zip, TRUE, FALSE)
                        
  )
  
  return(checked_zip)    
  
}

frame_state_performance$matches_zip_code <- check_zip_range(frame_state_performance$three_digit_zip)

primary_place_of_performance_zip_4_valid <- frame_state_performance[which(frame_state_performance$matches_zip_code == TRUE & 
                                                                            is.na(frame_state_performance$zip_code) == FALSE), "state_name"]
field_profile_usa_spending[40,"validity"] <- (nrow(primary_place_of_performance_zip_4_valid)/nrow(usa_spending_transaction))*100

##cfda_number

v_cfda_number <- function(database, column){
  
  valid_entries <- database[which(database[[column]] != 000 & usa_spending_transaction[[column]] != 999 &
                                    str_detect(database[[column]], "^[0-9]{2}\\.[0-9]{3}$") == TRUE), column]
  
  percent_valid <- nrow(valid_entries)/nrow(database)*100
  column_number <- match(column, colnames(database))
  
  field_profile_usa_spending[column_number, "validity"] <- percent_valid
  
  assign('field_profile_usa_spending', field_profile_usa_spending, envir=.GlobalEnv)
  
}

v_cfda_number(usa_spending_transaction, "cfda_number")

#cfda_number_invalid <- usa_spending_transaction[which(
   #usa_spending_transaction$cfda_number == 000 | usa_spending_transaction$cfda_number == 999 | 
   #grepl("[0-9]{2}\\.[0-9]{3}$", usa_spending_transaction$cfda_number) == FALSE), "cfda_number"]


##cfda_title

v_cfda_title <- function(database, column){
  
  valid_entries <- database[which(str_detect(database[[column]], "^[0-9a-zA-Z -\\. \\(\\)]+$") == TRUE), column]
  
  percent_valid <- nrow(valid_entries)/nrow(database)*100
  column_number <- match(column, colnames(database))
  
  field_profile_usa_spending[column_number, "validity"] <- percent_valid
  
  assign('field_profile_usa_spending', field_profile_usa_spending, envir=.GlobalEnv)
  
}

v_cfda_title(usa_spending_transaction, "cfda_title")

##assistance_type_code

#02 Block Grant, 03 Formula Grant, 04 Project Grant, 05 Cooperative Agreement

v_assistance_type_code <- function(database, column){
  
  valid_entries <- database[which(str_detect(database[[column]], "^[0]{1}[2-5]{1}$") == TRUE), column]
  
  percent_valid <- nrow(valid_entries)/nrow(database)*100
  column_number <- match(column, colnames(database))
  
  field_profile_usa_spending[column_number, "validity"] <- percent_valid
  
  assign('field_profile_usa_spending', field_profile_usa_spending, envir=.GlobalEnv)
  
}

v_assistance_type_code(usa_spending_transaction, "assistance_type_code")

##assistance_type_description
#Only three different entries here, all are valid.

field_profile_usa_spending[44,"validity"] <- field_profile_usa_spending[44,"completeness"]

##award_description

v_description <- function(database, column){
  
  valid_entries <- database[which(str_detect(database[[column]], "^[a-zA-Z -\\.:\\,]+$") == TRUE), column]
  
  percent_valid <- nrow(valid_entries)/nrow(database)*100
  column_number <- match(column, colnames(database))
  
  field_profile_usa_spending[column_number, "validity"] <- percent_valid
  
  assign('field_profile_usa_spending', field_profile_usa_spending, envir=.GlobalEnv)
  
}

v_description(usa_spending_transaction, "award_description")

##record_type_code
#Record type may be ‘Individual’ or ‘Aggregate’. Aggregate reporting is for payments to individuals identifiable by a geographical unit. 
#Individual reporting consists of “action-by-action” transactions. Aggregate is marked as 1. Individual is marked as 2.
#Because we are looking at transactions, all of these should be equal to 2:

v_record_type_code <- function(database, column){
  
  valid_entries <- database[which(str_detect(database[[column]], "^[2]{1}$") == TRUE), column]
  
  percent_valid <- nrow(valid_entries)/nrow(database)*100
  column_number <- match(column, colnames(database))
  
  field_profile_usa_spending[column_number, "validity"] <- percent_valid
  
  assign('field_profile_usa_spending', field_profile_usa_spending, envir=.GlobalEnv)
  
}

v_record_type_code(usa_spending_transaction, "record_type_code")

##record_type_description

#The only value here is "NON-AGGREGATE RECORD" so whatever is filled in is valid, but this column doesn't seem useful overall.

field_profile_usa_spending[47,"validity"] <- field_profile_usa_spending[47,"completeness"]

##last_modified_date

v_last_modified_date <- function(database, column){

  valid_entries <- database[which(str_detect(database[[column]], "^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9][ ][0-9]{2}:[0-9]{2}:[0-9]{2}") == TRUE), column]
  
  percent_valid <- nrow(valid_entries)/nrow(database)*100
  column_number <- match(column, colnames(database))
  
  field_profile_usa_spending[column_number, "validity"] <- percent_valid
  
  assign('field_profile_usa_spending', field_profile_usa_spending, envir=.GlobalEnv)
  
}

v_last_modified_date(usa_spending_transaction, "last_modified_date")


write_csv(field_profile_usa_spending, 'field_profile_usa_spending.csv')
