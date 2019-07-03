library(R.utils)
library(tidyverse)
library(data.table)
library(plyr)
options(scipen=999)

usa_spending_transaction <- read_csv("data/working/2016-multiple-usaspending-transaction-6_agencies_field_select.csv")

##Creating Data Frame

field_profile_usa_spending <- data.frame(field = as.character(colnames(usa_spending_transaction)), 
                            completeness = as.numeric(NA),
                            validity = as.numeric(NA), consistency = as.numeric(NA), 
                            uniqueness = as.numeric(NA))

field_profile_usa_spending$completeness <- mutate(field_profile_usa_spending, is.na(usa_spending_transaction))

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
#https://datalab.usaspending.gov/assets/analyst-guide-1-2.pdf
#https://fas.org/sgp/crs/misc/R44027.pdf

##award_id_fain

##modification_number

##award_id_uri

##federal_action_obligation

##non_federal_funding_amount

##total_funding_amount

##action_date
action_date_valid <- usa_spending_transaction[which(grepl("^201[5-6]-[0-1][0-9]-[0-3][0-9]", usa_spending_transaction$action_date) == TRUE), "action_date"]
field_profile_usa_spending[8,"validity"] <- (nrow(action_date_valid)/nrow(usa_spending_transaction))*100


##period_of_performance_start_date
performance_start_date_valid <- usa_spending_transaction[which(grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]", usa_spending_transaction$period_of_performance_start_date) == TRUE), "period_of_performance_start_date"]
field_profile_usa_spending[9,"validity"] <- (nrow(performance_start_date_valid)/nrow(usa_spending_transaction))*100


##period_of_performance_current_end_date
performance_end_date_valid <- usa_spending_transaction[which(grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]", usa_spending_transaction$period_of_performance_current_end_date) == TRUE), "period_of_performance_current_end_date"]
field_profile_usa_spending[10,"validity"] <- (nrow(performance_end_date_valid)/nrow(usa_spending_transaction))*100


##awarding_agency_code

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

##recipient_name

##recipient_parent_duns

##recipient_parent_name

##recipient_country_code

##recipient_country_name

##recipient_address_line_1

##recipient_address_line_2

##recipient_city_code

##recipient_city_name

##recipient_state_code

##recipient_state_name

##recipient_zip_code

##primary_place_of_performance_country_code

##primary_place_of_performance_country_name

##primary_place_of_performance_code

##primary_place_of_performance_city_name

##primary_place_of_performance_state_name

##primary_place_of_performance_zip_4

##cfda_number

cfda_number_valid <- usa_spending_transaction[which(usa_spending_transaction$cfda_number != 000 & usa_spending_transaction$cfda_number != 999 & 
                                            grepl("[0-9]{2}\\.[0-9]{3}$", usa_spending_transaction$cfda_number) == TRUE), "cfda_number"]

field_profile_usa_spending[42,"validity"] <- (nrow(cfda_number_valid)/nrow(usa_spending_transaction))*100


##cfda_title

##assistance_type_code

#assistance_type_description

##award_description

##record_type_code

##record_type_description

##last_modified_date