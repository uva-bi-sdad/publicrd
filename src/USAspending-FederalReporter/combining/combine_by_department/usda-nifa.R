library(R.utils)
library(tidyverse)
library(data.table)
library(dplyr)

##Standardizing CFDA codes:

#Read in Fed Reporter

usda_fed_reporter_nifa <- read_csv("data/working/usda_fed-reporter_2010-2016.csv") %>% 
  filter(str_detect(AGENCY, "NIFA"))

usda_fed_reporter_nifa_project_numbers <- usda_fed_reporter_nifa %>% 
  select(PROJECT_NUMBER)
usda_fed_reporter_nifa_project_numbers$fedmatch <- TRUE

#Read in USAspending

usda_usa_spending_grants_nifa <- read_csv("data/working/combine_by_department/USDA/usda_usa-spending_grants_2016.csv") %>% 
  filter(str_detect(awarding_sub_agency_name, "NATIONAL INSTITUTE OF FOOD AND AGRICULTURE")) %>% 
  select(-X1)

#Read in glossary

#glossary <- read_csv("data/working/NIFA.csv") %>% 
#  filter(str_detect(TYPE, "PROJECT")) %>% 
#  select(`Project Title`, `Accession Number`, `Award Number`, `Recipient Organization`)

#colnames(glossary)[colnames(glossary)=="Project Title"] <- "project_title_glossary"
#colnames(glossary)[colnames(glossary)=="Accession Number"] <- "accession_number_glossary"
#colnames(glossary)[colnames(glossary)=="Award Number"] <- "award_number_glossary"
#colnames(glossary)[colnames(glossary)=="Recipient Organization"] <- "recipient_organization_glossary"
#glossary$project_title_glossary <- toupper(glossary$project_title_glossary)

#USAspending contains unhyphenated award numbers followed by a decimal point and last digit, so get rid of this last decimal point.

decimal_remover <- function(column_with_decimals){
  
  decimal_removed <- ifelse(str_detect(column_with_decimals, "\\.") == TRUE, 
                            gsub("\\.[0-9]$", "", column_with_decimals), 
                            column_with_decimals)
  
  return(decimal_removed)
  
}

usda_usa_spending_grants_nifa$fain_matcher <- decimal_remover(usda_usa_spending_grants_nifa$award_id_fain)

#Adding hyphens to these strings in USAspending to match with Fed Reporter Award Numbers:

hyphen_adder <- function(column_without_hyphens){
  
  hyphens_added <- ifelse(str_detect(column_without_hyphens, "[0-9]{13}") == TRUE, 
                          str_replace(column_without_hyphens,"(\\d{4})(\\d{5})(\\d{5})$","\\1-\\2-\\3"),
                          column_without_hyphens)
  
  return(hyphens_added)
  
}

usda_usa_spending_grants_nifa$readable_award_number <- hyphen_adder(usda_usa_spending_grants_nifa$fain_matcher)
usda_usa_spending_grants_nifa$fain_matcher <- NULL

usda_usa_spending_grants_nifa_project_numbers <- usda_usa_spending_grants_nifa %>% select(readable_award_number)
usda_usa_spending_grants_nifa_project_numbers$fedmatch <- TRUE

#Add Glossary to USAspending

#glossary_with_usa_spending <- left_join(usda_usa_spending_grants_nifa, glossary, by = c("readable_award_number" = "award_number_glossary"))

#Add Glossary to Fed Reporter

#glossary_with_fed_reporter <- left_join(usda_fed_reporter_nifa, glossary, by = c("PROJECT_NUMBER" = "accession_number_glossary"))

#Shows that all entries with accession codes in Fed Reporter *DO NOT* have a corresponding award number, so these cannot 
#be linked to USAspending. Thus, we merge the award codes that we do have:

#usa_spending_fed_reporter_nifa <- merge(x = usda_fed_reporter_nifa, y = usda_usa_spending_grants_nifa, 
#                                        by.x = c("PROJECT_NUMBER"), by.y = c("readable_award_number"), all = TRUE)

#usa_spending_fed_reporter_nifa_matches <- merge(x = usda_fed_reporter_nifa, y = usda_usa_spending_grants_nifa, 
#                                                by.x = c("PROJECT_NUMBER"), by.y = c("readable_award_number"))

#Match records between databases:

usda_nifa_fed_reporter_2016_matchflag <- left_join(usda_fed_reporter_nifa, usda_usa_spending_grants_nifa_project_numbers, 
                                                   by = c("PROJECT_NUMBER" = "readable_award_number"))
usda_nifa_fed_reporter_2016_matchflag$fedmatch <- ifelse(is.na(usda_nifa_fed_reporter_2016_matchflag$fedmatch) == TRUE, 
                                                         FALSE, 
                                                         TRUE)
usda_nifa_fed_reporter_2016_matchflag$resflag <- usda_nifa_fed_reporter_2016_matchflag$fedmatch


usda_nifa_usa_spending_grants_2016_matchflag <- left_join(usda_usa_spending_grants_nifa, usda_fed_reporter_nifa_project_numbers, 
                                                          by = c("readable_award_number" = "PROJECT_NUMBER"))
usda_nifa_usa_spending_grants_2016_matchflag$fedmatch <- ifelse(is.na(usda_nifa_usa_spending_grants_2016_matchflag$fedmatch) == TRUE, 
                                                                FALSE, 
                                                                TRUE)
usda_nifa_usa_spending_grants_2016_matchflag$resflag <- usda_nifa_usa_spending_grants_2016_matchflag$fedmatch

#Export fixed data:
#write.csv(usda_nifa_fed_reporter_2016_matchflag, "usda_nifa_fed-reporter_2016_matchflag.csv", row.names = FALSE)
#write.csv(usda_nifa_usa_spending_grants_2016_matchflag, "usda_nifa_usa-spending_grants_2016_matchflag.csv", row.names = FALSE)

#Percent of matches in each one:

percent_match_usa_spending <- 100*length(
  usda_nifa_usa_spending_grants_2016_matchflag$fedmatch[usda_nifa_usa_spending_grants_2016_matchflag$fedmatch==T])/
  nrow(usda_nifa_usa_spending_grants_2016_matchflag)

percent_match_fed_reporter <- 100*length(
  usda_nifa_fed_reporter_2016_matchflag$fedmatch[usda_nifa_fed_reporter_2016_matchflag$fedmatch==T])/
  nrow(usda_nifa_fed_reporter_2016_matchflag)

#Recombine into one USDA File

#I could not find a way to match ARS or FS between these two databases. Generally, if an award number was not listed in Fed Reporter,
#there was no award number at all, which means that it cannot be found in USAspending.

usda_usa_spending_grants <- read_csv("data/working/combine_by_department/USDA/usda_usa-spending_grants_2016.csv") %>% 
  select(-X1)

merger_usda_nifa_usa_spending_grants_2016 <- usda_nifa_usa_spending_grants_2016_matchflag %>% 
  select(award_id_fain, fedmatch, resflag)

usda_usa_spending_grants_2016 <- left_join(usda_usa_spending_grants, merger_usda_nifa_usa_spending_grants_2016)
usda_usa_spending_grants_2016[c("fedmatch", "resflag")][is.na(usda_usa_spending_grants_2016[c("fedmatch", "resflag")])] <- FALSE

write.csv(usda_usa_spending_grants_2016, "usda_usa-spending_grants_2016_matchflag.csv", row.names = FALSE)
