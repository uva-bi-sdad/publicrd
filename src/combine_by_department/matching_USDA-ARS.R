library(R.utils)
library(tidyverse)
library(data.table)
library(dplyr)

##Standardizing CFDA codes:

#Read in Fed Reporter

usda_fed_reporter_ars <- read_csv("data/working/combine_by_department/USDA/usda_fed-reporter_2016.csv") %>% 
  filter(str_detect(AGENCY, "ARS")) %>% 
  select(-X1)

#Read in USAspending

usda_usa_spending_grants_ars <- read_csv("data/working/combine_by_department/USDA/usda_usa-spending_grants_2016.csv") %>% 
  filter(str_detect(awarding_sub_agency_name, "AGRICULTURAL RESEARCH SERVICE")) %>% 
  select(-X1)

#Read in Glossary

glossary <- read_csv("data/working/ARS.csv") %>% 
  filter(str_detect(TYPE, "PROJECT")) %>% 
  select(`Project Title`, `Accession Number`, `Award Number`, `Recipient Organization`)

colnames(glossary)[colnames(glossary)=="Project Title"] <- "project_title_glossary"
colnames(glossary)[colnames(glossary)=="Accession Number"] <- "accession_number_glossary"
colnames(glossary)[colnames(glossary)=="Award Number"] <- "award_number_glossary"
colnames(glossary)[colnames(glossary)=="Recipient Organization"] <- "recipient_organization_glossary"
glossary$project_title_glossary <- toupper(glossary$project_title_glossary)

#Extract Accession Numbers from Fed Reporter:

prefix_deleter <- function(column_with_prefixes){
  
  prefix_deleted <- ifelse(str_detect(column_with_prefixes, "ARS\\-") == TRUE, 
                          str_replace(column_with_prefixes,"(ARS\\-)(\\d{7})$","\\2"),
                          column_with_prefixes)
  
  return(prefix_deleted)
  
}

usda_fed_reporter_ars$accession_number <- prefix_deleter(usda_fed_reporter_ars$PROJECT_NUMBER)

#Obtain corresponding award numbers from accession numbers

glossary_accession_to_award <- glossary %>% select(accession_number_glossary, award_number_glossary)
glossary_usda_fed_reporter_ars <- left_join(usda_fed_reporter_ars, glossary_accession_to_award, by = c("accession_number" = "accession_number_glossary"))
glossary_usda_usa_spending_ars <- left_join(usda_usa_spending_grants_ars, glossary_accession_to_award, by = c("award_id_fain" = "accession_number_glossary"))

#No corresponding 