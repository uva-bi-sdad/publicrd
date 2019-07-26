library(data.table)
library(tidyverse)
library(dplyr)
library(stringr)

#Load USAspending Contracts resflag and delete current resflag column:
usa_spending_contracts_2016_resflag <- read_csv("data/working/combined_usa-spending/usa-spending_contracts_2016_resflag.csv") %>%
  select(-resflag)

#Create new column for just first character:
usa_spending_contracts_2016_resflag$psc_first_char <- substr(usa_spending_contracts_2016_resflag$product_or_service_code, 1,1)

#Function to mark entries with "A" as true. If the letter "A" is present, then new column will be marked TRUE. If no "A" then marked FALSE:

research_marker <- function(psc_column){
  
  a_marked_true <- ifelse(str_detect(psc_column, "^A$") == TRUE, 
                          TRUE,
                          FALSE)
  
  return(a_marked_true)
  
}

#Create column resflag through this function
usa_spending_contracts_2016_resflag$resflag <- research_marker(usa_spending_contracts_2016_resflag$psc_first_char)

#Write .csv with slightly different name
write.csv(usa_spending_contracts_2016_resflag, "data/working/combined_usa-spending/usa-spending_contracts_2016_resflag_new.csv", row.names = FALSE)

