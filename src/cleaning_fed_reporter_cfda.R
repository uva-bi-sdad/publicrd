library(R.utils)
library(tidyverse)
library(data.table)
library(dplyr)

##Standardizing CFDA codes:

#Read in dataset and CFDA Glossary

fed_reporter <- read_csv("data/original/2016-data_multiple_fed-reporter.csv")
CFDA_glossary <- read_csv("glossary/CFDA_glossary_final.csv")

#List of valid CFDA codes before fix:

cfda_code_valid_before <- fed_reporter[which(grepl("[0-9]{2}\\.[0-9]{3}$", fed_reporter$CFDA_CODE) == TRUE), "CFDA_CODE"]

#Add Glossary to Main Table and specify relevant matching column:

colnames(CFDA_glossary)[colnames(CFDA_glossary)=="department_abb"] <- "DEPARTMENT"
CFDA_glossary$agency_name <- NULL
CFDA_glossary$CFDA <- NULL

data_fixed <- left_join(fed_reporter, CFDA_glossary, by = "DEPARTMENT")

##Cleaning Function:

#If CFDA is only three digits, then paste department digits from glossary before it, separated by period.
#Else reprint what is already there.

fixing_cfda_code <- function(cfda_project_code){
  
  fixed_cfda <-ifelse(cfda_project_code != 000 & cfda_project_code != 999 & grepl("^[0-9]{3}$", cfda_project_code) == TRUE,
                      paste(data_fixed$department_digits,cfda_project_code, sep = "."),
                      cfda_project_code)
  
  return(fixed_cfda)
}

#Update Data:

data_fixed$CFDA_CODE <- fixing_cfda_code(data_fixed$CFDA_CODE)
data_fixed$department_digits <- NULL

#Check how many CFDA codes were fixed:

cfda_code_valid_after <- data_fixed[which(grepl("[0-9]{2}\\.[0-9]{3}$", data_fixed$CFDA_CODE) == TRUE), "CFDA_CODE"]

#Export fixed data:
write.csv(data_fixed, "fed_reporter_fixed_cfda.csv", row.names = FALSE)