library(R.utils)
library(tidyverse)
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)

##Get Departments:

X2008_data_multiple_fed_reporter <- read_csv("data/original/2008-data_multiple_fed-reporter.csv")
X2009_data_multiple_fed_reporter <- read_csv("data/original/2009-data_multiple_fed-reporter.csv")
X2010_data_multiple_fed_reporter <- read_csv("data/original/2010-data_multiple_fed-reporter.csv")
X2011_data_multiple_fed_reporter <- read_csv("data/original/2011-data_multiple_fed-reporter.csv")
X2012_data_multiple_fed_reporter <- read_csv("data/original/2012-data_multiple_fed-reporter.csv")
X2013_data_multiple_fed_reporter <- read_csv("data/original/2013-data_multiple_fed-reporter.csv")
X2014_data_multiple_fed_reporter <- read_csv("data/original/2014-data_multiple_fed-reporter.csv")
X2015_data_multiple_fed_reporter <- read_csv("data/original/2015-data_multiple_fed-reporter.csv")
X2016_data_multiple_fed_reporter <- read_csv("data/original/2016-data_multiple_fed-reporter.csv")
X2017_data_multiple_fed_reporter <- read_csv("data/original/2017-data_multiple_fed-reporter.csv")
X2018_data_multiple_fed_reporter <- read_csv("data/original/2018-data_multiple_fed-reporter.csv")

departments_federal_reporter <- rbind(
  X2008_data_multiple_fed_reporter,
  X2009_data_multiple_fed_reporter,
  X2010_data_multiple_fed_reporter,
  X2011_data_multiple_fed_reporter,
  X2012_data_multiple_fed_reporter,
  X2013_data_multiple_fed_reporter,
  X2014_data_multiple_fed_reporter,
  X2015_data_multiple_fed_reporter,
  X2016_data_multiple_fed_reporter,
  X2017_data_multiple_fed_reporter,
  X2018_data_multiple_fed_reporter
)

shortened_departments_federal_reporter <- departments_federal_reporter %>%
  select(PROJECT_ID, DEPARTMENT, AGENCY, IC_CENTER, PROJECT_NUMBER, PROJECT_TITLE, PROJECT_TERMS, CONTACT_PI_PROJECT_LEADER, OTHER_PIS, ORGANIZATION_NAME, CFDA_CODE, FY_TOTAL_COST)

#Fix CFDA Codes:

CFDA_glossary <- read_csv("glossary/CFDA_glossary_final.csv") %>%
  select(department_abb, department_digits)
colnames(CFDA_glossary)[colnames(CFDA_glossary)=="department_abb"] <- "DEPARTMENT"

data_fixed <- left_join(shortened_departments_federal_reporter, CFDA_glossary, by = "DEPARTMENT")

##Cleaning Function:

fixing_cfda_code <- function(cfda_project_code){
  
  fixed_cfda <-ifelse(cfda_project_code != 000 & cfda_project_code != 999 & grepl("^[0-9]{3}$", cfda_project_code) == TRUE,
                      paste(data_fixed$department_digits,cfda_project_code, sep = "."),
                      cfda_project_code)
  
  return(fixed_cfda)
}

#Update Data:

data_fixed$CFDA_CODE <- fixing_cfda_code(data_fixed$CFDA_CODE)
data_fixed$department_digits <- NULL

#Join federal reporter columns to abstracts:

abstracts_federal_reporter_first_last <- read_csv("data/working/federal_reporter/abstracts_federal-reporter_first-last.csv")
abstracts_federal_reporter_combined <- left_join(abstracts_federal_reporter_first_last, data_fixed, by = "PROJECT_ID")

write_csv(abstracts_federal_reporter_combined, "data/working/federal_reporter/abstracts_federal_reporter_combined.csv")
