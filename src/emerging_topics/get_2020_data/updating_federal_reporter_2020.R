library(tidyverse)
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(readr)

#Federal Reporter 2020 Abstract Pull

X2008_abstracts_federal_reporter <- read_csv("~/git/publicrd/data/prd/RND_Topic_Modelling/federal-reporter_updated/abstracts/FedRePORTER_PRJABS_C_FY2008.csv")
X2009_abstracts_federal_reporter <- read_csv("~/git/publicrd/data/prd/RND_Topic_Modelling/federal-reporter_updated/abstracts/FedRePORTER_PRJABS_C_FY2009.csv")
X2010_abstracts_federal_reporter <- read_csv("~/git/publicrd/data/prd/RND_Topic_Modelling/federal-reporter_updated/abstracts/FedRePORTER_PRJABS_C_FY2010.csv")
X2011_abstracts_federal_reporter <- read_csv("~/git/publicrd/data/prd/RND_Topic_Modelling/federal-reporter_updated/abstracts/FedRePORTER_PRJABS_C_FY2011.csv")
X2012_abstracts_federal_reporter <- read_csv("~/git/publicrd/data/prd/RND_Topic_Modelling/federal-reporter_updated/abstracts/FedRePORTER_PRJABS_C_FY2012.csv")
X2013_abstracts_federal_reporter <- read_csv("~/git/publicrd/data/prd/RND_Topic_Modelling/federal-reporter_updated/abstracts/FedRePORTER_PRJABS_C_FY2013.csv")
X2014_abstracts_federal_reporter <- read_csv("~/git/publicrd/data/prd/RND_Topic_Modelling/federal-reporter_updated/abstracts/FedRePORTER_PRJABS_C_FY2014.csv")
X2015_abstracts_federal_reporter <- read_csv("~/git/publicrd/data/prd/RND_Topic_Modelling/federal-reporter_updated/abstracts/FedRePORTER_PRJABS_C_FY2015.csv")
X2016_abstracts_federal_reporter <- read_csv("~/git/publicrd/data/prd/RND_Topic_Modelling/federal-reporter_updated/abstracts/FedRePORTER_PRJABS_C_FY2016.csv")
X2017_abstracts_federal_reporter <- read_csv("~/git/publicrd/data/prd/RND_Topic_Modelling/federal-reporter_updated/abstracts/FedRePORTER_PRJABS_C_FY2017.csv")
X2018_abstracts_federal_reporter <- read_csv("~/git/publicrd/data/prd/RND_Topic_Modelling/federal-reporter_updated/abstracts/FedRePORTER_PRJABS_C_FY2018.csv")
X2019_abstracts_federal_reporter <- read_csv("~/git/publicrd/data/prd/RND_Topic_Modelling/federal-reporter_updated/abstracts/FedRePORTER_PRJABS_C_FY2019.csv")

#Tag with year:

X2008_abstracts_federal_reporter$FY <-2008
X2009_abstracts_federal_reporter$FY <-2009
X2010_abstracts_federal_reporter$FY <-2010
X2011_abstracts_federal_reporter$FY <-2011
X2012_abstracts_federal_reporter$FY <-2012
X2013_abstracts_federal_reporter$FY <-2013
X2014_abstracts_federal_reporter$FY <-2014
X2015_abstracts_federal_reporter$FY <-2015
X2016_abstracts_federal_reporter$FY <-2016
X2017_abstracts_federal_reporter$FY <-2017
X2018_abstracts_federal_reporter$FY <-2018
X2019_abstracts_federal_reporter$FY <-2019

#Federal Reporter 2020 Abstracts Merged Into One File

A_abstracts_federal_reporter <- rbind(
  X2008_abstracts_federal_reporter,
  X2009_abstracts_federal_reporter,
  X2010_abstracts_federal_reporter,
  X2011_abstracts_federal_reporter,
  X2012_abstracts_federal_reporter,
  X2013_abstracts_federal_reporter,
  X2014_abstracts_federal_reporter,
  X2015_abstracts_federal_reporter,
  X2016_abstracts_federal_reporter,
  X2017_abstracts_federal_reporter,
  X2018_abstracts_federal_reporter,
  X2019_abstracts_federal_reporter
)

#Federal Reporter 2020 Metadata Pull:

Y2008_data_multiple_fed_reporter <- read_csv("~/git/publicrd/data/prd/RND_Topic_Modelling/federal-reporter_updated/metadata/FedRePORTER_PRJ_C_FY2008.csv")
Y2009_data_multiple_fed_reporter <- read_csv("~/git/publicrd/data/prd/RND_Topic_Modelling/federal-reporter_updated/metadata/FedRePORTER_PRJ_C_FY2009.csv")
Y2010_data_multiple_fed_reporter <- read_csv("~/git/publicrd/data/prd/RND_Topic_Modelling/federal-reporter_updated/metadata/FedRePORTER_PRJ_C_FY2010.csv")
Y2011_data_multiple_fed_reporter <- read_csv("~/git/publicrd/data/prd/RND_Topic_Modelling/federal-reporter_updated/metadata/FedRePORTER_PRJ_C_FY2011.csv")
Y2012_data_multiple_fed_reporter <- read_csv("~/git/publicrd/data/prd/RND_Topic_Modelling/federal-reporter_updated/metadata/FedRePORTER_PRJ_C_FY2012.csv")
Y2013_data_multiple_fed_reporter <- read_csv("~/git/publicrd/data/prd/RND_Topic_Modelling/federal-reporter_updated/metadata/FedRePORTER_PRJ_C_FY2013.csv")
Y2014_data_multiple_fed_reporter <- read_csv("~/git/publicrd/data/prd/RND_Topic_Modelling/federal-reporter_updated/metadata/FedRePORTER_PRJ_C_FY2014.csv")
Y2015_data_multiple_fed_reporter <- read_csv("~/git/publicrd/data/prd/RND_Topic_Modelling/federal-reporter_updated/metadata/FedRePORTER_PRJ_C_FY2015.csv")
Y2016_data_multiple_fed_reporter <- read_csv("~/git/publicrd/data/prd/RND_Topic_Modelling/federal-reporter_updated/metadata/FedRePORTER_PRJ_C_FY2016.csv")
Y2017_data_multiple_fed_reporter <- read_csv("~/git/publicrd/data/prd/RND_Topic_Modelling/federal-reporter_updated/metadata/FedRePORTER_PRJ_C_FY2017.csv")
Y2018_data_multiple_fed_reporter <- read_csv("~/git/publicrd/data/prd/RND_Topic_Modelling/federal-reporter_updated/metadata/FedRePORTER_PRJ_C_FY2018.csv")
Y2019_data_multiple_fed_reporter <- read_csv("~/git/publicrd/data/prd/RND_Topic_Modelling/federal-reporter_updated/metadata/FedRePORTER_PRJ_C_FY2019.csv")

#Federal Reporter 2020 Merged Into One File

A_metadatafederalreporter <- rbind(
  Y2008_data_multiple_fed_reporter,
  Y2009_data_multiple_fed_reporter,
  Y2010_data_multiple_fed_reporter,
  Y2011_data_multiple_fed_reporter,
  Y2012_data_multiple_fed_reporter,
  Y2013_data_multiple_fed_reporter,
  Y2014_data_multiple_fed_reporter,
  Y2015_data_multiple_fed_reporter,
  Y2016_data_multiple_fed_reporter,
  Y2017_data_multiple_fed_reporter,
  Y2018_data_multiple_fed_reporter,
  Y2019_data_multiple_fed_reporter
)


#Fix CFDA Codes:

CFDA_glossary <- read_csv("~/git/publicrd/glossary/CFDA_glossary_final.csv") %>%
  select(department_abb, department_digits)
colnames(CFDA_glossary)[colnames(CFDA_glossary)=="department_abb"] <- "DEPARTMENT"

data_fixed <- left_join(A_metadatafederalreporter, CFDA_glossary, by = "DEPARTMENT")

#Cleaning Function:

fixing_cfda_code <- function(cfda_project_code){

  fixed_cfda <-ifelse(cfda_project_code != 000 & cfda_project_code != 999 & grepl("^[0-9]{3}$", cfda_project_code) == TRUE,
                      paste(data_fixed$department_digits,cfda_project_code, sep = "."),
                      cfda_project_code)

  return(fixed_cfda)
}

#Update Data:

data_fixed$CFDA_CODE <- fixing_cfda_code(data_fixed$CFDA_CODE)
data_fixed$department_digits <- NULL
A_metadatafederalreporter <- data_fixed
rm(data_fixed)

#Combine Metadata with Abstracts

AAA_updated_federal_reporter <- full_join(A_abstracts_federal_reporter, A_metadatafederalreporter, by = "PROJECT_ID")
#write.csv(AAA_updated_federal_reporter, file = "~/git/publicrd/data/prd/RND_Topic_Modelling/federal-reporter_updated/working_federal_reporter_2020.csv", row.names = FALSE)
