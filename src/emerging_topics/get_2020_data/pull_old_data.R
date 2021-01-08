library(R.utils)
library(readr)
library(tidyverse)
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(dplyr)

##Federal Reporter Abstract Profiling
#Checking Federal Reporter FY2008-FY2018

#Import Abstract Files:

X2008_abstracts_federal_reporter <- read_csv("~/git/publicrd/data/prd/DigitalOcean_Backup/public_rd/original/federal-reporter_abstracts/2008-abstracts_federal-reporter.csv")
X2009_abstracts_federal_reporter <- read_csv("~/git/publicrd/data/prd/DigitalOcean_Backup/public_rd/original/federal-reporter_abstracts/2009-abstracts_federal-reporter.csv")
X2010_abstracts_federal_reporter <- read_csv("~/git/publicrd/data/prd/DigitalOcean_Backup/public_rd/original/federal-reporter_abstracts/2010-abstracts_federal-reporter.csv")
X2011_abstracts_federal_reporter <- read_csv("~/git/publicrd/data/prd/DigitalOcean_Backup/public_rd/original/federal-reporter_abstracts/2011-abstracts_federal-reporter.csv")
X2012_abstracts_federal_reporter <- read_csv("~/git/publicrd/data/prd/DigitalOcean_Backup/public_rd/original/federal-reporter_abstracts/2012-abstracts_federal-reporter.csv")
X2013_abstracts_federal_reporter <- read_csv("~/git/publicrd/data/prd/DigitalOcean_Backup/public_rd/original/federal-reporter_abstracts/2013-abstracts_federal-reporter.csv")
X2014_abstracts_federal_reporter <- read_csv("~/git/publicrd/data/prd/DigitalOcean_Backup/public_rd/original/federal-reporter_abstracts/2014-abstracts_federal-reporter.csv")
X2015_abstracts_federal_reporter <- read_csv("~/git/publicrd/data/prd/DigitalOcean_Backup/public_rd/original/federal-reporter_abstracts/2015-abstracts_federal-reporter.csv")
X2016_abstracts_federal_reporter <- read_csv("~/git/publicrd/data/prd/DigitalOcean_Backup/public_rd/original/federal-reporter_abstracts/2016-abstracts_federal-reporter.csv")
X2017_abstracts_federal_reporter <- read_csv("~/git/publicrd/data/prd/DigitalOcean_Backup/public_rd/original/federal-reporter_abstracts/2017-abstracts_federal-reporter.csv")
X2018_abstracts_federal_reporter <- read_csv("~/git/publicrd/data/prd/DigitalOcean_Backup/public_rd/original/federal-reporter_abstracts/2018-abstracts_federal-reporter.csv")

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


#Review whether there are any duplicate abstracts

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
  X2018_abstracts_federal_reporter
)

#Federal Reporter Project Data Pull:

Y2008_data_multiple_fed_reporter <- read_csv("~/git/publicrd/data/prd/DigitalOcean_Backup/public_rd/original/2008-data_multiple_fed-reporter.csv")
Y2009_data_multiple_fed_reporter <- read_csv("~/git/publicrd/data/prd/DigitalOcean_Backup/public_rd/original/2009-data_multiple_fed-reporter.csv")
Y2010_data_multiple_fed_reporter <- read_csv("~/git/publicrd/data/prd/DigitalOcean_Backup/public_rd/original/2010-data_multiple_fed-reporter.csv")
Y2011_data_multiple_fed_reporter <- read_csv("~/git/publicrd/data/prd/DigitalOcean_Backup/public_rd/original/2011-data_multiple_fed-reporter.csv")
Y2012_data_multiple_fed_reporter <- read_csv("~/git/publicrd/data/prd/DigitalOcean_Backup/public_rd/original/2012-data_multiple_fed-reporter.csv")
Y2013_data_multiple_fed_reporter <- read_csv("~/git/publicrd/data/prd/DigitalOcean_Backup/public_rd/original/2013-data_multiple_fed-reporter.csv")
Y2014_data_multiple_fed_reporter <- read_csv("~/git/publicrd/data/prd/DigitalOcean_Backup/public_rd/original/2014-data_multiple_fed-reporter.csv")
Y2015_data_multiple_fed_reporter <- read_csv("~/git/publicrd/data/prd/DigitalOcean_Backup/public_rd/original/2015-data_multiple_fed-reporter.csv")
Y2016_data_multiple_fed_reporter <- read_csv("~/git/publicrd/data/prd/DigitalOcean_Backup/public_rd/original/2016-data_multiple_fed-reporter.csv")
Y2017_data_multiple_fed_reporter <- read_csv("~/git/publicrd/data/prd/DigitalOcean_Backup/public_rd/original/2017-data_multiple_fed-reporter.csv")
Y2018_data_multiple_fed_reporter <- read_csv("~/git/publicrd/data/prd/DigitalOcean_Backup/public_rd/original/2018-data_multiple_fed-reporter.csv")


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
  Y2018_data_multiple_fed_reporter
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

#Flag and tally duplicates:

A_abstracts_federal_reporter$IS_DUPLICATED <- duplicated(abstracts_federal_reporter$ABSTRACT)

#Combine Metadata with Abstracts

AAA_updated_federal_reporter <- full_join(A_abstracts_federal_reporter, A_metadatafederalreporter, by = "PROJECT_ID")



#Full Dataset

write.csv(AAA_updated_federal_reporter, file = "~/git/dspg20RnD/data/old_federal_reporter.csv", row.names = FALSE)

#Filter Unique

unique_abstracts <- filter(abstracts_federal_reporter, IS_DUPLICATED == FALSE)
percent_unique <- 100*nrow(unique_abstracts)/nrow(abstracts_federal_reporter)

#53% of abstracts from 2008-2018 are unique, 47% are duplicates

unique_abstracts <- unique_abstracts %>%
  select(-IS_DUPLICATED)

#write.csv(abstracts_federal_reporter, file = "data/working/federal_reporter/duplicate_abstracts.csv", row.names = FALSE)
#write.csv(unique_abstracts, file = "data/working/federal_reporter/abstracts_federal-reporter.csv", row.names = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
