library(R.utils)
library(tidyverse)
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)

##Get Departments:

X2008_data_multiple_fed_reporter <- read_csv("data/original/2008-data_multiple_fed-reporter.csv") %>%
  select(PROJECT_ID, DEPARTMENT, AGENCY)
X2009_data_multiple_fed_reporter <- read_csv("data/original/2009-data_multiple_fed-reporter.csv") %>%
  select(PROJECT_ID, DEPARTMENT, AGENCY)
X2010_data_multiple_fed_reporter <- read_csv("data/original/2010-data_multiple_fed-reporter.csv") %>%
  select(PROJECT_ID, DEPARTMENT, AGENCY)
X2011_data_multiple_fed_reporter <- read_csv("data/original/2011-data_multiple_fed-reporter.csv") %>%
  select(PROJECT_ID, DEPARTMENT, AGENCY)
X2012_data_multiple_fed_reporter <- read_csv("data/original/2012-data_multiple_fed-reporter.csv") %>%
  select(PROJECT_ID, DEPARTMENT, AGENCY)
X2013_data_multiple_fed_reporter <- read_csv("data/original/2013-data_multiple_fed-reporter.csv") %>%
  select(PROJECT_ID, DEPARTMENT, AGENCY)
X2014_data_multiple_fed_reporter <- read_csv("data/original/2014-data_multiple_fed-reporter.csv") %>%
  select(PROJECT_ID, DEPARTMENT, AGENCY)
X2015_data_multiple_fed_reporter <- read_csv("data/original/2015-data_multiple_fed-reporter.csv") %>%
  select(PROJECT_ID, DEPARTMENT, AGENCY)
X2016_data_multiple_fed_reporter <- read_csv("data/original/2016-data_multiple_fed-reporter.csv") %>%
  select(PROJECT_ID, DEPARTMENT, AGENCY)
X2017_data_multiple_fed_reporter <- read_csv("data/original/2017-data_multiple_fed-reporter.csv") %>%
  select(PROJECT_ID, DEPARTMENT, AGENCY)
X2018_data_multiple_fed_reporter <- read_csv("data/original/2018-data_multiple_fed-reporter.csv") %>%
  select(PROJECT_ID, DEPARTMENT, AGENCY)

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

abstracts_federal_reporter_first_last <- read_csv("data/working/federal_reporter/abstracts_federal-reporter_first-last.csv")
abstracts_federal_reporter_combined <- left_join(abstracts_federal_reporter_first_last, departments_federal_reporter, by = "PROJECT_ID") %>%
  select(PROJECT_ID, DEPARTMENT, AGENCY, ABSTRACT, FIRST_CHAR, LAST_CHAR, FY)

write_csv(abstracts_federal_reporter_combined, "data/working/federal_reporter/abstracts_federal_reporter_combined.csv")
