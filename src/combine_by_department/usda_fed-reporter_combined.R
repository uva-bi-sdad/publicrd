library(R.utils)
library(tidyverse)
library(data.table)
library(dplyr)

X2010_data_multiple_fed_reporter <- read_csv("data/original/2010-data_multiple_fed-reporter.csv") %>%
  filter(str_detect(DEPARTMENT, "USDA"))
X2011_data_multiple_fed_reporter <- read_csv("data/original/2011-data_multiple_fed-reporter.csv") %>%
  filter(str_detect(DEPARTMENT, "USDA"))
X2012_data_multiple_fed_reporter <- read_csv("data/original/2012-data_multiple_fed-reporter.csv") %>%
  filter(str_detect(DEPARTMENT, "USDA"))
X2013_data_multiple_fed_reporter <- read_csv("data/original/2013-data_multiple_fed-reporter.csv") %>%
  filter(str_detect(DEPARTMENT, "USDA"))
X2014_data_multiple_fed_reporter <- read_csv("data/original/2014-data_multiple_fed-reporter.csv") %>%
  filter(str_detect(DEPARTMENT, "USDA"))
X2015_data_multiple_fed_reporter <- read_csv("data/original/2015-data_multiple_fed-reporter.csv") %>%
  filter(str_detect(DEPARTMENT, "USDA"))
X2016_data_multiple_fed_reporter <- read_csv("data/original/2016-data_multiple_fed-reporter.csv") %>%
  filter(str_detect(DEPARTMENT, "USDA"))

fed_reporter_USDA_2010_to_2016 <- rbind(X2010_data_multiple_fed_reporter, 
                                        X2011_data_multiple_fed_reporter, 
                                        X2012_data_multiple_fed_reporter, 
                                        X2013_data_multiple_fed_reporter, 
                                        X2014_data_multiple_fed_reporter, 
                                        X2015_data_multiple_fed_reporter, 
                                        X2016_data_multiple_fed_reporter)

write.csv(fed_reporter_USDA_2010_to_2016, "fed_reporter_USDA_2010_to_2016.csv", row.names = FALSE)
