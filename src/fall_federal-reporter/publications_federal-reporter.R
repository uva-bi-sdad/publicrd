library(R.utils)
library(tidyverse)
library(data.table)
library(dplyr)
library(stringr)

##Federal Reporter Publications Profiling
#Checking Federal Reporter FY2008-FY2018

X2008_pub_federal_reporter <- read_csv("data/original/federal-reporter_publications/2008-pub_federal-reporter.csv")
X2009_pub_federal_reporter <- read_csv("data/original/federal-reporter_publications/2009-pub_federal-reporter.csv")
X2010_pub_federal_reporter <- read_csv("data/original/federal-reporter_publications/2010-pub_federal-reporter.csv")
X2011_pub_federal_reporter <- read_csv("data/original/federal-reporter_publications/2011-pub_federal-reporter.csv")
X2012_pub_federal_reporter <- read_csv("data/original/federal-reporter_publications/2012-pub_federal-reporter.csv")
X2013_pub_federal_reporter <- read_csv("data/original/federal-reporter_publications/2013-pub_federal-reporter.csv")
X2014_pub_federal_reporter <- read_csv("data/original/federal-reporter_publications/2014-pub_federal-reporter.csv")
X2015_pub_federal_reporter <- read_csv("data/original/federal-reporter_publications/2015-pub_federal-reporter.csv")
X2016_pub_federal_reporter <- read_csv("data/original/federal-reporter_publications/2016-pub_federal-reporter.csv")
X2017_pub_federal_reporter <- read_csv("data/original/federal-reporter_publications/2017-pub_federal-reporter.csv")
X2018_pub_federal_reporter <- read_csv("data/original/federal-reporter_publications/2018-pub_federal-reporter.csv")

#Match Federal Reporter to Publications

publications_federal_reporter <- rbind(
  X2008_pub_federal_reporter,
  X2009_pub_federal_reporter,
  X2010_pub_federal_reporter,
  X2011_pub_federal_reporter,
  X2012_pub_federal_reporter,
  X2013_pub_federal_reporter,
  X2014_pub_federal_reporter,
  X2015_pub_federal_reporter,
  X2016_pub_federal_reporter,
  X2017_pub_federal_reporter,
  X2018_pub_federal_reporter
)

publications_federal_reporter$PUB_EXISTS <- TRUE

write.csv(publications_federal_reporter, file = "data/working/publications_federal-reporter.csv", row.names = FALSE)

