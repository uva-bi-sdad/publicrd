library(R.utils)
library(tidyverse)
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)

##Federal Reporter Abstract Profiling
#Checking Federal Reporter FY2008-FY2018

#Import Abstract Files

X2008_abstracts_federal_reporter <- read_csv("data/original/federal-reporter_abstracts/2008-abstracts_federal-reporter.csv")
X2009_abstracts_federal_reporter <- read_csv("data/original/federal-reporter_abstracts/2009-abstracts_federal-reporter.csv")
X2010_abstracts_federal_reporter <- read_csv("data/original/federal-reporter_abstracts/2010-abstracts_federal-reporter.csv")
X2011_abstracts_federal_reporter <- read_csv("data/original/federal-reporter_abstracts/2011-abstracts_federal-reporter.csv")
X2012_abstracts_federal_reporter <- read_csv("data/original/federal-reporter_abstracts/2012-abstracts_federal-reporter.csv")
X2013_abstracts_federal_reporter <- read_csv("data/original/federal-reporter_abstracts/2013-abstracts_federal-reporter.csv")
X2014_abstracts_federal_reporter <- read_csv("data/original/federal-reporter_abstracts/2014-abstracts_federal-reporter.csv")
X2015_abstracts_federal_reporter <- read_csv("data/original/federal-reporter_abstracts/2015-abstracts_federal-reporter.csv")
X2016_abstracts_federal_reporter <- read_csv("data/original/federal-reporter_abstracts/2016-abstracts_federal-reporter.csv")
X2017_abstracts_federal_reporter <- read_csv("data/original/federal-reporter_abstracts/2017-abstracts_federal-reporter.csv")
X2018_abstracts_federal_reporter <- read_csv("data/original/federal-reporter_abstracts/2018-abstracts_federal-reporter.csv")

#Review whether there are any duplicate abstracts

abstracts_federal_reporter <- rbind(
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

abstracts_federal_reporter$IS_DUPLICATED <- duplicated(abstracts_federal_reporter$ABSTRACT)
unique_abstracts <- filter(abstracts_federal_reporter, IS_DUPLICATED == FALSE)

percent_unique <- 100*nrow(unique_abstracts)/nrow(abstracts_federal_reporter)

#There are no duplicate project codes, but there are duplicate abstracts 
#53% of abstracts from 2008-2018 are unique, 47% are duplicates

##Examine Abstract Lengths:

abstracts_federal_reporter$CHAR_LENGTH <- str_length(abstracts_federal_reporter$ABSTRACT)

#Histogram 1:
#Not clean looking. Only 300 out of a million abstracts are above 10,000
hist(abstracts_federal_reporter$CHAR_LENGTH, breaks = 100)

#Histogram 2:
#Limit x-axis to 10,000 and name axes and add color
hist(abstracts_federal_reporter$CHAR_LENGTH, 
     breaks = 500,
     xlim = c(0,10000),
     xlab = "Character Length",
     main = "Abstract Lengths",
     col = "blue"
     )

#Clearly the vast majority of these abstracts are cut off at some point.
