library(R.utils)
library(tidyverse)
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)

##Federal Reporter Abstract Profiling
#Checking Federal Reporter FY2008-FY2018

#Import Abstract Files:

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

#Flag and tally duplicates:

abstracts_federal_reporter$IS_DUPLICATED <- duplicated(abstracts_federal_reporter$ABSTRACT)
unique_abstracts <- filter(abstracts_federal_reporter, IS_DUPLICATED == FALSE)
percent_unique <- 100*nrow(unique_abstracts)/nrow(abstracts_federal_reporter)

#53% of abstracts from 2008-2018 are unique, 47% are duplicates

unique_abstracts <- unique_abstracts %>% 
  select(-IS_DUPLICATED)

#write.csv(abstracts_federal_reporter, file = "data/working/federal_reporter/duplicate_abstracts.csv", row.names = FALSE)
#write.csv(unique_abstracts, file = "data/working/federal_reporter/abstracts_federal-reporter.csv", row.names = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##Examine Abstract Lengths:

abstracts_federal_reporter$CHAR_LENGTH <- str_length(abstracts_federal_reporter$ABSTRACT)
freq_charlength <- tally(abstracts_federal_reporter$CHAR_LENGTH)

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
