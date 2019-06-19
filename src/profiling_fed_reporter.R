library(R.utils)
library(tidyverse)
library(data.table)

gunzip("/home/af5ug/publicrd/data/original/2016-data_multiple_fed-reporter.csv.gz")

fed_reporter <- fread("/home/af5ug/publicrd/data/original/2016-data_multiple_fed-reporter.csv")

str(fed_reporter)
head(fed_reporter)

field_profile <- data.frame(field = as.character(colnames(fed_reporter)), completeness = as.numeric(NA),
                            validity = as.numeric(NA), consistency = as.numeric(NA), 
                            uniqueness = as.numeric(NA))

field_profile$completeness <- mutate(field_profile, is.na(fed_reporter))


na_count <- function(x){
  ((sum(is.na(x)) + sum(x == "", na.rm = TRUE))/nrow(fed_reporter))*100
}

field_profile$completeness <- apply(fed_reporter, MARGIN = 2, na_count)

head(fed_reporter$PROJECT_ID)
class(fed_reporter$PROJECT_ID)
sum(nchar(fed_reporter$PROJECT_ID) != 6)

fed_reporter[which(nchar(fed_reporter$PROJECT_ID) != 6),"PROJECT_ID"]

#found that PROJECT_ID is usually 6 characters but sometimes 7 characters, always an integer, 
#going to consider that valid

field_profile[1,"validity"] <- 100

#PROJECT_TERMS too qualitative to determine whether it is valid


#Validity for PROJECT_TITLE: all are character, checking to see if any are potentially abstracts

#getting lengths of titles and checking distribution
title_length <- nchar(fed_reporter$PROJECT_TITLE)
summary(title_length)
hist(title_length)

#saw a right-skewed distribution with group of outliers at 200 characters and above. Investigating
#how many are above 200 characters, what common lengths might be, and manually inspecting some outliers
long_title <- fed_reporter[which(title_length[,1] >= 200), "PROJECT_TITLE"]
sum(nchar(long_title$PROJECT_TITLE) == 200)
unique(nchar(long_title$PROJECT_TITLE))
table(nchar(long_title$PROJECT_TITLE))
filter(long_title, nchar(PROJECT_TITLE) == 255)[1,]
filter(long_title, nchar(PROJECT_TITLE) == 254)[1:5,]

#Found that 1,102 of 1,416 titles above 200 characters in length were precisely 255 characters. A
#manual inspection of some of these showed that they were sentences (probably abstracts) or a 
#title followed by an abstract. There were 217 254-character titles, which also appeared to be abstracts.
#The 31 200-character titles were actually titles. For purposes of initial profiling, considering
#255 and 254 character titles to be invalid (but a deeper audit is likely needed before making
#decisions about the project)

field_profile[3,"validity"] <- 100-((1102+217)/nrow(fed_reporter))*100
