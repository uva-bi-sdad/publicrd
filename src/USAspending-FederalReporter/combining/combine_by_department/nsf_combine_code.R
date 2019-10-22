###Combining USA Spending grants 2016 with Fed RePORTER FY14-16 for the NSF

library(data.table)
library(tidyverse)
library(lubridate)

loc <- "data/working/combine_by_department/NSF"

nsf_grant <- fread(file.path(loc, "nsf_usa-spending_grants_2016.csv"))
nsf_fed <- fread(file.path(loc, "nsf_fed-reporter_2016.csv"))

##this is a straightforward join--no transformations needed

#what is the match rate like? 85% of usaspending transactions have a match somewhere in fedreporter;
#91% of fedreporter FY16 projects have a match somewhere in usaspending fy16.
sum(nsf_grant$award_id_fain %in% nsf_fed$PROJECT_NUMBER)/nrow(nsf_grant)
sum(filter(nsf_fed, FY == 2016)$PROJECT_NUMBER %in% nsf_grant$award_id_fain)/sum(nsf_fed$FY == 2016)
sum(nsf_fed$PROJECT_NUMBER %in% nsf_grant$award_id_fain)/nrow(nsf_fed)

##are there any patterns in what is matched/not matched?
#putting the match flag in nsf_fed:
nsf_fed <- mutate(nsf_fed, grantmatch = PROJECT_NUMBER %in% nsf_grant$award_id_fain)
nsf_fed_2016 <- filter(nsf_fed, FY == 2016)
#putting the match flag in usa spending:
nsf_grant <- mutate(nsf_grant, fedmatch = award_id_fain %in% nsf_fed$PROJECT_NUMBER)
#putting in the research flag, to be updated later with information from CFDA codes:
nsf_grant <- mutate(nsf_grant, resflag = fedmatch)

#grants mostly doesn't have office information (and fedreporter doesn't have any at all), 
#so can't see any differences in that
ggplot(nsf_grant) + geom_bar(aes(x = awarding_office_name, fill = fedmatch)) + ylim(c(0,50))+coord_flip()
#start/end dates: those that did not match in USA Spending tended to start earlier
nsf_fed$PROJECT_START_DATE <- mdy(nsf_fed$PROJECT_START_DATE)
nsf_fed$PROJECT_END_DATE <- mdy(nsf_fed$PROJECT_END_DATE)
nsf_fed_2016 <- filter(nsf_fed, FY == 2016)
nsf_grant$period_of_performance_start_date <- ymd(nsf_grant$period_of_performance_start_date)
ggplot(nsf_fed_2016) + geom_histogram(aes(x=PROJECT_START_DATE, fill = grantmatch))
ggplot(nsf_grant) + geom_histogram(aes(x=period_of_performance_start_date, fill = fedmatch), bins = 25)

#since numbers are a 1:1 match, don't need to put that into either file. writing out files with match flag:

write.csv(nsf_grant, file.path(loc,"nsf_usa-spending_grants_2016_matchflag.csv"))
write.csv(nsf_fed, file.path(loc, "nih_fed-reporter_2016_matchflag.csv"))
