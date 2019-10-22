library(data.table)
library(tidyverse)
library(lubridate)

loc <- "data/working/combine_by_department/NASA"
nasa_grant <- fread(file.path(loc, "nasa_usa-spending_grants_2016.csv"))
nasa_fed <- fread(file.path(loc, "nasa_fed-reporter_2016.csv"))

##58% of 2016 USA Spending grant transactions were somewhere in Fed RePORTER FY14-16
sum(nasa_grant$award_id_fain %in% nasa_fed$PROJECT_NUMBER)/nrow(nasa_grant)
#94% of FedRePORTER FY16 transactions were somewere in FY16 USA Spending grant transactions
sum(nasa_fed[nasa_fed$FY == 2016]$PROJECT_NUMBER %in% nasa_grant$award_id_fain)/sum(nasa_fed$FY == 2016)

#putting a match flag in the grants file:
nasa_grant_match <- mutate(nasa_grant, fed_reporter_match = award_id_fain %in% nasa_fed$PROJECT_NUMBER)
sum(nasa_grant_match$fed_reporter_match)/nrow(nasa_grant_match)

#putting a match flag in the fedreporter file
nasa_fed_match <- mutate(nasa_fed, has_match = PROJECT_NUMBER %in% nasa_grant$award_id_fain)
sum(nasa_fed_match$has_match)/nrow(nasa_fed_match)

#making match column into res column:
nasa_grant_match$resflag <- nasa_grant_match$fed_reporter_match

#writing out
write.csv(nasa_fed_match, file.path(loc, "nasa_fed-reporter_2016_matchflag.csv"))
write.csv(nasa_grant_match, file.path(loc, "nasa_usa-spending_grants_2016_matchflag.csv"))


#grants that DON'T have a match--what's the deal there

nmatch <- filter(nasa_grant_match, fed_reporter_match == FALSE)
match <- filter(nasa_grant_match, fed_reporter_match == TRUE)

#how much money listed in USASpend for FY16 transactions? non-matching transactions were more likely to be $0 (34%) or negative (21%)
#than matching transactions (16% had $0, 4% were negative)
summary(nmatch$federal_action_obligation)
sum(nmatch$federal_action_obligation == 0)/nrow(nmatch)
summary(match$federal_action_obligation)
sum(match$federal_action_obligation == 0)/nrow(match)

sum(nmatch$federal_action_obligation < 0)/nrow(nmatch)
sum(match$federal_action_obligation < 0)/nrow(match)

#period of performance start dates? (very small number of NAs)
nmatch$period_of_performance_start_date <- ymd(nmatch$period_of_performance_start_date)
match$period_of_performance_start_date <- ymd(match$period_of_performance_start_date)
sum(is.na(nmatch$period_of_performance_start_date ))
sum(is.na(match$period_of_performance_start_date))
#how many started before FY14? (none, in either)

sum(nmatch$period_of_performance_start_date < mdy("10-01-2013"), na.rm = TRUE)/nrow(nmatch)
min(nmatch$period_of_performance_start_date, na.rm = TRUE)
sum(match$period_of_performance_start_date < mdy("10-01-2013"), na.rm = TRUE)/nrow(match)
min(match$period_of_performance_start_date, na.rm = TRUE)

#how many per sub-agency?
#no sub-agencies included in USA Spending:
nasa_grant_match %>% group_by(awarding_sub_agency_name) %>% summarise(count = n()) %>% arrange(desc(count))
#mostly awarding office is blank, but in a small minority has an office, no notable differences between them:
nasa_grant_match %>% group_by(awarding_office_name) %>% summarise(count = n(), matched = sum(fed_reporter_match)) %>% mutate(perc_match = matched/count) %>% arrange(desc(count))


#anything related to CFDAs? science, space tech, exploration, aeronautics tended to have more matches; education, cross-agency
#support, etc had many fewer.
nasa_grant_match %>% group_by(cfda_title) %>% summarise(count = n(), matched = sum(fed_reporter_match)) %>% mutate(perc_match = matched/count) %>% arrange(desc(count))
