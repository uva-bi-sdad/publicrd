library(data.table)
library(tidyverse)
library(lubridate)

loc <- "data/working/combine_by_department/NIH"

nih_grant <- fread(file.path(loc, "nih_usa-spending_grants_2016.csv"))
nih_fed <- fread(file.path(loc, "nih_fed-reporter_2016.csv"))

nih_fed$usa_grant_fain_id <- substr(nih_fed$PROJECT_NUMBER, 2, 12)

sum(nih_grant$award_id_fain %chin% nih_fed$usa_grant_fain_id)/nrow(nih_grant)
sum(nih_fed[nih_fed$FY == 2016, "usa_grant_fain_id"] %in% nih_grant$award_id_fain)/sum(nih_fed$FY == 2016)

##above doesn't work because one record in fedreporter has NA in its fiscal year:
nih_fed[is.na(nih_fed$FY) == TRUE,]
##looked it up directly on FedRePORTER and it belongs in FY14 (https://projectreporter.nih.gov/project_info_details.cfm?aid=8691845)

nih_fed[PROJECT_NUMBER == "5R01GM023244-39","FY"] <- 2014

##trying again: 82% match
sum(filter(nih_fed, FY == 2016)$usa_grant_fain_id %in% nih_grant$award_id_fain)/sum(nih_fed$FY == 2016)
nih_fed$usa_grant_match <- nih_fed$usa_grant_fain_id %in% nih_grant$award_id_fain 

##adding fed project_number to USASpending
#is there a match?
nih_grant_match <- mutate(nih_grant, fed_reporter_match = award_id_fain %in% nih_fed$usa_grant_fain_id)


#adding a match is complicated--it's not a 1:1 relationship. there are multiple fedreporter
#projects for each usaspending transaction. duplicates within the same year as well.
#this gives a summary of how many fedreporter records there are per usaspending transaction
nih_grant_match_dupe_summary <- nih_grant %>% left_join(nih_fed[,c("usa_grant_fain_id", "PROJECT_NUMBER")], by = c("award_id_fain" = "usa_grant_fain_id")) %>%
  group_by(award_id_fain) %>% summarise(fed_project_count = n()) %>% group_by(fed_project_count) %>% summarise(count = n())
#looks at it within just one year--still multiples
nih_grant_match_2016 <- nih_grant %>% left_join(nih_fed[nih_fed$FY == 2016,c("usa_grant_fain_id", "PROJECT_NUMBER")], by = c("award_id_fain" = "usa_grant_fain_id"))

#FOR NOW, just putting in a flag that there is a match

##putting a research flag in USAspending for any grant that has a fedreporter match
nih_grant_match$resflag <- nih_grant_match$hasmatch

##writing out file with match and research flags in it
write.csv(nih_grant_match, file.path(loc, "nih_usa-spending_grants_2016_matchflag.csv"))

##putting 2016 USA spending grant match flag in fedreporter file
nih_fed_match <- mutate(nih_fed, has_match = usa_grant_fain_id %in% nih_grant$award_id_fain)
##writing that out
write.csv(nih_fed_match, file.path(loc, "nih_fed-reporter_2016_matchflag.csv"))

##putting a research flag in USAspending for any grant that has a fedreporter match
nih_grant_match$resflag <- nih_grant_match$hasmatch

##anything distinctive about fed reporter projects that do not have a match in usaspending? non-matches were more
##likely not to have budget start/end dates, but that was only in 40% of non-matches. otherwise,
##didn't find anything, but things I checked are below

nmatch <- !filter(nih_fed, FY == 2016)$usa_grant_fain_id %in% nih_grant$award_id_fain
match <- filter(nih_fed, FY == 2016)$usa_grant_fain_id %in% nih_grant$award_id_fain
nmatch_fed <- filter(nih_fed, FY == 2016)[nmatch,]
match_fed <- filter(nih_fed, FY == 2016)[match,]

#how much money listed in FedRep for FY16?
summary(nmatch_fed$FY_TOTAL_COST)
summary(match_fed$FY_TOTAL_COST)

#budget start dates/end dates?
nmatch_fed$BUDGET_END_DATE <- mdy(nmatch_fed$BUDGET_END_DATE)
nmatch_fed$BUDGET_START_DATE <- mdy(nmatch_fed$BUDGET_START_DATE)
nih_fed$BUDGET_END_DATE <- mdy(nih_fed$BUDGET_END_DATE)
nih_fed$BUDGET_START_DATE <- mdy(nih_fed$BUDGET_START_DATE)
match_fed$BUDGET_END_DATE <- mdy(match_fed$BUDGET_END_DATE)
match_fed$BUDGET_START_DATE <- mdy(match_fed$BUDGET_START_DATE)
#39% of budget end dates are NA for non-matching records, almost none are NA for matching records:
sum(is.na(nmatch_fed$BUDGET_END_DATE))/nrow(nmatch_fed)
sum(is.na(match_fed$BUDGET_END_DATE))/nrow(match_fed)
#39% of budget start dates are NA for non-matching records, none are NA for matching records:
sum(is.na(nmatch_fed$BUDGET_START_DATE))/nrow(nmatch_fed)
sum(is.na(match_fed$BUDGET_START_DATE))/nrow(match_fed)
sum(is.na(nmatch_fed$BUDGET_START_DATE) | is.na(nmatch_fed$BUDGET_END_DATE))/nrow(nmatch_fed)
#how many budgets end before FY16 (1 in non-match)
sum(nmatch_fed$BUDGET_END_DATE < mdy("10-01-2015"), na.rm = TRUE)
#how many start after FY16--less than 1% of both matches and non-matches
sum(nmatch_fed$BUDGET_START_DATE >= mdy("10-01-2016"), na.rm = TRUE)/sum(is.na(nmatch_fed$BUDGET_START_DATE) == FALSE)
sum(match_fed$BUDGET_START_DATE >= mdy("10-01-2016"), na.rm = TRUE)/sum(is.na(match_fed$BUDGET_START_DATE) == FALSE)

nih_fed %>% filter(FY == 2016) %>% ggplot(aes(x = BUDGET_START_DATE))+geom_histogram(stat = "count")+facet_grid(rows = vars(usa_grant_match))
nih_fed %>% filter(FY == 2016) %>% ggplot(aes(x = BUDGET_START_DATE, color = usa_grant_match), stat(density))+geom_histogram(stat = "count")


View(nih_fed %>% filter(FY == 2016, is.na(BUDGET_START_DATE)==FALSE, BUDGET_START_DATE > mdy("9-30-2015")) %>% 
  group_by(year(BUDGET_START_DATE), month(BUDGET_START_DATE)) %>% summarise(count = n(), matchcount = sum(usa_grant_match, na.rm = TRUE)) %>%
  mutate(nmatchcount = count - matchcount))

#differences in centers that are represented? not really--every center that has something in 
#2016 has some matches and some non-matches
nih_fed %>% filter(FY == 2016) %>% ggplot(aes(x = IC_CENTER, fill = usa_grant_match))+geom_bar()

nih_grant_matchflag <- fread(file.path(loc, "nih_usa-spending_grants_2016_matchflag.csv"))
nih_grant_matchflag$resflag <- nih_grant_matchflag$hasmatch
