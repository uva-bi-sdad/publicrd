library(R.utils)
library(tidyverse)
library(data.table)
library(lubridate)

loc <- file.path("data/original")
USAspend <- fread(file.path(loc, "2016-data_multiple_usa-spending_transaction_prime.csv"))
USAspend_prime <- fread(file.path(loc, "2016-data_multiple_usa-spending-prime-awards.csv"))


USAspend$period_of_performance_start_date <- as.Date(USAspend$period_of_performance_start_date, "%Y-%m-%d")
sum(is.na(USAspend$period_of_performance_start_date))/nrow(USAspend)
min(USAspend$period_of_performance_start_date, na.rm = TRUE)
max(USAspend$period_of_performance_start_date, na.rm = TRUE)

USAspend_project <- USAspend %>% group_by(award_id_fain, period_of_performance_start_date) %>% summarise(transaction_count = n(), transaction_total = sum(federal_action_obligation))

ggplot(USAspend_project)+geom_histogram(aes(x=period_of_performance_start_date), bins = year(max(USAspend$period_of_performance_start_date, na.rm = TRUE)) - year(min(USAspend$period_of_performance_start_date, na.rm = TRUE)))
ggplot(USAspend_project)+geom_jitter(aes(x=period_of_performance_start_date, y=1), alpha = .1)

sum(USAspend_project$period_of_performance_start_date >= as.Date("2015-10-01") & USAspend_project$period_of_performance_start_date < as.Date("2016-10-01"), na.rm = TRUE)/nrow(USAspend)
sum(USAspend_project$period_of_performance_start_date >= as.Date("2014-10-01") & USAspend_project$period_of_performance_start_date < as.Date("2015-10-01"), na.rm = TRUE)/nrow(USAspend)
sum(USAspend_project$period_of_performance_start_date >= as.Date("2013-10-01") & USAspend_project$period_of_performance_start_date < as.Date("2014-10-01"), na.rm = TRUE)/nrow(USAspend)
sum(USAspend_project$period_of_performance_start_date >= as.Date("2012-10-01") & USAspend_project$period_of_performance_start_date < as.Date("2013-10-01"), na.rm = TRUE)/nrow(USAspend)


sum(USAspend_project$period_of_performance_start_date >= as.Date("2015-10-01") & USAspend_project$period_of_performance_start_date < as.Date("2016-10-01"), 
    USAspend_project$period_of_performance_start_date >= as.Date("2014-10-01") & USAspend_project$period_of_performance_start_date < as.Date("2015-10-01"),
    USAspend_project$period_of_performance_start_date >= as.Date("2013-10-01") & USAspend_project$period_of_performance_start_date < as.Date("2014-10-01"), na.rm = TRUE)/nrow(USAspend)

USAspend_2016start <- USAspend[USAspend$period_of_performance_start_date >= as.Date("2015-10-01") & USAspend$period_of_performance_start_date < as.Date("2016-10-01"),]    

###attempts to make IDs don't have one simple solution, UNFORTUNATELY. not sure if this is because of different
###types of IDs between agencies, or because intramural grants tend to have different formatting...? also why
###is intramural grant data in this download

fed_2016 <- fread(file.path(loc, "2016-data_multiple_fed-reporter.csv"))
fed_2016$USA_ID <- substr(fed_2016$PROJECT_NUMBER, 2, 12)

###fedreporter includes a number of governmental recipients, including National Institutes of Health, Forest Service, and Agricultural Research Service:

USA_2016_recip <- USAspend_2016start %>% group_by(recipient_name) %>% summarise(count = n(), total = sum(federal_action_obligation)) %>% arrange(desc(count))
fed_2016_recip <- fed_2016 %>% group_by(ORGANIZATION_NAME, DUNS_NUMBER) %>% summarise(count = n(), total = sum(FY_TOTAL_COST)) %>% arrange(desc(count))

table(nchar(fed_2016$PROJECT_NUMBER))

fed_id_length <- fed_2016 %>% mutate(id_length = nchar(PROJECT_NUMBER)) %>% group_by(id_length) %>% summarise(count = n()) %>% arrange(desc(count))

id_char_15 <- fed_2016 %>% mutate(id_length = nchar(PROJECT_NUMBER)) %>% filter(id_length == 15) %>% group_by(ORGANIZATION_NAME) %>% summarise(count = n()) %>% arrange(desc(count))
id_char_7 <- fed_2016 %>% mutate(id_length = nchar(PROJECT_NUMBER)) %>% filter(id_length == 7) %>% group_by(ORGANIZATION_NAME) %>% summarise(count = n()) %>% arrange(desc(count))
id_char_22 <- fed_2016 %>% mutate(id_length = nchar(PROJECT_NUMBER)) %>% filter(id_length == 22) %>% group_by(ORGANIZATION_NAME) %>% summarise(count = n()) %>% arrange(desc(count))
id_char_17 <- fed_2016 %>% mutate(id_length = nchar(PROJECT_NUMBER)) %>% filter(id_length == 17) %>% group_by(ORGANIZATION_NAME) %>% summarise(count = n()) %>% arrange(desc(count))

View(fed_2016 %>% mutate(id_length = nchar(PROJECT_NUMBER)) %>% group_by(DEPARTMENT, AGENCY, id_length) %>% summarise(count = n()) %>% arrange(DEPARTMENT, AGENCY))

table(nchar(USAspend_2016start$award_id_fain))

usa_id_length <- USAspend_2016start %>% mutate(id_length = nchar(award_id_fain)) %>% group_by(id_length) %>% summarise(count = n()) %>% arrange(desc(count))
View(USAspend_2016start %>% mutate(id_length = nchar(award_id_fain)) %>% group_by(awarding_agency_name, awarding_sub_agency_name, id_length) %>% summarise(count = n()) %>% arrange(awarding_agency_name))

###matching IDs among agencies that have the same character lengths, comparing projects that were
###awarded in 2016 to 2016 transactions in USASpending

usa_nsf <- USAspend %>% filter(awarding_sub_agency_name == "NATIONAL SCIENCE FOUNDATION")
fed_2016_nsf <- fed_2016 %>% filter(AGENCY == "NSF")

sum(fed_2016_nsf$PROJECT_NUMBER %in% usa_nsf$award_id_fain)
sum(fed_2016_nsf$PROJECT_NUMBER %in% usa_nsf$award_id_fain)/nrow(fed_2016_nsf)

fed_2016_nsf <- mutate(fed_2016_nsf, in_USA = PROJECT_NUMBER %in% usa_nsf$award_id_fain)

sum(fed_2016_nsf$in_USA)

fed_2016_nsf$PROJECT_START_DATE <- mdy(fed_2016_nsf$PROJECT_START_DATE)
ggplot(data = fed_2016_nsf) + geom_histogram(aes(x = PROJECT_START_DATE), bins = 50)+
  facet_grid(rows=vars(in_USA))+labs(title = "Start dates of FedReporter NSF projects awarded in 2016",
                                     subtitle = "Split by whether or not they match to a 2016 transaction in USASpending")

ggplot(data = fed_2016_nsf) + geom_histogram(aes(x = log(FY_TOTAL_COST)), bins = 50)+
  facet_grid(rows=vars(in_USA))+labs(title = "Total cost of FedReporter NSF projects awarded in 2016",
  subtitle = "Split by whether or not they match to a 2016 transaction in USASpending")

##checking to see if a large number of USASpending grants had NSF as the funding agency but not
##the awarding agency (nope):

nrow(USAspend %>% filter(funding_sub_agency_name == "NATIONAL SCIENCE FOUNDATION", awarding_sub_agency_name != "NATIONAL SCIENCE FOUNDATION"))

fed_2016_nsf %>% filter(in_USA == FALSE) %>% group_by(ORGANIZATION_NAME) %>% summarise(count = n()) %>% arrange(desc(count))

##uploading USAspend data for 2017 to see if I can match the remaining IDs (nope, got nothing)

usa_2017_nsf <- fread(file.path(loc, "2017-data_nsf_usa-spending_transaction_prime.csv"))
usa_2017_nsf$award_id_fain <- as.character(usa_2017_nsf$award_id_fain)
fed_2016_nsf_nmatch <- fed_2016_nsf %>% filter(in_USA == FALSE) 
sum(fed_2016_nsf_nmatch$PROJECT_NUMBER %in% usa_2017_nsf$award_id_fain)
table(nchar(usa_2017_nsf$award_id_fain))


##uploading USAspend data for 2015 to see if I can match the remaining IDs (nope, got nothing)
usa_2015_nsf <- fread(file.path(loc, "2015-data_nsf_usa-spending_transaction_prime.csv"))
usa_2015_nsf$award_id_fain <- as.character(usa_2015_nsf$award_id_fain)

fed_2016_nsf_nmatch <- fed_2016_nsf %>% filter(in_USA == FALSE) 
sum(fed_2016_nsf_nmatch$PROJECT_NUMBER %in% usa_2015_nsf$award_id_fain)
table(nchar(usa_2015_nsf$award_id_fain))

###looking at 2016 USA Spending sub-award transactions (no matches there either)
usa_2016_sub <- fread(file.path(loc, "2016-data_multiple_usa-spending_transaction_sub.csv"))
sum(fed_2016_nsf_nmatch$PROJECT_NUMBER %in% usa_2016_sub$prime_award_fain)

##CHECKING FOR NASA:

usa_nasa <- USAspend %>% filter(awarding_sub_agency_name == "NATIONAL AERONAUTICS AND SPACE ADMINISTRATION")
fed_2016_nasa <- fed_2016 %>% filter(AGENCY == "NASA")

sum(fed_2016_nasa$PROJECT_NUMBER %in% usa_nasa$award_id_fain)
sum(fed_2016_nasa$PROJECT_NUMBER %in% usa_nasa$award_id_fain)/nrow(fed_2016_nasa)
fed_2016_nasa <- mutate(fed_2016_nasa, in_USA = PROJECT_NUMBER %in% usa_nasa$award_id_fain)
fed_2016_nasa_nmatch <- filter(fed_2016_nasa, in_USA == FALSE)

usa_nasa <- mutate(usa_nasa, in_fed = award_id_fain %in% fed_2016_nasa$PROJECT_NUMBER)
sum(usa_nasa$in_fed)/nrow(usa_nasa)

View(fed_2016_nasa_nmatch %>% group_by(ORGANIZATION_NAME) %>% summarise(count = n()) %>% arrange(desc(count)))

ggplot(data = usa_nasa) + geom_histogram(aes(x = period_of_performance_start_date), bins = 50)+
  facet_grid(rows=vars(in_fed))+labs(title = "Start dates of USASpending NASA projects",
                                     subtitle = "Split by whether or not they match to a 2016 award in FedRePORTER")

sum(usa_nasa$total_funding_amount)
sum(filter(usa_nsf, total_funding_amount < 0)$total_funding_amount)

sum(usa_nsf$total_funding_amount)
sum(filter(usa_nasa, total_funding_amount < 0)$total_funding_amount)

#similar check for NSF: how many 2016 NSF USASpending projects are included in FedReporter 2016?

usa_nsf <- mutate(usa_nsf, in_fed = award_id_fain %in% fed_2016_nsf$PROJECT_NUMBER)
sum(usa_nsf$in_fed)
sum(usa_nsf$in_fed)/nrow(usa_nsf)


##how many of the nasa usaspending transactions come from awards with start dates in FY16, FY15, or FY14?

USA_nasa_project <- USAspend %>% filter(awarding_sub_agency_name == "NATIONAL AERONAUTICS AND SPACE ADMINISTRATION") %>% group_by(award_id_fain, period_of_performance_start_date) %>% summarise(transaction_count = n(), transaction_total = sum(federal_action_obligation))

USA_nasa_project$period_of_performance_start_date <- as.Date(USA_nasa_project$period_of_performance_start_date, "%Y-%m-%d")

sum(USA_nasa_project$period_of_performance_start_date >= as.Date("2015-10-01") & USA_nasa_project$period_of_performance_start_date < as.Date("2016-10-01"), 
    USA_nasa_project$period_of_performance_start_date >= as.Date("2014-10-01") & USA_nasa_project$period_of_performance_start_date < as.Date("2015-10-01"),
    USA_nasa_project$period_of_performance_start_date >= as.Date("2013-10-01") & USA_nasa_project$period_of_performance_start_date < as.Date("2014-10-01"), na.rm = TRUE)/nrow(USA_nasa_project)

#what if I add in 2013?

sum(USA_nasa_project$period_of_performance_start_date >= as.Date("2015-10-01") & USA_nasa_project$period_of_performance_start_date < as.Date("2016-10-01"), 
    USA_nasa_project$period_of_performance_start_date >= as.Date("2014-10-01") & USA_nasa_project$period_of_performance_start_date < as.Date("2015-10-01"),
    USA_nasa_project$period_of_performance_start_date >= as.Date("2013-10-01") & USA_nasa_project$period_of_performance_start_date < as.Date("2014-10-01"), 
    USA_nasa_project$period_of_performance_start_date >= as.Date("2012-10-01") & USA_nasa_project$period_of_performance_start_date < as.Date("2013-10-01"), na.rm = TRUE)/nrow(USA_nasa_project)


##how many of the nsf usaspending transactions come from awards with start dates in FY16, FY15, or FY14?

USA_nsf_project <- USAspend %>% filter(awarding_sub_agency_name == "NATIONAL SCIENCE FOUNDATION") %>% group_by(award_id_fain, period_of_performance_start_date) %>% summarise(transaction_count = n(), transaction_total = sum(federal_action_obligation))

USA_nsf_project$period_of_performance_start_date <- as.Date(USA_nsf_project$period_of_performance_start_date, "%Y-%m-%d")

sum(USA_nsf_project$period_of_performance_start_date >= as.Date("2015-10-01") & USA_nsf_project$period_of_performance_start_date < as.Date("2016-10-01"), 
    USA_nsf_project$period_of_performance_start_date >= as.Date("2014-10-01") & USA_nsf_project$period_of_performance_start_date < as.Date("2015-10-01"),
    USA_nsf_project$period_of_performance_start_date >= as.Date("2013-10-01") & USA_nsf_project$period_of_performance_start_date < as.Date("2014-10-01"), na.rm = TRUE)/nrow(USA_nsf_project)

#what if I add in FY13?
sum(USA_nsf_project$period_of_performance_start_date >= as.Date("2015-10-01") & USA_nsf_project$period_of_performance_start_date < as.Date("2016-10-01"), 
    USA_nsf_project$period_of_performance_start_date >= as.Date("2014-10-01") & USA_nsf_project$period_of_performance_start_date < as.Date("2015-10-01"),
    USA_nsf_project$period_of_performance_start_date >= as.Date("2013-10-01") & USA_nsf_project$period_of_performance_start_date < as.Date("2014-10-01"), 
    USA_nsf_project$period_of_performance_start_date >= as.Date("2012-10-01") & USA_nsf_project$period_of_performance_start_date < as.Date("2013-10-01"), na.rm = TRUE)/nrow(USA_nsf_project)


#looking at NASA--taking into account that universities/nonprofits might have been miscategorized--how much 
#external grant funding is included in FY16 across all recipient types?
usa_nasa_allsource <- fread(file.path(loc, "2016-data_nasa_usa-spending_transaction_prime_all-recip-but-gov.csv"))
sum(usa_nasa_allsource$federal_action_obligation)
sum(usa_nasa_allsource$federal_action_obligation)/1454755200

##pulling together fedreporter id #s for NSF and NASA projects with award notification dates in FY16, FY15, FY14

fed_2015 <- fread(file.path(loc, "2015-data_multiple_fed-reporter.csv"))
fed_2015_nasa_nsf <- fed_2015[DEPARTMENT %in% c("NSF", "NASA")]

fed_2014 <- fread(file.path(loc, "2014-data_multiple_fed-reporter.csv"))
fed_2014_nasa_nsf <- fed_2014[DEPARTMENT %in% c("NSF", "NASA")]

proj_nsf_16_15_14 <- rbind(select(fed_2016_nsf, PROJECT_NUMBER, DEPARTMENT, FY_TOTAL_COST), 
                           select(filter(fed_2015_nasa_nsf, DEPARTMENT == "NSF"), PROJECT_NUMBER, DEPARTMENT, FY_TOTAL_COST), 
                           select(filter(fed_2014_nasa_nsf, DEPARTMENT == "NSF"), PROJECT_NUMBER, DEPARTMENT, FY_TOTAL_COST))

USA_nsf_project <- mutate(USA_nsf_project, in_fed_16_15_14 = award_id_fain %in% proj_nsf_16_15_14$PROJECT_NUMBER)

sum(USA_nsf_project$in_fed_16_15_14)/nrow(USA_nsf_project)

sum(filter(USA_nsf_project, period_of_performance_start_date < as.Date("2016-10-01"), 
       period_of_performance_start_date >= as.Date("2013-10-01"))$in_fed_16_15_14)/nrow(filter(USA_nsf_project, period_of_performance_start_date < as.Date("2016-10-01"), 
       period_of_performance_start_date >= as.Date("2013-10-01")))

proj_nasa_16_15_14 <- rbind(select(fed_2016_nasa, PROJECT_NUMBER, DEPARTMENT, FY_TOTAL_COST), 
                           select(filter(fed_2015_nasa_nsf, DEPARTMENT == "NASA"), PROJECT_NUMBER, DEPARTMENT, FY_TOTAL_COST), 
                           select(filter(fed_2014_nasa_nsf, DEPARTMENT == "NASA"), PROJECT_NUMBER, DEPARTMENT, FY_TOTAL_COST))

USA_nasa_project <- mutate(USA_nasa_project, in_fed_16_15_14 = award_id_fain %in% proj_nasa_16_15_14$PROJECT_NUMBER)
sum(USA_nasa_project$in_fed_16_15_14)/nrow(USA_nasa_project)
sum(filter(USA_nasa_project, period_of_performance_start_date < as.Date("2016-10-01"), 
           period_of_performance_start_date >= as.Date("2013-10-01"))$in_fed_16_15_14)/
            nrow(filter(USA_nasa_project, period_of_performance_start_date < as.Date("2016-10-01"), 
            period_of_performance_start_date >= as.Date("2013-10-01")))
