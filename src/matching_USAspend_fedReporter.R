library(R.utils)
library(tidyverse)
library(data.table)

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
