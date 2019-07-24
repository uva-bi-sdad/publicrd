###experimenting with CFDA codes in the Department of Energy data, since we can't match any of it to 
###Fed RePORTER

library(data.table)
library(tidyverse)
library(lubridate)
library(stringr)

loc <- "data/working/combine_by_department/DOE"

doe_grant <- fread(file.path(loc, "doe_usa-spending_grants_2016.csv"), drop = "V1")

sum(is.na(doe_grant$cfda_number))

doe_cfda <- doe_grant %>% group_by(cfda_number, cfda_title) %>% summarise(count = n()) %>% arrange(desc(count))

doe_cfda_title <- unique(doe_grant$cfda_title)