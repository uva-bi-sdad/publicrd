# NSF FY18 grants (prime and sub) to VA Universities/Colleges

library(readr)
library(ggplot2)
library(dplyr)
library(naniar)
library(stringr)
library(readxl)
library(tidyr)

# 
# DATA INGESTION ------------------------------------------------------------------------
#

# USAspending - NSF FY18 grants

# prime grants
nsf_g18 <- read_csv("data/prd/USAspending_Exploration/NSF/FY18/All_Assistance_PrimeTransactions_2020-09-25_H18M10S45_1.csv")
# sub grants
nsf_sg18 <- read_csv("data/prd/USAspending_Exploration/NSF/FY18/All_Assistance_Subawards_2021-01-07_H17M19S13_1.csv")

# write out subgrants to rds file
#write_rds(nsf_sg18, "src/USAspending_Exploration_Fall2020/Case_Studies/VA_SnE_App/usa_nsf_fy18_sub.rds")


# USAspending - NSF F18 contracts (Need to check that there is no contract data for VA schools)
nsf_c18 <- read_csv("data/prd/USAspending_Exploration/NSF/FY18/All_Contracts_PrimeTransactions_2020-09-25_H18M07S04_1.csv")


# FSS   
fss_18 <- read_excel("src/USAspending_Exploration_Fall2020/Case_Studies/fss18-dt-tab013_edit.xlsx", skip = 3)
fss_18$duns_number <- str_pad(fss_18$duns_number, 9, pad = "0")

# IPEDS: duns numbers and location data for VA schools
ipeds <- read_csv("src/USAspending_Exploration_Fall2020/Case_Studies/ipeds_VA.csv")
ipeds$`Dun and Bradstreet numbers (HD2019)` <- str_pad(ipeds$`Dun and Bradstreet numbers (HD2019)`, 9, pad = "0")
colnames(ipeds) <- c("UnitID", "Institution", "duns_number", "longitude", "latitude")


#
#  SnE DOLLARS DATASET: NSF, FY18 recipients, filtered for VA universities/colleges ------------------------------------------------------------------
#

# ***ASSUMPTION: All NSF FY18 grants are SnE****

# **grants money**

# USAspending SnE dollar amounts for every recipient

# prime awards
usa_18_prime <- nsf_g18 %>%
  group_by(recipient_duns) %>% # recipient_name, 
  summarise(USA_SnE_prime = sum(federal_action_obligation))

# sub awards
usa_18_sub <- nsf_sg18 %>%
  group_by(subawardee_duns) %>% # subawardee_name, 
  summarise(USA_SnE_sub = sum(subaward_amount))

# merge prime and sub awards
usa_18 <- merge(usa_18_prime, usa_18_sub, by.x = "recipient_duns", by.y = "subawardee_duns", all = TRUE)


# filter USAspending for only VA school duns numbers -- join fss_18 & usa_18 and then ipeds on duns numbers
#*** This missed ODU Research Foundation

#VA_duns <- c("105300446", "077817450", "879325355")  # VCU, GMU, JMU
#VA_schools <- grouped_df %>% 
#  filter(recipient_duns %in% VA_duns)

# add FSS dollars to VA schools USAspending data -- this is a short term solution. We will most likely need a 
# text (probabilistic) matching algorithm. For now, I copied and pasted duns numbers from IPEDS into the FSS data.

VA_schools <- merge(fss_18[fss_18$State == "Virginia", c(1,3,12)], 
                    usa_18[!is.na(usa_18$recipient_duns), ], 
                    by.x = "duns_number", by.y = "recipient_duns", all.x = TRUE)

VA_schools <- merge(VA_schools, ipeds[!is.na(ipeds$duns_number), 3:5], by = "duns_number", all.x = TRUE)

VA_schools <- VA_schools %>%
  rename(fss_institution = `State, outlying area, and institution`,
         #usa_recipient_name = recipient_name,
         FSS_SnE = NSF)
VA_schools$FSS_SnE <- VA_schools$FSS_SnE*1000

# ODU duns need to be replaced with the duns for ODU Research Foundation (ODU was not in USAspending)

odu_row <- VA_schools[VA_schools$duns_number == "041448465", ]
odu_row$duns_number <- "077945947"
#odu_row$usa_recipient_name <- usa_18[!is.na(usa_18$recipient_duns) & usa_18$recipient_duns == "077945947", "recipient_name"]
odu_row$USA_SnE_prime <- usa_18[!is.na(usa_18$recipient_duns) & usa_18$recipient_duns == "077945947", "USA_SnE_prime"]
odu_row$USA_SnE_sub <- usa_18[!is.na(usa_18$recipient_duns) & usa_18$recipient_duns == "077945947", "USA_SnE_sub"]
VA_schools[VA_schools$duns_number == "041448465", ] <- odu_row

# calculate the percent difference between FSS and USAspending

# fix data types first
#VA_schools$USA_SnE_prime <- as.numeric(VA_schools$USA_SnE)
#VA_schools$usa_recipient_name <- as.character(VA_schools$usa_recipient_name)

VA_schools$USA_SnE_total <- rowSums(VA_schools[ , 4:5], na.rm = TRUE)
VA_schools$pct_diff <- 100*(VA_schools$FSS_SnE - VA_schools$USA_SnE_total)/VA_schools$FSS_SnE
VA_schools <- VA_schools[ , c(1,2,6,7,3,4,5,8,9)]

# **contracts money**

# USAspending SnE dollar amounts for every recipient
usa_c <- nsf_c18 %>%
  group_by(recipient_name, recipient_duns) %>%
  summarise(USA_SnE = sum(federal_action_obligation))


# check if any of the VA schools were awarded NSF contracts

t1 <- merge(VA_schools, usa_c, by.x = "duns_number", by.y = "recipient_duns", all.x = TRUE) # no contract data
t2 <- usa_c[usa_c$recipient_duns == "041448465", ] # empty, ODU duns (instead of ODU Research Foundation)
rm(t1, t2, usa_c, nsf_c18)


#
# WRITE DATASET ----------------------------------------------------------------------------------
#

write_rds(VA_schools, "src/USAspending_Exploration_Fall2020/Case_Studies/VA_SnE.rds")




# # number of organizations supported
# 
# length(unique(nsf_g18$recipient_duns))  # 1957
# length(unique(nsf_g18$recipient_name))  # 1860
# 
# table(nsf_g18$cfda_number)
# table(nsf_g18$cfda_title)
# 
# t1 <- unique(nsf_g18$recipient_name)



