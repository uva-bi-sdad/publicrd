# can we benchmark NSF R&D spending to JHU, VCU, JMU for FY18?

library(readr)
library(ggplot2)
library(dplyr)
library(naniar)
library(stringr)
library(readxl)
library(tidyr)

#
# DATA INGESTION: read in FY18 data - for contracts and grants from NSF ------------------------------------------
#

# grants

nsf_g <- read_csv("data/prd/USAspending_Exploration/NSF/FY18/All_Assistance_PrimeTransactions_2020-09-25_H18M10S45_1.csv")

# contracts

nsf_c <- read_csv("data/prd/USAspending_Exploration/NSF/FY18/All_Contracts_PrimeTransactions_2020-09-25_H18M07S04_1.csv")

nsf_c_he <- nsf_c %>% 
  filter(educational_institution == TRUE)  # only 5 of these, no RnD/SnE PSCs...we can ignore

#
# FILTER: hopkins only.  JHU duns is 001910777
#

jhu_g <- nsf_g %>%
  filter(recipient_duns == "001910777")    # 154 grants

jhu_c <- nsf_c %>%
  filter(recipient_duns == "001910777")    # no contracts to JHU

jhu_g_parent <- nsf_g %>%
  filter(recipient_parent_duns == "001910777")   # 154

identical(jhu_g, jhu_g_parent)  #TRUE -- I'll just use jhu_g


# number of unique projects

length(unique(jhu_g$award_id_fain))  #136


#
# BENCHMARKING ---------------------------------------------------
#


#benchmark from FSS for NSF RnD to JHU : 35,824,300
#benchmark from FSS for NSF SnE to JHU : 36,662,100

# get CFDAs 

table(jhu_g$cfda_number)  # we filtered all of these CFDAs as SnE
table(jhu_g$cfda_title)

# SnE estimate

# USAspending amount of federal dollars from NSF to John's Hopkins, 2018

sum(jhu_g$federal_action_obligation)  # 41,548,497 --- about 5 mil too much

# difference to benchmark

36662100 - sum(jhu_g$federal_action_obligation)   # -4,886,397


cfda_sum <- jhu_g %>%
  group_by(cfda_number, cfda_title) %>%
  summarise(dollars = sum(federal_action_obligation)) 


#
# FIND COMBINATION THAT SUMS TO 35,824,300 - can do, but should I?
#



#
#  Try with VCU --------------------------------------------------------------
#

vcu_g <- nsf_g %>%
  filter(recipient_duns == "105300446")    # 44 grants, no results for duns = "194797346"

vcu_c <- nsf_c %>%
  filter(recipient_duns == "105300446")    # no contracts to VCU

vcu_g_parent <- nsf_g %>%
  filter(recipient_parent_duns == "105300446")   # 44

identical(vcu_g, vcu_g_parent)  #TRUE -- I'll just use vcu_g

# number of unique projects

length(unique(vcu_g$award_id_fain))  # 35

#
# BENCHMARKING ---------------------------------------------------
#


#benchmark from FSS table 15 for NSF RnD to VCU : 3,772,100
#benchmark from FSS table 13 for NSF SnE to VCU : 5,388,100

# get CFDAs 

table(vcu_g$cfda_number)  # we filtered all of these CFDAs as SnE
table(vcu_g$cfda_title)

# SnE estimate

# USAspending amount of federal dollars from NSF to John's Hopkins, 2018

sum(vcu_g$federal_action_obligation)  # 6,681,524 --- about 1.3 mil too much

# difference to benchmark

5388100 - sum(vcu_g$federal_action_obligation)   # -1,293,424

cfda_sum <- vcu_g %>%
  group_by(cfda_number, cfda_title) %>%
  summarise(dollars = sum(federal_action_obligation)) 

write_csv(vcu_g, "vcu_case_study.csv")


#
# JMU -----------------------------------------
# 

jmu_g <- nsf_g %>%
  filter(recipient_duns == "879325355")    # 17 grants

jmu_c <- nsf_c %>%
  filter(recipient_duns == "879325355")    # no contracts 

jmu_g_parent <- nsf_g %>%
  filter(recipient_parent_duns == "879325355")   # 17

identical(jmu_g, jmu_g_parent)  #TRUE 

# number of unique projects

length(unique(jmu_g$award_id_fain))  # 16

#
# BENCHMARKING ---------------------------------------------------
#


#benchmark from FSS table 15 for NSF RnD to JMU : 1,639,900
#benchmark from FSS table 13 for NSF SnE to JMU : 2,796,500

# get CFDAs 

table(jmu_g$cfda_number)  # we filtered all of these CFDAs as SnE
table(jmu_g$cfda_title)

# SnE estimate

# USAspending amount of federal dollars from NSF to John's Hopkins, 2018

sum(jmu_g$federal_action_obligation)  # 2,938,119 --- about 150,000 too much

# difference to benchmark

2796500 - sum(jmu_g$federal_action_obligation)   # -141,619

cfda_sum <- jmu_g %>%
  group_by(cfda_number, cfda_title) %>%
  summarise(dollars = sum(federal_action_obligation)) 

write_csv(jmu_g, "jmu_case_study.csv")

#
#  Look for combination that gets within 1700 of benchmark (bc of rounding - each entry could be off by < 100)
#

dollars <- jmu_g$federal_action_obligation
bm <- 2796500
perm_save <- rep(0,100)
diff <- rep(0,100)

j = 0
for(i in 1:(2^17-1))
{
  idx <- as.logical(intToBits(i))
  
  amount <- sum(dollars[idx])
  
  if( (amount >= (bm - 3250)) & (amount <= (bm + 3250)) ) 
  {
    perm_save[j] <- i
    diff[j] <- bm - amount 
    j = j + 1
  }
  
}
