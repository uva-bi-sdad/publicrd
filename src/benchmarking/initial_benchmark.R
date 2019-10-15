library(data.table)
library(tidyverse)
library(ggmosaic)

###Initial benchmarking

loc <- "data/working/combined_usa-spending"

#combined grant files:
grant <- fread(file.path(loc, "usa-spending_grants_2016_matchflag.csv"))

#combined contract files:
contract <- fread(file.path(loc, "usa-spending_contracts_2016_resflag.csv"))

#benchmarks for higher ed:
bench_loc <- "data/original"
bench <- fread(file.path(bench_loc, "fy16_highered_obligation_benchmark.csv"))

#CFDA codes:
cfda <- fread("data/working/cfda_isSNE_initial.csv")

##For this round, we are ONLY comparing to higher ed data. However, the way recipient
##types are noted in the actual USA Spending data is super weird, but it's easy to down-
##load data that is only for higher ed. So here we are getting all the award IDs
##for higher ed contracts/grants in FY16.

he_loc <- "data/original"
g_highered <- fread(file.path(he_loc, "2016-data_multiple_usa-spending_transaction_grants-only_highered-only.csv"))
c_highered <- fread(file.path(he_loc, "2016-data_multiple_usa-spending_transaction_contracts-only_highered-only.csv"))
g_h_id <- unique(g_highered$award_id_fain)
c_h_id <- unique(c_highered$award_id_piid)

grant_h <- filter(grant, award_id_fain %chin% g_h_id)
contract_h <- filter(contract, award_id_piid %chin% c_h_id)

###checking to see what agencies etc we have in our data--just the ones we care about (yay!)

grant_h %>% group_by(awarding_agency_name) %>% summarise(count = n()) %>% arrange(desc(count))
contract_h %>% group_by(awarding_agency_name) %>% summarise(count = n()) %>% arrange(desc(count))

grant_h %>% filter(awarding_agency_name == "DEPARTMENT OF HEALTH AND HUMAN SERVICES (HHS)") %>% group_by(awarding_sub_agency_name) %>% summarise(count = n()) %>% arrange(desc(count))
contract_h %>% filter(awarding_agency_name == "DEPARTMENT OF HEALTH AND HUMAN SERVICES (HHS)") %>% group_by(awarding_sub_agency_name) %>% summarise(count = n()) %>% arrange(desc(count))

#putting in a column with the clean name we actually care about
rename <- function(x){if(x == "DEPARTMENT OF DEFENSE (DOD)"){
  "DOD"
  } else if(x == "DEPARTMENT OF HEALTH AND HUMAN SERVICES (HHS)"){
  "NIH"
  } else if(x == "DEPARTMENT OF AGRICULTURE (USDA)"){
    "USDA"
  } else if(x == "NATIONAL AERONAUTICS AND SPACE ADMINISTRATION (NASA)"){
    "NASA"
  } else if(x == "DEPARTMENT OF ENERGY (DOE)"){
    "DOE"
  } else if(x == "NATIONAL SCIENCE FOUNDATION (NSF)"){
    "NSF"
  } else{
    "error"
  }
}

grant_h$agency <- sapply(grant_h$awarding_agency_name, rename)   
contract_h$agency <- sapply(contract_h$awarding_agency_name, rename)

grant_h %>% group_by(awarding_agency_name, agency) %>% summarise(count = n())
contract_h %>% group_by(awarding_agency_name, agency) %>% summarise(count = n())

##checking--how much "negative" money is going into each agency (unknown if NCSES tracks this)
grant_h %>% filter(federal_action_obligation < 0) %>% group_by(awarding_agency_name, agency) %>% summarise(total = sum(federal_action_obligation))
contract_h %>% filter(federal_action_obligation < 0) %>% group_by(awarding_agency_name, agency) %>% summarise(total = sum(federal_action_obligation))

###identifying SnE in grants by CFDA codes

cfda_sne <- filter(cfda, isSnE == 1)$code

grant_h$sne_cfda <- grant_h$cfda_number %in% cfda_sne

grant_h %>% group_by(sne_cfda) %>% summarise(count = n())
grant_h %>% group_by(sne_cfda, resflag) %>% summarise(count = n())

grant_h$resflag <- grant_h$sne_cfda + grant_h$fedmatch > 0

grant_h %>% group_by(sne_cfda, fedmatch, resflag) %>% summarise(count = n())
  
###setting up result summary dataframe

summary <- data.frame(agency = unique(contract_h$agency), contract_trans_count = NA, 
                      contract_trans_res_count = NA, contract_obligation_total = NA,
                      contract_obligation_res_total = NA, grant_trans_count = NA,
                      grant_trans_res_count = NA, grant_obligation_total = NA,
                      grant_obligation_res_total = NA,
                      all_trans_count = NA, all_trans_res_count = NA,
                      all_obligation_total = NA, all_obligation_res_total = NA,
                      total_perc_obligation_res = NA)

###getting totals for the contract data

summary$contract_trans_count <- sapply(summary$agency, function(x){nrow(filter(contract_h, agency == x))})
summary$contract_trans_res_count <- sapply(summary$agency, function(x){nrow(filter(contract_h, agency == x,
                                      resflag == TRUE))})
summary$contract_obligation_total <- sapply(summary$agency, function(x){sum(filter(contract_h, agency == x)$federal_action_obligation)})
summary$contract_obligation_res_total <- sapply(summary$agency, function(x){sum(filter(contract_h, agency == x, resflag == TRUE)$federal_action_obligation)})


###getting totals for the grant data


summary$grant_trans_count <- sapply(summary$agency, function(x){nrow(filter(grant_h, agency == x))})
summary$grant_trans_res_count <- sapply(summary$agency, function(x){nrow(filter(grant_h, agency == x,
                                                                                   resflag == TRUE))})
summary$grant_obligation_total <- sapply(summary$agency, function(x){sum(filter(grant_h, agency == x)$federal_action_obligation)})
summary$grant_obligation_res_total <- sapply(summary$agency, function(x){sum(filter(grant_h, agency == x, resflag == TRUE)$federal_action_obligation)})

summary$grant_obligation_non_res_total <- summary$grant_obligation_total - summary$grant_obligation_res_total
summary$contract_obligation_non_res_total <- summary$contract_obligation_total - summary$contract_obligation_res_total


###overall numbers

summary$all_trans_count <- summary$grant_trans_count + summary$contract_trans_count
summary$all_trans_res_count <- summary$grant_trans_res_count + summary$contract_trans_res_count
summary$all_obligation_total <- summary$grant_obligation_total + summary$contract_obligation_total
summary$all_obligation_res_total <- summary$grant_obligation_res_total + summary$contract_obligation_res_total
summary$all_non_res_obligation_total <- summary$all_obligation_total - summary$all_obligation_res_total
summary$total_perc_obligation_res <- summary$all_obligation_res_total/summary$all_obligation_total

###adding in benchmark numbers
summary <- left_join(summary, bench[,c("agency_short", "SnE")], by = c("agency" = "agency_short"))

summary$SnE <- as.numeric(summary$SnE)

###how do we compare to benchmark
summary$perc_of_SnE <- summary$all_obligation_res_total/summary$SnE

summary$non_id_total <- summary$SnE - summary$all_obligation_res_total


###getting a less-shitty-looking version of the summary table
summary_pretty <- select(summary, 1:19)
colnames(summary_pretty) <- c("agency", "con_transaction_count", "con_transaction_SnE_count",
                              "con_obligation_total", "con_obligation_SnE_total",
                              "grant_transaction_count", "grant_transaction_SnE_count",
                              "grant_obligation_total", "grant_obligation_SnE_total",
                              "all_transaction_count", "all_transaction_SnE_count",
                              "all_obligation_total", "all_obligation_SnE_total",
                              "SnE_as_perc_of_total_obligation", "x", "y", "z", "FSS_SnE_benchmark",
                              "percent_of_benchmark_accounted_for")

summary_pretty <- select(summary_pretty, 1:14, 18:19)

write.csv(summary_pretty, "src/benchmarking/initial_benchmark_summary.csv")

######Counts of transactions attributable to various identifiers (in progress)

how_id_grant <- grant_h %>% group_by(fedmatch, sne_cfda, resflag) %>% summarise(count = n(), total = sum(federal_action_obligation))

colnames(how_id_grant) <- c("FedRePORTER", "CFDA", "overall_SnE_match", "transaction_count", "total_obligation_amount")
write.csv(summary_pretty, "src/benchmarking/initial_benchmark_how_id.csv")

#how_id <- data.frame(agency = unique(contract_h$agency), all_notSnE = NA, all_SnE = NA, 
#                     all_fedRepMatch_only = NA, all_cfdaMatch_only = NA, all_fedRepMatch_and_cfdaMatch = NA,
#                     all_productSupportMatch = NA, grant_notSnE = NA, grant_SnE = NA, 
#                    grant_fedRepMatch_only = NA, grant_cfdaMatch_only = NA, grant_fedRepMatch_and_cfdaMatch = NA,
#                     grant_productSupportMatch = NA, contract_notSnE = NA, contract_SnE = NA, 
#                    contract_fedRepMatch_only = NA, contract_cfdaMatch_only = NA, contract_fedRepMatch_and_cfdaMatch = NA,
#                    contract_productSupportMatch = NA)
#
#how_id$grant_notSnE <- sapply(how_id$agency, function(x){sum(filter(grant_h, agency == x)$resflag == FALSE)})
#how_id$grant_SnE <- sapply(how_id$agency, function(x){sum(filter(grant_h, agency == x)$resflag == TRUE)})
#how_id$grant_cfdaMatch_only <- sapply(how_id$agency, function(x){sum(grant_h$agency == x & grant_h$cfda_sne == TRUE & grant_h$fedmatch == FALSE)})
#  
#nrow(filter(grant_h, agency == x, cfda_sne == TRUE, fedmatch == FALSE))})

#how_id$contract_notSnE <- sapply(how_id$agency, function(x){sum(filter(contract_h, agency == x)$resflag == FALSE)})
#how_id$contract_SnE <- sapply(how_id$agency, function(x){sum(filter(contract_h, agency == x)$resflag == TRUE)})



############ MAKING PLOTS ############ 


##long-format data
res_non_res <- melt(summary, id.vars = c("agency"), measure.vars = c("grant_obligation_res_total", 
                "grant_obligation_non_res_total", "contract_obligation_res_total", 
                "contract_obligation_non_res_total"))
rnr_awardtype <- function(x){if(x %in% c("grant_obligation_res_total", "grant_obligation_non_res_total")){
  "grant"
} else if (x %in% c("contract_obligation_non_res_total", "contract_obligation_res_total")){
  "contract"
} else{
  "error"
}}
rnr_sne <- function(x){if(x %in% c("grant_obligation_res_total", "contract_obligation_res_total")){
  "science and engineering"
} else if (x %in% c("contract_obligation_non_res_total", "grant_obligation_non_res_total")){
  "other"
} else{
  "error"
}}
res_non_res$awardtype <- sapply(res_non_res$variable, rnr_awardtype)
res_non_res$science_eng <- sapply(res_non_res$variable, rnr_sne)

res_non_res$science_eng <- factor(res_non_res$science_eng, levels = c("science and engineering", "other"))

#mosaic plots
ggplot(res_non_res) + geom_mosaic(aes(x = product(science_eng, awardtype), weight = value, fill = science_eng)) + 
  facet_wrap("agency") + labs(title = "FY16 Grant & Contract Funding to Higher Education", x = "Award Type", y = "Spending Type")

###benchmark bars
sne_bench <- melt(summary, id.vars = c("agency"), measure.vars = c("all_obligation_res_total", 
                      "SnE", "non_id_total"))

sne <- summary %>% select(agency, SnE) %>% mutate(type = "FSS Benchmark")
colnames(sne) <- c("agency", "amount", "type")
fss <- summary %>% select(agency, all_obligation_res_total) %>% mutate(type = "S&E Spending")
colnames(fss) <- c("agency", "amount", "type")

sne_bench_2 <- rbind(sne, fss)

bench_rename <- function(x){if(x == "all_obligation_res_total"){
  "Identified S&E"
} else if (x == "SnE"){
  "FSS benchmark"
} else if(x == "non_id_total"){
  "Not identified as S&E"
} else{
  "error"
}
  }

sne_bench$type <- sapply(sne_bench$variable, bench_rename)

sne_bench[sne_bench$agency == "DOD" & sne_bench$type == "Not identified as S&E", "value"] <- 0

ggplot(sne_bench_2) + geom_bar(aes(x = agency, y = amount/1000000000, group = type, fill = type), stat = "identity", 
                               position = "dodge") + labs(title = "Identified science and engineering spending compared to Federal Support Survey",
                              subtitle = "Among awards to institutes ofhigher education in FY16", y = "amount (billions)") + theme_minimal()

ggplot() + geom_bar(data = filter(sne_bench_2, type == "FSS Benchmark"), aes(x = agency, y = amount, group = type, fill = type), stat = "identity", alpha = .5) + 
  geom_bar(data = filter(sne_bench_2, type == "S&E Spending"), aes(x = agency, y = amount, group = type, fill = type), stat = "identity", alpha = .5) + 
  scale_fill_manual(values=c("yellow", "blue")) +
  labs(title = "Identified science and engineering spending compared to Federal Support Survey",
                                                          subtitle = "Among awards to institutes ofhigher education in FY16")+ theme_minimal()

###the following plot sucks and I can't get it to work :(
#ggplot(filter(sne_bench, type %in% c("Identified S&E", "Not identified as S&E"))) + geom_bar(aes(x = agency, y = value, group = type, fill = type), stat = "identity", 
#                             position = "fill") + labs(title = "Identified science and engineering spending compared to Federal Support Survey",
#                             subtitle = "Among awards to institutes ofhigher education in FY16") + coord_flip()


