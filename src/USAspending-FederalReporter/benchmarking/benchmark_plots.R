library(data.table)
library(tidyverse)
library(dplyr)
library(ggmosaic)

#Summary table generated from "src/USAspending-FederalReporter/benchmarking/benchmark_with_CFDA"

summary <- read_csv("src/USAspending-FederalReporter/benchmarking/usaspending_benchmark_summary.csv")

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

sne <- summary %>% select(agency, SnE) %>% mutate(type = "FSS Result")
colnames(sne) <- c("agency", "amount", "type")
fss <- summary %>% select(agency, all_obligation_res_total) %>% mutate(type = "USAspending Estimate")
colnames(fss) <- c("agency", "amount", "type")

sne_bench_2 <- rbind(sne, fss)

bench_rename <- function(x){if(x == "all_obligation_res_total"){
  "Identified S&E"
} else if (x == "SnE"){
  "FSS Result"
} else if(x == "non_id_total"){
  "Not identified as S&E"
} else{
  "error"
}
}

sne_bench$type <- sapply(sne_bench$variable, bench_rename)

sne_bench[sne_bench$agency == "DOD" & sne_bench$type == "Not identified as S&E", "value"] <- 0

ggplot(sne_bench_2) + geom_bar(aes(x = agency, y = amount/1000000000, group = type, fill = type), stat = "identity",
                               position = "dodge") + labs(title = "", subtitle = "", 
                                                          y = "Amount (Billions)", x = "Agency") + theme_minimal(base_size = 16) + scale_fill_discrete(name="S&E Funding Totals")

ggplot() + geom_bar(data = filter(sne_bench_2, type == "FSS Result"), 
                    aes(x = agency, y = amount, group = type, fill = type), stat = "identity", alpha = .5) + 
  geom_bar(data = filter(sne_bench_2, type == "USAspending Estimate"), 
           aes(x = agency, y = amount, group = type, fill = type), stat = "identity", alpha = .5) + 
  scale_fill_manual(values=c("yellow", "blue")) +
  labs(title = "Identified science and engineering spending compared to Federal Support Survey",
       subtitle = "Among awards to institutes ofhigher education in FY16")+ theme_minimal()
