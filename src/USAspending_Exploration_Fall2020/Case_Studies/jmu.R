# USAspending Exploration: NSF S&E and R&D spending to JMU for FY16-18

library(readr)
library(ggplot2)
library(dplyr)
library(naniar)
library(stringr)
library(readxl)
library(tidyr)

#
# DATA INGESTION: NSF contracts and grants ------------------------------------------
#

# grants

nsf_g16 <- read_csv("data/prd/USAspending_benchmarking/NSF/FY16/All_Assistance_PrimeTransactions_2020-10-23_H19M21S10_1.csv")
nsf_g17 <- read_csv("data/prd/USAspending_benchmarking/NSF/FY17/All_Assistance_PrimeTransactions_2020-09-25_H17M36S08_1.csv")
nsf_g18 <- read_csv("data/prd/USAspending_benchmarking/NSF/FY18/All_Assistance_PrimeTransactions_2020-09-25_H18M10S45_1.csv")

nsf_g16$FY <- "2016"
nsf_g17$FY <- "2017"
nsf_g18$FY <- "2018"
nsf_g <- rbind(nsf_g16, nsf_g17, nsf_g18)

rm(nsf_g16, nsf_g17, nsf_g18)

# contracts

nsf_c16 <- read_csv("data/prd/USAspending_benchmarking/NSF/FY16/All_Contracts_PrimeTransactions_2020-10-23_H19M20S19_1.csv")
nsf_c17 <- read_csv("data/prd/USAspending_benchmarking/NSF/FY17/All_Contracts_PrimeTransactions_2020-09-25_H17M35S17_1.csv")
nsf_c18 <- read_csv("data/prd/USAspending_benchmarking/NSF/FY18/All_Contracts_PrimeTransactions_2020-09-25_H18M07S04_1.csv")

nsf_c <- rbind(nsf_c16, nsf_c17, nsf_c18)

rm(nsf_c16, nsf_c17, nsf_c18)

#
# FILTER: grants for James Madison only.  JMU duns number is 879325355.  ----------------------------------------
#

jmu_g <- nsf_g %>%
  filter(recipient_duns == "879325355")   

jmu_g_parent <- nsf_g %>%
  filter(recipient_parent_duns == "879325355")   

identical(jmu_g, jmu_g_parent)  #TRUE -- I'll just use jmu_g

rm(jmu_g_parent)

# check if any contracts to JMU exist (even though we aren't using contract data)

jmu_c <- nsf_c %>%
  filter(recipient_duns == "879325355")    # no contracts to JMU

jmu_c_parent <- nsf_c %>%
  filter(recipient_parent_duns == "879325355")   # 0

rm(jmu_c)
rm(jmu_c_parent)
rm(nsf_c)

#
#  DESCRIPTIVE STATISTICS: for JMU grants -------------------------------------------------------------
#

# number of transactions

nrow(jmu_g %>% filter(FY == "2016")) # 11
nrow(jmu_g %>% filter(FY == "2017")) # 10
nrow(jmu_g %>% filter(FY == "2018")) # 17

# number of unique projects

length(unique(jmu_g$award_id_fain))  #31

length(unique(jmu_g[jmu_g$FY == "2016", ]$award_id_fain)) #11
length(unique(jmu_g[jmu_g$FY == "2017", ]$award_id_fain)) #10
length(unique(jmu_g[jmu_g$FY == "2018", ]$award_id_fain)) #16


# CFDAs 

tab1 <- table(jmu_g$cfda_number)  # NOTE: we filtered all of these CFDAs as SnE - expected for NSF
tab2 <- table(jmu_g$cfda_title)

# dollar amounts by CFDAs

cfda_sum <- jmu_g %>%
  group_by(cfda_number, cfda_title, FY) %>%
  summarise(n = n(),
            dollars = sum(federal_action_obligation)) 

#
# FEASIBILITY: how close are USAspending data and FSS data? ------------------------------------------
#

# FY 18

jmu_g18 <- jmu_g %>%
  filter(FY == "2018")


# FSS table 15 for NSF RnD to JMU : 1,639,900
rnd_fss <- 1639900

# FSS table 13 for NSF SnE to JMU : 2,796,500
sne_fss <- 2796500


# RnD estimate: NSF funds all RnD except polar program and CFDA 47.076: EDUCATION AND HUMAN RESOURCES
# which includes some RnD but also training and fellowships (not RnD)

rnd_subset <- jmu_g18 %>%
  filter(cfda_number != "47.076")

# inspect the projects that do have CFDA 47.076 and do not include those for graduate fellowships or Noyce scholarships

rnd_EHR_subset <- jmu_g18 %>% 
  filter(cfda_number == "47.076" & award_description != "GRADUATE RESEARCH FELLOWSHIP PROGRAM (GRFP)"
         & award_description != "ROBERT NOYCE TEACHER SCHOLARSHIP PROGRAM FOR EDUCATION OF SECONDARY STEM TEACHERS IN RURAL VIRGINIA")

# USAspending dollar amount

rnd_usa <- sum(rnd_subset$federal_action_obligation) + sum(rnd_EHR_subset$federal_action_obligation)  

# difference to fss amount

rnd_diff <- rnd_fss - rnd_usa   

# percent diff

rnd_pct_diff <- 100*rnd_diff/rnd_fss  # may want to change denominator


# SnE estimate: use all projects since we filtered all CFDAs as SnE

# USAspending dollar amount 

sne_usa <- sum(jmu_g18$federal_action_obligation)  

# difference to fss amount

sne_diff <- sne_fss - sne_usa   

# percent diff

sne_pct_diff <- 100*sne_diff/sne_fss # may want to change denominator


#
#  PLOT RESULTS: tables, plots, etc. --------------------------------------------------------
#

# descriptive stats

# library(data.table)
# 
# setDT(cfda_sum)
# cfda_sum

# plot dollar amounts

dollar_comparision <- data.frame(c("USAspending", "FSS", "USAspending", "FSS"), 
                                 c("SnE", "SnE", "RnD", "RnD"),
                                 c(sne_usa/1000000, sne_fss/1000000, rnd_usa/1000000, rnd_fss/1000000))
colnames(dollar_comparision) <- c("Source", "Category", "Amount")

ggplot(dollar_comparision, aes(fill=Source, y=Amount, x=Category)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = round(Amount,2)), position=position_dodge(width=0.9), vjust=-0.25) +
  geom_label(x=1, y=1.5, label=paste0("Pct Diff: ", round(rnd_pct_diff,2), "%"), 
             color = "black", fill="white") +
  geom_label(x=2, y=2.5, label=paste0("Pct Diff: ", round(sne_pct_diff,2), "%"), 
             color = "black", fill="white") +
  ylab("Dollar Amount (millions)") +
  ggtitle("James Madison University, NSF FY18 Grants")


# FY 17

jmu_g17 <- jmu_g %>%
  filter(FY == "2017")


# FSS table 15 for NSF RnD to JMU : 1,175,400
rnd_fss <- 1175400

# FSS table 13 for NSF SnE to JMU : 1,417,300
sne_fss <- 1417300


# RnD estimate: NSF funds all RnD except polar program and CFDA 47.076: EDUCATION AND HUMAN RESOURCES
# which includes some RnD but also training and fellowships (not RnD)

rnd_subset <- jmu_g17 %>%
  filter(cfda_number != "47.076")

# inspect the projects that do have CFDA 47.076 and do not include those for graduate fellowships or Noyce scholarships

rnd_EHR_subset <- jmu_g17 %>% 
  filter(cfda_number == "47.076" & award_description != "GRADUATE RESEARCH FELLOWSHIP PROGRAM (GRFP)"
         & award_description != "ROBERT NOYCE TEACHER SCHOLARSHIP PROGRAM FOR EDUCATION OF SECONDARY STEM TEACHERS IN RURAL VIRGINIA")

# USAspending dollar amount

rnd_usa <- sum(rnd_subset$federal_action_obligation) + sum(rnd_EHR_subset$federal_action_obligation)  

# difference to fss amount

rnd_diff <- rnd_fss - rnd_usa   

# percent diff

rnd_pct_diff <- 100*rnd_diff/rnd_fss  # may want to change denominator


# SnE estimate: use all projects since we filtered all CFDAs as SnE

# USAspending dollar amount 

sne_usa <- sum(jmu_g17$federal_action_obligation)  

# difference to fss amount

sne_diff <- sne_fss - sne_usa   

# percent diff

sne_pct_diff <- 100*sne_diff/sne_fss # may want to change denominator


#
#  PLOT RESULTS: tables, plots, etc. --------------------------------------------------------
#

# descriptive stats

# library(data.table)
# 
# setDT(cfda_sum)
# cfda_sum

# plot dollar amounts

dollar_comparision <- data.frame(c("USAspending", "FSS", "USAspending", "FSS"), 
                                 c("SnE", "SnE", "RnD", "RnD"),
                                 c(sne_usa/1000000, sne_fss/1000000, rnd_usa/1000000, rnd_fss/1000000))
colnames(dollar_comparision) <- c("Source", "Category", "Amount")

ggplot(dollar_comparision, aes(fill=Source, y=Amount, x=Category)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = round(Amount,2)), position=position_dodge(width=0.9), vjust=-0.25) +
  geom_label(x=1, y=1, label=paste0("Pct Diff: ", round(rnd_pct_diff,2), "%"), 
             color = "black", fill="white") +
  geom_label(x=2, y=1.25, label=paste0("Pct Diff: ", round(sne_pct_diff,2), "%"), 
             color = "black", fill="white") +
  ylab("Dollar Amount (millions)") +
  ggtitle("James Madison University, NSF FY17 Grants")


# FY 16

jmu_g16 <- jmu_g %>%
  filter(FY == "2016")


# FSS table 15 for NSF RnD to JMU : 997,100
rnd_fss <- 997100

# FSS table 13 for NSF SnE to JMU : 1,238,400	
sne_fss <- 1238400	


# RnD estimate: NSF funds all RnD except polar program and CFDA 47.076: EDUCATION AND HUMAN RESOURCES
# which includes some RnD but also training and fellowships (not RnD)

rnd_subset <- jmu_g16 %>%
  filter(cfda_number != "47.076")

# inspect the projects that do have CFDA 47.076 and do not include those for graduate fellowships or Noyce scholarships

rnd_EHR_subset <- jmu_g16 %>% 
  filter(cfda_number == "47.076" & award_description != "GRADUATE RESEARCH FELLOWSHIP PROGRAM (GRFP)"
         & award_description != "ROBERT NOYCE TEACHER SCHOLARSHIP PROGRAM FOR EDUCATION OF SECONDARY STEM TEACHERS IN RURAL VIRGINIA")

# USAspending dollar amount

rnd_usa <- sum(rnd_subset$federal_action_obligation) + sum(rnd_EHR_subset$federal_action_obligation)  

# difference to fss amount

rnd_diff <- rnd_fss - rnd_usa   

# percent diff

rnd_pct_diff <- 100*rnd_diff/rnd_fss  # may want to change denominator


# SnE estimate: use all projects since we filtered all CFDAs as SnE

# USAspending dollar amount 

sne_usa <- sum(jmu_g16$federal_action_obligation)  

# difference to fss amount

sne_diff <- sne_fss - sne_usa   

# percent diff

sne_pct_diff <- 100*sne_diff/sne_fss # may want to change denominator


#
#  PLOT RESULTS: tables, plots, etc. --------------------------------------------------------
#

# descriptive stats

# library(data.table)
# 
# setDT(cfda_sum)
# cfda_sum

# plot dollar amounts

dollar_comparision <- data.frame(c("USAspending", "FSS", "USAspending", "FSS"), 
                                 c("SnE", "SnE", "RnD", "RnD"),
                                 c(sne_usa/1000000, sne_fss/1000000, rnd_usa/1000000, rnd_fss/1000000))
colnames(dollar_comparision) <- c("Source", "Category", "Amount")

ggplot(dollar_comparision, aes(fill=Source, y=Amount, x=Category)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = round(Amount,2)), position=position_dodge(width=0.9), vjust=-0.25) +
  geom_label(x=1, y=0.8, label=paste0("Pct Diff: ", round(rnd_pct_diff,2), "%"), 
             color = "black", fill="white") +
  geom_label(x=2, y=0.8, label=paste0("Pct Diff: ", round(sne_pct_diff,2), "%"), 
             color = "black", fill="white") +
  ylab("Dollar Amount (millions)") +
  ggtitle("James Madison University, NSF FY16 Grants")



#
#  SnE COMBINATIONS: --------------------------------------------------------------------------
#  Look for combination that gets within 1700 of benchmark (bc of rounding - each entry could be off by < 100)
#

dollars <- jmu_g18$federal_action_obligation
# keep only amounts != $0
dollars <- dollars[dollars != 0]
num_proj <- length(dollars)

bm <- sne_fss
combo_num <- rep(0,50)
combo_mat <- matrix(0, 50, num_proj)
diff <- rep(0,50)

j = 1
for(i in 1:(2^num_proj-1))
{
  idx <- as.logical(intToBits(i))
  
  amount <- sum(dollars[idx])
  
  if( (amount >= (bm - 10000)) & (amount <= (bm + 10000)) ) 
  {
    combo_num[j] <- i
    combo_mat[j, ] <- idx[1:num_proj]
    diff[j] <- bm - amount 
    j = j + 1
  }
  
}

#  format combo_mat

# remove all zero rows

row_sums <- rowSums(combo_mat)
row_sums <- row_sums[row_sums > 0]
n <- length(row_sums)

combo_df <- as.data.frame(combo_mat[1:n, ])

write_csv(jmu_g18, "src/USAspending_Exploration_Fall2020/Case_Studies/jmu18.csv")
write_csv(combo_df, "src/USAspending_Exploration_Fall2020/Case_Studies/combos.csv")


#
#  RnD COMBINATIONS: --------------------------------------------------------------------------
#  Look for combination that gets within 1700 of benchmark (bc of rounding - each entry could be off by < 100)
#

dollars <- jmu_g18$federal_action_obligation
# keep only amounts != $0
dollars <- dollars[dollars != 0]
num_proj <- length(dollars)

bm <- rnd_fss
combo_num <- rep(0,50)
combo_mat <- matrix(0, 50, num_proj)
diff <- rep(0,50)

j = 1
for(i in 1:(2^num_proj-1))
{
  idx <- as.logical(intToBits(i))
  
  amount <- sum(dollars[idx])
  
  if( (amount >= (bm - 2000)) & (amount <= (bm + 2000)) ) 
  {
    combo_num[j] <- i
    combo_mat[j, ] <- idx[1:num_proj]
    diff[j] <- bm - amount 
    j = j + 1
  }
  
}

#  format combo_mat

# remove all zero rows

row_sums <- rowSums(combo_mat)
row_sums <- row_sums[row_sums > 0]
n <- length(row_sums)

combo_df <- as.data.frame(combo_mat[1:n, ])

#write_csv(jmu_g18, "src/USAspending_Exploration_Fall2020/Case_Studies/jmu18.csv")
#write_csv(combo_df, "src/USAspending_Exploration_Fall2020/Case_Studies/combos.csv")