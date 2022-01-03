library(R.utils)
library(tidyverse)
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)

#Histograms
abstracts_federal_reporter_cleaned <- read_csv("data/working/federal_reporter/abstracts_federal_reporter_combined.csv")

#NSF: 116,510 abstracts
abstracts_NSF <- abstracts_federal_reporter_cleaned %>%
  filter(DEPARTMENT == "NSF")
NSF_hist <- hist(nchar(abstracts_NSF$ABSTRACT), breaks = 100, 
                 xlim = c(0,10000),
                 xlab = "Character Length",
                 main = "NSF Abstract Lengths",
                 col = "blue")

#NIH: 367697 abstracts
abstracts_NIH <- abstracts_federal_reporter_cleaned %>%
  filter(AGENCY == "NIH")
NIH_hist <- hist(nchar(abstracts_NIH$ABSTRACT), breaks = 200,
                 xlim = c(0,10000),
                 xlab = "Character Length",
                 main = "NIH Abstract Lengths",
                 col = "purple")

#NASA: 16,006 abstracts
abstracts_NASA <- abstracts_federal_reporter_cleaned %>%
  filter(DEPARTMENT == "NASA")
NASA_hist <- hist(nchar(abstracts_NASA$ABSTRACT), breaks = 200,
                  xlim = c(0,10000),
                  xlab = "Character Length",
                  main = "NASA Abstract Lengths",
                  col = "orange")

#USDA: 27,304 abstracts
abstracts_USDA <- abstracts_federal_reporter_cleaned %>%
  filter(DEPARTMENT == "USDA")
abstracts_NIFA <- abstracts_federal_reporter_cleaned %>%
  filter(AGENCY == "NIFA")
abstracts_ARS <- abstracts_federal_reporter_cleaned %>%
  filter(AGENCY == "ARS")
abstracts_FS <- abstracts_federal_reporter_cleaned %>%
  filter(AGENCY == "FS")

USDA_hist <- hist(nchar(abstracts_USDA$ABSTRACT), breaks = 200,
                  xlim = c(0,10000),
                  xlab = "Character Length",
                  main = "USDA Abstract Lengths",
                  col = "green")
NIFA_hist <- hist(nchar(abstracts_NIFA$ABSTRACT), breaks = 200,
                  xlim = c(0,10000),
                  xlab = "Character Length",
                  main = "NIFA Abstract Lengths",
                  col = "green")
ARS_hist <- hist(nchar(abstracts_ARS$ABSTRACT), breaks = 200,
                 xlim = c(0,10000),
                 xlab = "Character Length",
                 main = "ARS Abstract Lengths",
                 col = "green")
FS_hist <- hist(nchar(abstracts_FS$ABSTRACT), breaks = 200,
                xlim = c(0,10000),
                xlab = "Character Length",
                main = "FS Abstract Lengths",
                col = "green")

#DOD: 6951 abstracts
abstracts_DOD <- abstracts_federal_reporter_cleaned %>%
  filter(DEPARTMENT == "DOD")
abstracts_CNRP <- abstracts_federal_reporter_cleaned %>%
  filter(AGENCY == "CNRM")
abstracts_CDMRP <- abstracts_federal_reporter_cleaned %>%
  filter(AGENCY == "CDMRP")
abstracts_CCCRP <- abstracts_federal_reporter_cleaned %>%
  filter(AGENCY == "CCCRP")
abstracts_DVBIC <- abstracts_federal_reporter_cleaned %>%
  filter(AGENCY == "DVBIC")

DOD_hist <- hist(nchar(abstracts_DOD$ABSTRACT), breaks = 200,
                 xlim = c(0,10000),
                 xlab = "Character Length",
                 main = "DOD Abstract Lengths",
                 col = "red")
CNRP_hist <- hist(nchar(abstracts_CNRP$ABSTRACT), breaks = 200,
                  xlim = c(0,10000),
                  xlab = "Character Length",
                  main = "CNRP Abstract Lengths",
                  col = "red")
CDMRP_hist <- hist(nchar(abstracts_CDMRP$ABSTRACT), breaks = 200,
                   xlim = c(0,10000),
                   xlab = "Character Length",
                   main = "CDMRP Abstract Lengths",
                   col = "red")
CCCRP_hist <- hist(nchar(abstracts_CCCRP$ABSTRACT), breaks = 200,
                   xlim = c(0,10000),
                   xlab = "Character Length",
                   main = "CCCRP Abstract Lengths",
                   col = "red")
DVBIC_hist <- hist(nchar(abstracts_DVBIC$ABSTRACT), breaks = 200,
                   xlim = c(0,10000),
                   xlab = "Character Length",
                   main = "DVBIC Abstract Lengths",
                   col = "red")
