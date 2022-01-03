# Edited to fix data type errors/missing values when reading data using read_csv

# NOTE: Set Working Directory to publicrd/.  Data has already been written to RDS files. Go to START HERE section

library(tidyverse)
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(readr)
library(XML)
library(methods)
library(readxl)

# turns off scientific notation so casting project id's to character isn't a problem
options(scipen = 999)  

#Federal Reporter Abstract Pull -------------------------------------------------------------------------
# THIS SECTION IS GOOD, no parsing warnings/errors

X2008_abstracts_federal_reporter <- read_csv("data/prd/Federal_RePORTER/FR-2021FEB/FR-raw_proj_abstracts-CSV/FedRePORTER_PRJABS_C_FY2008.csv")
X2009_abstracts_federal_reporter <- read_csv("data/prd/Federal_RePORTER/FR-2021FEB/FR-raw_proj_abstracts-CSV/FedRePORTER_PRJABS_C_FY2009.csv")
X2010_abstracts_federal_reporter <- read_csv("data/prd/Federal_RePORTER/FR-2021FEB/FR-raw_proj_abstracts-CSV/FedRePORTER_PRJABS_C_FY2010.csv")
X2011_abstracts_federal_reporter <- read_csv("data/prd/Federal_RePORTER/FR-2021FEB/FR-raw_proj_abstracts-CSV/FedRePORTER_PRJABS_C_FY2011.csv")
X2012_abstracts_federal_reporter <- read_csv("data/prd/Federal_RePORTER/FR-2021FEB/FR-raw_proj_abstracts-CSV/FedRePORTER_PRJABS_C_FY2012.csv")
X2013_abstracts_federal_reporter <- read_csv("data/prd/Federal_RePORTER/FR-2021FEB/FR-raw_proj_abstracts-CSV/FedRePORTER_PRJABS_C_FY2013.csv")
X2014_abstracts_federal_reporter <- read_csv("data/prd/Federal_RePORTER/FR-2021FEB/FR-raw_proj_abstracts-CSV/FedRePORTER_PRJABS_C_FY2014.csv")
X2015_abstracts_federal_reporter <- read_csv("data/prd/Federal_RePORTER/FR-2021FEB/FR-raw_proj_abstracts-CSV/FedRePORTER_PRJABS_C_FY2015.csv")
X2016_abstracts_federal_reporter <- read_csv("data/prd/Federal_RePORTER/FR-2021FEB/FR-raw_proj_abstracts-CSV/FedRePORTER_PRJABS_C_FY2016.csv")
X2017_abstracts_federal_reporter <- read_csv("data/prd/Federal_RePORTER/FR-2021FEB/FR-raw_proj_abstracts-CSV/FedRePORTER_PRJABS_C_FY2017.csv")
X2018_abstracts_federal_reporter <- read_csv("data/prd/Federal_RePORTER/FR-2021FEB/FR-raw_proj_abstracts-CSV/FedRePORTER_PRJABS_C_FY2018.csv")
X2019_abstracts_federal_reporter <- read_csv("data/prd/Federal_RePORTER/FR-2021FEB/FR-raw_proj_abstracts-CSV/FedRePORTER_PRJABS_C_FY2019.csv")
X2020_abstracts_federal_reporter <- read_csv("data/prd/Federal_RePORTER/FR-2021DEC/raw_abstracts-CSV/FedRePORTER_PRJABS_C_FY2020.csv")

#Tag with year:

X2008_abstracts_federal_reporter$FY <-2008
X2009_abstracts_federal_reporter$FY <-2009
X2010_abstracts_federal_reporter$FY <-2010
X2011_abstracts_federal_reporter$FY <-2011
X2012_abstracts_federal_reporter$FY <-2012
X2013_abstracts_federal_reporter$FY <-2013
X2014_abstracts_federal_reporter$FY <-2014
X2015_abstracts_federal_reporter$FY <-2015
X2016_abstracts_federal_reporter$FY <-2016
X2017_abstracts_federal_reporter$FY <-2017
X2018_abstracts_federal_reporter$FY <-2018
X2019_abstracts_federal_reporter$FY <-2019
X2020_abstracts_federal_reporter$FY <-2020

#Federal Reporter 2020 Abstracts Merged Into One File

A_abstracts_federal_reporter <- rbind(
  X2008_abstracts_federal_reporter,
  X2009_abstracts_federal_reporter,
  X2010_abstracts_federal_reporter,
  X2011_abstracts_federal_reporter,
  X2012_abstracts_federal_reporter,
  X2013_abstracts_federal_reporter,
  X2014_abstracts_federal_reporter,
  X2015_abstracts_federal_reporter,
  X2016_abstracts_federal_reporter,
  X2017_abstracts_federal_reporter,
  X2018_abstracts_federal_reporter,
  X2019_abstracts_federal_reporter,
  X2020_abstracts_federal_reporter
)

# write to RDS 
#write_rds(A_abstracts_federal_reporter, "data/prd/Federal_RePORTER/FR-2021DEC/raw_abstracts-CSV/FR_raw-abstracts_2021DEC16.rds")



#Federal Reporter Metadata Pull: --------------------------------------------------------------

# 2008: 104076 records
Y2008_data_multiple_fed_reporter <- xmlToDataFrame("data/prd/Federal_RePORTER/FR-2021DEC/raw_metadata-XML/FedRePORTER_PRJ_X_FY2008.xml")

# 2009: 118557 records
Y2009_data_multiple_fed_reporter <- xmlToDataFrame("data/prd/Federal_RePORTER/FR-2021DEC/raw_metadata-XML/FedRePORTER_PRJ_X_FY2009.xml")

# 2010: 111400
Y2010_data_multiple_fed_reporter <- xmlToDataFrame("data/prd/Federal_RePORTER/FR-2021DEC/raw_metadata-XML/FedRePORTER_PRJ_X_FY2010.xml")


# 2011: 98064 -- PROBLEM at line 2484208: unescaped & in project number for project id = 709388.  Fixing this first

#--------------------------------------------------------------------------------------
# ALREADY RAN - only needs to be run once
#
# text_2011 <- readLines("data/prd/Federal_RePORTER/FR-2021DEC/raw_metadata-XML/FedRePORTER_PRJ_X_FY2011.xml")
# line_num = 2484182  # index from FEB 2021 update: 2484208
# text_2011[line_num]  
# text_2011[line_num] <- sub("&", "&amp;", text_2011[line_num], fixed=TRUE)
# writeLines(text_2011, "data/prd/Federal_RePORTER/FR-2021DEC/raw_metadata-XML/FedRePORTER_PRJ_X_FY2011_CORRECTED.xml")
#-------------------------------------------------------------------------------------------

Y2011_data_multiple_fed_reporter <- xmlToDataFrame("data/prd/Federal_RePORTER/FR-2021DEC/raw_metadata-XML/FedRePORTER_PRJ_X_FY2011_CORRECTED.xml")


# 2012: 93367
Y2012_data_multiple_fed_reporter <- xmlToDataFrame("data/prd/Federal_RePORTER/FR-2021DEC/raw_metadata-XML/FedRePORTER_PRJ_X_FY2012.xml")

# 2013: 91637
Y2013_data_multiple_fed_reporter <- xmlToDataFrame("data/prd/Federal_RePORTER/FR-2021DEC/raw_metadata-XML/FedRePORTER_PRJ_X_FY2013.xml")

# 2014: 90944
Y2014_data_multiple_fed_reporter <- xmlToDataFrame("data/prd/Federal_RePORTER/FR-2021DEC/raw_metadata-XML/FedRePORTER_PRJ_X_FY2014.xml")

# 2015: 92265
Y2015_data_multiple_fed_reporter <- xmlToDataFrame("data/prd/Federal_RePORTER/FR-2021DEC/raw_metadata-XML/FedRePORTER_PRJ_X_FY2015.xml")

# 2016: 91126    (no probs with read_csv or fread)
Y2016_data_multiple_fed_reporter <- xmlToDataFrame("data/prd/Federal_RePORTER/FR-2021FEB/FR-raw_proj_metadata-XML/FedRePORTER_PRJ_X_FY2016.xml")

# 2017: 88422
Y2017_data_multiple_fed_reporter <- xmlToDataFrame("data/prd/Federal_RePORTER/FR-2021FEB/FR-raw_proj_metadata-XML/FedRePORTER_PRJ_X_FY2017.xml")

# 2018: 96211
Y2018_data_multiple_fed_reporter <- xmlToDataFrame("data/prd/Federal_RePORTER/FR-2021FEB/FR-raw_proj_metadata-XML/FedRePORTER_PRJ_X_FY2018.xml")

# 2019: 80068
Y2019_data_multiple_fed_reporter <- xmlToDataFrame("data/prd/Federal_RePORTER/FR-2021FEB/FR-raw_proj_metadata-XML/FedRePORTER_PRJ_X_FY2019.xml")

# 2020: 
Y2020_data_multiple_fed_reporter <- xmlToDataFrame("data/prd/Federal_RePORTER/FR-2021FEB/FR-raw_proj_metadata-XML/FedRePORTER_PRJ_X_FY2020.xml")



# -------------------------------------------------------------------------------------------------------------
# OLD APPROACH - cost column type error fixed, but parsing issues with quotes in field (PIs) still exist 
# 
# Y2008_data_multiple_fed_reporter <- read_csv("data/prd/RND_Topic_Modelling/federal-reporter_updated/metadata/FedRePORTER_PRJ_C_FY2008.csv", 
#                                              col_types = "ccccccccccccccccccccccnn")
# Y2009_data_multiple_fed_reporter <- read_csv("data/prd/RND_Topic_Modelling/federal-reporter_updated/metadata/FedRePORTER_PRJ_C_FY2009.csv", 
#                                              col_types = "ccccccccccccccccccccccnn")
# Y2010_data_multiple_fed_reporter <- read_csv("data/prd/RND_Topic_Modelling/federal-reporter_updated/metadata/FedRePORTER_PRJ_C_FY2010.csv", 
#                                              col_types = "ccccccccccccccccccccccnn")
# Y2011_data_multiple_fed_reporter <- read_csv("data/prd/RND_Topic_Modelling/federal-reporter_updated/metadata/FedRePORTER_PRJ_C_FY2011.csv", 
#                                              col_types = "ccccccccccccccccccccccnn")
# Y2012_data_multiple_fed_reporter <- read_csv("data/prd/RND_Topic_Modelling/federal-reporter_updated/metadata/FedRePORTER_PRJ_C_FY2012.csv", 
#                                              col_types = "ccccccccccccccccccccccnn")
# Y2013_data_multiple_fed_reporter <- read_csv("data/prd/RND_Topic_Modelling/federal-reporter_updated/metadata/FedRePORTER_PRJ_C_FY2013.csv", 
#                                              col_types = "ccccccccccccccccccccccnn")
# Y2014_data_multiple_fed_reporter <- read_csv("data/prd/RND_Topic_Modelling/federal-reporter_updated/metadata/FedRePORTER_PRJ_C_FY2014.csv", 
#                                              col_types = "ccccccccccccccccccccccnn")
# Y2015_data_multiple_fed_reporter <- read_csv("data/prd/RND_Topic_Modelling/federal-reporter_updated/metadata/FedRePORTER_PRJ_C_FY2015.csv", 
#                                             col_types = "ccccccccccccccccccccccnn")
# Y2016_data_multiple_fed_reporter <- read_csv("data/prd/RND_Topic_Modelling/federal-reporter_updated/metadata/FedRePORTER_PRJ_C_FY2016.csv", 
#                                              col_types = "ccccccccccccccccccccccnn")
# Y2017_data_multiple_fed_reporter <- read_csv("data/prd/RND_Topic_Modelling/federal-reporter_updated/metadata/FedRePORTER_PRJ_C_FY2017.csv", 
#                                              col_types = "ccccccccccccccccccccccnn")
# Y2018_data_multiple_fed_reporter <- read_csv("data/prd/RND_Topic_Modelling/federal-reporter_updated/metadata/FedRePORTER_PRJ_C_FY2018.csv", 
#                                              col_types = "ccccccccccccccccccccccnn")
# Y2019_data_multiple_fed_reporter <- read_csv("data/prd/RND_Topic_Modelling/federal-reporter_updated/metadata/FedRePORTER_PRJ_C_FY2019.csv", 
#                                              col_types = "ccccccccccccccccccccccnn")
# 
# # Also tried to use fread -- 
# # fread stopped reading early in many years because there must have been a line break in the title data 
# # field, so instead:
# # used XML data from Federal RePORTER and this was fixed (also fixed parsing errors with quotes)
# 
# Y2011_data_multiple_fed_reporter <- fread("data/prd/RND_Topic_Modelling/federal-reporter_updated/metadata/FedRePORTER_PRJ_C_FY2011.csv", 
#                                           colClasses = list(character=1:22, numeric=23:24))
# 
# #-----------------------------------------------------------------------------------------


#Federal Reporter 2020 Merged Into One File

A_metadatafederalreporter <- rbind(
  Y2008_data_multiple_fed_reporter,
  Y2009_data_multiple_fed_reporter,
  Y2010_data_multiple_fed_reporter,
  Y2011_data_multiple_fed_reporter,
  Y2012_data_multiple_fed_reporter,
  Y2013_data_multiple_fed_reporter,
  Y2014_data_multiple_fed_reporter,
  Y2015_data_multiple_fed_reporter,
  Y2016_data_multiple_fed_reporter,
  Y2017_data_multiple_fed_reporter,
  Y2018_data_multiple_fed_reporter,
  Y2019_data_multiple_fed_reporter,
  Y2020_data_multiple_fed_reporter
)

# write to RDS 
#write_rds(A_metadatafederalreporter, "data/prd/Federal_RePORTER/FR-2021DEC/raw_metadata-XML/FR_raw-metadata_2021DEC16.rds")



# Data already written - START HERE --------------------------------------------------------------

A_abstracts_federal_reporter <- readRDS("data/prd/Federal_RePORTER/FR-2021DEC/raw_abstracts-CSV/FR_raw-abstracts_2021DEC16.rds")
A_metadatafederalreporter <- readRDS("data/prd/Federal_RePORTER/FR-2021DEC/raw_metadata-XML/FR_raw-metadata_2021DEC16.rds")


# Add in NSF data for FYs 19-20.  This data came from Federal RePORTER search/export function. 
# Datasets include abstracts and metadata.

nsf_19 <- read_excel("data/prd/Federal_RePORTER/FR-2021DEC/FR_Expt_17Dec2021_NSF_FY19.xlsx", 
                     col_types = "text")
nsf_20 <- read_excel("data/prd/Federal_RePORTER/FR-2021DEC/FR_Expt_17Dec2021_NSF_FY20.xlsx",
                     col_types = "text")

colnames(nsf_19) <- c("ABSTRACT", "PROJECT_TITLE", "DEPARTMENT", "AGENCY", "PROJECT_ID", "PROJECT_NUMBER", 
                      "PROJECT_START_DATE", "PROJECT_END_DATE", "DUNS_NUMBER", "ORGANIZATION_NAME", 
                      "BUDGET_START_DATE", "BUDGET_END_DATE", "FY", "CFDA_CODE", "FY_TOTAL_COST", 
                      "FY_TOTAL_COST_SUB_PROJECTS")

colnames(nsf_20) <- c("ABSTRACT", "PROJECT_TITLE", "DEPARTMENT", "AGENCY", "PROJECT_ID", "PROJECT_NUMBER", 
                      "PROJECT_START_DATE", "PROJECT_END_DATE", "DUNS_NUMBER", "ORGANIZATION_NAME", 
                      "BUDGET_START_DATE", "BUDGET_END_DATE", "FY", "CFDA_CODE", "FY_TOTAL_COST", 
                      "FY_TOTAL_COST_SUB_PROJECTS")


# Format fixes in project metadata and abstracts ----------------------------------------------------------------


nsf_19$DEPARTMENT <- "NSF"
nsf_20$DEPARTMENT <- "NSF"

# Change cost and subcost columns to type numerical

A_metadatafederalreporter$FY_TOTAL_COST <- as.numeric(A_metadatafederalreporter$FY_TOTAL_COST)
A_metadatafederalreporter$FY_TOTAL_COST_SUB_PROJECTS <- as.numeric(A_metadatafederalreporter$FY_TOTAL_COST_SUB_PROJECTS)
nsf_19$FY_TOTAL_COST <- as.numeric(nsf_19$FY_TOTAL_COST)
nsf_19$FY_TOTAL_COST_SUB_PROJECTS <- as.numeric(nsf_19$FY_TOTAL_COST_SUB_PROJECTS)
nsf_20$FY_TOTAL_COST <- as.numeric(nsf_20$FY_TOTAL_COST)
nsf_20$FY_TOTAL_COST_SUB_PROJECTS <- as.numeric(nsf_20$FY_TOTAL_COST_SUB_PROJECTS)


# Change project_id and FY type to character in abstracts to match with metadata types

A_abstracts_federal_reporter$PROJECT_ID <- as.character(A_abstracts_federal_reporter$PROJECT_ID)
A_abstracts_federal_reporter$FY <- as.character(A_abstracts_federal_reporter$FY)
# NSF FY 19-20 data already has these types


# Add NSF FY 19-20 data to abstracts and metadata

A_abstracts_federal_reporter <- rbind(A_abstracts_federal_reporter, 
                                      nsf_19[ , c("PROJECT_ID", "ABSTRACT", "FY")],
                                      nsf_20[ , c("PROJECT_ID", "ABSTRACT", "FY")])

A_metadatafederalreporter <- plyr::rbind.fill(A_metadatafederalreporter,
                         nsf_19[ , c("PROJECT_ID", "PROJECT_TITLE", "DEPARTMENT", "AGENCY", 
                                     "PROJECT_NUMBER", "PROJECT_START_DATE", "PROJECT_END_DATE",
                                     "DUNS_NUMBER", "ORGANIZATION_NAME", "BUDGET_START_DATE",
                                     "BUDGET_END_DATE", "CFDA_CODE", "FY", "FY_TOTAL_COST",
                                     "FY_TOTAL_COST_SUB_PROJECTS")],
                         nsf_20[ , c("PROJECT_ID", "PROJECT_TITLE", "DEPARTMENT", "AGENCY", 
                                     "PROJECT_NUMBER", "PROJECT_START_DATE", "PROJECT_END_DATE",
                                     "DUNS_NUMBER", "ORGANIZATION_NAME", "BUDGET_START_DATE",
                                     "BUDGET_END_DATE", "CFDA_CODE", "FY", "FY_TOTAL_COST",
                                     "FY_TOTAL_COST_SUB_PROJECTS")])
                         

# Set empty strings ("") to NAs in the project metadata

A_metadatafederalreporter[A_metadatafederalreporter == ""] <- NA


#Fix CFDA Codes:

CFDA_glossary <- read_csv("glossary/CFDA_glossary_final.csv") %>%
  select(department_abb, department_digits)
colnames(CFDA_glossary)[colnames(CFDA_glossary)=="department_abb"] <- "DEPARTMENT"

data_fixed <- left_join(A_metadatafederalreporter, CFDA_glossary, by = "DEPARTMENT")

#Cleaning Function:

fixing_cfda_code <- function(cfda_project_code){

  fixed_cfda <-ifelse(cfda_project_code != '000' & cfda_project_code != '999' & grepl("^[0-9]{3}$", cfda_project_code) == TRUE,
                      paste(data_fixed$department_digits,cfda_project_code, sep = "."),
                      cfda_project_code)

  return(fixed_cfda)
}

#Update Data:

data_fixed$CFDA_CODE <- fixing_cfda_code(data_fixed$CFDA_CODE)
data_fixed$department_digits <- NULL
A_metadatafederalreporter <- data_fixed
rm(data_fixed)


#Combine Metadata with Abstracts ----------------------------------------------------------------

AAA_updated_federal_reporter <- full_join(A_abstracts_federal_reporter, A_metadatafederalreporter, by = "PROJECT_ID")
#write.csv(AAA_updated_federal_reporter, file = "~/git/publicrd/data/prd/RND_Topic_Modelling/federal-reporter_updated/working_federal_reporter_2020.csv", row.names = FALSE)



# write out to different file types -----------------------------------------------------------

write.csv(AAA_updated_federal_reporter, file = "data/prd/Federal_RePORTER/FR-2021DEC/FR_raw_2021DEC17.csv", row.names = FALSE)
write_rds(AAA_updated_federal_reporter, "data/prd/Federal_RePORTER/FR-2021DEC/FR_raw_2021DEC17.rds")

#library(feather)  # didn't read in for python
#write_feather(AAA_updated_federal_reporter, "data/prd/Federal_RePORTER/FR_raw_2021FEB24.feather")
