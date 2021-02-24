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

#Federal Reporter 2020 Abstract Pull -------------------------------------------------------------------------
# THIS SECTION IS GOOD, no parsing warnings/errors

X2008_abstracts_federal_reporter <- read_csv("data/prd/Federal_RePORTER/FR-raw_proj_abstracts-CSV/FedRePORTER_PRJABS_C_FY2008.csv")
X2009_abstracts_federal_reporter <- read_csv("data/prd/Federal_RePORTER/FR-raw_proj_abstracts-CSV/FedRePORTER_PRJABS_C_FY2009.csv")
X2010_abstracts_federal_reporter <- read_csv("data/prd/Federal_RePORTER/FR-raw_proj_abstracts-CSV/FedRePORTER_PRJABS_C_FY2010.csv")
X2011_abstracts_federal_reporter <- read_csv("data/prd/Federal_RePORTER/FR-raw_proj_abstracts-CSV/FedRePORTER_PRJABS_C_FY2011.csv")
X2012_abstracts_federal_reporter <- read_csv("data/prd/Federal_RePORTER/FR-raw_proj_abstracts-CSV/FedRePORTER_PRJABS_C_FY2012.csv")
X2013_abstracts_federal_reporter <- read_csv("data/prd/Federal_RePORTER/FR-raw_proj_abstracts-CSV/FedRePORTER_PRJABS_C_FY2013.csv")
X2014_abstracts_federal_reporter <- read_csv("data/prd/Federal_RePORTER/FR-raw_proj_abstracts-CSV/FedRePORTER_PRJABS_C_FY2014.csv")
X2015_abstracts_federal_reporter <- read_csv("data/prd/Federal_RePORTER/FR-raw_proj_abstracts-CSV/FedRePORTER_PRJABS_C_FY2015.csv")
X2016_abstracts_federal_reporter <- read_csv("data/prd/Federal_RePORTER/FR-raw_proj_abstracts-CSV/FedRePORTER_PRJABS_C_FY2016.csv")
X2017_abstracts_federal_reporter <- read_csv("data/prd/Federal_RePORTER/FR-raw_proj_abstracts-CSV/FedRePORTER_PRJABS_C_FY2017.csv")
X2018_abstracts_federal_reporter <- read_csv("data/prd/Federal_RePORTER/FR-raw_proj_abstracts-CSV/FedRePORTER_PRJABS_C_FY2018.csv")
X2019_abstracts_federal_reporter <- read_csv("data/prd/Federal_RePORTER/FR-raw_proj_abstracts-CSV/FedRePORTER_PRJABS_C_FY2019.csv")

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
  X2019_abstracts_federal_reporter
)

# write to RDS 
# write_rds(A_abstracts_federal_reporter, "data/prd/Federal_RePORTER/FR-raw_proj_abstracts-CSV/FR_raw-abstracts_2021FEB23.rds")



#Federal Reporter 2020 Metadata Pull: --------------------------------------------------------------

# 2008: 104076 records
Y2008_data_multiple_fed_reporter <- xmlToDataFrame("data/prd/Federal_RePORTER/FR-raw_proj_metadata-XML/FedRePORTER_PRJ_X_FY2008.xml")

# 2009: 118557 records
Y2009_data_multiple_fed_reporter <- xmlToDataFrame("data/prd/Federal_RePORTER/FR-raw_proj_metadata-XML/FedRePORTER_PRJ_X_FY2009.xml")

# 2010: 111400
Y2010_data_multiple_fed_reporter <- xmlToDataFrame("data/prd/Federal_RePORTER/FR-raw_proj_metadata-XML/FedRePORTER_PRJ_X_FY2010.xml")


# 2011: 98064 -- PROBLEM at line 2484208: unescaped & in project number for project id = 709388.  Fixing this first

#--------------------------------------------------------------------------------------
# ALREADY RAN - only needs to be run once
#
# text_2011 <- readLines("data/prd/RND_Topic_Modelling/Fed-RePORTER-XML/FedRePORTER_PRJ_X_FY2011.xml")
# text_2011[2484208]
# text_2011[2484208] <- sub("&", "&amp;", text_2011[2484208], fixed=TRUE)
# writeLines(text_2011, "data/prd/RND_Topic_Modelling/Fed-RePORTER-XML/FedRePORTER_PRJ_X_FY2011_CORRECTED.xml")
#-------------------------------------------------------------------------------------------

Y2011_data_multiple_fed_reporter <- xmlToDataFrame("data/prd/Federal_RePORTER/FR-raw_proj_metadata-XML/FedRePORTER_PRJ_X_FY2011_CORRECTED.xml")


# 2012: 93367
Y2012_data_multiple_fed_reporter <- xmlToDataFrame("data/prd/Federal_RePORTER/FR-raw_proj_metadata-XML/FedRePORTER_PRJ_X_FY2012.xml")

# 2013: 91637
Y2013_data_multiple_fed_reporter <- xmlToDataFrame("data/prd/Federal_RePORTER/FR-raw_proj_metadata-XML/FedRePORTER_PRJ_X_FY2013.xml")

# 2014: 90944
Y2014_data_multiple_fed_reporter <- xmlToDataFrame("data/prd/Federal_RePORTER/FR-raw_proj_metadata-XML/FedRePORTER_PRJ_X_FY2014.xml")

# 2015: 92265
Y2015_data_multiple_fed_reporter <- xmlToDataFrame("data/prd/Federal_RePORTER/FR-raw_proj_metadata-XML/FedRePORTER_PRJ_X_FY2015.xml")

# 2016: 91126    (no probs with read_csv or fread)
Y2016_data_multiple_fed_reporter <- xmlToDataFrame("data/prd/Federal_RePORTER/FR-raw_proj_metadata-XML/FedRePORTER_PRJ_X_FY2016.xml")

# 2017: 88422
Y2017_data_multiple_fed_reporter <- xmlToDataFrame("data/prd/Federal_RePORTER/FR-raw_proj_metadata-XML/FedRePORTER_PRJ_X_FY2017.xml")

# 2018: 96211
Y2018_data_multiple_fed_reporter <- xmlToDataFrame("data/prd/Federal_RePORTER/FR-raw_proj_metadata-XML/FedRePORTER_PRJ_X_FY2018.xml")

# 2019: 80068
Y2019_data_multiple_fed_reporter <- xmlToDataFrame("data/prd/Federal_RePORTER/FR-raw_proj_metadata-XML/FedRePORTER_PRJ_X_FY2019.xml")


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
  Y2019_data_multiple_fed_reporter
)

# write to RDS 
# write_rds(A_metadatafederalreporter, "data/prd/Federal_RePORTER/FR-raw_proj_metadata-XML/FR_raw-metadata_2021FEB18.rds")



# Data already written - START HERE --------------------------------------------------------------

A_abstracts_federal_reporter <- readRDS("data/prd/Federal_RePORTER/FR-raw_proj_abstracts-CSV/FR_raw-abstracts_2021FEB23.rds")
A_metadatafederalreporter <- readRDS("data/prd/Federal_RePORTER/FR-raw_proj_metadata-XML/FR_raw-metadata_2021FEB18.rds")


# Format fixes in project metadata and abstracts ----------------------------------------------------------------


# Change cost and subcost columns to type numerical

A_metadatafederalreporter$FY_TOTAL_COST <- as.numeric(A_metadatafederalreporter$FY_TOTAL_COST)
A_metadatafederalreporter$FY_TOTAL_COST_SUB_PROJECTS <- as.numeric(A_metadatafederalreporter$FY_TOTAL_COST_SUB_PROJECTS)

# Change project_id and FY type to character in abstracts to match with metadata types

A_abstracts_federal_reporter$PROJECT_ID <- as.character(A_abstracts_federal_reporter$PROJECT_ID)
A_abstracts_federal_reporter$FY <- as.character(A_abstracts_federal_reporter$FY)

# Set empty strings ("") to NAs in the project metadata

A_metadatafederalreporter[A_metadatafederalreporter == ""] <- NA


#Fix CFDA Codes:

CFDA_glossary <- read_csv("glossary/CFDA_glossary_final.csv") %>%
  select(department_abb, department_digits)
colnames(CFDA_glossary)[colnames(CFDA_glossary)=="department_abb"] <- "DEPARTMENT"

data_fixed <- left_join(A_metadatafederalreporter, CFDA_glossary, by = "DEPARTMENT")

#Cleaning Function:

fixing_cfda_code <- function(cfda_project_code){

  fixed_cfda <-ifelse(cfda_project_code != 000 & cfda_project_code != 999 & grepl("^[0-9]{3}$", cfda_project_code) == TRUE,
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

write.csv(AAA_updated_federal_reporter, file = "data/prd/Federal_RePORTER/FR_raw_2021FEB24.csv", row.names = FALSE)
write_rds(AAA_updated_federal_reporter, "data/prd/Federal_RePORTER/FR_raw_2021FEB24.rds")

library(feather)
write_feather(AAA_updated_federal_reporter, "data/prd/Federal_RePORTER/FR_raw_2021FEB24.feather")
