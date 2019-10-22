library(R.utils)
library(tidyverse)
library(data.table)
library(plyr)

fed_reporter <- read_csv("data/original/2016-data_multiple_fed-reporter.csv")

##Creating Data Frame

field_profile <- data.frame(field = as.character(colnames(fed_reporter)), completeness = as.numeric(NA),
                            validity = as.numeric(NA))

field_profile$completeness <- mutate(field_profile, is.na(fed_reporter))


##~~~~~~~~~~~~~~~~COMPLETENESS~~~~~~~~~~~~~~~~

##Creating completeness function

#Function to calculate proportion of N/A responses in dataset
na_count <- function(x){
  (sum(is.na(x)) + sum(x == "", na.rm = TRUE))/nrow(fed_reporter)
}

#Function to calculate % complete
percent_complete <- function(x){
  (1-na_count(x))*100
}

#Update field_profile with completeness
field_profile$completeness <- apply(fed_reporter, MARGIN = 2, percent_complete)

##~~~~~~~~~~~~~~~~VALIDITY~~~~~~~~~~~~~~~~

##PROJECT_ID
head(fed_reporter$PROJECT_ID)
class(fed_reporter$PROJECT_ID)

sum(nchar(fed_reporter$PROJECT_ID) != 6)
fed_reporter[which(nchar(fed_reporter$PROJECT_ID) != 6),"PROJECT_ID"]
#Found that PROJECT_ID is usually 6 characters but sometimes 7 characters, always an integer.
#We are going to consider that valid
field_profile[1,"validity"] <- 100


##PROJECT_TERMS
#PROJECT_TERMS too qualitative to determine whether it is valid


##PROJECT_TITLE
#All are character; checking to see if any are potentially abstracts:

#Getting lengths of titles and checking distribution
title_length <- nchar(fed_reporter$PROJECT_TITLE)
summary(title_length)

#Saw a right-skewed distribution with group of outliers at 200 characters and above. Investigating
#how many are above 200 characters, what common lengths might be, and manually inspecting some outliers
long_title <- fed_reporter[which(title_length >= 200), "PROJECT_TITLE"]
sum(nchar(long_title$PROJECT_TITLE) == 200)
unique(nchar(long_title$PROJECT_TITLE))
table(nchar(long_title$PROJECT_TITLE))
filter(long_title, nchar(PROJECT_TITLE) == 255)[1,]
filter(long_title, nchar(PROJECT_TITLE) == 254)[1:5,]

#Found that 1,102 of 1,416 titles above 200 characters in length were precisely 255 characters. A
#manual inspection of some of these showed that they were sentences (probably abstracts) or a 
#title followed by an abstract. There were 217 254-character titles, which also appeared to be abstracts.
#The 31 200-character titles were actually titles. For purposes of initial profiling, considering
#255 and 254 character titles to be invalid (but a deeper audit is likely needed before making
#decisions about the project)

field_profile[3,"validity"] <- 100-((1102+217)/nrow(fed_reporter))*100

##DEPARTMENT

#Since there are very few poosible entries, we can filter the data set into
#unique entries and check if they are valid.

unique(fed_reporter$DEPARTMENT)
department_frequency <- count(fed_reporter, "fed_reporter$DEPARTMENT")
#Eight unique entries: HHS, USDA, EPA, VA, NSF, NASA, DOD and ED
#These are all departments or independent agencies so we are going to say that
#they are valid.

field_profile[4,"validity"] <- 100

##AGENCY

#Since there are very few poosible entries, we can filter the data set into
#unique entries and check if they are valid.

unique(fed_reporter$AGENCY)
agency_frequency <- count(fed_reporter, "fed_reporter$AGENCY")
#Eighteen unique entries, all of which are valid agencies.

field_profile[5,"validity"] <- 100

##IC_CENTER

unique(fed_reporter$IC_CENTER)
IC_CENTER_frequency <- count(fed_reporter, "fed_reporter$IC_CENTER")
#There are 47 results, all of which are valid agency names. I found a good glossary with most of the acronyms listed here:
#https://research.umbc.edu/files/2014/10/Examples-of-PHS-Funding-Agencies.docx.pdf
field_profile[6,"validity"] <- (nrow(fed_reporter)-sum(is.na(fed_reporter$IC_CENTER)))/nrow(fed_reporter)*100


##PROJECT_NUMBER
#Each department has its own formatting method, tried to cover the common ones here although more detail is likely needed.

#For NIH
#Information on formatting: https://era.nih.gov/files/Deciphering_NIH_Application.pdf
project_number_nih <- fed_reporter[which(grepl("^[0-9]{1}[A-Z0-9]{3}[A-Z]{2}[0-9]{6}-[0-9]{2}$", fed_reporter$PROJECT_NUMBER) == TRUE), "PROJECT_NUMBER"]
project_number_nih_suffix <- fed_reporter[which(grepl("^[0-9]{1}[A-Z0-9]{3}[A-Z]{2}[0-9]{6}-[0-9]{2}[A-Z][0-9]$", fed_reporter$PROJECT_NUMBER) == TRUE), "PROJECT_NUMBER"]


#For DOD
#Two letters followed by 6 digits. Might have suffix if project has been relaunched, etc.
project_number_dod <- fed_reporter[which(grepl("^[A-Z]{2}[0-9]{6}$", fed_reporter$PROJECT_NUMBER) == TRUE), "PROJECT_NUMBER"]
project_number_dod_suffix <- fed_reporter[which(grepl("^[A-Z]{2}[0-9]{6}[A-Z][0-9]$", fed_reporter$PROJECT_NUMBER) == TRUE), "PROJECT_NUMBER"]

#For USDA and NSF
#7 letter code. Looks like for USDA, projects in ARS and USFS have prefixes and that might be redundant with agency or not be valid
project_number_usda_nsf <- fed_reporter[which(grepl("[0-9]{7}$", fed_reporter$PROJECT_NUMBER) == TRUE), "PROJECT_NUMBER"]

project_number_invalid <- fed_reporter[which(grepl("^[0-9]{1}[A-Z0-9]{3}[A-Z]{2}[0-9]{6}-[0-9]{2}$", fed_reporter$PROJECT_NUMBER) == FALSE &
                                               grepl("^[0-9]{1}[A-Z0-9]{3}[A-Z]{2}[0-9]{6}-[0-9]{2}[A-Z][0-9]$", fed_reporter$PROJECT_NUMBER) == FALSE &
                                               grepl("^[A-Z]{2}[0-9]{6}$", fed_reporter$PROJECT_NUMBER) == FALSE &
                                               grepl("^[A-Z]{2}[0-9]{6}[A-Z][0-9]$", fed_reporter$PROJECT_NUMBER) == FALSE &
                                               grepl("[0-9]{7}$", fed_reporter$PROJECT_NUMBER) == FALSE), "PROJECT_NUMBER"]
#Many of these have a four digit number in parentheses after the code. I do not know if this is correct so I am 
#conservatively marking these as invalid, but the rest of these entries are likely valid. I will try to find more info about this, but
#an initial search turned up nothing about what these four digit codes mean.

#Although not entirely complete, this identifies almost all of the main notations covered in this category.
field_profile[7,"validity"] <- ((nrow(fed_reporter)-nrow(project_number_invalid))/(nrow(fed_reporter))*100)


##PROJECT_START_DATE and PROJECT_END_DATE

#While the date formatting must eventually be changed, we can do a preliminary
#check that the dates in the current formatting make sense.

#For PROJECT_START_DATE
start_date_valid <- fed_reporter[which(grepl("^[0-9]|1[0-2]/
                                               [0-9]|[1-3][0-9]/
                                               [1-2][0,9][0-9][0-9]$", fed_reporter$PROJECT_START_DATE) == TRUE), "PROJECT_START_DATE"]

field_profile[8,"validity"] <- (nrow(start_date_valid)/nrow(fed_reporter))*100

#For PROJECT_END_DATE
end_date_valid <- fed_reporter[which(grepl("^[0-9]|1[0-2]/
                                             [0-9]|[1-3][0-9]/
                                             [1-2][0,9][0-9][0-9]$", fed_reporter$PROJECT_END_DATE) == TRUE), "PROJECT_END_DATE"]

field_profile[9,"validity"] <- (nrow(end_date_valid)/nrow(fed_reporter))*100

##CONTACT_PI_PROJECT_LEADER
#Test to make sure no numbers
project_leader_numbers <- fed_reporter[which(grepl("[0-9]", fed_reporter$CONTACT_PI_PROJECT_LEADER) == TRUE), "CONTACT_PI_PROJECT_LEADER"]
#Only one response has a number in it, appears to be an accidental zero instead of an O middle initial.

#Determine typical length in this section
project_leader_length <- nchar(fed_reporter$CONTACT_PI_PROJECT_LEADER)
summary(project_leader_length)
hist(project_leader_length)
#No upper outliers. Seems to be several lower outliers.
project_leader_invalid <- fed_reporter[which(nchar(fed_reporter$CONTACT_PI_PROJECT_LEADER) == 1), "CONTACT_PI_PROJECT_LEADER"]
#Several one character long "names" that are clearly not valid.
project_leader_valid <- fed_reporter[which(nchar(fed_reporter$CONTACT_PI_PROJECT_LEADER) > 1), "CONTACT_PI_PROJECT_LEADER"]

field_profile[10,"validity"] <- (nrow(project_leader_valid)/nrow(fed_reporter))*100


##OTHER_PIS

#Test to make sure no numbers
other_pis_numbers <- fed_reporter[which(grepl("[0-9]", fed_reporter$OTHER_PIS) == TRUE), "OTHER_PIS"]
#Only two responses have numbers in them.

#Determine typical length in this section
other_pis_length <- nchar(fed_reporter$OTHER_PIS)
summary(other_pis_length)
hist(other_pis_length)
#Several very high character responses, checking ones above 200 characters:
long_other_pis <- fed_reporter[which(other_pis_length >= 200), "OTHER_PIS"]
#These do not actually seem incorrect, just much longer entries in same format (lists of names separated with semicolons).
other_pis_invalid <- fed_reporter[which(nchar(fed_reporter$OTHER_PIS) == 1), "OTHER_PIS"]
#No lower outliers.
other_pis_valid <- fed_reporter[which(nchar(fed_reporter$OTHER_PIS) > 1 & grepl("[0-9]", fed_reporter$OTHER_PIS) == FALSE), "OTHER_PIS"]

field_profile[11,"validity"] <- (nrow(other_pis_valid)/nrow(fed_reporter))*100

##CONGRESSIONAL_DISTRICT

#Congressional district numbers range from 1 to 53, depending on how many each state has (California with 53 as max). 
#There are no 0s and there should be nothing above 53. 
#Additional checks can be done to make sure it is valid for each state (we can do that in the consistency section), 
#but at a preliminary level we can weed out anything else.
congressional_district_valid <- fed_reporter[which(grepl("^[0-5][1-9]$", fed_reporter$CONGRESSIONAL_DISTRICT) == TRUE), "CONGRESSIONAL_DISTRICT"]
field_profile[12,"validity"] <- (nrow(congressional_district_valid)/nrow(fed_reporter))*100

##DUNS_NUMBER

#Must be nine digits and only numbers
#Using regex to collect numbers-only entries with exactly nine digits
duns_number_valid <- fed_reporter[which(grepl("^[0-9]{9}$", fed_reporter$DUNS_NUMBER) == TRUE), "DUNS_NUMBER"]
#Only two non-N/A DUNS that do not follow the formatting

field_profile[13,"validity"] <- (nrow(duns_number_valid)/nrow(fed_reporter))*100

##ORGANIZATION_NAME
#Checking length of names:
orgname_length <- nchar(fed_reporter$ORGANIZATION_NAME)
summary(orgname_length)
hist(orgname_length)
#Very sharp drop around 40 chars. Looking at everything above 40:
long_orgname <- fed_reporter[which(orgname_length > 40), "ORGANIZATION_NAME"]
#None of these really stand out but the all of the abbreviations and very sharp drop in frequency suggests
#that most organizations were capped at 40 characters. I didn't see any invalid organization names though.

field_profile[14,"validity"] <- field_profile[14, "completeness"]


##ORGANIZATION_CITY
#Checking length of names:
orgcity_length <- nchar(fed_reporter$ORGANIZATION_CITY)
summary(orgcity_length)
hist(orgcity_length)
short_orgcity <- fed_reporter[which(orgcity_length <= 5), "ORGANIZATION_CITY"]
#Nothing here seems out of the ordinary, although I have spotted a few county names (e.g. Bronx) so we will
#have to screen this for consistency at a later stage.

#Checking if there are any numbers in the name:
organization_city_valid <- fed_reporter[which(grepl("[a-zA-Z. -]", fed_reporter$ORGANIZATION_CITY)), "ORGANIZATION_CITY"]
organization_city_invalid <- fed_reporter[which(grepl("[^a-zA-Z. -]", fed_reporter$ORGANIZATION_CITY)), "ORGANIZATION_CITY"]
#These include zip codes, commas and other oddities, so marking as invalid.

field_profile[15,"validity"] <- (nrow(organization_city_valid)/nrow(fed_reporter)*100)


##ORGANIZATION_STATE
unique(fed_reporter$ORGANIZATION_STATE)
count(fed_reporter$ORGANIZATION_STATE)

#69 results, all two letters:
#50 US states+ 6 US commonwealths and territories + 6 Canadian provinces + 5 Miscellaneous = 67 valid codes

#Glossary for non-US States:
#District of Columbia (DC), Puerto Rico (PR), Virgin Islands (VI), Guam (GU), Northern Mariana Islands (MP), American Samoa (AS)
#Alberta (AB), Nova Scotia (NS), British Columbia (BC), Manitoba (MB), Quebec (QC), Ontario (ON) 
#Marshall Islands (MH), Palau (PW), Micronesia (FM), US Armed Forces-Europe (AE), US Armed Forces-Pacific (AP)

#(PQ) is an invalid version of Quebec. Changed from PQ to QC in 1991.
#(ST) is invalid, appears to be typo for St. George in Bermuda.
#Number of PQ+ST is only 10 mismarked total.

field_profile[16,"validity"] <- ((nrow(fed_reporter) - sum(is.na(fed_reporter$ORGANIZATION_STATE)))-10)/(nrow(fed_reporter))*100


##ORGANIZATION_ZIP

#Zip codes should be five digits, nine digits or five digits followed by a 
#hyphen then four additional digits.

#Normally formatted Zip Codes
organization_zip_valid <- fed_reporter[which(grepl("^[0-9]{5}$", fed_reporter$ORGANIZATION_ZIP) == TRUE |
                                  grepl("^[0-9]{9}$", fed_reporter$ORGANIZATION_ZIP) == TRUE | 
                                  grepl("^[0-9]{5}-[0-9]{4}$", fed_reporter$ORGANIZATION_ZIP) == TRUE), "ORGANIZATION_ZIP"]

#Abnormally formatted Zip Codes
orgzip_length <- nchar(fed_reporter$ORGANIZATION_ZIP)
properlength <- ifelse(orgzip_length == 5 |
                         orgzip_length == 9 |
                         orgzip_length == 10, TRUE, FALSE)

improperzip <- fed_reporter[which(grepl("[0-9-]", fed_reporter$ORGANIZATION_ZIP) == FALSE | 
                                    properlength == FALSE), "ORGANIZATION_ZIP"]

field_profile[17,"validity"] <- (nrow(organization_zip_valid)/nrow(fed_reporter))*100


##ORGANIZATION_COUNTRY
country_frequency <- count(fed_reporter, "fed_reporter$ORGANIZATION_COUNTRY")
field_profile[18,"validity"] <- field_profile[18,"completeness"]


##BUDGET_START_DATE and BUDGET_END_DATE

#For BUDGET_START_DATE
budget_start_date_valid <- fed_reporter[which(grepl("^[0-9]|1[0-2]/
                                         [0-9]|[1-3][0-9]/
                                         [1-2][0,9][0-9][0-9]$", fed_reporter$BUDGET_START_DATE) == TRUE), "BUDGET_START_DATE"]

field_profile[19,"validity"] <- (nrow(budget_start_date_valid)/nrow(fed_reporter))*100

#For BUDGET_END_DATE
budget_end_date_valid <- fed_reporter[which(grepl("^[0-9]|1[0-2]/
                                         [0-9]|[1-3][0-9]/
                                         [1-2][0,9][0-9][0-9]$", fed_reporter$BUDGET_END_DATE) == TRUE), "BUDGET_END_DATE"]

field_profile[20,"validity"] <- (nrow(budget_end_date_valid)/nrow(fed_reporter))*100


##CFDA_CODE
#Must be either [3 digits] or [2 digits, a period, then 3 more digits]
#A CFDA code of 000 or 999 means that some information is missing 
# More information: https://www.ofm.wa.gov/sites/default/files/public/legacy/policy/95.20.htm
#so we need to weed these out as well

cfda_code_valid <- fed_reporter[which(fed_reporter$CFDA_CODE != 000 & fed_reporter$CFDA_CODE != 999 & 
                                 grepl("^[0-9]{3}$", fed_reporter$CFDA_CODE) == TRUE |
                                 grepl("[0-9]{2}\\.[0-9]{3}$", fed_reporter$CFDA_CODE) == TRUE), "CFDA_CODE"]

field_profile[21,"validity"] <- (nrow(cfda_code_valid)/nrow(fed_reporter))*100

##FY
#Has to be a year, sample data is 2016.
unique(fed_reporter$FY)
#Every single value is 2016, so all are valid.
field_profile[22,"validity"] <- 100


##FY_TOTAL_COST and FY_TOTAL_COST_SUB_PROJECTS
#After initial viewing, there are answers of $1 or $2, which seem like placeholder values and not valid. Code for two digits or higher:
total_cost_valid <- fed_reporter[which(grepl("^[0-9][0-9]+$", fed_reporter$FY_TOTAL_COST) == TRUE), "FY_TOTAL_COST"]

field_profile[23,"validity"] <- (nrow(total_cost_valid)/nrow(fed_reporter))*100

#Same occurrences for SUB_PROJECTS so same code for two digits or higher:
total_cost_sub_valid <- fed_reporter[which(grepl("^[0-9][0-9]+$", fed_reporter$FY_TOTAL_COST_SUB_PROJECTS) == TRUE), "FY_TOTAL_COST_SUB_PROJECTS"]

field_profile[24,"validity"] <- (nrow(total_cost_sub_valid)/nrow(fed_reporter))*100

View(field_profile)

##~~~~~~~~~~~~~~~~BENCHMARKING~~~~~~~~~~~~~~~~

#Three column data table: department name, funding according to FedREPORTER, funding according to SSF survey
benchmarking_fed_reporter <- data.frame(department = as.numeric(unique(fed_reporter$DEPARTMENT)), fed_reporter_total = as.numeric(NA),
                                        fss_rd = as.numeric(NA), fss_se = as.numeric(NA), percent_of_fss_in_fed_reporter = as.numeric(NA))

#First Column: Department Names
benchmarking_fed_reporter$department <- unique(fed_reporter$DEPARTMENT)

#Second Column: Filter FedREPORTER by each department, then return sum of "TOTAL_COST" for given department:
benchmarking_fed_reporter[1, "fed_reporter_total"] <- sum(drop_na(fed_reporter[which(grepl("HHS", fed_reporter$DEPARTMENT) == TRUE), "FY_TOTAL_COST"]))
benchmarking_fed_reporter[2, "fed_reporter_total"] <- sum(drop_na(fed_reporter[which(grepl("USDA", fed_reporter$DEPARTMENT) == TRUE), "FY_TOTAL_COST"]))
benchmarking_fed_reporter[3, "fed_reporter_total"] <- sum(drop_na(fed_reporter[which(grepl("EPA", fed_reporter$DEPARTMENT) == TRUE), "FY_TOTAL_COST"]))

benchmarking_fed_reporter[5, "fed_reporter_total"] <- sum(drop_na(fed_reporter[which(grepl("NSF", fed_reporter$DEPARTMENT) == TRUE), "FY_TOTAL_COST"]))
benchmarking_fed_reporter[6, "fed_reporter_total"] <- sum(drop_na(fed_reporter[which(grepl("NASA", fed_reporter$DEPARTMENT) == TRUE), "FY_TOTAL_COST"]))
benchmarking_fed_reporter[7, "fed_reporter_total"] <- sum(drop_na(fed_reporter[which(grepl("DOD", fed_reporter$DEPARTMENT) == TRUE), "FY_TOTAL_COST"]))
benchmarking_fed_reporter[8, "fed_reporter_total"] <- sum(drop_na(fed_reporter[which(grepl("ED", fed_reporter$DEPARTMENT) == TRUE), "FY_TOTAL_COST"]))

#Third Column: Sum R&D according to FSS

fss_nonprofit <- read_csv("data/original/2016-data_fss_nonprofit_rd.csv")
fss_university <- read_csv("data/original/2016-data_fss_university_rd.csv")

benchmarking_fed_reporter[1, "fss_rd"] <- (fss_nonprofit$HHS[1]+fss_university$`R&D Total`[11])*1000
benchmarking_fed_reporter[2, "fss_rd"] <- (fss_nonprofit$USDA[1]+fss_university$`R&D Total`[2])*1000
benchmarking_fed_reporter[3, "fss_rd"] <- (fss_nonprofit$EPA[1]+fss_university$`R&D Total`[30])*1000

benchmarking_fed_reporter[5, "fss_rd"] <- (fss_nonprofit$NSF[1]+fss_university$`R&D Total`[32])*1000
benchmarking_fed_reporter[6, "fss_rd"] <- (fss_nonprofit$NASA[1]+fss_university$`R&D Total`[31])*1000
benchmarking_fed_reporter[7, "fss_rd"] <- (fss_nonprofit$DOD[1]+fss_university$`R&D Total`[4])*1000
benchmarking_fed_reporter[8, "fss_rd"] <- (fss_nonprofit$ED[1]+fss_university$`R&D Total`[9])*1000

#Fourth Column: Sum Obligation according to FSS

benchmarking_fed_reporter[1, "fss_se"] <- (fss_nonprofit$HHS[1]+fss_university$`All federal obligations`[11])*1000
benchmarking_fed_reporter[2, "fss_se"] <- (fss_nonprofit$USDA[1]+fss_university$`All federal obligations`[2])*1000
benchmarking_fed_reporter[3, "fss_se"] <- (fss_nonprofit$EPA[1]+fss_university$`All federal obligations`[30])*1000

benchmarking_fed_reporter[5, "fss_se"] <- (fss_nonprofit$NSF[1]+fss_university$`All federal obligations`[32])*1000
benchmarking_fed_reporter[6, "fss_se"] <- (fss_nonprofit$NASA[1]+fss_university$`All federal obligations`[31])*1000
benchmarking_fed_reporter[7, "fss_se"] <- (fss_nonprofit$DOD[1]+fss_university$`All federal obligations`[4])*1000
benchmarking_fed_reporter[8, "fss_se"] <- (fss_nonprofit$ED[1]+fss_university$`All federal obligations`[9])*1000

#Fifth Column: Percent of FSS accounted for by FedREPORTER

benchmarking_fed_reporter[1, "percent_of_fss_in_fed_reporter"] <- (benchmarking_fed_reporter[1, "fed_reporter_total"]/benchmarking_fed_reporter[1, "fss_rd"])*100
benchmarking_fed_reporter[2, "percent_of_fss_in_fed_reporter"] <- (benchmarking_fed_reporter[2, "fed_reporter_total"]/benchmarking_fed_reporter[2, "fss_rd"])*100
benchmarking_fed_reporter[3, "percent_of_fss_in_fed_reporter"] <- (benchmarking_fed_reporter[3, "fed_reporter_total"]/benchmarking_fed_reporter[3, "fss_rd"])*100

benchmarking_fed_reporter[5, "percent_of_fss_in_fed_reporter"] <- (benchmarking_fed_reporter[5, "fed_reporter_total"]/benchmarking_fed_reporter[5, "fss_rd"])*100
benchmarking_fed_reporter[6, "percent_of_fss_in_fed_reporter"] <- (benchmarking_fed_reporter[6, "fed_reporter_total"]/benchmarking_fed_reporter[6, "fss_rd"])*100
benchmarking_fed_reporter[7, "percent_of_fss_in_fed_reporter"] <- (benchmarking_fed_reporter[7, "fed_reporter_total"]/benchmarking_fed_reporter[7, "fss_rd"])*100
benchmarking_fed_reporter[8, "percent_of_fss_in_fed_reporter"] <- (benchmarking_fed_reporter[8, "fed_reporter_total"]/benchmarking_fed_reporter[8, "fss_rd"])*100

View(benchmarking_fed_reporter)