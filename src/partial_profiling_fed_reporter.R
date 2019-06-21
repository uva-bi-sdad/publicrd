library(tidyverse)
library(plyr)
fed_reporter <- read_csv("data/original/2016-data_multiple_fed-reporter.csv")
View(fed_reporter)

##Evaluating Validity of DEPARTMENT

#Since there are very few poosible entries, we can filter the data set into
#unique entries and check if they are valid.

unique(fed_reporter$DEPARTMENT)
department_frequency <- count(fed_reporter, "fed_reporter$DEPARTMENT")
View(department_frequency)
#Eight unique entries: HHS, USDA, EPA, VA, NSF, NASA, DOD and ED


##Evaluating Validity of AGENCY

#Since there are very few poosible entries, we can filter the data set into
#unique entries and check if they are valid.

unique(fed_reporter$AGENCY)
agency_frequency <- count(fed_reporter, "fed_reporter$AGENCY")
View(agency_frequency)
#Eighteen unique entries, all of which are valid agencies.


##Evaluating Validity of IC_CENTER


##Evaluating Validity of PROJECT_NUMBER


##Evaluating Validity of PROJECT_START_DATE and PROJECT_END_DATE

#While the date formatting must eventually be changed, we can do a preliminary
#check that the dates in the current formatting make sense.



##CONTACT_PI_PROJECT_LEADER


##OTHER_PIS


##CONGRESSIONAL_DISTRICT


##DUNS_NUMBER

#Must be nine digits and only numbers

#Using regex to only collect numbers-only entries with exactly nine digits
properduns <- fed_reporter[which(grepl("^[0-9]{9}$", fed_reporter$DUNS_NUMBER) == TRUE), "DUNS_NUMBER"]
View(properduns)

#Using regex to isolate improperly formatted entries
improperduns <- fed_reporter[which(grepl("^[0-9]{9}$", fed_reporter$DUNS_NUMBER) == FALSE), "DUNS_NUMBER"]
View(improperduns)
View(drop_na(improperduns))
#Only two non-N/A DUNS that do not follow the formatting


##ORGANIZATION_NAME



##ORGANIZATION_CITY


##ORGANIZATION_STATE
unique(fed_reporter$ORGANIZATION_STATE)
count(fed_reporter$ORGANIZATION_STATE)
#There are 69 different postal code ISO in this column


##ORGANIZATION_ZIP

#Zip codes should be five digits, nine digits or five digits followed by a 
#hyphen then four additional digits.

#Normally formatted Zip Codes
properzip <- fed_reporter[which(grepl("^[0-9]{5}$", fed_reporter$ORGANIZATION_ZIP) == TRUE |
                                  grepl("^[0-9]{9}$", fed_reporter$ORGANIZATION_ZIP) == TRUE | 
                                  grepl("^[0-9]{5}-[0-9]{4}$", fed_reporter$ORGANIZATION_ZIP) == TRUE), "ORGANIZATION_ZIP"]
View(properzip)


#Abnormally formatted Zip Codes
orgzip_length <- nchar(fed_reporter$ORGANIZATION_ZIP)
properlength <- ifelse(orgzip_length == 5 |
                         orgzip_length == 9 |
                         orgzip_length == 10, TRUE, FALSE)

improperzip <- fed_reporter[which(grepl("[0-9-]", fed_reporter$ORGANIZATION_ZIP) == FALSE | properlength == FALSE), "ORGANIZATION_ZIP"]
View(drop_na(improperzip))


##ORGANIZATION_COUNTRY

#

unique(fed_reporter$ORGANIZATION_COUNTRY)
country_frequency <- count(fed_reporter, "fed_reporter$ORGANIZATION_COUNTRY")
View(country_frequency)
