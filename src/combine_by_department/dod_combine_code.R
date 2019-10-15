library(data.table)
library(tidyverse)
library(lubridate)
library(stringr)

loc <- "data/working/combine_by_department/DOD"

dod_grant <- fread(file.path(loc, "dod_usa-spending_grants_2016.csv"), drop = "V1")
dod_fed <- fread(file.path(loc, "dod_fed-reporter_2016.csv"), drop = "V1")
cdmrp <- fread(file.path(loc, "CDMRP.csv"))

dod_fed %>% group_by(AGENCY) %>% summarise(count = n()) %>% arrange(desc(count))

#taking the dashes out of the award numbers in the CDMRP cross-reference file
cdmrp$clean_award_number <- gsub("-","",cdmrp$award_number)

#checking to see how many fed reporter projects have a cdmrp proposal number (96%), and how many
#CDMRP projects have a matching fedreporter project (15%)
sum(dod_fed$PROJECT_NUMBER %in% cdmrp$proposal_number)/nrow(filter(dod_fed, AGENCY == "CDMRP"))
sum(cdmrp$proposal_number %in% dod_fed$PROJECT_NUMBER)/nrow(cdmrp)

#adding the award number from cdmrp to fed reporter

dod_fed <- left_join(dod_fed, cdmrp[,c("proposal_number", "clean_award_number")], by = c("PROJECT_NUMBER" = "proposal_number"))
                                                                                         
#how many FAIN IDs in the grants data match the award numbers in cdmrp? (14%) there isn't great labelling of
#sub-agencies/offices for DOD in USA Spending, so unknown how much ground we're really covering

sum(dod_grant$award_id_fain %in% cdmrp$clean_award_number)/nrow(dod_grant)
dod_grant %>% group_by(awarding_sub_agency_name) %>% summarise(count = n()) %>% arrange(desc(count))
dod_grant %>% group_by(awarding_office_name) %>% summarise(count = n()) %>% arrange(desc(count))

#putting fed reporter match flag in DOD USA Spending data
dod_grant <- mutate(dod_grant, fedmatch = award_id_fain %in% dod_fed$clean_award_number)

#putting USA Spending match flag in DOD Fed RePORTER data
dod_fed <- mutate(dod_fed, spendmatch = clean_award_number %in% dod_grant$award_id_fain)


##there are LOTS of CFDA codes that directly say "basic scientific research", "research and development",
##etc. if one were to HYPOTHETICALLY use this to classify research/not-research, catches TONS of stuff
##that the CDMRP file misses:

View(dod_grant %>% group_by(cfda_number, cfda_title) %>% summarise(count = n()) %>% arrange(desc(count)))

cfda_grant <- c("12.300", "12.431", "12.420", "12.42", "12.800", "12.630", "12.63", "12.910", "12.91", "12.351", 
                "12.750", "12.75", "12.630", "12.63", "12.360", "12.36", "12.351", "12.114", "12.615")

sum(dod_grant$cfda_number %in% cfda_grant & dod_grant$fedmatch == FALSE)
sum(dod_grant$cfda_number %in% cfda_grant)

unique(filter(dod_grant, fedmatch == TRUE)$cfda_title)

####not set on doing this yet, so just putting in the resflag based on fedmatch

dod_grant$resflag <- dod_grant$fedmatch

###writing out

write.csv(dod_grant, file.path(loc, "dod_usa_spending_grants_2016_resflag.csv"))
write.csv(dod_fed, file.path(loc, "dod_fed-reporter_2016_matchflag.csv"))
