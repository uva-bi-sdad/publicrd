#Upload separate department matchflag documents

#USDA
usda_usa_spending_grants_2016_matchflag <- read_csv("data/working/usda_usa-spending_grants_2016_matchflag.csv")

#DOD
dod_usa_spending_grants_2016_matchflag <- read_csv("data/working/combine_by_department/DOD/dod_usa_spending_grants_2016_resflag.csv") %>%
  select(-X1)

#NASA
nasa_usa_spending_grants_2016_matchflag <- read_csv("data/working/combine_by_department/NASA/nasa_usa-spending_grants_2016_matchflag.csv") %>%
  select(-X1, -V1) %>%
  rename(fedmatch = fed_reporter_match)

#NIH
nih_usa_spending_grants_2016_matchflag <- read_csv("data/working/combine_by_department/NIH/nih_usa-spending_grants_2016_matchflag.csv") %>%
  select(-X1, -V1, -V1_1, -matchcount) %>%
  rename(fedmatch = hasmatch)

#NSF
nsf_usa_spending_grants_2016_matchflag <- read_csv("data/working/combine_by_department/NSF/nsf_usa-spending_grants_2016_matchflag.csv") %>%
  select(-X1, -V1)

#DOE
doe_usa_spending_grants_2016_matchflag <- read_csv("data/working/combine_by_department/DOE/doe_usa-spending_grants_2016.csv") %>%
  add_column(fedmatch = FALSE, resflag = FALSE) %>%
  select(-X1)

#Combine Together into one document:
usa_spending_grants_2016_matchflag <- rbind(usda_usa_spending_grants_2016_matchflag,
                                            dod_usa_spending_grants_2016_matchflag,
                                            nasa_usa_spending_grants_2016_matchflag,
                                            nih_usa_spending_grants_2016_matchflag,
                                            nsf_usa_spending_grants_2016_matchflag,
                                            doe_usa_spending_grants_2016_matchflag)

#Write and save .csv
write.csv(usa_spending_grants_2016_matchflag, "usa_spending_grants_2016_matchflag.csv", row.names = FALSE)
