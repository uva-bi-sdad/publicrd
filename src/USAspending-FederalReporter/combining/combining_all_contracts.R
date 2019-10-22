#Combining USAspending contracts

#DOD
dod_usa_spending_contracts_2016_resflag <- read_csv("data/working/combine_by_department/DOD/dod_usa-spending_contracts_2016_resflag.csv") %>%
  select(-X1, -V1)

#USDA
usda_usa_spending_contracts_2016_resflag <- read_csv("data/working/combine_by_department/USDA/usda_usa-spending_contracts_2016_resflag.csv") %>%
  select(-X1, -V1)

#DOE
doe_usa_spending_contracts_2016_resflag <- read_csv("data/working/combine_by_department/DOE/doe_usa-spending_contracts_2016_resflag.csv") %>%
  select(-X1, -V1)

#NASA
nasa_usa_spending_contracts_2016_resflag <- read_csv("data/working/combine_by_department/NASA/nasa_usa-spending_contracts_2016_resflag.csv") %>%
  select(-X1)

#NIH
nih_usa_spending_contracts_2016_resflag <- read_csv("data/working/combine_by_department/NIH/nih_usa-spending_contracts_2016_resflag.csv") %>%
  select(-X1)

#NSF
nsf_usa_spending_contracts_2016_resflag <- read_csv("data/working/combine_by_department/NSF/nsf_usa-spending_contracts_2016_resflag.csv") %>%
  select(-X1)

usa_spending_contracts_2016_resflag <- rbind(dod_usa_spending_contracts_2016_resflag,
                                             usda_usa_spending_contracts_2016_resflag,
                                             doe_usa_spending_contracts_2016_resflag,
                                             nasa_usa_spending_contracts_2016_resflag,
                                             nih_usa_spending_contracts_2016_resflag,
                                             nsf_usa_spending_contracts_2016_resflag)

write.csv(usa_spending_contracts_2016_resflag, "usa-spending_contracts_2016_resflag.csv", row.names = FALSE)
