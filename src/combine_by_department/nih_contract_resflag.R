library(data.table)
library(tidyverse)
library(lubridate)

loc <- "data/working/combine_by_department/NIH"
nih_contract <- fread(file.path(loc, "nih_usa-spending_contracts_2016.csv"))
nih_contract <- nih_contract[,3:69]

prod_code_summary <- nih_contract %>% group_by(product_or_service_code, product_or_service_code_description) %>%
  summarise(count = n()) %>% arrange(desc(count))

prod_code_rnd <- prod_code_summary %>% filter(substr(product_or_service_code_description, 1, 3) == "R&D" |
                                                product_or_service_code %in% c("C118", "R405", "B599", "B529", "B537", "B504", "B599", "B542", "B506"))

nih_contract_resflag <- mutate(nih_contract, resflag = product_or_service_code %in% prod_code_rnd$product_or_service_code)
View(select(nih_contract_resflag, resflag, product_or_service_code, product_or_service_code_description))
View(nih_contract_resflag %>% filter(resflag == FALSE) %>% group_by(product_or_service_code, product_or_service_code_description) %>% summarise(count = n()) %>% arrange(desc(count)))

write.csv(nih_contract_resflag, file.path(loc, "nih_usa-spending_contracts_2016_resflag.csv"))
