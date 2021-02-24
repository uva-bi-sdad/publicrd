# Check missingness in Total Cost data for raw Fed RePORTER data

library(tidyverse)
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(readr)
library(naniar)

df <- readRDS("data/prd/Federal_RePORTER/FR-raw_proj_metadata-XML/FR_raw-metadata_2021FEB18.rds")

# Change cost and subcost columns to type numerical

df$FY_TOTAL_COST = as.numeric(df$FY_TOTAL_COST)
df$FY_TOTAL_COST_SUB_PROJECTS = as.numeric(df$FY_TOTAL_COST_SUB_PROJECTS)

miss_var_summary(df) # probably want to replace blanks with NA


# Plot total cost missingness by FY -----------------------------------------------------------

missing_cost <- df %>% 
  group_by(FY) %>% 
  summarise(num = n(),
            num_miss_cost = sum(is.na(FY_TOTAL_COST)),
            pct_miss_cost = 100*sum(is.na(FY_TOTAL_COST)/length(FY_TOTAL_COST)))

# plot missing cost pct

ggplot(data = missing_cost, mapping = aes(x = FY, y = pct_miss_cost)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total Cost: Percent Missing by FY", y = "Percent Missing") +
  theme_minimal()
  
  
# Plot total cost missingness by FY and Department -----------------------------------------------

missing_cost_by_dept <- df %>% 
  group_by(FY, DEPARTMENT) %>% 
  summarise(num = n(),
            num_miss_cost = sum(is.na(FY_TOTAL_COST)),
            pct_miss_cost = 100*sum(is.na(FY_TOTAL_COST)/length(FY_TOTAL_COST)))

# plot missing cost pct

ggplot(data = missing_cost_by_dept, mapping = aes(x = FY, y = pct_miss_cost)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  facet_wrap(vars(DEPARTMENT), nrow=2) +
  labs(title = "Total Cost: Percent Missing by FY and Department", y = "Percent Missing") +
  theme_bw() +
  theme(axis.text.x=element_text(angle = 60, vjust = 0.5, size=10))



  
  