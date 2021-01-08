library(readr)

df = read_csv("./data/final/app_data.csv")

write_rds(df, "./data/final/app_data.rds")
