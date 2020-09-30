# read in dataset code added --------------

library(readr)
library(gridExtra)
library(ggplot2)
library(dplyr)
library(stringr)

data_for_liz <- read_csv("../../data/final/dashboard_data/data_for_liz.csv")

# ------------------
# 
# # Find Start Year --------
# 
# data_for_liz$START_YEAR = 0
# 
# for(i in 1:nrow(data_for_liz))
# {
#   d = data_for_liz$PROJECT_START_DATE[i]
# 
#   splitdate = strsplit(d, "/")
#   
#   if(length(splitdate[[1]]) == 3)
#   {
#     y = splitdate[[1]][3]
#   }
#   else
#   {
#     y = splitdate[[1]][1]
#   }
#   
#   data_for_liz$START_YEAR[i] <- as.integer(y) 
# }
# 
# # ---------------------------------------

# new code to find start year

data_for_liz$START_YEAR <- str_sub(data_for_liz$PROJECT_START_DATE, -4)
data_for_liz$START_YEAR <- as.integer(data_for_liz$START_YEAR)


# ----------------------------------

data_for_liz %>%
  count(DEPARTMENT) %>%
  mutate(perc = (n / nrow(data_for_liz)) *100) %>%
  ggplot(aes(x = reorder(DEPARTMENT, -perc), y = perc)) +
  geom_bar(stat = "identity", show.legend = FALSE, fill = "#0072B2") +
  ylim(0, 100) +
  labs(x = "Department", y = "Percent of Dataset") +
  ggtitle("Abstract Count by Funding Department") +
  theme_bw() -> dept_gr


data_for_liz %>%
  ggplot(aes(x = as.factor(FY), fill = as.factor(FY))) +
  geom_bar(show.legend = FALSE) +
  labs(x = "Year") +
  ggtitle("Abstract Count by Year") +
  theme_bw()

data_for_liz %>%
  filter(START_YEAR > 2007) %>%
  filter(START_YEAR < 2020) %>%
  ggplot(aes(x = as.factor(START_YEAR))) +
  geom_bar(show.legend = FALSE, fill = "#0072B2") +
  xlab("Year") +
  ylab("Number of Abstracts") +
  ggtitle("Abstract Count by Project Start Year") +
  #labs(main = "Abstract Count by Project Start Year", x = "Year") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 35)) -> date_gr

data_for_liz$ab_char <- nchar(data_for_liz$ABSTRACT)
mean(data_for_liz$ab_char)
min(data_for_liz$ab_char)
max(data_for_liz$ab_char)

data_for_liz %>%
  filter(ab_char < 10001) %>%
  ggplot(aes(x = ab_char)) +
  geom_histogram(binwidth = 100, fill = "#0072B2") +
  xlab("Number of Characters") +
  ylab("Number of Abstracts") +
  ggtitle("Number of Characters per Abstracts") +
  theme_bw() -> char_hist

#grid.arrange(dept_gr, date_gr, char_hist, nrow = 2)

grobs = list(dept_gr, date_gr, char_hist)

margin = theme(plot.margin = unit(c(1,1,0.5,1), "cm"))  # top, right, bottom, left
grid.arrange(grobs = lapply(grobs, "+", margin), nrow = 2)


