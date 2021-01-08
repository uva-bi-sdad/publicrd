library(tidyverse)
library(tidytext)
library(readr)
library(naniar)

raw_abstracts_2020 <- read_csv("~/git/dspg20rnd/dspg20RnD/data/original/working_federal_reporter_2020.csv")
View(raw_abstracts_2020)

abstracts_2019 <- raw_abstracts_2020 %>%
  filter(FY.x == 2019)

sum(is.na(raw_abstracts_2020$IC_CENTER)) #1156137

gg_miss_var(raw_abstracts_2020, show_pct = FALSE) +
  labs(y = "% Missing")
  #100% Missing: IC_CENTER, BUDGET_START_DATA,
    # BUDGET_END_DATE, FY_TOTAL_COST_SUB_PROJECTS

gg_miss_var(abstracts_2019)

raw_abstracts_2020 %>%
  select(-c(IC_CENTER, BUDGET_START_DATE, BUDGET_END_DATE, FY_TOTAL_COST_SUB_PROJECTS, OTHER_PIS)) %>% #Expect OTHER_PIS to have a high rate of NA values
  gg_miss_var(show_pct = TRUE) +
  labs(y = "% Missing")

#Convert to tidy format and remove stopwords

tidy_all <- tibble(text = raw_abstracts_2020$ABSTRACT)
tidy_2019 <- tibble(text = abstracts_2019$ABSTRACT)
tidy_titles <- tibble(text = raw_abstracts_2020$PROJECT_TITLE)

tidy_all <- tidy_all %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

tidy_2019 <- tidy_2019 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

tidy_titles <- tidy_titles %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

#Graphically display most prevalent words

tidy_2019 %>%
  filter(n > 250000) %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Words: 2019 Abstracts")

tidy_all %>%
  count(word, sort = TRUE) %>%
  filter(n > 500000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Words: All Abstracts")

tidy_titles %>%
  count(word, sort = TRUE) %>%
  filter(n > 10000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Titles: All Abstracts")

#Count abstracts by ___
count(raw_abstracts_2020, FY.x) %>%
  view()

raw_abstracts_2020 %>%
  filter(FY.y < 2020) %>%
  ggplot() +
  geom_bar(aes(x = FY.y))

count(raw_abstracts_2020, DEPARTMENT)
ggplot(raw_abstracts_2020) +
  geom_bar(aes(x = DEPARTMENT))

count(raw_abstracts_2020, AGENCY)
ggplot(raw_abstracts_2020) +
  geom_bar(aes(x = AGENCY)) +
  facet_wrap(~ FY.y)

ggplot(raw_abstracts_2020) +
  geom_histogram(aes(x = FY_TOTAL_COST), binwidth = 500000)

#Abstracts nchar

raw_abstracts_2020$ab_char <- nchar(raw_abstracts_2020$ABSTRACT)

raw_abstracts_2020 %>%
  #filter(ab_char < 10000) %>%
  ggplot(aes(x = ab_char)) +
  geom_histogram(bins = 50) +
  facet_wrap(~ DEPARTMENT)

raw_abstracts_2020 %>%
  #filter(ab_char < 150) %>%
  ggplot() +
  stat_summary(aes(x = DEPARTMENT, y = ab_char),
               fun.ymin = min,
               fun.ymax = max,
               fun.y = mean)

 #geom_bar(aes(x = DEPARTMENT, y = mean(ab_char)), group = 2)
##WORK IN PROGRESS


all <- count(raw_abstracts_2020, DEPARTMENT)

raw_abstracts_2020 %>%
  filter(ab_char < 150) %>%
  count(DEPARTMENT) %>%


raw_abstracts_2020 %>%
  count(DEPARTMENT)


raw_abstracts_2020 %>%
  ggplot(aes(as.Char(FY.x), FY_TOTAL_COST)) +
  geom_point()

count(raw_abstracts, FY)
ggplot(raw_abstracts) +
  geom_bar(aes(x = FY))

count(raw_abstracts, DEPARTMENT)
ggplot(raw_abstracts) +
  geom_bar(aes(x = DEPARTMENT))

count(raw_abstracts, AGENCY)
ggplot(raw_abstracts) +
  geom_bar(aes(x = AGENCY))

count(raw_abstracts, ORGANIZATION_NAME)
ggplot(raw_abstracts) +
  geom_bar(aes(x = ORGANIZATION_NAME))

#Reporting on the cost column

#Most frequent words by agency

#Something with the project terms(?)

#Abstract length (by agency, other variables)
raw_abstracts$characters <- nchar(raw_abstracts$ABSTRACT)

raw_abstracts %>% #This doesn't totally work
  filter(AGENCY == 'NIH') %>%
  ggplot(aes(characters)) +
  geom_histogram()
