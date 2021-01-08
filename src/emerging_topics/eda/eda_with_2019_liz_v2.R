
##This code is rough and a bit disorganized. I used this file to develop code for the Rmd file '02_liz_eda.Rmd' in the same GitHub repository. If you are looking to reuse this code, I recommend looking at that file.

library(tidyverse)
library(tidytext)
library(readr)
library(naniar)

raw_abstracts_2020 <- read.csv("~/git/dspg20rnd/dspg20RnD/data/original/working_federal_reporter_2020.csv")
View(raw_abstracts_2020)

abstracts_2019 <- raw_abstracts_2020 %>%
  filter(FY.x == 2019)

sum(is.na(raw_abstracts_2020$IC_CENTER)) #622428

gg_miss_var(raw_abstracts_2020, show_pct = TRUE, facet = FY.x) +
  labs(y = "% Missing")
  #100% Missing: IC_CENTER, BUDGET_START_DATA,
    # BUDGET_END_DATE, FY_TOTAL_COST_SUB_PROJECTS

pdf(file = "missing_by_year.pdf", width = 20, height = 15)
g <- gg_miss_var(raw_abstracts_2020, show_pct = TRUE, facet = FY.x) +
  labs(y = "% Missing by Year")
print(g)
dev.off()

gg_miss_var(abstracts_2019)

#raw_abstracts_2020 %>%
  #select(-c(IC_CENTER, BUDGET_START_DATE, BUDGET_END_DATE, FY_TOTAL_COST_SUB_PROJECTS, OTHER_PIS)) %>% #Expect OTHER_PIS to have a high rate of NA values
  #gg_miss_var(show_pct = TRUE) +
  #labs(y = "% Missing")

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
  filter(n > 30000) %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Words: 2019 Abstracts")

tidy_all %>%
  count(word, sort = TRUE) %>%
  filter(n > 200000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Words: All Abstracts")

tidy_titles %>%
  count(word, sort = TRUE) %>%
  filter(n > 20000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Titles: All Abstracts")

#Count abstracts

count(raw_abstracts_2020, FY.x) %>% #by year
  view()

raw_abstracts_2020 %>% #graph of year
  #filter(FY.y < 2020) %>%
  ggplot() +
  geom_bar(aes(x = FY.x))

count(raw_abstracts_2020, DEPARTMENT) #by department
ggplot(raw_abstracts_2020) +
  geom_bar(aes(x = DEPARTMENT)) +
  facet_wrap(~ FY.x)

count(raw_abstracts_2020, AGENCY) #by agency
ggplot(raw_abstracts_2020) +
  geom_bar(aes(x = AGENCY)) +
  facet_wrap(~ FY.x)

ggplot(raw_abstracts_2020) +
  geom_point(aes(x = FY.x, y = FY_TOTAL_COST))

#Abstracts nchar

raw_abstracts_2020$ab_char <- nchar(raw_abstracts_2020$ABSTRACT)

raw_abstracts_2020 %>%
  filter(ab_char < 150) %>%
  count(DEPARTMENT)



all_char <- raw_abstracts_2020 %>%
  count(DEPARTMENT)

low <- c(78, 0, 13, 3929, 169, 156, 158, 9)
sum(low)

all_char <- cbind(all_char, low)

rel <- (low/all_char$n) * 100
all_char <- cbind(all_char, rel)

ggplot(all_char, aes(x = DEPARTMENT, y = rel, colour = DEPARTMENT)) +
  geom_bar(stat = "identity")

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

#Reporting on the cost column
sum(is.na(raw_abstracts_2020$FY_TOTAL_COST)) #287698

raw_abstracts_2020 %>%
  filter(FY_TOTAL_COST < 200000000) %>%
  ggplot(aes(x = FY.x, y = FY_TOTAL_COST, colour = DEPARTMENT)) +
  geom_point()

raw_abstracts_2020 %>%
  filter(FY_TOTAL_COST < 10000) %>%
  count()

raw_abstracts_2020 %>%
  count(DEPARTMENT) %>%
  ggplot(aes(x = n, y = raw_abstracts_2020$FY_TOTAL_COST)) +
  geom_point()


#Most frequent words by agency

#Something with the project terms(?)

#Abstract length (by agency, other variables)
raw_abstracts_2020$characters <- nchar(raw_abstracts_2020$ABSTRACT)

raw_abstracts %>% #This doesn't totally work
  filter(AGENCY == 'NIH') %>%
  ggplot(aes(characters)) +
  geom_histogram()


pdf(file = "missing_dep.pdf", width = 10, height = 15)
g <- gg_miss_var(show_pct = TRUE, facet = DEPARTMENT) +
  labs(y = "% Missing by Department")
print(g)
dev.off()

library(reshape2)

tidy_joined <- full_join(tidy_all, tidy_titles, by = "word")

tidy_joined %>%
  melt() %>%
  ggplot(aes(word, value, fill = variable)) +
  geom_bar(stat = "identity", width = .5, position = "dodge", show.legend = FALSE) +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Titles: All Abstracts")

abstracts <- raw_abstracts_2020 %>%
  select(DEPARTMENT, AGENCY, ORGANIZATION_NAME, ABSTRACT)

tidy_abstracts <- tibble(dept = raw_abstracts_2020$DEPARTMENT, agen = raw_abstracts_2020$AGENCY, org = raw_abstracts_2020$ORGANIZATION_NAME, ic = raw_abstracts_2020$IC_CENTER, text = raw_abstracts_2020$ABSTRACT)

tidy_abstracts <- tidy_abstracts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(agen, word, sort = TRUE)

tidy_abstracts %>%
  filter(word %in% c("pandemic", "virus", "influenza", "1918", "spanish", "zika", "hiv", "aids", "vaccine", "flu", "pandemics", "mers", "sars", "disease", "diseases", "plauge", "typhus", "communicable", "contagious", "epidemiology", "contagion", "symptomatic", "asymptomatic", "contactless", "edpidemic", "quarantine", "immunity", "incubation", "transmission", "patient", "patients", "covid")) %>%
  filter(n > 500) %>%
  filter(ic != "NA") %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = ic)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle(label = "Abstracts:", subtitle = "Pandemic Terms")
