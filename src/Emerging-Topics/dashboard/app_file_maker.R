#Create files for import into shiny app

final_abstracts <- read.csv("~/git/dspg20rnd/dspg20RnD/data/final/dashboard_data/app_data.csv")

library(tidyverse)
library(tidytext)

#tidied abstracts

#tidy_words <- tibble(text = final_abstracts$ABSTRACT)

#tidy_words <- tidy_words %>%
 #unnest_tokens(word, text) %>%
  #anti_join(stop_words) %>%
  #count(word, sort = TRUE)

#saveRDS(tidy_words, "~/git/dspg20rnd/dspg20RnD/src/r_shiny_app_v2/data/tidy_words.rds")

#Abstracts tidied by department
tidy_abstracts <- tibble(dept = final_abstracts$DEPARTMENT, text = final_abstracts$ABSTRACT)

tidy_abstracts <- tidy_abstracts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(dept, word, sort = TRUE)

total_abstracts <- tidy_abstracts %>%
  group_by(dept) %>%
  summarize(total = sum(n))

tidy_abstracts <- left_join(tidy_abstracts, total_abstracts)

tidy_abstracts <- tidy_abstracts %>%
  bind_tf_idf(word, dept, n)

tidy_abstracts <- tidy_abstracts %>%
  select(dept, word, n, tf_idf)

saveRDS(tidy_abstracts, "~/git/dspg20rnd/dspg20RnD/src/r_shiny_app_v2/data/tidy_abstracts_dept.rds")

#Abstracts tidied by year
library(stringr)

final_abstracts$Year <- final_abstracts$PROJECT_START_DATE %>%
  str_sub(-4, -1)

tidy_year <- tibble(year = final_abstracts$Year, text = final_abstracts$ABSTRACT)

tidy_year <- tidy_year %>%
  filter(year > 2009) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(year, word, sort = TRUE)

tidy_year$year <- as.numeric(tidy_year$year)

saveRDS(tidy_year, "~/git/dspg20rnd/dspg20RnD/src/r_shiny_app_v2/data/tidy_year.rds")

final_abstracts$ID <- c(1:690814)
tidy_year_ab <- tibble(year = final_abstracts$Year, id = final_abstracts$ID, text = final_abstracts$ABSTRACT)
id_year <- tibble(year = final_abstracts$Year, id = final_abstracts$ID)

tidy_year_ab <- tidy_year_ab %>%
  filter(year > 2009) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, id, sort = TRUE)

tidy_year_ab <- right_join(tidy_year_ab, id_year, by = "id")

tidy_year_ab_count <- tidy_year_ab %>%
  select(word, year) %>%
  count(year, word, sort = TRUE)

tidy_year_ab_count$year <- as.numeric(tidy_year_ab_count$year)

saveRDS(tidy_year_ab_count, "~/git/dspg20rnd/dspg20RnD/src/r_shiny_app_v2/data/tidy_year_abst.rds")

#All topics
topics <- read_csv("~/git/dspg20rnd/dspg20RnD/src/r_shiny_app_v2/data/clean_no_mu6_SeventyFive_FullDF.csv")
topics <- topics %>%
  filter(START_YEAR > 2009) %>%
  filter(START_YEAR < 2020)

saveRDS(topics, "~/git/dspg20rnd/dspg20RnD/src/r_shiny_app_v2/data/seventyfivetopicsdf.rds")

all_topics <- topics %>%
  group_by(Topic, `Top 10 Words`) %>%
  summarise(`Top 10 Words` = paste0(unique(`Top 10 Words`), collapse = " "))

saveRDS(all_topics, "~/git/dspg20rnd/dspg20RnD/src/r_shiny_app_v2/data/all_topics.rds")

#Coronavirus topics
corona <- read_csv("~/git/dspg20rnd/dspg20RnD/src/r_shiny_app_v2/data/clean_no_mu_Thirty_Corona_edit.csv")
corona <- corona %>%
  filter(START_YEAR > 2009) %>%
  filter(START_YEAR < 2020)

corona$Topic_Legend[291:300] = "influenza, phage_display_library,<br>surface_plasmon_resonance, task, vaccinate"

saveRDS(corona, "~/git/dspg20rnd/dspg20RnD/src/r_shiny_app_v2/data/thirtycoronatopics.rds")

corona_topic <- corona %>%
  group_by(Topic, `Top 10 Words`) %>%
  summarise(`Top 10 Words` = paste0(unique(`Top 10 Words`), collapse = ""))

saveRDS(corona_topic, "~/git/dspg20rnd/dspg20RnD/src/r_shiny_app_v2/data/corona_topic.rds")

#Pandemics topics

pandemic <- read_csv("~/git/dspg20rnd/dspg20RnD/src/r_shiny_app_v2/data/clean_no_mu6_Thirty_Pandemic_Topics.csv")
pandemic <- pandemic %>%
  filter(START_YEAR > 2009) %>%
  filter(START_YEAR < 2020)

saveRDS(pandemic, "~/git/dspg20rnd/dspg20RnD/src/r_shiny_app_v2/data/thirtypandemictopics.rds")

pandemic_topic <- pandemic %>%
  group_by(Topic, `Top 10 Words`) %>%
  summarise(`Top 10 Words` = paste0(unique(`Top 10 Words`), collapse = " "))

saveRDS(pandemic_topic, "~/git/dspg20rnd/dspg20RnD/src/r_shiny_app_v2/data/pandemic_topic.rds")
