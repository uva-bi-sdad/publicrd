library(rvest)
library(maditr)
library(stringr)
library(janitor)
library(readxl)

CDMRP <- read_excel("data/original/Awards.xlsx") %>% 
  clean_names() %>%
  dt_filter(!is.na(x = research_topic))  %>%
  dt_mutate(research_topic_secondary = ifelse(test = is.na(x = award_number),
                                              yes = research_topic,
                                              no = NA)) %>%
  dt_mutate(research_topic_secondary = shift(x = research_topic_secondary,
                                             n = 1L,
                                             type = "lead") %>%
              str_extract(pattern = "(?<=^Secondary: ).*"),
            research_topic = str_extract(string = research_topic,
                                         pattern = "(?<=^Primary: ).*")) %>%
  setnames(old = "research_topic", new = "research_topic_primary") %>%
  dt_filter(!is.na(x = award_number))

write.csv(CDMRP, file = "CDMRP.csv",row.names=FALSE)
