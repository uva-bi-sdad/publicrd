#LDA First Attempt:

library(tm)
library(RTextTools)
library(topicmodels)

abstracts_federal_reporter_combined <- read_csv("data/working/federal_reporter/abstracts_federal_reporter_combined.csv")
test_data <- head(abstracts_federal_reporter_combined,100) %>%
  select(ABSTRACT) %>% drop_na()
test_data$ABSTRACT <- str_replace_all(test_data$ABSTRACT," will "," ")

test_data_true <- create_matrix(as.vector(test_data$ABSTRACT), language="english", removeNumbers=TRUE, stemWords=TRUE, weighting=weightTf)
lda_model <- LDA(test_data_true, 5,  method = "VEM", control = NULL, model = NULL)
terms(lda_model)
