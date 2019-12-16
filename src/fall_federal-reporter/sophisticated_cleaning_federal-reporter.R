###First attempt to extract full sentences from the data set:

library(R.utils)
library(tidyverse)
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(corpus)

abstracts_federal_reporter_sentence <- read_csv("data/working/federal_reporter/abstracts_federal_reporter_combined.csv")

##First Sentence Separation

#Extract first sentences with corpus package
sentences <-text_split(abstracts_federal_reporter_sentence$ABSTRACT, "sentences")
first_sentences <- sentences[sentences$index == 1,]
row.names(first_sentences) <- NULL

#Fix NA abstract in row 9
new_row <- data.frame(9,1,NA)
colnames(new_row) <- c("parent", "index", "text")
first_sentences <- rbind(first_sentences[1:8,], new_row, first_sentences[9:550087,])
row.names(first_sentences) <- NULL
#write.csv(first_sentences, file = "data/working/federal_reporter/first_sentences.csv", row.names = FALSE)


#Merge into main dataset
first_sentences_fix <- read_csv("data/working/federal_reporter/first_sentences.csv")
abstracts_federal_reporter_sentence$parent <- seq.int(nrow(first_sentences_fix))
abstracts_federal_reporter <- left_join(abstracts_federal_reporter_sentence, first_sentences_fix, by = "parent") %>%
  select(-index, -parent)
colnames(abstracts_federal_reporter)[17] <- "FIRST_SENTENCE"

