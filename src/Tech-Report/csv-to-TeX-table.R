# Convert excel tables to LaTeX for Emerging Topics paper

library(readr)
library(dplyr)

options(scipen=999) # prevent scientific notation for slopes in df

#
# Pandemics table ----------------------------------------------------------
#

df <- read_csv("pan_topics.csv")

df <- df %>%
  arrange(desc(Slope))

df$Slope <- round(df$Slope, 6)

# change "_" to "\_" for outputting an actual underscore in TeX and not going into math mode
df$`Topic Words` <- gsub("_", "\\\\_", df$`Topic Words`)

write.table(df, file = "pan-topics-table.txt", quote = FALSE, sep = " & ", eol = " \\\\ \\hline ", 
            row.names = TRUE)


#
# Coronavirus table ----------------------------------------------------------
#

df <- read_csv("cor_topics.csv")

df <- df %>%
  arrange(desc(Slope))

df$Slope <- round(df$Slope, 6)

# change "_" to "\_" for outputting an actual underscore in TeX and not going into math mode
df$`Topic Words` <- gsub("_", "\\\\_", df$`Topic Words`)

write.table(df, file = "cor-topics-table.txt", quote = FALSE, sep = " & ", eol = " \\\\ \\hline ", 
            row.names = TRUE)



#
# Entire Corpus table ---------------------------------------------------------
#

df <- read_csv("full_topics.csv")

df <- df %>%
  arrange(`Topic Words`)

df$Slope <- round(df$Slope, 6)

# change "_" to "\_" for outputting an actual underscore in TeX and not going into math mode
df$`Topic Words` <- gsub("_", "\\\\_", df$`Topic Words`)

write.table(df, file = "full-topics-table.txt", quote = FALSE, sep = " & ", eol = " \\\\ \\hline ", 
            row.names = FALSE)



#
# Table of NMF, 75 results - coherence, representation, dominance ----------------------------
#

# publicrd/src/emerging_topics/emerging_topics/topic_model_tuning/results
df <- readRDS("nmf_results.rds")

df <- df %>%
  select(coherence_mean, percent_docs_containing_topic, percent_times_max_topic, top_10_words) %>%
  rename(Coherence = coherence_mean,
         `Percent Representation` = percent_docs_containing_topic,
         `Percent Dominant Topic` = percent_times_max_topic,
         `Topic Words` = top_10_words) %>%
  arrange(`Topic Words`)

df$Coherence <- round(df$Coherence, 2)
df$`Percent Representation` <- round(df$`Percent Representation`, 2)
df$`Percent Dominant Topic` <- round(df$`Percent Dominant Topic`, 2)

# change "_" to "\_" for outputting an actual underscore in TeX and not going into math mode
df$`Topic Words` <- gsub("_", "\\\\_", df$`Topic Words`)

write.table(df, file = "NMF-topics-results.txt", quote = FALSE, sep = " & ", eol = " \\\\ \\hline ", 
            row.names = FALSE)



#
# Table of NMF topic results - lists topic number and top 5 words --------------------------------------
#

df <- read_csv("CSVs/full_topics_df.csv")


# cut down list of words to top 5 (not 10)

df$`Top Five Terms` <- NA

for(i in 1:nrow(df))
{
  wds <- df$`Topic Words`[i]
  wds <-strsplit(wds, ", ")
  
  top5 <- wds[[1]][1:5]
  top5_str <- paste(top5[1], top5[2], top5[3], top5[4], top5[5], sep = ", ")
  df$`Top Five Terms`[i] <- top5_str
}

df <- df %>%
  arrange(`Top Five Terms`)


# add topic labels

df$Label <- NA

for(i in 1:75)
{
  df$Label[i] <- paste0("FR", i)
}


df <- df %>%
  select(Label, `Top Five Terms`) 

# change "_" to "\_" for outputting an actual underscore in TeX and not going into math mode
df$`Top Five Terms` <- gsub("_", "\\\\_", df$`Top Five Terms`)

write.table(df, file = "nmf-75-topics-table.txt", quote = FALSE, sep = " & ", eol = " \\\\ \\hline ", 
            row.names = FALSE)




