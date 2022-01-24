# Convert excel tables to LaTeX for Emerging Topics paper

library(readr)
library(dplyr)

options(scipen=999) # prevent scientific notation for slopes in df

#
# Pandemics tables ----------------------------------------------------------
#

df <- read_csv("~/publicrd/src/Paper/05_case_studies/csvs/pan_05topics.csv")

# create table with only labels and topic words

df1 <- df %>%
  select(Label, `Topic Words`) 

# change "_" to "\_" for outputting an actual underscore in TeX and not going into math mode
df1$`Topic Words` <- gsub("_", "\\\\_", df1$`Topic Words`)

write.table(df1, file = "pan-topics-table.txt", quote = FALSE, sep = " & ", eol = " \\\\ \\hline ", 
            row.names = FALSE)


# create table with labels, top words, n, slope and p-value

df$Slopex100 <- round(df$Slopex100, 6)
df$`p-value` <- round(df$`p-value`, 6)

# change "_" to "\_" for outputting an actual underscore in TeX and not going into math mode
df$`Topic Words` <- gsub("_", "\\\\_", df$`Topic Words`)

write.table(df, file = "pan-05table.txt", quote = FALSE, sep = " & ", eol = " \\\\ \\hline ", 
            row.names = FALSE)




#
# Coronavirus table ----------------------------------------------------------
#

df <- read_csv("cor_topics.csv")

# create table with only labels and topic words

df1 <- df %>%
  select(Label, `Topic Words`) 

# change "_" to "\_" for outputting an actual underscore in TeX and not going into math mode
df1$`Topic Words` <- gsub("_", "\\\\_", df1$`Topic Words`)

write.table(df1, file = "cor-topics-table.txt", quote = FALSE, sep = " & ", eol = " \\\\ \\hline ", 
            row.names = FALSE)


# create table with labels, top words, slope and p-value

df$Slopex100 <- round(df$Slopex100, 6)
df$`p-value` <- round(df$`p-value`, 6)

# change "_" to "\_" for outputting an actual underscore in TeX and not going into math mode
df$`Topic Words` <- gsub("_", "\\\\_", df$`Topic Words`)

write.table(df, file = "cor-table.txt", quote = FALSE, sep = " & ", eol = " \\\\ \\hline ", 
            row.names = FALSE)



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

df <- read_csv("CSVs/nmf_100_results.csv")


# cut down list of words to top 5 (not 10)

df$`Top Five Terms` <- NA

for(i in 1:nrow(df))
{
  wds <- df$`topic_words`[i]
  wds <- gsub('\'', '', wds)
  wds <- gsub('\\[', '', wds)
  wds <- gsub(']', '', wds)
  wds <-strsplit(wds, ", ")
  
  top5 <- wds[[1]][1:5]
  top5_str <- paste(top5[1], top5[2], top5[3], top5[4], top5[5], sep = ", ")
  df$`Top Five Terms`[i] <- top5_str
}

df <- df %>%
  arrange(`Top Five Terms`)


# add topic labels

df$Label <- NA

for(i in 1:nrow(df))
{
  df$Label[i] <- paste0("X", i)
}


df <- df %>%
  select(Label, `Top Five Terms`) 

# change "_" to "\_" for outputting an actual underscore in TeX and not going into math mode
df$`Top Five Terms` <- gsub("_", "\\\\_", df$`Top Five Terms`)

write.table(df, file = "tables/nmf-100-topics-table.txt", quote = FALSE, sep = " & ", eol = " \\\\ \\hline ", 
            row.names = FALSE)



#
# Pandemics TM table ----------------------------------------------------------
#

df <- read_csv("TM_pan_topics.csv")

# create table with only labels and topic words

df1 <- df %>%
  select(`Topic Words`) 

# change "_" to "\_" for outputting an actual underscore in TeX and not going into math mode
df1$`Topic Words` <- gsub("_", "\\\\_", df1$`Topic Words`)

write.table(df1, file = "TM-pan-topics-table.txt", quote = FALSE, sep = " & ", eol = " \\\\ \\hline ", 
            row.names = FALSE)


#
# Coronavirus TM table ----------------------------------------------------------
#

df <- read_csv("TM_cor_topics.csv")

# create table with only labels and topic words

df1 <- df %>%
  select(`Topic Words`) 

# change "_" to "\_" for outputting an actual underscore in TeX and not going into math mode
df1$`Topic Words` <- gsub("_", "\\\\_", df1$`Topic Words`)

write.table(df1, file = "TM-cor-topics-table.txt", quote = FALSE, sep = " & ", eol = " \\\\ \\hline ", 
            row.names = FALSE)




#
#  Pandemics Keywords -------------------------
#

kw = c('1918_influenza_pandemic',
        '1918_pandemic',
        '1957_1968_pandemic',
        '2009_pandemic_h1n1',
        'aidspandemic',
        'andpandemic',
        'anotherpandemic',
        'apandemic',
        'assesspandemic',
        'betweenpandemic',
        'bothpandemic',
        'causedpandemic',
        'chikvpandemic',
        'cov_2pandemic',
        'covid19_pandemic',
        'covid19pandemic',
        'covid_19_pandemic',
        'covid_19pandemic',
        'covid_19pandemic5',
        'covid_pandemic',
        'covidpandemic',
        'criticalpandemic',
        'currentpandemic',
        'detetermrminineififththesuprragenomeofftthepandemiciccllonesiis',
        'devastatingpandemic',
        'ebolapandemic',
        'emergingpandemic',
        'escalatingpandemic',
        'establishingpandemic',
        'forpandemic',
        'frompandemic',
        'futurepandemic',
        'globalpandemic',
        'greatpandemic2',
        'growingpandemic',
        'h1n1_pandemic',
        'h1n1pandemic',
        'hivpandemic',
        'howpandemic',
        'humanpandemic',
        'increasedpandemic',
        'influenzapandemic',
        'inpandemic',
        'interpandemic',
        'interpandemic_pandemic',
        'majorpandemic',
        'multiclade_recombinant_pandemic',
        'newpandemic',
        'non_pandemic',
        'occasional_pandemic',
        'occasionalpandemic',
        'ofpandemic',
        'ofseasonal_pandemic',
        'ongoingpandemic',
        'pandemic',
        'pandemic1',
        'pandemic2',
        'pandemic2009',
        'pandemic57499',
        'pandemic_1918',
        'pandemic_1918_1919',
        'pandemic_1957',
        'pandemic_flu',
        'pandemic_h1n1',
        'pandemic_preparedness',
        'pandemic_sobering',
        'pandemically',
        'pandemicand',
        'pandemiccompare',
        'pandemiccovid_19',
        'pandemicdisease',
        'pandemicemergence',
        'pandemicflu',
        'pandemich3n2',
        'pandemicha',
        'pandemichas',
        'pandemichave',
        'pandemichuman',
        'pandemicin',
        'pandemicinfection',
        'pandemicinfluence',
        'pandemicinfluenza',
        'pandemicinvolve',
        'pandemiclike',
        'pandemicon',
        'pandemicpose',
        'pandemicpreparedness',
        'pandemicprogresse',
        'pandemicproportion',
        'pandemicremain',
        'pandemics',
        'pandemics18',
        'pandemicsabstractthe',
        'pandemicsetting',
        'pandemicsh1n1',
        'pandemicstrain',
        'pandemicthat',
        'pandemicthis',
        'pandemicthreat',
        'pandemicvaccine',
        'pandemicwill',
        'pandemicwith',
        'possiblepandemic',
        'prepandemic',
        'prepandemic_vaccination',
        'recurrentpandemic',
        'seasonal_pandemic',
        'severepandemic',
        'thecovid_19_pandemic',
        'thepandemic',
        'thispandemic',
        'threepandemic',
        'understandpandemic',
        'withpandemic',
        'worldwide_pandemic_1957')

# change "_" to "\_" for outputting an actual underscore in TeX and not going into math mode
kw <- gsub("_", "\\\\_", kw)

df <- data.frame(matrix(ncol = 5, nrow = 23))
colnames(df) <- c("col1", "col2", "col3", "col4", "col5")

df$col1 <- kw[1:23]
df$col2 <- kw[24:46]
df$col3 <- kw[47:69]
df$col4 <- kw[70:92]
df$col5 <- kw[93:115]

write.table(df, file = "pan-keywords.txt", quote = FALSE, sep = " & ", eol = " \\\\ ", 
            row.names = FALSE)
