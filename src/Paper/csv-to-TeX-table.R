# Convert excel tables to LaTeX for Emerging Topics paper

library(readr)
library(dplyr)
library(scales)
library(stringr)

options(scipen=999) # prevent scientific notation for slopes in df


#
# Table with labels and top words ---------------------------------------------------------
#


df <- read_csv("full_50topics.csv")

# create table with only labels and topic words

df1 <- df %>%
  select(Label, `Topic Words`) 

# change "_" to "\_" for outputting an actual underscore in TeX and not going into math mode
df1$`Topic Words` <- gsub("_", "\\\\_", df1$`Topic Words`)

write.table(df1, file = "full-50topics-table.txt", quote = FALSE, sep = " & ", eol = " \\\\ \\hline ", 
            row.names = FALSE)


#
# Trends results table -----------------------------------
#

df <- read_csv("pan_20topics.csv")

df$Slopex100 <- round(df$Slopex100, 4)
df$`p-value` <- round(df$`p-value`, 4)
df$n <- comma(df$n, accuracy = 1)

df$Slopex100 <- as.character(df$Slopex100)
mask = (substr(df$Slopex100,1,1) == "-")
df[mask, ]$Slopex100 <- str_pad(df[mask, ]$Slopex100, width = 7, side = "right", pad = "0")
df[!mask, ]$Slopex100 <- str_pad(df[!mask, ]$Slopex100, width = 6, side = "right", pad = "0")
# technically we should add the == 0 check, FR 22 rounds to 0. Manually added . in Overleaf

df$`p-value` <- as.character(df$`p-value`)
mask = (df$`p-value` == "0")
df[mask, ]$`p-value` <- "0.0000" 
df[!mask, ]$`p-value` <- str_pad(df[!mask, ]$`p-value`, width = 6, side = "right", pad = "0")


# change "_" to "\_" for outputting an actual underscore in TeX and not going into math mode
df$`Topic Words` <- gsub("_", "\\\\_", df$`Topic Words`)

write.table(df, file = "pan-20topics-trends.txt", quote = FALSE, sep = " & ", eol = " \\\\ \\hline ", 
            row.names = FALSE)



#
#  Pandemics Keywords -----------------------------------------
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
