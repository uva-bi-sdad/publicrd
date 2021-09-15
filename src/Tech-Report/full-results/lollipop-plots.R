# Plotting full corpus topic model results - Topic Appearance and Predominant Topic

library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(latex2exp)

df <- read_csv("full_nmf_results.csv")

# add in topic ranking by percent docs containing topic, largest percent = 1
# not using this anymore - labeling by FR1, FR2, etc. 

#r1 <- order(df$percent_docs_containing_topic, decreasing = TRUE)
#df$rank_docs_contain_topic <- NA
#df$rank_docs_contain_topic[r1] <- 1:75 

#
# extract words from topic_words column and create actual list rather than a string -------------
#

topic_wds_10 <- list()
topic_wds_5 <- list()
topic_wds_3 <- list()

for(i in 1:nrow(df))
{
  wds <- df$topic_words[i]
  wds <- str_replace_all(wds, "[[:punct:]]", "")
  wds <-strsplit(wds, " ")
  
  top5 <- wds[[1]][1:5]
  top5_str <- paste(top5[1], top5[2], top5[3], top5[4], top5[5], sep = ", ")
  topic_wds_5[i] <- top5_str 
  
  top3 <- wds[[1]][1:3]
  top3_str <- paste(top3[1], top3[2], top3[3], sep = ", ")
  topic_wds_3[i] <- top3_str
  
  top10 <- wds[[1]][1:10]
  top10_str <- paste(top10[1], top10[2], top10[3], top10[4], top10[5], 
                     top10[6], top10[7], top10[8], top10[9], top10[10], 
                     sep = ", ")
  topic_wds_10[i] <- top10_str 
}


df$top_10_words <- as.character(topic_wds_10)
df$top_5_words <- as.character(topic_wds_5)
df$top_3_words <- as.character(topic_wds_3)


#
# Add labels to topics -- labeled by ABC order ----------------------------------------------
#

df <- df %>%
  arrange(top_10_words)

df$topic_label <- ""

for(i in 1:75)
{
  df$topic_label[i] <- paste0("FR", i)
}


#
# Percentage of Docs per Topic ----------------------------------
#

plot_df <- df %>% select(top_5_words, percent_docs_containing_topic, topic_label)
plot_df <- plot_df[order(plot_df$percent_docs_containing_topic, decreasing=FALSE),]

p_df <- head(plot_df,10)
p_df <- rbind(p_df, tail(plot_df,10))
p_df$group <- c(rep("least",10), rep("most", 10))

#for(i in 1:20)
#{
#  p_df$top_5_words[i] <- paste0("Topic ", 21-i, ": ", p_df$top_5_words[i])  
#}
p_df$top_5_words <- factor(p_df$top_5_words, levels = unique(p_df$top_5_words))


# Lollipop plot - percentage of docs per topic

text_col <- ifelse(p_df$group == "most", "black", "black")

ggplot(data=p_df, aes(x=percent_docs_containing_topic, y=top_5_words)) +
  geom_segment(aes(x=0, y=top_5_words , xend=percent_docs_containing_topic, yend=top_5_words, color=group), size=2) +
  geom_point(aes(color=group), size=4) + #color="blue", size=1, alpha=1) +
  geom_text(aes(x = -2.2, label=topic_label)) + # size=4) +
  scale_color_manual(values=c("navy", "orange")) +
  #xlim(-3,52) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 13, color = text_col), #margin = margin(r = -2)), #hjust=0), 
        axis.text.x = element_text(size = 13, color = "black"),
        axis.title.x = element_text(size = 13, color = "black"),
        plot.title = element_text(size = 16, color = "black", hjust = -0.25)) +
  theme(legend.position = "none") +
  ggtitle("Topic Appearance in Abstracts") +
  ylab("") + #("Topic Words") +
  xlab("Percentage of Abstracts")

# saved as 1250 x 600 png

#
# Dominant Topic Percentage --------------------------------
#


plot_df <- df %>% select(top_5_words, percent_times_max_topic, topic_label)
plot_df <- plot_df[order(plot_df$percent_times_max_topic, decreasing=FALSE),]

p_df <- head(plot_df,10)
p_df <- rbind(p_df, tail(plot_df,10))
p_df$group <- c(rep("least",10), rep("most", 10))

#for(i in 1:20)
#{
#  p_df$top_5_words[i] <- paste0("Topic ", 21-i, ": ", p_df$top_5_words[i])  
#}
p_df$top_5_words <- factor(p_df$top_5_words, levels = unique(p_df$top_5_words))


# Lollipop plot - Discrete Ranking

text_col <- ifelse(p_df$group == "most", "black", "black")

ggplot(data=p_df, aes(x=percent_times_max_topic, y=top_5_words)) +
  geom_segment(aes(x=0, y=top_5_words , xend=percent_times_max_topic, yend=top_5_words, color=group), size=2) +
  geom_point(aes(color=group), size=4) + #color="blue", size=1, alpha=1) 
  geom_text(aes(x = -0.2, label=topic_label)) +
  scale_color_manual(values=c("navy", "orange")) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 13, color = text_col), 
        axis.text.x = element_text(size = 13, color = "black"),
        axis.title.x = element_text(size = 13, color = "black"),
        plot.title = element_text(size = 16, color = "black", hjust = -0.15)) +
  theme(legend.position = "none") +
  ggtitle("Predominant Topic in Abstracts") +
  ylab("") +
  xlab("Percentage of Abstracts")

#ggsave("figures/predominant_topics.png", width = 30, height = 15, units = "cm", scale = 1, dpi = 800)

# saved as 1250 x 600 png


# # plotting all data
# ggplot(data=df, aes(x=percent_times_max_topic, y=top_5_words)) +
#   geom_segment(aes(x=0, y=top_5_words , xend=percent_times_max_topic, yend=top_5_words)) + #color=group), size=2) +
#   geom_point() + #aes(color=group), size=4) + #color="blue", size=1, alpha=1) 
#   geom_text(aes(x = -0.1, label=rank_docs_contain_topic), size=2) +
#   #scale_color_manual(values=c("navy", "orange")) +
#   theme_minimal() +
#   theme(axis.text.y = element_text(size = 6, color = text_col), 
#         axis.text.x = element_text(size = 13, color = "black"),
#         axis.title.x = element_text(size = 13, color = "black"),
#         plot.title = element_text(size = 13, color = "black", hjust = -0.15)) +
#   theme(legend.position = "none") +
#   ggtitle("Predominant Topic in Abstracts") +
#   ylab("") +
#   xlab("Percentage of Abstracts")


# 
# Coherence by topic --------------------------------------
#
# 
# 
# pal <- c("#232d4b", "#2c4f6b", "#0e879c", "#60999a", "#d1e0bf", "#d9e12b", "#e6ce3a", "#e6a01d", "#e57200", "#fdfdfd")
# 
# 
# plot_nmf <- df %>% select(top_5_words, coherence_mean)
# plot_nmf <- plot_nmf[order(plot_nmf$coherence_mean, decreasing=FALSE),]
# plot_nmf$top_5_words <- factor(plot_nmf$top_5_words, levels = unique(plot_nmf$top_5_words))
# 
# p_df <- head(plot_nmf,10)
# p_df <- rbind(p_df, tail(plot_nmf,10))
# p_df
# 
# # p_df%>%
# #       #arrange(desc(coherence_mean)) %>%
# #       #top_n(-10) %>%
# #       ggplot(aes(top_5_words, coherence_mean)) +
# #       geom_col(show.legend = TRUE, fill = pal[3]) +
# #       labs(x = "Topic Words", y = TeX("$C_V$ Topic Coherence"), title = TeX("Most and Least Coherent Topics")) +
# #       coord_flip() +
# #       theme_bw()
# 
# ggplot(data=p_df, aes(x=coherence_mean, y=top_5_words)) +
#   geom_segment(aes(x=0, y=top_5_words , xend=coherence_mean, yend=top_5_words), color=pal[3], size=2) +
#   geom_point(color = pal[3], size=4) + #color="blue", size=1, alpha=1) 
#   theme_minimal() +
#   theme(axis.text.y = element_text(size = 10)) +
#   labs(y = "Topic Words", x = TeX("$C_V$ Topic Coherence"), title = TeX("Most and Least Coherent   Topics"))




