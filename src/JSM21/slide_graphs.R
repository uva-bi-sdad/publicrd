# Create graphs for JSM slides

library(ggplot2)

# Pandemics Corpus

df <- data.frame(year = c(2010:2019),
                 n = c(274, 286, 237, 156, 196, 192, 199, 176, 169, 161))
df$year = as.factor(df$year)

ggplot(data=df, aes(x=year, y=n)) +
  geom_bar(stat="identity", fill = 'navy') +
  theme_minimal() +
  labs(title = "Number of Projects By Start Year",
       x = "Project Start Year",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 16))

  

# AI Corpus

df <- data.frame(year = c(2010:2018),
                 n = c(91,  82, 109,  84,  95, 119, 160, 216, 345))
df$year = as.factor(df$year)

ggplot(data=df, aes(x=year, y=n)) +
  geom_bar(stat="identity", fill = 'navy') +
  theme_minimal() +
  labs(title = "Number of Projects By Start Year",
       x = "Project Start Year",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 16))
