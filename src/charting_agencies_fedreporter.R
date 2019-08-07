library(tidyverse)
library(treemap)
library(data.table)

fed_reporter <- fread("/home/af5ug/publicrd/data/original/2016-data_multiple_fed-reporter.csv")

treemap(fed_reporter, index = c("DEPARTMENT", "AGENCY"), vSize = "FY_TOTAL_COST", type = "index")

fed_reporter_summary <- fed_reporter %>% group_by(DEPARTMENT,AGENCY, IC_CENTER) %>% summarise(count = n(), fy_total = sum(FY_TOTAL_COST, na.rm = TRUE))

fed_reporter_summary[which(is.na(fed_reporter_summary$fy_total)),"fy_total"] <- 0

colnames(fed_reporter_summary) <- c("Department", "Agency", "Count", "FY16_Total_Cost")

f_r_s_dept <- fed_reporter_summary %>% group_by(DEPARTMENT) %>% summarise(count = sum(count), fy_total = sum(fy_total))

ggplot(fed_reporter_summary)+
  geom_point(aes(x = Count, y=FY16_Total_Cost, color = Department))+
  ggtitle("Grants and Spending by Agency in Federal RePORTER")

ggplot(fed_reporter_summary)+
  geom_point(aes(x = Count, y=FY16_Total_Cost, color = Department))+
  ggtitle("Grants and Spending by Agency in Federal RePORTER")+xlim(c(0,3000))

treemap(fed_reporter_summary, index = c("DEPARTMENT", "AGENCY"), vSize = "fy_total")
treemap(fed_reporter_summary, index = c("DEPARTMENT", "AGENCY"), vSize = "count")

ggplot(fed_reporter_summary)+geom_treemap(aes(area = fy_total, 
                      subgroup = DEPARTMENT, fill = DEPARTMENT))+geom_treemap_text(aes(area = fy_total, label = DEPARTMENT))

ggplot(fed_reporter_summary, aes(x = Agency)) + geom_col(aes(y=-(Count))+geom_col(aes(y=FY16_Total_Cost))+coord_flip()

ggplot(fed_reporter_summary)+
  geom_segment(aes(x=1, xend = 1, y=0, yend=90000), color = "gray", size = .25) + geom_segment(aes(x=4, xend = 4, y=0, yend=90000), color = "gray", size = .25)+
  geom_point(aes(x=1, y=Count, color = Department))+
  geom_point(aes(x=4, y=FY16_Total_Cost/10000, color = Department))+
  geom_segment(aes(x=1, xend=4, y=Count, yend = FY16_Total_Cost/10000, color = Department), size = .5)+
  scale_y_continuous(name = "Count",labels = NULL, breaks  = NULL, sec.axis = dup_axis(name = "FY16 Total"))+
  ggtitle("FY16 Research Grants by Agency, Fed RePORTER")+
  scale_x_continuous(labels = NULL, breaks = NULL, name = NULL) + 
  theme_minimal()

+ sec_axis(trans = "~ .",name = "FY16 Total")

