library(data.table)
library(tidyverse)
library(ggmosaic)
library(ggbeeswarm)

###fetching finished summaries

loc_nsf <- "data/working/combine_by_department/NSF"
loc_nih <- "data/working/combine_by_department/NIH"
loc_nasa <- "data/working/combine_by_department/NASA"

nsf_summary <- fread(file.path(loc_nsf, "nsf_summary.csv"))
nih_summary <- fread(file.path(loc_nih, "nih_summary.csv"))
nasa_summary <- fread(file.path(loc_nasa, "nasa_summary.csv"))

summary <- rbind(nasa_summary, nih_summary, nsf_summary)

summary$non_res_obligation_total <- summary$obligation_total - summary$res_obligation_total


##total amounts vs total amounts that we can say are research
total_amount <- melt(summary, id.vars = c("agency", "type"), measure.vars = c("obligation_total", "res_obligation_total", "non_res_obligation_total"))
res_non_res <- filter(total_amount, variable %in% c("res_obligation_total", "non_res_obligation_total"))
res_non_res$variable <- as.character(res_non_res$variable)
res_non_res[res_non_res == "res_obligation_total"] <- "Research Total"
res_non_res[res_non_res == "non_res_obligation_total"] <- "Non-Research Total"
colnames(res_non_res) <- c("agency","c_or_g","type","value")

res_non_res$variable <- as.character(res_non_res$variable)
ggplot(res_non_res)+geom_bar(aes(x=agency, y = value, fill = c_or_g), stat = "sum")
ggplot(res_non_res)+geom_bar(aes(x=agency, y = value, fill = type), stat = "sum") + labs(y = "Total Obligations", x = "Agency",
       title = "FY16 Spending by Agency and Research") + theme_classic()


#as parts of 100%
ggplot(filter(total_amount, variable %in% c("res_obligation_total", "non_res_obligation_total")))+geom_bar(aes(x=agency, y = value, fill = variable), stat = "sum", position = "fill")

#mosaic plots?
ggplot(res_non_res) + geom_mosaic(aes(x = product(type, c_or_g), weight = value, fill = type)) + 
  facet_grid(cols = vars(agency)) + labs(title = "FY16 Spending in Three Science Agencies", x = "Award Type", y = "Spending Type")
ggplot(filter(res_non_res, agency == "NASA")) + geom_mosaic(aes(x = product(type, c_or_g), weight = value, fill = type))+
  labs(title = "NASA Spending in FY16", y = element_blank(), x = element_blank())
ggplot(filter(res_non_res, agency == "NIH")) + geom_mosaic(aes(x = product(type, c_or_g), weight = value, fill = type))+
  labs(title = "NIH Spending in FY16", y = element_blank(), x = element_blank())
ggplot(filter(res_non_res, agency == "NSF")) + geom_mosaic(aes(x = product(type, c_or_g), weight = value, fill = type))+
  labs(title = "NSF Spending in FY16", y = element_blank(), x = element_blank())


nasa_tot <- 1167286100+287455200
sum(nasa_summary$res_obligation_total)/nasa_tot

nasa_grant <- nasa_grant[,3:76]
ggplot(nasa_grant)+geom_beeswarm(aes(x=1, y=1, size = federal_action_obligation))

nasa_grant_short <- select(nasa_grant, award_id_fain, federal_action_obligation, resflag)
nasa_grant_short$fundtype <- "grant"
colnames(nasa_grant_short) <- c("id", "amount", "research", "fundtype")
nasa_contract_short <- select(nasa_contract, award_id_piid, federal_action_obligation, resflag)
nasa_contract_short$fundtype <- "contract"
colnames(nasa_contract_short) <- c("id", "amount", "research", "fundtype")
nasa_together_short <- rbind(nasa_grant_short, nasa_contract_short)
nasa_together_short$agency <- "NASA"
write.csv(nasa_together_short, "nasabees.csv")
nasa_together_short$combine <- paste(nasa_together_short$research,nasa_together_short$fundtype, sep = " ")

ggplot(nasa_together_short, aes(y = combine, x = amount))+geom_density_ridges(scale = .8)+scale_x_continuous(limits = c(0,1500000))
