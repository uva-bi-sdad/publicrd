##Examine starts and ends of entries:
#If the first character is not a capital letter then the abstract starts with
#something other than a sentence (extraneous elements should be removed) 

abstracts_federal_reporter_cleaned <- read_csv("data/working/federal_reporter/abstracts_federal_reporter_combined.csv")


#Problem phrases:

#Abstract:
#ABSTRACT
#PROJECT SUMMARY:
#PROJECT SUMMARY/ABSTRACT
#****NON-TECHNICAL ABSTRACT****
#PROJECT SUMMARY / ABSTRACT

##Create Functon "flagged_phrase":
#flagged_phrase marks if an entry starts with an unnecessary tag by checking the first 100 characters for a desired phrase. 
#Then just enter in the character string and mutate columns to replace

flagged_phrase <- function(phrase){
  abstracts_federal_reporter_cleaned$clean <- str_detect(abstracts_federal_reporter_cleaned$FIRST_CHAR, paste0("^",phrase))
  return(abstracts_federal_reporter_cleaned$clean)
}

#Cleaned New
#clean_phrase <- function(phrase){
#  abstracts_federal_reporter_clean$cleaned <- ifelse(abstracts_federal_reporter_cleaned$clean == TRUE, 
#                                                        str_replace(abstracts_federal_reporter_cleaned$ABSTRACT, paste0("^",phrase),""), 
#                                                        abstracts_federal_reporter_cleaned$ABSTRACT)
#  return(abstracts_federal_reporter_combined$cleaned)
#}

#abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_combined %>% 
#  mutate(flagged_phrase("ABSTRACT:")) %>%
#  rename('flagged_phrase("ABSTRACT:")' = cleanup)
#colnames(abstracts_federal_reporter_cleaned)[8] <- "cleanup"
#abstracts_federal_reporter_cleaned$ABSTRACT <- abstracts_federal_reporter_cleaned %>%
#  mutate(clean_phrase("ABSTRACT:"))

#"ABSTRACT:"

abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>% 
  mutate(flagged_phrase("ABSTRACT:"))
colnames(abstracts_federal_reporter_cleaned)[8] <- "cleanup"
abstracts_federal_reporter_cleaned$ABSTRACT <- ifelse(abstracts_federal_reporter_cleaned$cleanup == TRUE, 
                                                      str_replace(abstracts_federal_reporter_cleaned$ABSTRACT, "^ABSTRACT:",""), 
                                                      abstracts_federal_reporter_cleaned$ABSTRACT)
abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>%
  select(-cleanup)

#"PROJECT SUMMARY:"

abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>% 
  mutate(flagged_phrase("PROJECT SUMMARY:"))
colnames(abstracts_federal_reporter_cleaned)[8] <- "cleanup"
abstracts_federal_reporter_cleaned$ABSTRACT <- ifelse(abstracts_federal_reporter_cleaned$cleanup == TRUE, 
                                                      str_replace(abstracts_federal_reporter_cleaned$ABSTRACT, "^PROJECT SUMMARY:",""), 
                                                      abstracts_federal_reporter_cleaned$ABSTRACT)
abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>%
  select(-cleanup)

#"PROJECT SUMMARY/ABSTRACT"

abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>% 
  mutate(flagged_phrase("PROJECT SUMMARY/ABSTRACT"))
colnames(abstracts_federal_reporter_cleaned)[8] <- "cleanup"
abstracts_federal_reporter_cleaned$ABSTRACT <- ifelse(abstracts_federal_reporter_cleaned$cleanup == TRUE, 
                                                      str_replace(abstracts_federal_reporter_cleaned$ABSTRACT, "^PROJECT SUMMARY/ABSTRACT",""), 
                                                      abstracts_federal_reporter_cleaned$ABSTRACT)
abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>%
  select(-cleanup)

#"****NON-TECHNICAL ABSTRACT****"

abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>% 
  mutate(flagged_phrase("\\*\\*\\*\\*NON-TECHNICAL ABSTRACT\\*\\*\\*\\*"))
colnames(abstracts_federal_reporter_cleaned)[8] <- "cleanup"
abstracts_federal_reporter_cleaned$ABSTRACT <- ifelse(abstracts_federal_reporter_cleaned$cleanup == TRUE, 
                                                      str_replace(abstracts_federal_reporter_cleaned$ABSTRACT, "^\\*\\*\\*\\*NON-TECHNICAL ABSTRACT\\*\\*\\*\\*",""), 
                                                      abstracts_federal_reporter_cleaned$ABSTRACT)
abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>%
  select(-cleanup)

#"PROJECT SUMMARY / ABSTRACT"

abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>% 
  mutate(flagged_phrase("PROJECT SUMMARY / ABSTRACT"))
colnames(abstracts_federal_reporter_cleaned)[8] <- "cleanup"
abstracts_federal_reporter_cleaned$ABSTRACT <- ifelse(abstracts_federal_reporter_cleaned$cleanup == TRUE, 
                                                      str_replace(abstracts_federal_reporter_cleaned$ABSTRACT, "^PROJECT SUMMARY / ABSTRACT",""), 
                                                      abstracts_federal_reporter_cleaned$ABSTRACT)
abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>%
  select(-cleanup)

#"Objective:"

abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>% 
  mutate(flagged_phrase("Objective:"))
colnames(abstracts_federal_reporter_cleaned)[8] <- "cleanup"
abstracts_federal_reporter_cleaned$ABSTRACT <- ifelse(abstracts_federal_reporter_cleaned$cleanup == TRUE, 
                                                      str_replace(abstracts_federal_reporter_cleaned$ABSTRACT, "^Objective:",""), 
                                                      abstracts_federal_reporter_cleaned$ABSTRACT)
abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>%
  select(-cleanup)

#7-digit number leading up to "This"

abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>% 
  mutate(flagged_phrase("^[0-9]{7}.+This"))
colnames(abstracts_federal_reporter_cleaned)[8] <- "cleanup"
abstracts_federal_reporter_cleaned$ABSTRACT <- ifelse(abstracts_federal_reporter_cleaned$cleanup == TRUE, 
                                                      str_replace(abstracts_federal_reporter_cleaned$ABSTRACT, "^[0-9]{7}.+This","This"), 
                                                      abstracts_federal_reporter_cleaned$ABSTRACT)
abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>%
  select(-cleanup)

#"DESCRIPTION (provided by applicant):"

abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>% 
  mutate(flagged_phrase("^DESCRIPTION (provided by applicant):"))
colnames(abstracts_federal_reporter_cleaned)[8] <- "cleanup"
abstracts_federal_reporter_cleaned$ABSTRACT <- ifelse(abstracts_federal_reporter_cleaned$cleanup == TRUE, 
                                                      str_replace(abstracts_federal_reporter_cleaned$ABSTRACT, "^DESCRIPTION (provided by applicant):",""), 
                                                      abstracts_federal_reporter_cleaned$ABSTRACT)
abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>%
  select(-cleanup)

#"Abstract"

abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>% 
  mutate(flagged_phrase("^Abstract"))
colnames(abstracts_federal_reporter_cleaned)[8] <- "cleanup"
abstracts_federal_reporter_cleaned$ABSTRACT <- ifelse(abstracts_federal_reporter_cleaned$cleanup == TRUE, 
                                                      str_replace(abstracts_federal_reporter_cleaned$ABSTRACT, "^Abstract",""), 
                                                      abstracts_federal_reporter_cleaned$ABSTRACT)
abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>%
  select(-cleanup)

#"ABSTRACT"

abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>% 
  mutate(flagged_phrase("^ABSTRACT"))
colnames(abstracts_federal_reporter_cleaned)[8] <- "cleanup"
abstracts_federal_reporter_cleaned$ABSTRACT <- ifelse(abstracts_federal_reporter_cleaned$cleanup == TRUE, 
                                                      str_replace(abstracts_federal_reporter_cleaned$ABSTRACT, "^ABSTRACT",""), 
                                                      abstracts_federal_reporter_cleaned$ABSTRACT)
abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>%
  select(-cleanup)

#"Project Summary"

abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>% 
  mutate(flagged_phrase("^Project Summary"))
colnames(abstracts_federal_reporter_cleaned)[8] <- "cleanup"
abstracts_federal_reporter_cleaned$ABSTRACT <- ifelse(abstracts_federal_reporter_cleaned$cleanup == TRUE, 
                                                      str_replace(abstracts_federal_reporter_cleaned$ABSTRACT, "^Project Summary",""), 
                                                      abstracts_federal_reporter_cleaned$ABSTRACT)
abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>%
  select(-cleanup)

#"TECHNICAL SUMMARY:"

abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>% 
  mutate(flagged_phrase("^TECHNICAL SUMMARY:"))
colnames(abstracts_federal_reporter_cleaned)[8] <- "cleanup"
abstracts_federal_reporter_cleaned$ABSTRACT <- ifelse(abstracts_federal_reporter_cleaned$cleanup == TRUE, 
                                                      str_replace(abstracts_federal_reporter_cleaned$ABSTRACT, "^TECHNICAL SUMMARY:",""), 
                                                      abstracts_federal_reporter_cleaned$ABSTRACT)
abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>%
  select(-cleanup)

#Tags leading up to the word "This" or "The" reflects a typo given that a word is smashed up against the word This or The with no space

abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>% 
  mutate(flagged_phrase("^.+[a-z]T"))
colnames(abstracts_federal_reporter_cleaned)[8] <- "cleanup"
abstracts_federal_reporter_cleaned$ABSTRACT <- ifelse(abstracts_federal_reporter_cleaned$cleanup == TRUE, 
                                                      str_replace(abstracts_federal_reporter_cleaned$ABSTRACT, "^.+[a-z]T","T"), 
                                                      abstracts_federal_reporter_cleaned$ABSTRACT)
abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>%
  select(-cleanup)

#" DESCRIPTION \\(provided by applicant\\):"

abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>% 
  mutate(flagged_phrase("^ DESCRIPTION (provided by applicant):"))
colnames(abstracts_federal_reporter_cleaned)[8] <- "cleanup"
abstracts_federal_reporter_cleaned$ABSTRACT <- ifelse(abstracts_federal_reporter_cleaned$cleanup == TRUE, 
                                                      str_replace(abstracts_federal_reporter_cleaned$ABSTRACT, "^ DESCRIPTION (provided by applicant):",""), 
                                                      abstracts_federal_reporter_cleaned$ABSTRACT)
abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>%
  select(-cleanup)

#" DESCRIPTION \\(provided by applicant\\) "

abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>% 
  mutate(flagged_phrase("^ DESCRIPTION (provided by applicant) "))
colnames(abstracts_federal_reporter_cleaned)[8] <- "cleanup"
abstracts_federal_reporter_cleaned$ABSTRACT <- ifelse(abstracts_federal_reporter_cleaned$cleanup == TRUE, 
                                                      str_replace(abstracts_federal_reporter_cleaned$ABSTRACT, "^ DESCRIPTION (provided by applicant) ",""), 
                                                      abstracts_federal_reporter_cleaned$ABSTRACT)
abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>%
  select(-cleanup)

#"DESCRIPTION \\(provided by applicant\\):"

abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>% 
  mutate(flagged_phrase("^DESCRIPTION \\(provided by applicant\\):"))
colnames(abstracts_federal_reporter_cleaned)[8] <- "cleanup"
abstracts_federal_reporter_cleaned$ABSTRACT <- ifelse(abstracts_federal_reporter_cleaned$cleanup == TRUE, 
                                                      str_replace(abstracts_federal_reporter_cleaned$ABSTRACT, "^DESCRIPTION \\(provided by applicant\\):",""), 
                                                      abstracts_federal_reporter_cleaned$ABSTRACT)
abstracts_federal_reporter_cleaned <- abstracts_federal_reporter_cleaned %>%
  select(-cleanup)


##End of entries:If "LAST_CHAR" does not end in "." then mark false:
abstracts_federal_reporter_cleaned$period <- str_detect(abstracts_federal_reporter_cleaned$LAST_CHAR, "^\\.")

##Too Short?
abstracts_federal_reporter_cleaned$length <- nchar(abstracts_federal_reporter_cleaned$ABSTRACT)
abstracts_federal_reporter_cleaned$adequate_length <- ifelse(nchar(abstracts_federal_reporter_cleaned$ABSTRACT) > 500, TRUE, FALSE)


write_csv(abstracts_federal_reporter_cleaned, "data/working/federal_reporter/abstracts_federal_reporter_cleaned.csv")
