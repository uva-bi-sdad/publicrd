library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(plotly)
library(wordcloud)
library(tidyverse)
library(ggplot2)
library(DT)
library(dplyr)
library(shinyjs)

source("theme.R")

  # DATA IMPORT -----------------------------------------------

tidy_abstracts <- readRDS("data/tidy_abstracts_dept.rds")
tidy_year <- readRDS("data/tidy_year.rds")
tidy_year_ab <- readRDS("data/tidy_year_abst.rds")
#pandemic_topic <- readRDS("data/pandemic_topic.rds")
#pandemic_topic <- readRDS("data/pandemic_topic.rds")
pandemic <- readRDS("data/thirtypandemictopics.rds")
#corona_topic <- readRDS("data/corona_topic.rds")
corona <- readRDS("data/thirtycoronatopics.rds")
#all_topics <- readRDS("data/all_topics.rds")
topics <- readRDS("data/seventyfivetopicsdf.rds")

opt_topics <- readRDS("data/opt_res.rds")
#reg_topics <- readRDS("data/reg_topics.rds")
pan_topics <- readRDS("data/pan_topics.rds")
cor_topics <- readRDS("data/cor_topics.rds")
full_topics <- readRDS("data/full_topics.rds")


jscode <- "var referer = document.referrer;
           var n = referer.includes('economic');
           var x = document.getElementsByClassName('logo');
           if (n != true) {
             x[0].innerHTML = '<a href=\"https://datascienceforthepublicgood.org/events/symposium2020/poster-sessions\">' +
                              '<img src=\"DSPG_white-01.png\", alt=\"DSPG 2020 Symposium Proceedings\", style=\"height:42px;\">' +
                             '</a>';
           } else {
             x[0].innerHTML = '<a href=\"https://datascienceforthepublicgood.org/economic-mobility/community-insights\">' +
                              '<img src=\"AEMLogoGatesColors-11.png\", alt=\"Gates Economic Mobility Case Studies\", style=\"height:42px;\">' +
                              '</a>';
           }
           "

  # UI ---------------------------------------------------------

shinyApp(
  ui = dashboardPagePlus(
    title = "DashboardPage",
    header = dashboardHeaderPlus(
      title = "DSPG 2020",
      left_menu = tagList(div("R&D ABSTRACTS: EMERGING TOPIC IDENTIFICATION", style="height:35px; display:flex; align-items: center; color: silver;"))

      ),

    # SIDEBAR (LEFT) ----------------------------------------------------------

    sidebar = dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        # menuItem(
        #   tabName = "homepage",
        #   text = "Home Page",
        #   icon = icon("home")
        # ),

        menuItem(
          tabName = "overview",
          text = "Project Overview",
          icon = icon("info circle")
        ),
        menuItem(
          tabName = "data",
          text = "Data & Methodology",
          icon = icon("database")
        ),

        menuItem(
          tabName = "graph",
          text = "Explore the Corpus",
          icon = icon("microscope")
        ),

        menuItem(
          tabName = "topicmodeling",
          text = "Topic Modeling",
          icon = icon("network-wired")
        ),

        menuItem(
          tabName = "both",
          text = "Hot & Cold Topics",
          icon = icon("fire")
        ),

        menuItem(
          tabName = "model",
          text = "Pandemics Case Studies",
          icon = icon("filter")
        ),

        menuItem(
          tabName = "team",
          text = "Team",
          icon = icon("user-friends")
        )
      )
    ),

  # BODY --------------------------------------------------------------------
    body = dashboardBody(
      useShinyjs(),
      customTheme,
      fluidPage(
      tabItems(


        # tabItem(tabName = "homepage",
        #         fluidRow(
        #           boxPlus(
        #             title = "Home Page",
        #             closable = FALSE,
        #             width = NULL,
        #             status = "warning",
        #             solidHeader = TRUE,
        #             collapsible = TRUE,
        #             column(12, img(src = "uva-dspg-logo.jpg", width = "300px", height = "300px"), align = "center"),
        #             column(12, h1("UVA Biocomplexity Institute"), align = "center"),
        #             column(12, h2("R&D Abstracts: Emerging Topic Identification"), align = "center")
        #
        #           ))),



        tabItem(tabName = "overview",
                fluidRow(
                  boxPlus(
                    title = "Project Overview",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    column(12, align = 'center', h1(strong("R&D Abstracts: Emerging Topic Identification"))),
                    br(),
                    h2("Project Description"),
                    p("This project uses topic models and visualization techniques to identify emerging research and development (R&D) topics within a corpus of publicly available abstracts from", a(href = "https://federalreporter.nih.gov/", "Federal RePORTER."), "The research builds upon an ongoing collaboration between the UVA Biocomplexity Institute Social and Decision Analytics Division (SDAD) and the National Center for Science and Engineering Statistics (NCSES) examining the use of administrative records to supplement or enhance data collected in NCSES surveys. Key components of our work include: (1) expanding the dataset to include the 2019 abstracts and (2) comparing two types of topic modeling techniques - Latent Dirichlet Allocation (LDA) and Nonnegative Matrix Factorization (NMF)."),
                    p("Our topic model analysis reveals 'hot' and 'cold' topics (i.e. those that increase or decrease in popularity) across time. We provide an interactive dashboard where users may explore our topic model results and investigate a pandemics case study. With the dashboard, users are able to view topics produced by the models, see key words associated with each topics, and track how the popularity of these topics fluctuate over time. Our project demonstrates the value of applying topic modeling and visualization to organize and interpret large government data sets and facilitate data-driven policy decisions."),
                    h2("Our Approach"),
                    img(src = "framework.png", width = "300px", align = "right"),
                    p("The foundation of our approach is the UVA SDAD Data Science Framework. Some highlights from applying the framework include:"),
                    #br(),
                    p(strong("Problem Identification:"), "Because we were extending a previous study, we began by reviewing prior work on this project and conducting a literature review focused on identifying emerging topics and topic model visualization."),
                    #br(),
                    p(strong("Data Discovery:"), "We discovered that Federal RePORTER had been updated with data from 2019, which was not included in the prior word, so we acquired the most recent set of abstracts to include in our analysis."),
                    p(strong("Data Wrangling:"), "We performed exploratory data analysis on our revised dataset and cleaned & processed abstracts to prepare for their use in topic modeling."),
                    p(strong("Statistical Modeling & Analyses:"), "We compared LDA and NMF to find an optimal topic model algorithm for our dataset. Informed by our literature review, we identified emerging topics and designated topics as 'hot' or 'cold' based on how their popularity changed across time. As a case study on emerging topics, we focused on identifying topics related to pandemic-related research."),
                    p(strong("Communication & Dissemination:"), "We constructed an RShiny dashboard, created a poster, and wrote a brief for SAGE Publications MethodSpace blog."),
                    h2("Ethical Considerations"),
                    p("Ethics are a key component of the Data Science Framework and inform every step of this process. We did not collect or utilize any individual or demographic data for this project, which minimizes the potential harm to individuals. However, in considering the larger implications of the project, we recognize that our dataset only included federally funded grants within the United States. It does not necessarily capture the full scope of research and development within the United States nor around the world. We also recognize that", a(href = "https://iaphs.org/identifying-implicit-bias-grant-reviews/", "implicit bias in research funding"), "may affect the representation of topics within our dataset and, while not addressed within the scope of this project, could serve as a focus for future analysis." )
                  )
                )),



        tabItem(tabName = "graph",
                fluidRow(
                  boxPlus(
                    title = "Welcome to our Dataset!",
                    closable = FALSE,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = NULL,
                    enable_sidebar = FALSE,
                    p("Use this page to explore the words in the abstracts of our R&D projects dataset.
                      You can search for any word to see its representation within our corpus over time and see
                      word importance and frequencies based upon the different funding agencies included in
                      Federal RePORTER. Please be patient, graphs may take a few seconds to load.")
                  ),
                  boxPlus(
                    title = "Explore Words in the Corpus",
                    closable = FALSE,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = NULL,
                    enable_sidebar = TRUE,
                    sidebar_background = "lightgrey", ##
                    sidebar_width = 20,
                    sidebar_start_open = TRUE,
                    sidebar_content = searchInput("search_term", label = "Type in your search term and then press the Enter key", value = "keyword"),
                    sidebar_title = "Search Term",
                    column(9, plotOutput("word_time")),
                    column(9, p("Note: Extremely frequently used words have been removed as possible search terms. In addition, the axis changes with the frequency of any given word."))
                  ),

                  boxPlus(
                    title = "Important Words by Funding Department",
                    closable = FALSE,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 6,
                    enable_sidebar = TRUE,
                    sidebar_background = "lightgrey", ##
                    sidebar_width = 20,
                    sidebar_start_open = TRUE,
                    sidebar_content = tagList(selectInput("department", "Select Funding Department",
                                                          choices = list("DOD", "ED", "EPA", "HHS",
                                                                         "NASA", "NSF", "USDA", "VA"),
                                                          selected = "HHS")),
                    column(9, plotOutput("important_words")),
                    footer = p("The weight of each word is given by the TFIDF for abstracts corresponding to the given department.
                               The weight can be thought of as a measure of importance of the word to the corpus.")
                  ),

                  boxPlus(
                    title = "Word Clouds by Funding Department",
                    closable = FALSE,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 6,
                    enable_sidebar = TRUE,
                    sidebar_background = "lightgrey", ##
                    sidebar_width = 22,
                    sidebar_start_open = TRUE,
                    sidebar_content = tagList(selectInput("selection", "Choose a department:",
                                                          choices = list("DOD", "ED", "EPA", "HHS",
                                                                         "NASA", "NSF", "USDA", "VA"),
                                                          selected = "DOD"),
                                              hr(),
                                              sliderInput("freq",
                                                          "Minimum Frequency:",
                                                          min = 1000,  max = 10000, value = 1000, step = 1000),
                                              sliderInput("max",
                                                          "Maximum Number of Words:",
                                                          min = 10,  max = 50,  value = 25, step = 5)),
                    column(9, plotOutput("wordcloud")),
                    footer = "The size of each word represents how frequently the word occurs in the abstracts for a given department.  Larger words appear more often."
                  )
                )),

# hot and cold ------------------------------------------------

        tabItem(tabName = "both",
                fluidRow(
                  boxPlus(
                    title = "Overview",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    enable_sidebar = FALSE,
                    column(12,
                           p("On the previous tab we looked at representation and “dominance” of the topics produced by the optimal topic model that we found, an NMF model with 75 topics.  We use this same model and now turn our focus to calculating the yearly prevalence of each topic within Federal RePORTER to determine which research topics are trending hot or cold over time.  As described in our Data and Methodology section, we follow the work in [1] and calculate each topic’s average weight per year from 2010-2019 and perform a linear smoothing of these averages to detect which topics have increased or decreased in prevalence over time.  A topic that increases in prevalence is considered “hot”, while a topic that decreases in prevalence is considered “cold”.")),
                    footer = p("[1] Griffiths, T., & Steyvers, M. (2004). Finding scientific topics. Proceedings of the National Academy of Sciences, USA, 101(1), 5228-35.", a(href = "https://doi.org/10.1073/pnas.0307752101", "https://doi.org/10.1073/pnas.0307752101."))
                  ),
                  boxPlus(
                    title = "Results for NMF model with 75 Topics",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    enable_sidebar = FALSE,
                    column(12, p("Explore topics by hovering over lines to see the year and proportion information for each topic."),
                                p("On the legend, click on a topic to deselect it, or double click on a topic to isolate it and see only one line on the plot. There are more functionality options above the legend. You can click on the Camera button to download the current view as a png. You can use the magnifying glass, plus, and minus sign to zoom in and out. On the double tag, you can compare proportions for all topics year over year. All interactive graphs are produced with Plotly."),
                                p("You can also view results in the table below which includes each topic’s linear smoothing coefficient, i.e., the slope of the trend line through the average weights per year for that topic, and categorization as “hot” or “cold”.  The ten words listed for each topic are those with the highest weights for that topic.  You can sort the table in ascending or descending order by any of the given columns by clicking on the up or down arrow next to the column name you wish to sort by.")
                           ),
                    column(12, plotlyOutput("emerging")),
                    column(12, br(), br()),
                    column(12, p("Search for a specific word to find which topics contain the search term.")),
                    column(12, DT::dataTableOutput("emerging_topics")),
                    column(12, br(), br()),
                    h4(strong("\"Hottest\" and \"Coldest\" Topics")),
                    column(12, p("We highlight the five “hottest” and “coldest” topics in the graphs below, i.e., the topics with the largest and smallest linear smoothing coefficients.  We include the line graph as given in the plot above and also the linear trend line for each topic.")),
                    column(12, img(src = "full_hot.png", width = "80%"), align = "center"),
                    column(12, br()),
                    column(12, img(src = "full_cold.png", width = "80%"), align = "center")
                    )
                )),


# data and methodology -------------------------------------------

        tabItem(tabName = "data",
                fluidRow(
                  boxPlus(
                    title = "Data Source",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    h3("Federal RePORTER"),
                    p("Our dataset consists of abstracts and project information for more than 1 million R&D grants entered into the ", a(href = "https://federalreporter.nih.gov/", "Federal RePORTER"), "system from 2008 - 2019. The Federal RePORTER database describes it as,  \"a collaborative effort led by STAR METRICS® to create a searchable database of scientific awards from [federal] agencies. This database promotes transparancy and engages the public, the research community, and agencies to describe federal science research investments and provide empirical data for science policy.\" Project information includes project title, department, agency, principal investigator, organization, and project start date. We downloaded our data using the Federal ExPORTER page.")),

                  boxPlus(
                    title = "Data Preparation",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    h3("Data Preparation"),
                    p(strong("Defining 'Emerging' Topics:")),
                    p("Our project maps the increase and decrease in new federally funded research projects associated with topics across time. We define a topic as having 'emerged' the first time a project or projects associated with the topic are identified in the corpus. We prepared our data for analysis in keeping with this goal."),
                    p("We needed to:"),
                    tags$ol(
                      tags$li("fill in missing information for project start date"),
                      tags$li("decide on a deduplication strategy for dealing with duplicate abstracts in the corpus, and"),
                      tags$li("clean and prepare the abstracts for use in topic modeling.")),
                    p(strong("Fill in Missing Project Start Dates:")),
                    p("In order to track emerging topics, we need every project in our dataset to have a start date. Our initial dataset had 1,156,137 projects and 13.4% were missing start dates. For projects missing start dates, we utilized the budget start date if available (57.1% of the budget start dates in the full dataset were missing). For projects missing a start date and budget start date, we utilized the fiscal year in which it was added to the Federal RePORTER database as the start date."),
                    p(strong("Deduplication Strategy:")),
                    p("Because our focus was on identifying topics associated with new research projects, we needed to control for multi-institutional projects (e.g., a single project associated with investigators across two or more universities) and extensions of previously established projects (e.g., projects that have their funding extended are recorded in Federal RePORTER with a new start date every time their funding is extended). Counting the same project more than once in a given year or more than once across the timespan of analysis could artificially inflate the number of new projects associated with a topic in a given year. "),
                    p("To do this, we identified and removed duplicate projects. We defined duplicate protects as ones with the same title, abstract, and start date. All but one instance of each set of duplicate projects was removed. The remaining instance of each project was appended to include the latest project end date and the number of unique organizations and principle investigators associated with the project."),
                    p(strong("Abstract Cleaning and Processing:")),
                    p("We removed projects with empty/null abstracts (e.g., abstracts listed as “NA”, “No Abstract Provided”) and projects with abstracts that contained less than 150 characters. These projects lacked sufficient information for the model to associate them with a topic. We cleaned the abstract text by removing elements that were not part of the abstract or not relevant to the specifics of the project (e.g. phrases such as 'non-technical abstract', 'description (provided by applicant) and 'end of abstract). We also removed principal investigator names, project organizations and project titles (if included in abstracts) in order to prevent topics forming around people or universities."),
                    p("After cleaning the abstract text, we used standard natural language processing steps to prepare our abstracts for use with topic models."),
                    tags$ol(
                      tags$li(strong("Tokenization and Lemmatization:"), "Abstracts are transformed into a list of their words (each word is a 'token') and each word is lemmatized (i.e., variations of same word and part-of-speech are reduced to their shared dictionary form of lemma). For example, jumped (verb) and jumps (verb) would both be reduced to jump (verb) but jumpy (adjective) would not."),
                      tags$li(strong("Stop Word Removal"), "Tokens belonging to a standard stop word list are removed. Stop words are common words that provide little information on content and do not contribute to topic meaning, They include words such as: 'as, by, if, most, several, whereas.; We also added custom stop words not included in the standard list (e.g. furthermore, overall, specifically)."),
                      tags$li(strong("N-grams Creation:"), "We utilized bi-grams and tri-grams in our abstract text. A bi-gram is two words that appear often enough one after the other that they get combined into one token. For example, 'anti_virus' is a bigram. A tri-gram is similar except it contains three words that appear sequentially.")),
                    p("We then removed non-alphanumeric characters in tokens, single character tokens, and numeric tokens that were not years. Listed in the table are the Python packages that we used for each step"),
                    tableOutput("packages"),
                    p("As a final step, inspired by the work in [1], we removed many of the most frequent (remaining) words in the corpus. For example, we removed words such as: research, study, project, use, result, understanding, and investigate. These words appear frequently in our corpus, but they do not contribute to topic meaning because they appear in such a high proportion of abstracts that they are not useful for differentiating across topics."),
                    p("Our final dataset includes 690,814 projects. See the graphs below for information about the number of abstracts in the corpus by project start year, the percent of abstracts by department, and the length of abstracts (in number of characters)."),
                    img(src = "corpus_eda.png", width="100%", align = "center"),
                    br(),
                    p("The figure on the top left shows that a very high proportion of all projects are funded by U.S. Department of Health and Human Services (HHS). This is due to the fact that HHS houses the National Institutes of Health (NIH), which is comprised of a large number of institutes, each of which is responsible for funding many projects. In addition, the figure on the top right shows the number of abstracts in the corpus across each of the project start years. We notice that there is a large spike in the number of abstracts in 2009 and 2010 before leveling off to a more stable level. This can likely be attributed to the increased science and science-related funding spurred by the",  a(href = "https://obamawhitehouse.archives.gov/administration/eop/cea/Estimate-of-Job-Creation/", "American Recovery and Reinvestment Act of 2009"), "that was designed to create more jobs after the Great Recession. Lastly, the figure on the bottom left displays the number of characters in each of the abstracts. The median length of the abstracts is roughly 2,500 characters, although there are many abstracts that are much longer, including some over 10,000 characters that are not shown in this figure."),
                    footer = p("[1] Schofield, A., Magnusson, M., Thompson, L., & Mimno, D. (2017). Understanding text pre-processing for latent Dirichlet allocation. Proceedings of the 1st Workshop for Women and Underrepresented Minorities in Natural Language Processing.", a(href = "https://www.cs.cornell.edu/~xanda/winlp2017.pdf.", "https://www.cs.cornell.edu/~xanda/winlp2017.pdf."))
                  ),

                  boxPlus(
                    title = "Topic Modeling",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    h3("Topic Modeling"),
                    p("When presented with a collection of text documents, or a corpus, one of the first steps one might take is to understand and classify each document into different topics or themes. For small corpora, it may be feasible to manually read and record the topics of each document; however, this is clearly impractical for large corpora of interest in recent years such as Twitter posts or articles from a scientific journal. Topic modeling helps resolve this issue by automatically processing a corpus and discovering topics that characterize the documents.  "),
                    p("An additional benefit is that a document can be assigned to miltiple topics rather than just one, a reasonable assumption given that many documents consist of a combination of themes.  We examined two topic modeling algorithms over the course of this project: Latent Dirichlet allocation (LDA) and non-negative matrix factorization (NMF).  We used the Python package Scikit-Learn for the implementations of both LDA and NMF. "),
                    p(strong("Topic Model Input and Output")),
                    p("LDA and NMF both take a term-document matrix of our cleaned and processed abstract text as input.  A term-document matrix is a mathematical representation of text data as numerical data that can then be analyzed using a statistical model or machine learning algorithm.  More specifically, a term-document matrix is comprised of the frequency count of each word in each document.   "),
                    p("LDA and NMF also output the same two matrices, consisting of the results of the topic model."),
                    tags$ol(
                      tags$li(strong("Document-Term Matrix:"), "this matrix contains information on which topics appear in which documents and how much they appear. LDA represents this information as probabilities of each topic in each document whereas NMF gives weights for each topic in each document."),
                      tags$li(strong("Topic-Term Matrix:"), "this matrix contains information on which words appear in which topics. Again, LDA represents this information using the probability of each word appearing in each topic and NMF gives weights for each word appearing in each topic.  When analyzing topics, we generally only look at the 5 or 10 words in each topic with the highest probability/weight.  ")),
                    p(strong("Latent Dirichlet Allocation - LDA")),
                    p("LDA is a statistical algorithm that generates topics probabilistically, sorting words based on their likelihood of appearing in the same document as one another and reporting these common word-association patterns as the corpus’s most probable topics.  In addition, LDA is a soft-clustering algorithm meaning that the same word can appear in multiple topics. "),
                    p("The main assumption underlying LDA is that the corpus is built using a generative structure that we are trying to recover.  More specifically, the assumption is that each document in the corpus is built word by word using the following steps: 1) pick a topic according to the probability distribution of topics in the corpus, 2) for the selected topic, pick a word according to the probability distribution of words for that topic, and 3) repeat steps 1-2 over and over to create a document.  LDA works to uncover this unobserved structure and thus outputs the document-topic distribution and topic-term distribution as results. "),
                    p("The seminal paper on LDA was written by David M. Blei, Andrew Y. Ng, and Michael I. Jordan [1]. "),
                    p(strong("Non-Negative Matrix Factorization")),
                    p("NMF is a linear algebra based method that is also a soft-clustering algorithm.  NMF is an approximate matrix decomposition that finds the document-topic matrix and topic-term matrix through iterative optimization.  The idea is that the document-term matrix can be approximated as the product of the document-topic matrix and the topic-term matrix, in effect clustering words together into topics, and weighting those topics amongst every document. This approximation yields the best attempt to recreate the original corpus with a topic structure.  The seminal paper on NMF was written by Daniel D. Lee and H. Sebastian Seung [2]. "),
                    column(12, img(src = "nmf_image.png", width = "70%"), align="center"), #, height = "100px"),
                    p("In our work, we use a weighted document-term matrix as input to NMF in order to achieve better topic modeling results.  Instead of only using the frequency of each word in each document, we use a term frequency-inverse document frequency (TFIDF) weighting scheme for each word in each document.  TFIDF has the effect of 'penalizing' words that appear in many documents in the corpus, which aids in topic modeling as these words are most likely not very specific to the topics themselves."),
                    p(strong("Evaluation of Topic Models")),
                    p("To evaluate the quality of our topic models, we need a measure or score of how well the model performed. We also want to ensure that the topics the model finds are coherent and human interpretable.  We generally only look at the top 5-10 words in each topic to interpret what topic is being represented."),
                    p("Given these goals, we use the measure of C", tags$sub("V"), "topic coherence as given in [3] to evaluate our topic models.  As shown in [3], C", tags$sub("V"), "topic coherence is the coherence measure most correlated to human interpretation of topics.  We find the C", tags$sub("V"), "coherence per topic, which is a score that encodes how often the topic words appear together in close proximity within the documents as well as semantic information. To find the C", tags$sub("V"), "topic coherence for the entire model, take take the average of all of the topic C", tags$sub("V"), "coherence scores. The optimal topic model for our corpus is then selected by comparing the C", tags$sub("V"), "topic coherence scores from each model and selecting the one with the highest score."),
                    footer = p("[1] Blei, D., Ng, A., & Jordan, M. (2003). Latent Dirichlet allocation.", em("Journal of Machine Learning Research,"), "3, 993-1022.", a(href = "http://jmlr.org/papers/volume3/blei03a/blei03a.pdf", "http://jmlr.org/papers/volume3/blei03a/blei03a.pdf."),
                               br(),
                               "[2] Lee, D., & Seung, H. (1999). Learning the parts of objects by non-negative matrix factorization.", em("Nature,"), "401, 788-791.",
                               br(),
                               "[3] Röder, M., Both, A., & Hinneburg, A. (2015). Exploring the space of topic coherence measures.", em("WSDM '15: Proceedings of the Eighth ACM International Conference on Web Search and Data Mining"), "(pp. 399-408). Association for Computing Machinery, New York, NY.", a(href = "https://doi.org/10.1145/2684822.2685324", "https://doi.org/10.1145/2684822.2685324"))
                  ),

                  boxPlus(
                    title = "Emerging Topics",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    h3("Emerging Topics"),
                    p("Given our optimal topic model, an NMF model with 75 topics, we analyze its results to discover and characterize 'hot' and 'cold' topics. To do so, we follow the approach of [1] with the exception that we use our optimal NMF topic model, not an LDA model. To categorize a topic as “hot” or “cold”, we first use the document-topic matrix to find the average weight of each topic in each year between 2010-2019. This creates a set of points for each topic, where each point has the form (year, weightyear).  We then model the relationship between the weights and years for each topic using linear regression. Topics that have regression lines with positive slopes are considered 'hot' and those that have regression lines with negative slopes are considered 'cold'."),
                    p("The slope of the regression line slope serves to capture the trend of the topic overtime.  For example, a “hot” topic means that over time research abstracts within Federal RePORTER’s database have a higher weight for that topic, i.e., the topic is more present in the corpus.  The magnitude of the regression line slope allows us to compare which topics are “hotter” or “colder”.  "),
                    p("We used the work of [2] as a reference for this emerging topics technique as well, but instead of using a time period of a few years per data point of the regression line, we chose the strategy of [1] and use a year per data point.  "),
                    p(strong("Pandemics Case Study")),
                    p("We conduct a pandemics case study where we explore emerging topics around the research areas of pandemics and coronavirus.  To do this we first use information retrieval techniques to create two smaller corpora: one that focuses on pandemics, and one that focuses on coronavirus.  We then use an NMF topic model of 30 topics on each smaller corpus and conduct the emerging topics analysis as above.  The size of each corpus is given in the table below. "),
                    tableOutput("case_study"),
                    p("To construct the smaller corpora we use a combination of three different information retrieval techniques.  The steps below outline the process for the pandemics corpus.  For the coronavirus corpus we follow the same steps except replace the word “pandemic” with “coronavirus”. "),
                    tags$ol(
                      tags$li(strong("Literal Term Matching:"), "we use the term-document matrix of frequency counts per word per document for our full dataset of abstracts to extract the 500 projects that have the most occurrences of the word “pandemic” in their abstracts. "),
                      tags$li(strong("TFIDF:"), "we use the TFIDF term-document matrix of weighted counts per word per document for our full dataset of abstracts to extract the 500 projects that have the largest weights for the word “pandemic” in their abstracts. "),
                      tags$li(strong("Latent Semantic Indexing (LSI):"), "we use a truncated singular value decomposition on the TFIDF term-document matrix for our full dataset of abstracts to extract the 500 projects that have abstracts most relevant to the search query for the word “pandemic”.  LSI differs from the previous two information retrieval approaches in the fact that project abstracts returned as relevant to the search query do not necessarily have to contain the word “pandemic”.  But they may contain words that are latently related to the word “pandemic”.  For more information about LSI, the interested reader can see the seminal paper [3].  We use the implementation of the truncated singular value decomposition in the Python package Scikit-Learn. ")),
                    p("To create the smaller corpus we then take the set of unique projects from the union of the results returned from the three information retrieval methods above.  "),
                    footer = p("[1] Griffiths, T., & Steyvers, M. (2004). Finding scientific topics. Proceedings of the National Academy of Sciences, USA, 101(1), 5228-35.", a(href = "https://doi.org/10.1073/pnas.0307752101", "https://doi.org/10.1073/pnas.0307752101."), br(),
                               "[2] Lee, H., & Kang, P. (2018). Identifying core topics in technology and innovation management studies: A topic model approach.", em("Journal of Technology Transfer,"), "43, 1291-1317.", a(href = "https://doi.org/10.1007/s10961-017-9561-4", "https://doi.org/10.1007/s10961-017-9561-4."), br(),
                               "[3] Deerwester, S., Dumais, S., Furnas, G., Landauer, T., & Harshman, R. (1990). Indexing by latent semantic analysis.", em("Journal of the American Society for Information Science,"), "41(6), 391-407.")
                    )

                  )),


# topic modeling ---------------------------------------------

        tabItem(tabName = "topicmodeling",
                fluidRow(
                  boxPlus(
                    title = "Topic Modeling Approach Details",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    enable_sidebar = FALSE,
                    column(12, p("After cleaning and processing the R&D abstracts in our corpus, we are ready to use them for topic modeling.  We create the term-document matrix from the cleaned abstracts and in the process filter the terms to include in the matrix.  We only include a term in the matrix if it meets the following criterion:"),
                      tags$ol(
                        tags$li("A term must appear in at least 20 documents in the corpus, and"),
                        tags$li("A term cannot appear in more than 60% of the documents of the corpus.")
                      ),
	                    p("This filtering of extremes allows us to remove terms that are not frequent enough to become a top 10 word in a topic, and to remove common words to the corpus that would not contribute to topic meaning.  We use the term-document matrix as input for LDA and the TFIDF term-document matrix as input for NMF.  Both matrices are created using the Python package Scikit-Learn."),
                      p("In addition to the term-document matrix, LDA and NMF also require the number of topics as input.  Unfortunately, we do not know the number of topics present in the corpus in advance.  We find our optimal topic model by varying the number of topics for NMF and LDA while tracking the CV topic coherence for each choice.  The model with the largest coherence is the optimal model."),
                      p("For the interested reader, LDA also takes two other parameters, α and β, that encode prior knowledge about the corpus.  Specifically,  α controls the document-topic density and β controls the topic-word density.  Setting a higher value of α means that documents are assumed to be made up of more topics whereas a higher value of beta means that topics are assumed to be made up of more of the words in the corpus.  In all of our LDA model runs, we fixed these parameters at α = 1/N, where N is the number of topics, and β = 0.1.  These parameter choices allow our documents to be made up of multiple topics and the topics to be specific.")
                      )
                    ),
                  boxPlus(
                    title = "Choosing the Optimal Topic Model",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    enable_sidebar = FALSE,
                    column(12,
                           p("We used the topic coherence metric to evaluate the topic models and select the best model. Topic coherence is a measure that calculates the degree of semantic similarity between high scoring words in the topic. The plots below display the CV topic coherence for varying numbers of topics for our LDA and NMF models. Since LDA and NMF are both stochastic algorithms, this means that they could yield different results for two runs of the same model using the same parameters. In the future, we plan to create similar plots where each point would represent the average coherence of ten runs of LDA or NMF with the corresponding number of topics."),
                           column(12, img(src = "LDA_NMF_tc.png", width = "100%"), align = "center"),
                           p("Overall, the NMF models have higher topic coherence than the LDA models at each of the number of topics. Across all of the models fit, the optimal topic model for our corpus is the NMF model with 75 topics. The jagged nature of these line graphs is due to the stochastic nature of NMF and LDA. In the future, the plots representing average coherence of 10 model runs for each number of topics will hopefully show smoother trends.")
                    )
                  ),
                  boxPlus(
                    title = "Optimal Topic Model Results",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    enable_sidebar = FALSE,
                    column(12,
                           p("Here, we analyze the topics generated by the optimal topic model by characterizing their coherency, representation, and dominance."),
                           column(12, img(src = "coherence.png", width = "90%"), align = "center"),
                           p("Using the topic coherence measure, we show the most and least coherent topics and the five words in each with the topic with highest weight. We see that topics relating to the liver, vaccines, students, neurodegenerative diseases, alcohol, schools, conferences, academic training, prostate cancer, and neurons have particularly high topic coherence. Within these topics, the words with the highest weight seem to have a high degree of semantic similarity. For example, the words vaccine, antibody, antigen, immune, and vaccination are all very similar thematically."),
                           column(12, img(src = "topic_rep.png", width = "90%"), align = "center"),
                           p("In addition, we analyzed the generated topics by looking at the topics that are in the highest and lowest percentage of documents in our corpus. We see that topics relating to the cell, patient therapies, proteins, technology, clinical research, mathematical models, genetics, population health, mice, and drug testing are the most highly represented in the corpus."),
                           column(12, img(src = "topic_rank.png", width = "90%"), align = "center"),
                           p("Lastly, we consider the “dominance” of the topics as calculated in [1]. The dominance measure is calculated by counting the number of articles which each topic has the highest proportion. Here, we see that topics relating to proteins, laboratories, gpcr, cancers, the immune system, and technology were the most dominant topics in over 3% of the corpus while the cell topic, most represented topic in figure above, was the dominant topic in less than 0.5% of the corpus."),
                           p("Across the three different measures, we notice that there is a relative lack of overlap between the topics that are found to be the most coherent, representative, and dominant. This could possibly be due to the stochastic nature of the algorithms yielding topics that are somewhat unstable. We provide complete results on these measures in the table below. You can sort the table in ascending or descending order by any of the given columns by clicking on the up or down arrow next to the column name you wish to sort by."),
                           column(12, br()),
                           column(12, DT::dataTableOutput("optimal_topics"))
                    ),
                    footer = p("[1] Lee, H., & Kang, P. (2018). Identifying core topics in technology and innovation management studies: A topic model approach.", em("Journal of Technology Transfer,"), "43, 1291-1317.", a(href = "https://doi.org/10.1007/s10961-017-9561-4", "https://doi.org/10.1007/s10961-017-9561-4."))
                  )
                )),


# case studies --------------------------------------------------

        tabItem(tabName = "model",
                fluidRow(
                  boxPlus(
                    title = "Subsetting a Corpus",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    enable_sidebar = FALSE,
                    column(12, p("Research on pandemics is an area of particular interest to our sponsor. We utilized information retrieval techniques to develop two smaller corpora from our larger dataset. One focused on pandemics and the other focused more specifically on abstracts related to coronavirus."),
                               p("We used a combination of three different information retrieval techniques in compiling both the pandemic and coronavirus corpora – Literal Term Matching, TFIDF, and Latent Semantic Indexing (LSI) – as outlined in our Data and Methodology section.  This resulted in a corpus of 1,137 projects related to pandemics and another corpus of 1,012 projects related to coronavirus. We then conducted the \"hot\" and \"cold\" topic analysis of [1] on each corpus.")
                           ),
                    footer = p("[1] Griffiths, T., & Steyvers, M. (2004). Finding scientific topics. Proceedings of the National Academy of Sciences, USA, 101(1), 5228-35.", a(href = "https://doi.org/10.1073/pnas.0307752101", "https://doi.org/10.1073/pnas.0307752101."))
                    ),

                  boxPlus(
                    title = "Case Study 1: Pandemics",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    enable_sidebar = FALSE,
                    column(12, h2("Pandemics"), align = 'center'),
                    column(12, p("Explore topics by hovering over lines to see the year and proportion information for each topic."),
                              p("On the legend, click on a topic to deselect it, or double click on a topic to isolate it and see only one line on the plot. There are more functionality options above the legend. You can click on the Camera button to download the current view as a png. You can use the magnifying glass, plus, and minus sign to zoom in and out. On the double tag, you can compare proportions for all topics year over year. All interactive graphs are produced with Plotly."),
                              p("You can also view results in the table below which includes each topic’s linear smoothing coefficient, i.e., the slope of the trend line through the average weights per year for that topic, and categorization as “hot” or “cold”. The ten words listed for each topic are those with the highest weights for that topic. You can sort the table in ascending or descending order by any of the given columns by clicking on the up or down arrow next to the column name you wish to sort by.")
                           ),
                    column(12, plotlyOutput("pandemics")),
                    column(12, br(), br()),
                    column(12, p("Search for a specific word to find which topics contain the search term.")),
                    column(12, DT::dataTableOutput("pandemics_topics")),
                    column(12, br(), br()),
                    h4(strong("\"Hottest\" and \"Coldest\" Topics")),
                    column(12, p("We highlight the five “hottest” and “coldest” topics in the graphs below, i.e., the topics with the largest and smallest linear smoothing coefficients.  We include the line graph as given in the plot above and also the linear trend line for each topic.  It is interesting to note that our results show a sharp increase in Zika virus research after the outbreak in 2015-2016.  We also see increased research in influenza in 2010 and 2011 after the swine flu pandemic in 2009-2010.")),
                    column(12, img(src = "pan_hot.png", width = "80%"), align = "center"),
                    column(12, br()),
                    column(12, img(src = "pan_cold.png", width = "80%"), align = "center")
                  ),
                  boxPlus(
                    title = "Case Study 2: Coronavirus",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    enable_sidebar = FALSE,
                    column(12, h2("Coronavirus"), align = 'center'),
                    column(12, p("Explore topics by hovering over lines to see the topic and proportion information."),
                              p("On the legend, click on a topic to deselect it, or double click on a topic to isolate it and see only one line on the plot. There are more functionality options above the legend. You can click on the Camera button to download the current view as a png. You can use the magnifying glass, plus, and minus sign to zoom in and out. On the double tag, you can compare proportions for all topics year over year. All interactive graphs are produced with Plotly."),
                           p("You can also view results in the table below which includes each topic’s linear smoothing coefficient, i.e., the slope of the trend line through the average weights per year for that topic, and categorization as “hot” or “cold”. The ten words listed for each topic are those with the highest weights for that topic. You can sort the table in ascending or descending order by any of the given columns by clicking on the up or down arrow next to the column name you wish to sort by.")
                          ),
                    column(12, plotlyOutput("coronavirus")),
                    column(12, br(), br()),
                    column(12, p("Search for a specific word to find which topics contain the search term.")),
                    column(12, DT::dataTableOutput("coronavirus_topics")),
                    column(12, br(), br()),
                    h4(strong("\"Hottest\" and \"Coldest\" Topics")),
                    column(12, p("We highlight the five “hottest” and “coldest” topics in the graphs below, i.e., the topics with the largest and smallest linear smoothing coefficients.  We include the line graph as given in the plot above and also the linear trend line for each topic.  We observe that research on Middle East respiratory syndrome (MERS) is increasing at the highest rate of the topics discovered by our model.  The emergence of MERS in 2012 provides a possible explanation for this increasing trend.")),
                    column(12, img(src = "cor_hot.png", width = "90%"), align = "center"),
                    column(12, br()),
                    column(12, img(src = "cor_cold.png", width = "90%"), align = "center")
                  )
                )),


# team ------------------------------------

        tabItem(tabName = "team",
                fluidRow(
                  boxPlus(
                    title = "Our Team",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    h2("DSPG Team Members"),
                    p("The", a(href = "https://biocomplexity.virginia.edu/social-decision-analytics/dspg-program", "Data Science for the Public Good Young Scholars program"), "is a summer immersive program held at the Biocomplexity Institute’s Social and Decision Analytics division (SDAD). The program engages students from across the country to work together on projects that address state, federal, and local government challenges around critical social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to determine how information generated within every community can be leveraged to improve quality of life and inform public policy."),

                    fluidRow(

                      style = "height:10px;"),
                    fluidRow(

                      # Lara
                      column(3,
                             div(class="panel panel-default",
                                 div(class="panel-body",  width = "600px",
                                     align = "center",
                                     div(
                                       tags$img(src = "teamphotos/lara.jpg",
                                                width = "100px", height = "100px")
                                     ),
                                     div(
                                       tags$h5("Lara Haase"),
                                       tags$h6( tags$i("Graduate Fellow"))
                                     ),
                                     div(
                                       "Lara is pursing a Masters of Science in Public Policy & Management - Data Analytics at Carnegie Mellon."
                                     )
                                 )
                             )
                      ),
                      # Martha
                      column(3,
                             div(class="panel panel-default",
                                 div(class="panel-body",  width = "600px",
                                     align = "center",
                                     div(
                                       tags$img(src = "teamphotos/MARTHA.jpg",
                                                width = "100px", height = "100px")
                                     ),
                                     div(
                                       tags$h5("Martha Czernuszenko"),
                                       tags$h6( tags$i("Intern"))
                                     ),
                                     div(
                                       "Martha recently graduated from The University of Texas where she studied Information Systems & Business Honors."
                                     )
                                 )
                             )
                      ),
                      # Liz
                      column(3,
                             div(class="panel panel-default",
                                 div(class="panel-body",  width = "600px",
                                     align = "center",
                                     div(
                                       tags$img(src = "teamphotos/LIZ.jpg",
                                                width = "100px", height = "100px")),
                                     div(
                                       tags$h5("Liz Miller"),
                                       tags$h6( tags$i("Intern"))
                                     ),
                                     div(
                                       "Liz is an incoming senior at William and Mary where she studies International Relations  & History."
                                     )
                                 )
                             )
                      ),
                      # Sean
                      column(3,
                             div(class="panel panel-default",
                                 div(class="panel-body",  width = "600px",
                                     align = "center",
                                     div(
                                       tags$img(src = "teamphotos/SEAN.jpg",
                                                width = "100px", height = "100px")),
                                     div(
                                       tags$h5("Sean Pietrowicz"),
                                       tags$h6( tags$i("Intern"))
                                     ),
                                     div(
                                       "Sean recently graduated from Notre Dame where he studied Applied Computational Math & Statistics"
                                     )
                                 )
                             )
                      ),
                      column(1)),


                    #SDAD
                    h2("UVA SDAD Team Members"),
                    p("The Social and Decision Analytics Division (SDAD) is one of three research divisions within the Biocomplexity Institute and Initiative at the University of Virginia. SDAD combines expertise in statistics and social and behavioral sciences to develop evidence-based research and quantitative methods to inform policy decision-making and evaluation. The researchers at SDAD span many disciplines including statistics, economics, sociology, psychology, political science, policy, health IT, public health, program evaluation, and data science.
                      The SDAD office is located near our nation's capital in Arlington, VA. You can
                      learn more about us at", a(href = "https://biocomplexity.virginia.edu/social-decision-analytics", "https://biocomplexity.virginia.edu/social-decision-analytics"), "."),
                    fluidRow(

                      style = "height:50px;"),

                    fluidRow(

                      # Kathryn
                      column(3,
                             div(class="panel panel-default",
                                 div(class="panel-body",  width = "600px",
                                     align = "center",
                                     div(
                                       tags$img(src = "teamphotos/KATHRYN.jpg",
                                                width = "100px", height = "100px")
                                     ),
                                     div(
                                       tags$h5("Kathryn Linehan"),
                                       tags$h6( tags$i())
                                     ),
                                     div(
                                       "Research Scientist"
                                     )
                                 )
                             )
                      ),
                      # Stephanie
                      column(3,
                             div(class="panel panel-default",
                                 div(class="panel-body",  width = "600px",
                                     align = "center",
                                     div(
                                       tags$img(src = "teamphotos/STEPHANIE.jpg",
                                                width = "100px", height = "100px")
                                     ),
                                     div(
                                       tags$h5("Stephanie Shipp"),
                                       tags$h6( tags$i())
                                     ),
                                     div(
                                       "Deputy Division Director & Research Professor"
                                     )
                                 )
                             )
                      ),
                      # Joel
                      column(3,
                             div(class="panel panel-default",
                                 div(class="panel-body",  width = "600px",
                                     align = "center",
                                     div(
                                       tags$img(src = "teamphotos/JOEL.jpg",
                                                width = "100px", height = "100px")),
                                     div(
                                       tags$h5("Joel Thurston"),
                                       tags$h6( tags$i())
                                     ),
                                     div(
                                       "Senior Scientist"
                                     )
                                 )
                             )
                      ),
                      # Eric
                      column(3,
                             div(class="panel panel-default",
                                 div(class="panel-body",  width = "600px",
                                     align = "center",
                                     div(
                                       tags$img(src = "teamphotos/ERIC.png",
                                                width = "100px", height = "100px")),
                                     div(
                                       tags$h5("Eric Oh"),
                                       tags$h6( tags$i())
                                     ),
                                     div(
                                       "Research Assistant Professor"
                                     )
                                 )
                             )
                      ),
                      column(1)),
                    h2("Project Sponsors"),
                    p("Our sponsor is The National Center for Science and Engineering Statistics (NCSES). NCSES's mandate is the collection, interpretation, analysis, and dissemination of objective data on the science and engineering enterprise."),
                    column(3,
                           div(class="panel panel-default",
                               div(class="panel-body",  width = "600px",
                                   align = "center",
                                   div(
                                     tags$img(src = "teamphotos/JOHN.png",
                                              width = "100px", height = "100px")),
                                   div(
                                     tags$h5("John Jankowski "),
                                     tags$h6( tags$i())
                                   ),
                                   div(
                                     "Director of R&D Statistics Program at NCSES"
                                   )
                               )
                           )
                    )
                    #h2("Acknowledgements"),
                    #p("[Optional: You can also include external collaborators in this section or a separate section.]")
                    )
                ))
      )
      ))
    ),

  server = function(input, output) {

    # Run JavaScript Code
    runjs(jscode)

    filtered_year <- reactive({
      dplyr::filter(tidy_year, word == input$search_term)
    })

    filtered_ab <- reactive({
      dplyr::filter(tidy_year_ab, word == input$search_term)
    })

    output$word_time <- renderPlot({
      ggplot() +
        geom_point(data = filtered_year(), mapping = aes(x = year, y = n), size = 3, color = "#0072B2") +
        geom_smooth(data = filtered_year(), aes(x= year, y = n), se = FALSE, color = "#56B4E9", size = 2) +
        geom_point(data = filtered_ab(), mapping = aes(x = year, y = n), size=3, color = "#D55E00") +
        geom_smooth(data = filtered_ab(), aes(x= year, y = n, group = 1), se = FALSE, color = "#CC79A7", size = 2) +
        labs(title = "Term Frequency Over Time", subtitle = "Search Any Term", color = 'Year', x = "Year", y = "Word Frequency") +
        scale_x_continuous(breaks = seq(2009, 2019, by = 1)) +
        scale_y_continuous(sec.axis = dup_axis(name = "Document Frequency")) +
        theme_bw() +
        theme(axis.text.y.left = element_text(color = "#0072B2")) +
        theme(axis.text.y.right = element_text(color = "#D55E00"))

    })

    output$important_words <- renderPlot({

      selected_type <- switch(input$department,
                              "DOD" =
                                tidy_abstracts[tidy_abstracts$dept == "DOD", ],
                              "ED" =
                                tidy_abstracts[tidy_abstracts$dept == "ED", ],
                              "EPA" =
                                tidy_abstracts[tidy_abstracts$dept == "EPA", ],
                              "HHS" =
                                tidy_abstracts[tidy_abstracts$dept == "HHS", ],
                              "NASA" =
                                tidy_abstracts[tidy_abstracts$dept == "NASA", ],
                              "NSF" =
                                tidy_abstracts[tidy_abstracts$dept == "NSF", ],
                              "USDA" =
                                tidy_abstracts[tidy_abstracts$dept == "USDA", ],
                              "VA" =
                                tidy_abstracts[tidy_abstracts$dept == "VA", ])

      selected_type %>%
        arrange(desc(tf_idf)) %>%
        mutate(word = factor(word, levels = rev(unique(word)))) %>%
        group_by(dept) %>%
        top_n(20) %>%
        ungroup() %>%
        ggplot(aes(word, tf_idf, fill = dept)) +
        geom_col(show.legend = FALSE) +
        labs(x = "word", y = "weight by department") +
        coord_flip() +
        theme_bw()
    })

    output$wordcloud <- renderPlot({
      selected_cloud <- switch(input$selection,
                               "DOD" =
                                 tidy_abstracts[tidy_abstracts$dept == "DOD", ],
                               "ED" =
                                 tidy_abstracts[tidy_abstracts$dept == "ED", ],
                               "EPA" =
                                 tidy_abstracts[tidy_abstracts$dept == "EPA", ],
                               "HHS" =
                                 tidy_abstracts[tidy_abstracts$dept == "HHS", ],
                               "NASA" =
                                 tidy_abstracts[tidy_abstracts$dept == "NASA", ],
                               "NSF" =
                                 tidy_abstracts[tidy_abstracts$dept == "NSF", ],
                               "USDA" =
                                 tidy_abstracts[tidy_abstracts$dept == "USDA", ],
                               "VA" =
                                 tidy_abstracts[tidy_abstracts$dept == "VA", ])

      selected_cloud %>%
        with(wordcloud(word, n, scale = c(2.5,1), #5, 1.5
                       min.freq = input$freq, max.words = input$max,
                       ordered.colors = TRUE))
    })

    output$packages <- renderTable({
      table <- matrix(c("Tokenization and Lemmatization", "stanza", "Stop Word List Creation", "spaCy", "N-Grams Creation", "gensim"), ncol = 2, byrow = TRUE)
      colnames(table) <- c("Step", "Python Package")
      table

    })

    output$case_study <- renderTable({
      case_table <- matrix(c("Pandemics", 1137, "Coronavirus", 1012), ncol = 2, byrow = TRUE)
      colnames(case_table) <- c("Corpus", "Number of Projects")
      case_table
    })

    output$emerging <- renderPlotly({

      plot_ly(topics, x = ~ START_YEAR, y = ~ Weight, type = "scatter", mode = "lines+markers", color = topics$Topic, name = topics$Topic_Legend) %>%
        layout(xaxis = list(title="Year"), yaxis = list(title="Mean Weight"))
    })

    output$emerging_topics <- DT::renderDataTable({
      datatable(full_topics, rownames = FALSE, options = list(
        order = list(list(2, 'asc'))))
    })

    output$pandemics <- renderPlotly({
      plot_ly(pandemic, x = ~ START_YEAR, y = ~ Weight, type = "scatter", mode = "lines+markers", color = pandemic$Topic, name = pandemic$Topic_Legend) %>%
        layout(xaxis = list(title="Year"), yaxis = list(title="Mean Weight"))
    })

    output$pandemics_topics <- DT::renderDataTable({
      datatable(pan_topics, rownames = FALSE, options = list(
        order = list(list(2, 'asc'))))
    })

    output$coronavirus <- renderPlotly({
      plot_ly(corona, x = ~ START_YEAR, y = ~ Weight, type = "scatter", mode = "lines+markers", color = corona$Topic, name = corona$Topic_Legend) %>%
        layout(xaxis = list(title="Year"), yaxis = list(title="Mean Weight"))
    })

    output$coronavirus_topics <- DT::renderDataTable({
      datatable(cor_topics, rownames = FALSE, options = list(
        order = list(list(2, 'asc'))))
    })

    output$optimal_topics <- DT::renderDataTable({
      datatable(opt_topics, rownames = FALSE, options = list(
        order = list(list(3, 'asc'))))
    })
  }
)
