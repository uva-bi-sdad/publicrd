# VA schools Case Study - grant money, NSF FY18

library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinydashboardPlus)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(DT)


# USAspending - NSF FY18 grants
nsf_g18 <- read_rds("usa_nsf_fy18.rds")

VA_SnE <- read_rds("VA_SnE.rds")

# convert to wide data for plots

dollar_comparison <- gather(VA_SnE, source, value, FSS_SnE, USA_SnE, factor_key = TRUE)

# make pct_diff bins for leaflet color of circle markers --------------------------

t1 <- 
  cut(VA_SnE[!is.na(VA_SnE$pct_diff) & VA_SnE$pct_diff < 0, "pct_diff"], 
      breaks = c(-Inf, 0), right = FALSE)
t1 <- as.character(t1)
VA_SnE[!is.na(VA_SnE$pct_diff) & VA_SnE$pct_diff < 0, "bin"] <- t1

t2 <- 
  cut(VA_SnE[!is.na(VA_SnE$pct_diff) & VA_SnE$pct_diff > 0, "pct_diff"], 
      breaks = c(0, Inf), right = TRUE)
t2 <- as.character(t2)
VA_SnE[!is.na(VA_SnE$pct_diff) & VA_SnE$pct_diff > 0, "bin"] <- t2

VA_SnE$bin <- factor(VA_SnE$bin, 
                       levels = c("[-Inf,0)", 
                                  "(0,Inf]")) 

levels(VA_SnE$bin)[levels(VA_SnE$bin) == "(0,Inf]"] <- "FSS > USAspending"                                  
levels(VA_SnE$bin)[levels(VA_SnE$bin) == "[-Inf,0)"] <- "USAspending > FSS" 


# make radius column for leaflet size of circle markers

VA_SnE$rad <- 5
VA_SnE[!is.na(VA_SnE$pct_diff) & abs(VA_SnE$pct_diff) < 10, "rad"] <- 6
VA_SnE[!is.na(VA_SnE$pct_diff) & abs(VA_SnE$pct_diff) > 10 & abs(VA_SnE$pct_diff) < 25, "rad"] <- 8
VA_SnE[!is.na(VA_SnE$pct_diff) & abs(VA_SnE$pct_diff) > 25, "rad"] <- 14

#
# USER INTERFACE ----------------------------------------------------------------------------------------------------
#

ui <- dashboardPage(
  title = "VA Case Study",
  header = dashboardHeader(
    title = "VA Case Study - NSF FY18 S&E Grants",
    titleWidth = 400
  ),
  sidebar = dashboardSidebar(width = "0px"),
  body = dashboardBody(
    
    fluidRow(
      
      boxPlus(
        title = "Virginia Colleges and Universities listed in FSS, table 13",
        width = 8,
        leafletOutput("VA_schools")
      ),
      
      boxPlus(
        title = "S&E: FSS vs USAspending",
        width = 4,
        plotOutput("sne_dollars")
      )
    ),
    
    fluidRow(
      boxPlus(
        title = "USA Spending Grants",
        width = 12,
        dataTableOutput("usa_grants")
      )
      
    )
  )
)


#
# SERVER ----------------------------------------------------------------------------------------------------
#

server <- function(input, output, session) {

  # map events
  
  # observeEvent(input$VA_schools_marker_click, { 
  #   click <- input$VA_schools_marker_click
  #   print(click)
  # })
  
  
  
  # leaflet map for VA universities and colleges ----------------------------
  
  output$VA_schools <- renderLeaflet({
  
    school_labels <- lapply(
      paste("<strong>FSS Institution: </strong>",
            VA_SnE$fss_institution,
            "<br />",
            "<strong>USAspending Institution: </strong>",
            VA_SnE$usa_recipient_name,
            "<br />",
            "<strong>Relative Percent Difference: </strong>",
            round(VA_SnE$pct_diff,2)
            ),
      htmltools::HTML
    )
    
    reds_blues <- c("#B2182B", "#2166AC")    #c("#B2182B", "#D6604D", "#4393C3", "#2166AC")
    pal <- colorFactor(reds_blues, domain = VA_SnE$bin)
  
    leaflet(data = VA_SnE) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(popup = school_labels, label = school_labels, layerId = ~duns_number, 
                       stroke = FALSE,
                       radius = ~rad,
                       color = ~pal(VA_SnE$bin), 
                       opacity = 0.7, fillOpacity = 0.7) %>%
      addLegend("bottomleft", pal = pal, values = ~bin, opacity = 0.7, 
                title = "Relative Percent Difference <br />
                *Circle Size: size of RPD") 
    
  })
  
  
  # plot of FSS vs USAspending for clicked on institution
  
  output$sne_dollars <- renderPlot({
    
    click <- input$VA_schools_marker_click
    
    if(is.null(click)){
      ggplot() +
        ggtitle("Click a map marker!")
    }
    else  #(!is.null(click$id))
    {
      data <- dollar_comparison %>%
        filter(duns_number == click$id)
      
      data$value <- data$value/1000000  # convert to millions
      
      pct_diff_y <- max(data$value)/2
      pct_diff_txt <- data$pct_diff[1]
      
      ggplot(data, aes(x = source, y=value, fill = source)) + 
        geom_bar(stat="identity") +
        geom_text(aes(label = round(value,2), vjust = -0.25)) +
        geom_label(x=1.5, y=pct_diff_y, label=paste0("Pct Diff: ", round(pct_diff_txt,2), "%"), 
                   color = "black", fill="white") +
        ylab("Dollar Amount (millions)") +
        ggtitle(paste0(data$fss_institution, " NSF FY18 Grants"))
    }
    
    })
  
  
  # table of institution grants
  
  output$usa_grants <- renderDataTable({
    
    click <- input$VA_schools_marker_click
    
    if(!is.null(click)){
      
      data <- nsf_g18 %>% 
        filter(recipient_duns == click$id) %>%
        select(recipient_duns, recipient_name, federal_action_obligation, 
               cfda_number, cfda_title, award_description)
      
      DT::datatable(
        data,
        rownames = FALSE,
        options = list( pageLength = 20, scrollX = T, #dom="BlfrtipS", iDisplayLength=-1,
                        columnDefs = list(list(className = 'dt-right', targets = "_all")))
      )
    }
    
  })
  

  }


#
# APP ----------------------------------------------------------------------------------------------------
#

shinyApp(ui, server)