#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(rsconnect)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggsci)
library(magrittr)
library(likert)
library(grid)

source('hnFunctions.R', local = TRUE)
source('hnData.R', local = TRUE)
source('hnObjects.R', local = TRUE)

########################################Application############################################
########################################User Interface#########################################
# Define UI for app
ui <- dashboardPage(
  # dashboard titel
  dashboardHeader(
    title = "Head and Neck Database App",
    titleWidth = 450
    ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("\'Physical\' Score", tabName = "dashboardP", icon = icon("object-align-bottom", lib = "glyphicon")),
      menuItem("\'Social\' Score", tabName = "dashboardS", icon = icon("hand-holding-medical"))
    )
  ),
  dashboardBody(
    titlePanel("Head and Neck - UW QOL Data"),
    appGreeting,
    fluidRow(
      column(
        width = 4,
        selectInput('code', 'Record ID Filter', choices = recordIds, selected = recordIds[1])
      ),
      column(
        width = 4,
        selectInput('site', 'Primary Tumor Site Filter', choices = primarySite, selected = primarySite[1])
      ),
      column(
        width = 4,
        selectInput('stage', 'Overall Stage Filter', choices = stages[2:6], selected = stages[2])
      )
    ),
    fluidRow(
      column(
        width = 8,
        p("For Selected Record...")
      )
    ),
    fluidRow(
      column(
        width = 8,
        textOutput("tumorSite", inline = TRUE)
      )
    ),
    fluidRow(
      column(
        width = 8,
        textOutput("tumorStage", inline = TRUE)
      )
    ),
    br(),
    #Adding tab content
    fluidRow(
      tabItems(
        tabItem(tabName = "dashboardP",
            box(
              width = 12,
              plotOutput("event_pScore")
            )),
        tabItem(tabName = "dashboardS",
            box(
              width = 12,
              plotOutput("event_sScore")
            ))
    ))
  ))



########################################Reactive Data########################################
server <- function(input, output) {
  
  ##The below two conditionals are essentially nested filter functions for the output.
  ##The plots calculate the filteredStageR value which is calculated from the 
  ##filteredSiteR value which is calculated from the dataset hnQolScored.
  filteredSiteR <- reactive({
    if (input$site == "All sites") {
      hnQolScored 
    } else {
      hnQolScored %>% 
        filter(primarySite == input$site)
    }
  })
  
  filteredStageR <- reactive({
    if (input$stage == "All stages") {
      filteredSiteR()
    } else {
      filteredSiteR() %>%
        filter(overallStage == input$stage)
    }
  })
  
  ######################Summary Objects###########################
  groupedQol_siteR <- reactive({
    if (input$site == "All sites") {
      hnQolScored %>%
        group_by(redcap_event_name)
    } else {
      hnQolScored %>%
        group_by(redcap_event_name) %>%
        filter(primarySite == input$site)
    }
  })
  
  groupedQol_stageR <- reactive({
    if (input$stage == "All stages") {
      groupedQol_siteR() 
    } else {
      groupedQol_siteR() %>%
        filter(overallStage == input$stage)
    }
  })
  
  groupedQolPR <- reactive({
    groupedQol_stageR() %>%
      summarise(uwPhysical = median(uwPhysical, na.rm = TRUE), .groups = 'keep')
  })
  
  groupedQolSR <- reactive({
    groupedQol_stageR() %>%
      summarise(uwSocial = median(uwSocial, na.rm = TRUE), .groups = 'keep')
  })
  
  ################################################################
  
  singleSub <- reactive(hnQolScored %>% 
                          filter(record_id == input$code)) 
  
  filterSite <- reactive(
    hnQolScored() %>%
      filter(primarySite == input$site)
  )
  
  filterStage <- reactive(
    hQolScored() %>%
      filter(overallStage == input$stage)
  )
  
  primarySiteR <- reactive(
    singleSub() %>%
    filter(record_id == input$code) %>%
    select(primarySite) %>%
    mutate(across(primarySite, as.character)) %>%
    unique() %>%
    singleTextValue()
  )
  
  primaryStageR <- reactive(
    singleSub() %>%
      filter(record_id == input$code) %>%
      select(overallStage) %>%
      mutate(across(overallStage, as.character)) %>%
      unique() %>%
      singleTextValue()
  )

########################################Reactive Output#######################################
  output$tumorSite <- renderText({
    paste("Primary site:", primarySiteR())
  })
  
  output$tumorStage <- renderText({
    paste("Overall stage:", primaryStageR())
  })
  
  output$event_pScore <- renderPlot({
    filteredStageR() %>%
      ggplot(aes(redcap_event_name, uwPhysical)) + 
      scale_y_continuous(
        limits = c(0, 105)
      ) +
      geom_line(aes(group = record_id), color = 'gray') + 
      stat_summary(fun.data = median_sd, geom = "errorbar", size = 1, width=0.1) +
      stat_summary(data = groupedQolPR(), group = 1, fun = median, na.rm = TRUE, geom = 'line', color = 'black') +
      stat_summary(data = groupedQolPR(), group = 1, fun = median, na.rm = TRUE, geom = 'point', shape = 17, size = 3, color = 'black') +
      geom_point(data = singleSub(), group = 1, size = 2, color = 'red') + #individual patient
      geom_line(data = singleSub(), group = 1, size = 1, color = 'red', alpha = .5) + #individual pt
      ggtitle('UW QOL \'Physical\' Score') +
      labs(x = 'Visit', y = 'Total Score (0 - 100)')
    })
  
  output$event_sScore <- renderPlot({
    filteredStageR() %>%
      ggplot(aes(redcap_event_name, uwSocial)) +
      scale_y_continuous(
        limits = c(0, 105)
      ) +
      geom_line(aes(group = record_id), color = 'gray') + 
      stat_summary(fun.data = median_sd, geom = "errorbar", size = 1, width=0.1) +
      stat_summary(data = groupedQolSR(), group = 1, fun = median, na.rm = TRUE, geom = 'line', color = 'black') +
      stat_summary(data = groupedQolSR(), group = 1, fun = median, na.rm = TRUE, geom = 'point', shape = 17, size = 3, color = 'black') +
      geom_point(data = singleSub(), group = 1, size = 2, color = 'red') + #individual patient
      geom_line(data = singleSub(), group = 1, size = 1, color = 'red', alpha = .5) + #individual pt
      ggtitle('UW QOL \'Social\' Score') +
      labs(x = 'Visit', y = 'Total Score (0 - 100)')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
