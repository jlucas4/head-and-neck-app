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

#Some functions written for this code 
#Stat functions
median_sd <- function(x, n=1) {
  tibble(y = median(x),
         sd = sd(x),
         ymin = y - n*sd,
         ymax = y + n*sd)
}

se <- function(x, ...) {
  sqrt(var(x, ...)/length(x))
}



mean_se <- function(x) {
  tibble(y = mean(x), 
         se = se(x),
         ymin = y - se,
         ymax = y + se)
}

singleTextValue <- function(df) {
  df[[1]]
}

#Function to rename factor levels for redcap_event_name
visitNames <- function(f) {
  f[f == 'previsit_1'] <- 'First Visit'
  f[f == 'followup_visit_1'] <- 'Follow-up 1'
  f[f == 'followup_visit_2'] <- 'Follow-up 2'
  f[f == 'followup_visit_3'] <- 'Follow-up 3'
  f[!(f %in% c('First Visit', 'Follow-up 1', 'Follow-up 2', 'Follow-up 3'))] <- '>1 year'
  f
}

#Backend data wrangling and cleanup - see hNQol.Rmd for more detail
#Import dataset
hN <- read_csv('HeadAndNeckCancerReg_DATA_03.08.21.csv') 

#Clean up the event name vector
hN$redcap_event_name %<>% str_replace('_arm_1', '') 

#Removed 'postvisit_1' from dataset since no surveys collected at this time point
hNFiltered <- hN %>%
  filter(!(redcap_event_name == 'postvisit_1')) %>%
  select(c(1:2, 47:73, 209:217, 223))

#Create key-value pairs so that primary site values inherit into all visits
hnSiteKey <- hNFiltered %>%
  mutate(
    primarySite = as_factor(
      case_when(
        site___1 == 1 ~ 'Oral Cavity',
        site___2 == 1 ~ 'Oropharynx',
        site___3 == 1 ~ 'Hypopharynx',
        site___4 == 1 ~ 'Larynx',
        site___5 == 1 ~ 'Nasal Cavity',
        site___6 == 1 ~ 'Nasopharynx',
        site___7 == 1 ~ 'Cutaneous',
        site___8 == 1 ~ 'Other',
        site___9 == 1 ~ 'Unknown Primary'
      )
    )
  ) %>%
  select(c(record_id, redcap_event_name, primarySite)) %>%
  pivot_wider(names_from = redcap_event_name, values_from = primarySite) %>%
  select(c(record_id, previsit_1)) %>%
  rename(primarySite = previsit_1)

#Key-Value Pairs for Staging Info
hnStageKey <- hNFiltered %>%
  mutate(
    overallStage = as_factor(
        case_when(
          overall_stage == 1 ~ '0',
          overall_stage == 2 ~ 'I',
          overall_stage == 3 ~ 'II',
          overall_stage == 4 ~ 'III',
          overall_stage == 5 ~ 'IVa',
          overall_stage == 6 ~ 'IVb',
          overall_stage == 7 ~ 'IVc'
        )
      )
    ) %>%
  select(c(record_id, redcap_event_name, overallStage)) %>%
  pivot_wider(names_from = redcap_event_name, values_from = overallStage) %>%
  select(c(record_id, previsit_1)) %>%
  rename(overallStage = previsit_1)

#Then applying function to data
hNFiltered$redcap_event_name %<>% as_factor %>%
  fct_relabel(visitNames)

#This recodes and scores the UW QOL Survey Data - based on Sykes email
hnQolScored <- hNFiltered %>%
  mutate(
    across(
      c(pain, appearance, activity, recreation, mood, compare_before),
      ~ case_when(
        . == 1 ~ 100,
        . == 2 ~ 75, 
        . == 3 ~ 50, 
        . == 4 ~ 25,
        . == 5 ~ 0)
    )
  ) %>%
  mutate(
    across(
      c(swallowing, speech, shoulder, taste, saliva, anxiety),
      ~ case_when(
        . == 1 ~ 100,
        . == 2 ~ 70,
        . == 3 ~ 30,
        . == 4 ~ 0)
    )
  ) %>%
  mutate(
    across(
      chewing, 
      ~ case_when(
        . == 1 ~ 100,
        . == 2 ~ 50,
        . == 3 ~ 0)
    )
  ) %>%
  mutate(
    across(
      c(hr_qol, overall_qol),
      ~ case_when(
        . == 1 ~ 100,
        . == 2 ~ 80,
        . == 3 ~ 60,
        . == 4 ~ 40,
        . == 5 ~ 20,
        . == 6 ~ 0
      )
    )
  ) %>%
  mutate(
    uwPhysical = rowMeans(
      select(.,
             c(chewing, swallowing, speech, taste, saliva, appearance)
      ), na.rm = TRUE
    )
  ) %>%
  mutate(
    uwSocial = rowMeans(
      select(.,
             c(anxiety, mood, pain, activity, recreation, shoulder)
      ), na.rm = TRUE
    )
  ) %>% 
  left_join(hnSiteKey, by = "record_id") %>% 
  left_join(hnStageKey, by = "record_id") 
#Last 2 lines inserts primary site and staging values

#Grouped stats summaries of the data for separate geom plots
groupedQolP <- hnQolScored %>%
  group_by(redcap_event_name) %>%
  summarise(uwPhysical = mean(uwPhysical, na.rm = TRUE), .groups = 'keep')

groupedQolS <- hnQolScored %>%
  group_by(redcap_event_name) %>%
  summarise(uwSocial = mean(uwSocial, na.rm = TRUE), .groups = 'keep')

#KEY CODE - Single Record - Will need to make this reactive for app
singleSub <- hnQolScored %>%
  filter(record_id == 258) %>%
  select(primarySite) %>%
  mutate(across(primarySite, as.character)) %>%
  unique()

#'good' example
#filter(record_id == 220) #'bad' example

#KEY CODE - Filter by site - Need to make this reactive
hNQolScoredSite <- hnQolScored %>%
  filter(primarySite == 'Oral Cavity')

#ggplot items
uwQolPlotPhysical <- ggplot(hnQolScored, aes(redcap_event_name, uwPhysical))

uwQolPlotSocial <- hnQolScored %>%
  ggplot(aes(redcap_event_name, uwSocial))

uwQolPlotPhysical +
  geom_point() +
  geom_line(aes(group = record_id))

#Boxplot
uwQolPlotPhysical +
  geom_boxplot() 
#geom_point(data = groupedQolP, group = 1, color = 'blue', alpha = .8, size = 1) +
#geom_line(data = groupedQolP, group = 1, color = 'blue', alpha = .8, size = 1) 
#geom_line(data = singleSub, group = 1) #Individual patient plotted

#Simple spaghetti looks messy
uwQolPlotPhysical + 
  geom_line(aes(group = record_id))

#Grayed out spaghetti with median and sd bars looks best
uwQolPlotPhysical + 
  geom_line(aes(group = record_id), color = 'gray') + 
  stat_summary(fun.data = median_sd, geom = "errorbar", size = 1, width=0.1) +
  stat_summary(data = groupedQolP, group = 1, fun = median, geom = 'line', size = 1, color = 'black') +
  stat_summary(data = groupedQolP, group = 1, fun = median, geom = 'point', shape = 17, size = 3, color = 'black')

#Above plot with the inclusion of single record to plot against the whole dataset
uwQolPlotPhysical + 
  geom_line(aes(group = record_id), color = 'gray') + 
  stat_summary(fun.data = median_sd, geom = "errorbar", width=0.1) +
  stat_summary(data = groupedQolP, group = 1, fun = median, geom = 'line', color = 'black') +
  stat_summary(data = groupedQolP, group = 1, fun = median, geom = 'point', shape = 17, size = 3, color = 'black') +
  geom_point(data = singleSub, group = 1, color = 'red') + #individual patient
  geom_line(data = singleSub, group = 1, color = 'red', alpha = .5) #individual pt


recordIds <- c(" ", hnQolScored$record_id)
primarySite <- c("All sites", levels(hnQolScored$primarySite))
stages <- c("All stages", levels(hnQolScored$overall_stage))
appGreeting <- 
  withTags({
    div(class = "header",
        p("Thanks for having a look around this work-in-progress application. 
          The plotted output below is based on our head and neck cancer database which has longitudinally
          been collected over the past several years. A small subset of that data is displayed here, 
          based on the", a(href = "http://www.hancsupport.com/sites/default/files/assets/pages/UW-QOL-update_2012.pdf", "University of Washington Quality of Life"),
          "(UW-QOL) questionnaire administered to our patients at each visit."),
        p("Click the sidebars to see two subsets of the survey data - there is a section examining patient-specific 
          subjective physical effects and a section examining social effects. Within each section, you can look at the data as a whole,
          or a subset of the data based on primary tumor site. The default selection is \'All sites\'"),
        p("Finally, each individual patient has a unique de-identified \'record-id\'. Select a record id
          to see the individual patient's data (in red) plotted against the whole dataset - be aware that not all records have plottable data 
          (try out", tags$b("#220"), "and", tags$b("#258"), "to see working examples). The faded gray lines 
          represent individual patients and create a \'sphagetti plot\' in the background. The dark line and triangles
          represent the median of the dataset, and the error bars span 1 standard deviation in either direction.")
    )
  })

########################################Application############################################
########################################User Interface#########################################
# Define UI for application that draws a histogram
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
        selectInput('code', 'Record ID Filter', choices = recordIds, selected = NULL)
      ),
      column(
        width = 4,
        selectInput('site', 'Primary Tumor Site Filter', choices = primarySite[c(1, 2, 4, 6, 7)], selected = primarySite[1])
      ),
      column(
        width = 4,
        selectInput('stage', 'Overall Stage Filter', choices = stages, selected = stages[1])
      )
    ),
    fluidRow(
      column(
        width = 8,
        textOutput("tumorSite", inline = TRUE)
      )
    ),
    fluidRow(tags$p(' ')),
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
  
  hnQolScoredR <- reactive({
    if (input$site != "All sites") {
      hnQolScored %<>% 
        filter(primarySite == input$site)
    } else {
      hnQolScored 
    }
  })
  
  singleSub <- reactive(hnQolScored %>% 
                          filter(record_id == input$code)) 
  
  filterSite <- reactive(
    hnQolScored() %>%
      filter(primarySite == input$site)
  )
  
  primarySiteR <- reactive(
    singleSub() %>%
    filter(record_id == input$code) %>%
    select(primarySite) %>%
    mutate(across(primarySite, as.character)) %>%
    unique() %>%
    singleTextValue()
  )

########################################Reactive Output#######################################
  output$tumorSite <- renderText({
    paste("Selected record primary site:", primarySiteR())
  })
  
  
  output$event_pScore <- renderPlot({
    hnQolScoredR() %>%
      ggplot(aes(redcap_event_name, uwPhysical)) + 
      geom_line(aes(group = record_id), color = 'gray') + 
      stat_summary(fun.data = median_sd, geom = "errorbar", size = 1, width=0.1) +
      stat_summary(data = groupedQolP, group = 1, fun = median, geom = 'line', color = 'black') +
      stat_summary(data = groupedQolP, group = 1, fun = median, geom = 'point', shape = 17, size = 3, color = 'black') +
      geom_point(data = singleSub(), group = 1, size = 2, color = 'red') + #individual patient
      geom_line(data = singleSub(), group = 1, size = 1, color = 'red', alpha = .5) + #individual pt
      ggtitle('UW QOL \'Physical\' Score') +
      labs(x = 'Visit', y = 'Total Score (0 - 100)')
    })
  
  output$event_sScore <- renderPlot({
    hnQolScoredR() %>%
      ggplot(aes(redcap_event_name, uwSocial)) +
      geom_line(aes(group = record_id), color = 'gray') + 
      stat_summary(fun.data = median_sd, geom = "errorbar", size = 1, width=0.1) +
      stat_summary(data = groupedQolS, group = 1, fun = median, geom = 'line', color = 'black') +
      stat_summary(data = groupedQolS, group = 1, fun = median, geom = 'point', shape = 17, size = 3, color = 'black') +
      geom_point(data = singleSub(), group = 1, size = 2, color = 'red') + #individual patient
      geom_line(data = singleSub(), group = 1, size = 1, color = 'red', alpha = .5) + #individual pt
      ggtitle('UW QOL \'Social\' Score') +
      labs(x = 'Visit', y = 'Total Score (0 - 100)')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
