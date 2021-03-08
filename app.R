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
median_sd = function(x, n=1) {
  tibble(y = median(x),
         sd = sd(x),
         ymin = y - n*sd,
         ymax = y + n*sd)
}

se = function(x, ...) {
  sqrt(var(x, ...)/length(x))
}



mean_se = function(x) {
  tibble(y = mean(x), 
         se = se(x),
         ymin = y - se,
         ymax = y + se)
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
hN <- read_csv('/Users/jakelucas/Documents/R_Data/HeadNeck/headNeckApp/head-neck-app/HeadAndNeckCancerReg_DATA_03.08.21.csv') 

#Clean up the event name vector
hN$redcap_event_name %<>% str_replace('_arm_1', '') 

#Removed 'postvisit_1' from dataset since no surveys collected at this time point
hNFiltered <- hN %>%
  filter(!(redcap_event_name == 'postvisit_1')) %>%
  select(c(1:2, 47:73))

#Then applying function to data - based on Sykes email 
hNFiltered$redcap_event_name %<>% as_factor %>%
  fct_relabel(visitNames)

#This recodes and scores the UW QOL Survey Data
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
  )

#Grouped stats summaries of the data for separate geom plots
groupedQolP <- hnQolScored %>%
  group_by(redcap_event_name) %>%
  summarise(uwPhysical = mean(uwPhysical, na.rm = TRUE), .groups = 'keep')

groupedQolS <- hnQolScored %>%
  group_by(redcap_event_name) %>%
  summarise(uwSocial = mean(uwSocial, na.rm = TRUE), .groups = 'keep')

#KEY CODE - Single Record - Will need to make this reactive for app
singleSub <- hnQolScored %>%
  filter(record_id == 258) #'good' example
#filter(record_id == 220) #'bad' example

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


recordIds <- setNames(hnQolScored$record_id, singleSub$record_id)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Head and Neck - UW QOL Data"),
  
  fluidRow(
    column(
      width = 6, 
      selectInput('code', 'Record ID', choices = hnQolScored$record_id)
      )
  ),
  mainPanel(
    width = 12,
    plotOutput("event_pScore")
    ), 
  mainPanel(
    width = 12,
    plotOutput("event_sScore")
  )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  singleSub <- reactive(hnQolScored %>% 
                          filter(record_id == input$code)) 
  output$event_pScore <- renderPlot({
    hnQolScored %>%
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
    hnQolScored %>%
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
