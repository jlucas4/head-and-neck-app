#Some functions written for the head and neck app 

#Stat functions
#Median + standard deviation for app. N = number of sds. Max does not go above 100
median_sd <- function(x, n=1, na.rm = TRUE) {
  tibble(y = median(x),
         sd = sd(x),
         ymin = y - n*sd,
         ymax = if (y + n*sd > 100) {
           100
         } else {
           y + n*sd
         })
}

#Standard error. Not currently used in app.
se <- function(x, ...) {
  sqrt(var(x, ...)/length(x))
}


#Mean + se. Not currently used in app.
mean_se <- function(x) {
  tibble(y = mean(x), 
         se = se(x),
         ymin = y - se,
         ymax = y + se)
}

#This function reduces a 1x1 dataframe down to a single value. It's a solution for 
#the textOutput in the app which takes a single value and displays it. Probably a 
#better way to do this... maybe the 'pluck' function? Regardless it works.
singleTextValue <- function(df) {
  df[[1]]
}

#Function to rename factor levels for redcap_event_name. All followups after
#Follow-up 3 are currently rolled in a '>1 year' bucket. Can alter this if needed.
visitNames <- function(f) {
  f[f == 'previsit_1'] <- 'First Visit'
  f[f == 'followup_visit_1'] <- 'Follow-up 1'
  f[f == 'followup_visit_2'] <- 'Follow-up 2'
  f[f == 'followup_visit_3'] <- 'Follow-up 3'
  f[!(f %in% c('First Visit', 'Follow-up 1', 'Follow-up 2', 'Follow-up 3'))] <- '>1 year'
  f
}

#Custom selectInput that accepts applicable arguments - shortens UI code
hnSelectInput <- function(id, label, choices, selected) {
  selectInput(id, label, choices = choices, selected = selected)
}
#Custom column function to de-duplicate selectInput code
hnFilterColumn <- function(f, width = 4) {
  column(
    width = 4,
    f
  )
}
#Custom row/column function for text output
hnTextRow <- function(f, width = 8) {
  fluidRow(
    column(
      width = 8,
      f
    )
  )
}

hnTabItem <- function(name, output, width = 12) {
  tabItem(tabName = name,
          box(
            width = 12,
            plotOutput(output)
          )
  )
}

#single record selection function
filter01 <- function(df, var, value) {
  df %>%
    filter({{var}} == {{value}})
}