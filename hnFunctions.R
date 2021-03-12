#Some functions written for the head and neck app 

#Stat functions
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
