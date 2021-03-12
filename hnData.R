#Head and neck data cleanup - see hNQol.Rmd for more detail

#Functions used for cleaning
source('rFunctions.R', local = TRUE)

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
        overall_stage == 5 ~ 'IV',
        overall_stage == 6 ~ 'IV',
        overall_stage == 7 ~ 'IV'
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
  filter(primarySite == "Oral Cavity") %>%
  summarise(uwPhysical = median(uwPhysical, na.rm = TRUE), .groups = 'keep')

groupedQolS <- hnQolScored %>%
  group_by(redcap_event_name) %>%
  summarise(uwSocial = median(uwSocial, na.rm = TRUE), .groups = 'keep')

#KEY CODE - Single Record - Will need to make this reactive for app
singleSub <- hnQolScored %>%
  filter(record_id == 258) %>%
  select(primarySite) %>%
  mutate(across(primarySite, as.character)) %>%
  unique()

#'good' example
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
  stat_summary(data = groupedQolP, group = 1, geom = 'line', color = 'black') +
  stat_summary(data = groupedQolP, group = 1, geom = 'point', shape = 17, size = 3, color = 'black') +
  geom_point(data = singleSub, group = 1, color = 'red') + #individual patient
  geom_line(data = singleSub, group = 1, color = 'red', alpha = .5) #individual pt

