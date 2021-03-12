#Objects used in app

#Some HTML code for the front page
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
          or a subset of the data based on the patient\'s primary tumor site and stage. The default selection is \'All sites\' and 
          \'All stages\'."),
        p("Finally, each individual patient has a unique de-identified \'record-id\'. Select a record id
          to see the individual patient's data (in red) plotted against the whole dataset - be aware that not all records have plottable data 
          (try out", tags$b("#220"), "and", tags$b("#258"), "to see working examples). When selected, the application will tell you
          the primary tumor site and the stage for a given record. The faded gray lines 
          represent individual patients and create a \'sphagetti plot\' in the background. The dark line and triangles
          represent the median of the dataset, and the error bars span 1 standard deviation in either direction.")
    )
  })

#Selections for filters
recordIds <- c("All records", hnQolScored$record_id)
primarySite <- c("All sites", levels(hnQolScored$primarySite))
stages <- sort(c("All stages", levels(hnQolScored$overallStage)))