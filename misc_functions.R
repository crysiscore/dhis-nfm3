# Helper Links

# working with json https://www.tutorialspoint.com/r/r_json_files.htm
# http://mail.ccsaude.org.mz:8080/dhis/api/events/query.json?orgUnit=ebcn8hWYrg3&program=VF4oiWHRMr3&programStage=uUiG6lVDpPa&ouMode=DESCENDANTS

require(httr)
require(magrittr)
library(plyr)


#############################################  Helper fuctions     ####################################################


dhisLogin <- function(username, password, base.url) {
  url <- paste0(base.url, "api/me")
  r <- GET(url, authenticate(username, password))
  if (r$status == 200L) {
    print("Logged in successfully!")
  } else {
    print("Could not login")
  }
}

getDataElements <- function(base.url) {
  url <-
    paste0(base.url,
           "api/dataElements?fields=id,name,shortName&paging=false")
  r <- content(GET(url, authenticate(username, password)), as = "parsed")
  do.call(rbind.data.frame, r$dataElements)
}


getOrganizationUnits <- function(base.url, location_id) {
  ## location pode ser distrito , provincia
  url <-
    paste0(
      base.url,
      paste0(
        "api/organisationUnits/",
        location_id,
        "?includeDescendants=true&level=3&fields=id,name,shortName&paging=false"
      )
    )
  r <- content(GET(url, authenticate(username, password)), as = "parsed")
  do.call(rbind.data.frame, r$organisationUnits)
}

getProgramStages <- function(base.url, program.id) {
  ## location pode ser distrito , provincia
  url <-
    paste0(
      base.url,
      paste0(
        "api/programs/",
        program.id,
        "/programStages?fields=id,name,description"
      )
    )
  r <- content(GET(url, authenticate(username, password)), as = "parsed")
  do.call(rbind.data.frame, r$programStages)
}



getTrackedInstances <- function(base.url, program.id,org.unit) {
  ## location pode ser distrito , provincia
  url <-
    paste0(
      base.url,
      paste0(
        "api/trackedEntityInstances/query.json?ou=",
        org.unit,
        "&program=",
        program.id,
        "&ouMode=DESCENDANTS"
      )
    )
  r <- content(GET(url, authenticate(username, password)), as = "parsed")
  df_temp_names <- do.call(rbind.data.frame, r$headers)
  header <- as.character(df_temp_names$column)
  df_temp_instances <- do.call(rbind.data.frame, r$rows)
  names(df_temp_instances) <-header
  df_temp_instances
}

getEvents <- function(base.url,org.unit,program.id, program.stage.id){
  url <-
    paste0(base.url,
           paste0(
             "api/events.json?orgUnit=",
             org.unit,
             '&program=',
             program.id,
             '&programStage=',
             program.stage.id,
             "&ouMode=DESCENDANTS&skipPaging=true"
           )
    )
  
  # Get the data
  r2 <- content(GET(url, authenticate(username, password)),as = "parsed")
  
  if(typeof(r2)=="list" && length(r2$events)>0) {
    
    # primeiro evento da lista com maior  nr de colunas
    index = which.max(lengths(r2$events, use.names = TRUE))
    event_metadata_col_names <- names(r2$events[[index]])
    event_values_col_names   <- names(r2$events[[index]]$dataValues[[1]])
    
    # Quantidade de variaveis de cada evento
    length(r2$events[[index]]$dataValues)
    #df_events_col_names <- c(event_metadata_names, event_values_col_names)
    
    # inicializar o df 
    df_event_values <- do.call(rbind.data.frame,r2$events[[1]]$dataValues)
    df_event_values <- df_event_values[1,]
    df_event_values$storedBy <- ""
    df_event_values$programStage <- ""
    df_event_values$status <- ""
    df_event_values$created <- ""
    #df_event_values$notes <- ""
    df_event_values$dueDate <- ""
    df_event_values$orgUnit <- ""
    df_event_values$orrgUnitName <- ""
    df_event_values$program <- ""
    df_event_values$trackedEntityIntance <- ""
    df_event_values$eventDate <- ""
    df_event_values$deleted <- ""
    df_event_values$href <- ""
    df_event_values$enrollment <- ""
    df_event_values$attributeCategoryOptions <- ""
    df_event_values$attributeOptionCombo <- ""
    df_event_values$event <- ""
    df_event_values$enrollmentStatus <- ""
    df_event_values <- df_event_values[0,]
    
    #  Junta todos  dataValues  de todos  eventos
    
    for (index in 1:length(r2$events)) {
      
      if(length(r2$events[[index]]$dataValues)>0) {
        
        temp <-  do.call(rbind.data.frame,r2$events[[index]]$dataValues)
        
        
        temp$storedBy <-r2$events[[index]]$storedBy
        temp$programStage <-r2$events[[index]]$programStage
        temp$status <- r2$events[[index]]$status
        temp$created <- r2$events[[index]]$created
        
        # Existem eventos sem notas
        #if(length(r2$events[[index]]$notes)>0){
        #
        #  temp$notes <- r2$events[[index]]$notes
        #}
        
        temp$dueDate <- r2$events[[index]]$dueDate
        temp$orgUnit <- r2$events[[index]]$orgUnit
        temp$orrgUnitName <- r2$events[[index]]$orgUnitName
        temp$program <- r2$events[[index]]$program
        temp$trackedEntityIntance <- r2$events[[index]]$trackedEntityInstance
        temp$eventDate <- r2$events[[index]]$eventDate
        temp$deleted <- r2$events[[index]]$deleted
        temp$href <- r2$events[[index]]$href
        temp$enrollment <- r2$events[[index]]$enrollment
        temp$attributeCategoryOptions <- r2$events[[index]]$attributeCategoryOptions
        temp$attributeOptionCombo <- r2$events[[index]]$attributeOptionCombo
        temp$event <- r2$events[[index]]$event
        temp$enrollmentStatus <- r2$events[[index]]$enrollmentStatus
        
        
        df_event_values <- rbind.fill(df_event_values, temp)
      }
    }
    
  }
  
  df_event_values
  
  
}



findDataElementByID <- function(id){
  
  dataElement <- dataElements[which(dataElements$id==id),]
  as.character(dataElement$name)
}


findTrackedInstanceByID <- function(id){
  
  trackedInstance <- trackedInstance[which(trackedInstances$Instance==id),]
  as.character(trackedInstance$name)
}


findProgramStageByID <- function(id){
  
  stage <- programStages[which(programStages$id==id),]
  as.character(stage$name)
}


calc_age <- function(birthDate, refDate = Sys.Date()) {
  
  require(lubridate)
  
  period <- as.period(interval(birthDate, refDate),
                      unit = "year")
  
  period$year
  
} 