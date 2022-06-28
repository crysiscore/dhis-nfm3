
#--   We need these two libraries                    --#
require(curl)
require(tidyr)

wd <- "/home/agnaldo/Git/dhis/NFM3/nfm3"
setwd(wd)
source('misc_functions.R')

# Login Information
base.url<-"https://mail.ccsaude.org.mz:5595/dhis/"
username<-"Agnaldo.Samuel"
password<-""


#FG Ficha de Notificacao de Casos para Apoio Legal DH NFM3
program.id <- 'VfcIAFV0kZy'  
org.unit <- 'ebcn8hWYrg3'               # CIDADE DE MAPUTO
program.stage.id <- 'KMjYg0iRcIR'       # Notificar o Caso



# DHIS API URL
# orgUnits: Cidade De Maputo: http://mail.ccsaude.org.mz:8080/dhis/api/26/organisationUnits/ebcn8hWYrg3?includeDescendants=true


dhisLogin(username,password,base.url)

# Lista das US  da Cidade de Maputo
#  OrganizationUnit id= ebcn8hWYrg3 (Cidade de Maputo)
unidadesSanitarias <- getOrganizationUnits(base.url,org.unit)

# Todos data Elements do DHIS2
dataElements <- getDataElements(base.url)
dataElements$name <- as.character(dataElements$name)
dataElements$shortName <- as.character(dataElements$shortName)
dataElements$id <- as.character(dataElements$id)



# NFM3 Program stages
# NOTIFICAR O CASO  iD =KMjYg0iRcIR
programStages <- getProgramStages(base.url,program.id)
programStages$name <- as.character(programStages$name)
#programStages$description <- as.character(programStages$description)
programStages$id <- as.character(programStages$id)

# Get all events
events <- getEvents(base.url,org.unit,program.id,program.stage.id)
events$dataElement <- as.character(events$dataElement)
events$programStage <- as.character(events$programStage)
events$dataElement <- sapply(events$dataElement, findDataElementByID)
events$programStage <- sapply(events$programStage, findProgramStageByID)


# Get all events
#events <- getEnrollments(base.url,org.unit,program.id,program.stage.id)


# Drop lastupdate column
events= events[ , - which(names(events) %in% c("lastUpdated"))]
events=spread(events, dataElement, value)

# get TrackedInstances
trackedInstances <- getTrackedInstances(base.url,program.id,org.unit)
trackedInstances = trackedInstances[ , - which(names(trackedInstances) %in% c("Created","Organisation unit",
                                                                            "Tracked entity type","Last updated" ,
                                                                            "Organisation unit name", "Inactive" ))]
#pacientes <- sapply(events$trackedEntityIntance, findDataElementByID)
events <- dplyr::left_join(events,trackedInstances, by=c("trackedEntityIntance"="Instance") )


rm(dataElement,programStages,unidadesSanitarias)
