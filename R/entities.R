#######################################################
### Create a constructor for meta information class ###
#######################################################

.MetaInfo <- function(){
  
  info <- list(
   # identifiers = NULL,
  #  comments = NULL,
   # descriptions = NULL,
  #  titles = NULL,
  #  subjects = NULL,
  #  publishers = NULL,
    creators = NULL
  #  contributors = NULL,
  #  audiences = NULL,
  #  rights = NULL,
  #  sameAs = NULL,
  #  seeAlso = NULL,
  #  hasSources = NULL,
  #  doi = NULL,
  #  date = NULL
  )

  return(info)
}


##############################################
### Create a constructor for DataSet class ###
##############################################

.Dataset <- function(){
  
  info <- list(
   meta = NULL,
  # ontologicalClasses = NULL,
  # visible = NULL,
  # temporary = NULL,
  # featured = NULL,
  # datasetUri = NULL,
  # byModel = NULL,
   dataEntry = NULL,
   features = NULL,
   totalRows = NULL,
   totalColumns = NULL
   #descriptors = NULL,
  # id = NULL,
  # existence = NULL
  )
  return(info)
}


##################################################
### Create a constructor for FeatureInfo class ###
#################################################

.FeatureInfo <- function(){
  
  info <- list(
   name = NULL,
   #units = NULL,
   #conditions = NULL,
   #category = NULL,
   key = NULL,
   uri = NULL
  )
  return(info)
}


##############################
### Create an EntryId list ###
##############################

.EntryId <- function(){
  
  info <- list(
   name = NULL
  # ownerUUID = NULL
   #URI = NULL,
   #type = NULL
  )
  return(info)
}


##############################
### Create an data entry list ###
##############################

.DataEntry <- function(){
  
  info <- list(
    entryId = NULL,
    values = NULL
  )
  return(info)
}