#' Get the Information of a Jaqpot Model
#'
#' The user can obtain the Information of a Jaqpot Model given the model id.
#'  
#' @param modelID A string containing the Jaqpot id of the model.
#'  
#' @param url The base path of Jaqpot services. This argument is optional and needs.
#' to be changed only if an alternative Jaqpot installation is used.
#' @return All information relevant to the model.
#' @details The user can obtain the information of a model that has been uploaded on Jaqpot by providing 
#' the appropriate model id. Note that that the user should also be the model creator.
#'
#' @examples
#'  \dontrun{
#' #model_info <- get.model.byID(modelID = "Lfz3aBdh4LlJIJxAvNqV")
#' }
#'
#' @export
get.model.byID <- function(modelID, url = 'https://api.jaqpot.org/jaqpot/'){
  
  loginpath <- url
  # Log into Jaqpot using the LoginJaqpot helper function in utils.R and return the jwt json 
  token <- .LoginJaqpot(loginpath)
  BasePath <- paste(loginpath, "services/", sep = "")
  # Split the jwt and retrieve the user id
  userID =   jose::jwt_split(token)$payload$sub
  # Retrive the information stored in the model (gets everything except from the raw model)
  model = .get.model.internal(modelID, token, userID, BasePath)
  
  return(model)
}




####################################################################################################
####################################################################################################

#' Get the Independent Features of a Jaqpot Model
#'
#' The user can obtain the independent features of a Jaqpot Model given the model id.
#'  
#' @param modelID A string containing the Jaqpot id of the model. 
#' @param url The base path of Jaqpot services. This argument is optional and needs 
#' to be changed only if an alternative Jaqpot installation is used.
#' @return  A vector containig the independent features of the model.
#' @details The user can obtain the independent features of a model that has been uploaded on Jaqpot by providing 
#' the appropriate model id. Note that that the user should also be the model creator.
#'
#' @examples
#'  \dontrun{
#' #model_feats <- get.model.feats(modelID = "Lfz3aBdh4LlJIJxAvNqV")
#' }
#'
#' @export
get.model.feats <- function(modelID, url = 'https://api.jaqpot.org/jaqpot/'){
  
  loginpath <- url
  # Log into Jaqpot using the LoginJaqpot helper function in utils.R and return the jwt json 
  token <- .LoginJaqpot(loginpath)
  BasePath <- paste(loginpath, "services/", sep = "")
  # Split the jwt and retrieve the user id
  userID =   jose::jwt_split(token)$payload$sub
  # Retrive the information stored in the model (gets everything except from the raw model)
  model = .get.model.internal(modelID, token, userID, BasePath)
  list_feats <- unlist(mod$additionalInfo$independentFeatures)
  feats <- rep(NA, length(list_feats))
  for(i in 1:length(list_feats)){
    feats[i] <- list_feats[[i]]
  }
  
  return(feats)
}




####################################################################################################
####################################################################################################


#' Get the Information of a Jaqpot Dataset
#'
#' The user can obtain the Information of a Jaqpot dataset given the dataset id.
#'  
#' @param datasetID A string containing the Jaqpot id of the dataset. 
#' @param url The base path of Jaqpot services. This argument is optional and needs 
#' to be changed only if an alternative Jaqpot installation is used.
#' @return All information relevant to the dataset.
#' @details The user can obtain the information of a dataset that has been uploaded on Jaqpot by providing 
#' the appropriate dataset id.Notethat the user should also be the dataset creator.
#'
#' @examples
#'  \dontrun{
#' #dataset_info <- get.dataset.byID(datasetID = "Lfz3aBdh4LlJIJxAvNqV")
#' }
#'
#' @export
get.dataset.byID <- function(datasetID, url = 'https://api.jaqpot.org/jaqpot/'){
  
  loginpath <- url
  # Log into Jaqpot using the LoginJaqpot helper function in utils.R and return the jwt json 
  token <- .LoginJaqpot(loginpath)
  BasePath <- paste(loginpath, "services/", sep = "")
  # Split the jwt and retrieve the user id
  userID =   jose::jwt_split(token)$payload$sub
  # Retrive the information stored in the model (gets everything except from the raw model)
  dataset = .get.dataset.internal(BasePath, token, datasetID)
  
  return(dataset)
}





####################################################################################################
####################################################################################################



#' Get the Features of a Jaqpot dataset
#'
#' The user can obtain the features of a Jaqpot dataset given the dataset id.
#'  
#' @param datasetID A string containing the Jaqpot id of the dataset. 
#' @param url The base path of Jaqpot services. This argument is optional and needs 
#' to be changed only if an alternative Jaqpot installation is used.
#' @return  A vector containig the features of the dataset.
#' @details The user can obtain the features of a dataset that has been uploaded on Jaqpot by providing 
#' the appropriate dataset id. Note that that the user should also be the dataset creator.
#'
#' @examples
#'  \dontrun{
#' #dataset_feats <- get.dataset.feats(datasetID = "Lfz3aBdh4LlJIJxAvNqV")
#' }
#'
#' @export
get.dataset.feats <- function(datasetID, url = 'https://api.jaqpot.org/jaqpot/'){
  
  loginpath <- url
  # Log into Jaqpot using the LoginJaqpot helper function in utils.R and return the jwt json 
  token <- .LoginJaqpot(loginpath)
  BasePath <- paste(loginpath, "services/", sep = "")
  # Split the jwt and retrieve the user id
  userID =   jose::jwt_split(token)$payload$sub
  # Retrive the information stored in the model (gets everything except from the raw model)
  dataset = .get.dataset.internal(BasePath, token, datasetID)
  feats <- dataset$features[,1]
  
  return(feats)
}





####################################################################################################
####################################################################################################


#' Get the Information of a Jaqpot Feature
#'
#' The user can obtain the Information of a Jaqpot feature given the feature id.
#'  
#' @param featID A string containing the Jaqpot id of the feature. 
#' @param url The base path of Jaqpot services. This argument is optional and needs 
#' to be changed only if an alternative Jaqpot installation is used.
#' @return All information relevant to the feature.
#' @details The user can obtain the information of a feature that is related to a dataset/model that has been uploaded on Jaqpot by providing 
#' the appropriate feature id.Notethat the user should also be the feature creator.
#'
#' @examples
#'  \dontrun{
#' #feature_info <- get.feature.byID(featID = "Lfz3aBdh4LlJIJxAvNqV")
#' }
#'
#' @export
get.feature.byID <- function(featID, url = 'https://api.jaqpot.org/jaqpot/'){
  
  loginpath <- url
  # Log into Jaqpot using the LoginJaqpot helper function in utils.R and return the jwt json 
  token <- .LoginJaqpot(loginpath)
  BasePath <- paste(loginpath, "services/", sep = "")
  # Split the jwt and retrieve the user id
  userID =   jose::jwt_split(token)$payload$sub
  # Retrive the information stored in the model (gets everything except from the raw model)
  feature = .get.feature.internal(BasePath, token, featID)

  return(feature)
}





####################################################################################################
####################################################################################################


#' Get a Number of Models Uploaded On Jaqpot
#'
#' The user can obtain a number of models uploaded on Jaqpot.
#'  
#'  @param min The minimum number of models to return.
#' @param max The maximum number of models to return.
#' @param url The base path of Jaqpot services. This argument is optional and needs 
#' to be changed only if an alternative Jaqpot installation is used.
#' @return All information of the upoaded models.
#' @details The user can obtain the information of a number of models that have been uploaded on Jaqpot.
#' Notethat the user should also be the model creator for all models.
#'
#' @examples
#'  \dontrun{
#' #get_models <- get.my.models()
#' }
#'
#' @export
get.my.models <- function(min = 0, max = 10, url = 'https://api.jaqpot.org/jaqpot/'){
  
  loginpath <- url
  # Log into Jaqpot using the LoginJaqpot helper function in utils.R and return the jwt json 
  token <- .LoginJaqpot(loginpath)
  BasePath <- paste(loginpath, "services/", sep = "")
  # Split the jwt and retrieve the user id
  userID =   jose::jwt_split(token)$payload$sub
  # Build the uri to apply get
  uri <- paste(BasePath, "model/", sep = "")
  # Authentication
  authentication = paste("Bearer", token, sep=" ")
  params <- list( 'min'= min, 'max'= max)
    tryCatch({
      res = httr::GET(url = uri, httr::add_headers(Authorization=authentication),  
                      httr::content_type("application/json"), httr::accept("application/json"),query=params)
      stopifnot(httr::status_code(res) < 300)
      res <- httr::content(res, "text", encoding = 'UTF-8')
      Response <- jsonlite::fromJSON(res)
    }, error = function(e) {
      e$message <-"Could not get information regarding the users models."
      stop(e)
    })
    return(Response)
    
}




####################################################################################################
####################################################################################################


#' Get a Number of Datasets Uploaded On Jaqpot
#'
#' The user can obtain a number of datasets uploaded on Jaqpot.
#'  
#'  @param min The minimum number of datasets to return.
#' @param max The maximum number of datasets to return.
#' @param url The base path of Jaqpot services. This argument is optional and needs 
#' to be changed only if an alternative Jaqpot installation is used.
#' @return All information of the upoaded datasets
#' @details The user can obtain the information of a number of datasets that have been uploaded on Jaqpot.
#' Notethat the user should also be the dataset creator for all models.
#'
#' @examples
#'  \dontrun{
#' #get_datasets <- get.my.datasets()
#' }
#'
#' @export
get.my.datasets <- function(min = 0, max = 10, url = 'https://api.jaqpot.org/jaqpot/'){
  
  loginpath <- url
  # Log into Jaqpot using the LoginJaqpot helper function in utils.R and return the jwt json 
  token <- .LoginJaqpot(loginpath)
  BasePath <- paste(loginpath, "services/", sep = "")
  # Split the jwt and retrieve the user id
  userID =   jose::jwt_split(token)$payload$sub
  # Build the uri to apply get
  uri <- paste(BasePath, "dataset/", sep = "")
  # Authentication
  authentication = paste("Bearer", token, sep=" ")
  params <- list( 'min'= min, 'max'= max)
  tryCatch({
    res = httr::GET(url = uri, httr::add_headers(Authorization=authentication),  
                    httr::content_type("application/json"), httr::accept("application/json"),query=params)
    stopifnot(httr::status_code(res) < 300)
    res <- httr::content(res, "text", encoding = 'UTF-8')
    Response <- jsonlite::fromJSON(res)
  }, error = function(e) {
    e$message <-"Could not get information regarding the users datasets."
    stop(e)
  })
  return(Response)
  
}



####################################################################################################
####################################################################################################


#' Get a Number of Models Belonging to a Certain Organisation
#'
#' The user can obtain a number of models that have been uploaded on Jaqpot and belong to a specific Organisation that the user is part of.
#'  
#' @param orgID A string containing the Jaqpot id of the organisation.
#' @param min The minimum number of models to return.
#' @param max The maximum number of models to return.
#' @param url The base path of Jaqpot services. This argument is optional and needs 
#' to be changed only if an alternative Jaqpot installation is used.
#' @return All information of the upoaded models of the selected organisation.
#' @details The user can obtain the information of a number of models that have been uploaded on Jaqpot and belong to a specific organisation.
#' Notethat the user should be part of the organisation .
#'
#' @examples
#'  \dontrun{
#' #get_org_models <- get.orgs.models()
#' }
#'
#' @export
get.orgs.models <- function(orgID, min = 0, max = 10, url = 'https://api.jaqpot.org/jaqpot/'){
  
  loginpath <- url
  # Log into Jaqpot using the LoginJaqpot helper function in utils.R and return the jwt json 
  token <- .LoginJaqpot(loginpath)
  BasePath <- paste(loginpath, "services/", sep = "")
  # Split the jwt and retrieve the user id
  userID =   jose::jwt_split(token)$payload$sub
  # Build the uri to apply get
  uri <- paste(BasePath, "model/", sep = "")
  # Authentication
  authentication = paste("Bearer", token, sep=" ")
  params <- list( 'min'= min, 'max'= max, 'organization' = orgID)
  tryCatch({
    res = httr::GET(url = uri, httr::add_headers(Authorization=authentication),  
                    httr::content_type("application/json"), httr::accept("application/json"),query=params)
    stopifnot(httr::status_code(res) < 300)
    res <- httr::content(res, "text", encoding = 'UTF-8')
    Response <- jsonlite::fromJSON(res)
  }, error = function(e) {
    e$message <-"Could not get information regarding the specific organisation"
    stop(e)
  })
  return(Response)
  
}



####################################################################################################
####################################################################################################


#' Get a Number of Models of a Specific Tag that Belong to a Specific Organisation
#'
#' The user can obtain a number of models that have been uploaded on Jaqpot, contain a specific tag and belong to a certain organisation.
#'  
#' @param orgID A string containing the Jaqpot id of the organisation.
#' @param tag A string containing the model tag.
#' @param min The minimum number of models to return.
#' @param max The maximum number of models to return.
#' @param url The base path of Jaqpot services. This argument is optional and needs 
#' to be changed only if an alternative Jaqpot installation is used.
#' @return All information of the upoaded models that contain the selected tag and belong to the selected organisation.
#' @details The user can obtain the information of a number of models that have been uploaded on Jaqpot, contain a specific tag and belong to a specific organisation.
#' Notethat the user should be part of the organisation .
#'
#' @examples
#'  \dontrun{
#' #get_tagandOrg_models <- get.orgs.models.byTag()
#' }
#'
#' @export
get.orgs.models.byTag <- function(orgID, tag, min = 0, max = 10, url = 'https://api.jaqpot.org/jaqpot/'){
  
  loginpath <- url
  # Log into Jaqpot using the LoginJaqpot helper function in utils.R and return the jwt json 
  token <- .LoginJaqpot(loginpath)
  BasePath <- paste(loginpath, "services/", sep = "")
  # Split the jwt and retrieve the user id
  userID =   jose::jwt_split(token)$payload$sub
  # Build the uri to apply get
  uri <- paste(BasePath, "model/", sep = "")
  # Authentication
  authentication = paste("Bearer", token, sep=" ")
  params <- list( 'min'= min, 'max'= max, 'tag'  = tag, 'organization'  = orgID)
  tryCatch({
    res = httr::GET(url = uri, httr::add_headers(Authorization=authentication),  
                    httr::content_type("application/json"), httr::accept("application/json"),query=params)
    stopifnot(httr::status_code(res) < 300)
    res <- httr::content(res, "text", encoding = 'UTF-8')
    Response <- jsonlite::fromJSON(res)
  }, error = function(e) {
    e$message <-"Could not get information regarding the specific tag and organisation."
    stop(e)
  })
  return(Response)
  
}



