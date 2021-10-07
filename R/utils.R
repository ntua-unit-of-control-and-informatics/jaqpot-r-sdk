
# This script contains helper functions for internal consumption



#########################################################################
### Internal function for logging in japqot. It returns the jwt token ###
#########################################################################

.LoginJaqpot <- function(basepath){
  tryCatch({
      auth.method <- readline("Please choose authentication method ([1]=login / [2]=Provide Api Key): ")
      stopifnot(auth.method %in% c(1,2))
      }, error = function(e){
            e$message <- "Invalid authentication method selected"
            stop(e)
      }
  )
  # If the user uses his Jaqpot credentials:
  if(auth.method == 1){
    # Get username and
    username <- readline("Username: ")
    password <- getPass::getPass(msg = "Password: ", noblank = FALSE, forcemask = FALSE)
    loginto <- paste(basepath, "services/aa/login/", sep = "")
    body <- list(username=username, password = password)
    httr::set_config(httr::config(ssl_verifypeer = 0L))
    
    tryCatch({
    res <-  httr::POST(loginto, body = body, encode = "form")
    stopifnot(httr::status_code(res) < 300)
    res <- httr::content(res, "text", encoding = 'UTF-8')
    authResponse <- jsonlite::fromJSON(res)
    token = authResponse$authToken
    }, error = function(e) {
          e$message <-"http call failed. Make sure you provided the correct username and password."
          stop(e)
    })
  } else if(auth.method == 2) {
    tryCatch({
      API_key <- getPass::getPass(msg = "API Key: ", noblank = FALSE, forcemask = FALSE)
      loginto <- paste(basepath, "services/aa/validate/accesstoken", sep = "")
      httr::set_config(httr::config(ssl_verifypeer = 0L))
      res <-  httr::POST(loginto, body = API_key)
      stopifnot(httr::status_code(res) < 300)
      token = API_key
    }, error = function(e) {
      e$message <-"http call failed. Make sure you provided the correct API key."
      stop(e)
    })
  
  } 
  return(token)
}



#############################################
### Internal function for posting a model ###
#############################################

.PostOnService <- function(basepath, token, json){
  # Create a string representing the authentication method (bearer authentication)
  authentication = paste("Bearer", token, sep=" ")
  url <- paste(basepath, "services/model", sep = "")
  # Post the information to jaqpot
  res = httr::POST(url = url, httr::add_headers(Authorization=authentication),
                   httr::accept_json(), httr::content_type("application/json"), body = json, encode = "json")
  # If the model is successfully uploaded, it will receive the status '200'
  code <- httr::status_code(res)
  if(code == 200 ){
    # Read the response returned by Jaqpot API
    resp <- httr::content(res, "text", encoding = 'UTF-8')
    # Deserialize the JSON object
    respon <- jsonlite::fromJSON(resp)
    response <- paste("Model created. The id is: ", respon$modelId,
                      ". Please visit the application to further document your model.", sep=" ")
    # Inform the user about the success of the upload process and the model id
    response
  } else {
    # If not successful, print the error code
    print(code)
    print("Unsuccessful connection with the Jaqpot server. ")
  }
}


################################################################
### Internal function for getting the information of a model ###
################################################################

.get.model.internal <- function(modelID, token, userID, BasePath){
  # Build the uri to apply get
  uri <- paste(BasePath, "model/", modelID, sep = "")
  # Authentication
  authentication = paste("Bearer", token, sep=" ")
  
  tryCatch({
    res = httr::GET(url = uri, httr::add_headers(Authorization=authentication),  httr::content_type("application/json"))
    stopifnot(httr::status_code(res) < 300)
    res <- httr::content(res, "text", encoding = 'UTF-8')
    Response <- jsonlite::fromJSON(res)
  }, error = function(e) {
    e$message <-"Could not find the model. Make sure you provided the correct model id."
    stop(e)
  })
  return(Response)
}



###################################################
### Internal function for creating a data entry ###
###################################################

.create.data.entry <- function(df, feat_map, userID){
  data_entry <- list()
  for (dataid  in 0:(dim(df)[1]-1)){
    # Get row values from user dataframe
    values_from_dataframe <- df[(dataid+1),]
    
    values <- list()
    for (key in names(feat_map)){
      values[[ as.character(feat_map[[key]])]] <-  values_from_dataframe[[key]][[1]]
    }
    entryId <- .EntryId()
    entryId$name <- as.character(dataid)
   # entryId$ownerUUID <- userID
    d_entry = .DataEntry()
    d_entry$entryId <- entryId
    d_entry$values <- values
    data_entry[[dataid+1]] <- d_entry
  }
  return(data_entry)
}



#########################################################
### Internal function for posting a dataset to Jaqpot ###
#########################################################

.create_dataset_sync <- function(BasePath, token, jsondataset){
  # Build the uri to apply get
  uri <- paste(BasePath, "dataset/", sep = "")
  # Authentication
  authentication = paste("Bearer", token, sep=" ")
  
  tryCatch({
    res = httr::POST(url = uri, httr::add_headers(Authorization=authentication),  
                     httr::content_type("application/json"),  body = jsondataset, encode = "json")
    stopifnot(httr::status_code(res) < 300)
    res <- httr::content(res, "text", encoding = 'UTF-8')
    Response <- jsonlite::fromJSON(res)
  }, error = function(e) {
    e$message <-"Could not post the dataset to Jaqpot. Make sure you provided the correct model id."
    stop(e)
  })
  return(Response)
  
}


#################################
### Internal model prediction ###
#################################

.models.predict <- function(BasePath, token, datasetUri, modelID){
  # Build the uri to apply get
  uri <- paste(BasePath, "model/", modelID, sep = "")
  # Authentication
  authentication = paste("Bearer", token, sep=" ")
  data <- list("dataset_uri" = datasetUri)
  tryCatch({
    res = httr::POST(url = uri, httr::add_headers(Authorization=authentication),  
                     httr::content_type("application/x-www-form-urlencoded"), httr::accept("application/json"), body = data, encode ="form")
    stopifnot(httr::status_code(res) < 300)
    res <- httr::content(res, "text", encoding = 'UTF-8')
    Response <- jsonlite::fromJSON(res)
  }, error = function(e) {
    e$message <-"Could not make a prediction using the given dataset Jaqpot. Make sure the values you provided can
    be used from the model and make sense."
    stop(e)
  })
  return(Response)
  
}


#########################
### Internal task get ###
#########################

.get.task<- function(BasePath, token, taskid){
  # Build the uri to apply get
  uri <- paste(BasePath, "task/", taskid, sep = "")
  # Authentication
  authentication = paste("Bearer", token, sep=" ")
  tryCatch({
    res = httr::GET(url = uri, httr::add_headers(Authorization=authentication),  
                     httr::content_type("application/x-www-form-urlencoded"), httr::accept("application/json"))
    stopifnot(httr::status_code(res) < 300)
    res <- httr::content(res, "text", encoding = 'UTF-8')
    Response <- jsonlite::fromJSON(res)
  }, error = function(e) {
    e$message <-"Could not get information regarding the task status."
    stop(e)
  })
  return(Response)
  
}

############################
### Internal get dataset ###
############################

.get.dataset.internal<- function(BasePath, token, predictedDatasetID){
  # Build the uri to apply get
  uri <- paste(BasePath, "dataset/", predictedDatasetID, sep = "")
  # Authentication
  authentication = paste("Bearer", token, sep=" ")
  params <- list( 'dataEntries'= "true",
                  'rowStart'= "0",
                  'rowMax'= '1000')
  tryCatch({
    res = httr::GET(url = uri, httr::add_headers(Authorization=authentication),  
                    httr::content_type("application/json"), httr::accept("application/json"),query=params)
    stopifnot(httr::status_code(res) < 300)
    res <- httr::content(res, "text", encoding = 'UTF-8')
    Response <- jsonlite::fromJSON(res)
  }, error = function(e) {
    e$message <-"Could not get informationi regarding the selected dataset."
    stop(e)
  })
  return(Response)
  
}



#################################
### Decode prediction dataset ###
#################################

.decode.predicted.dataset<- function(dataset){
  feat_info <- dataset$features
  # Create a matrix to store predictions, with ncol equal to feature number and nrow equal to total instances
  predicted <- matrix(rep(NA,dim(feat_info)[1]*dim(dataset$dataEntry)[1] ), ncol = dim(feat_info)[1] )
  colnames(predicted) <- feat_info$name
  for (j in 1:dim(feat_info)[1]){
    name <- feat_info[j,"name"]
    key <-  feat_info[j,"key"]
    dataEntries <- list()
    for (i in 1:dim(dataset$dataEntry)[1]){
      predicted[i,name] <- dataEntries<- dataset$dataEntry$values[i,colnames(dataset$dataEntry$values)==key]
    }   
  }
  predicted <- as.data.frame(predicted)
  return(predicted)
}



#######################
### Delete dataset ###
######################

.delete.dataset<- function(BasePath, token,datasetId){
  # Build the uri to apply get
  uri <- paste(BasePath, "dataset/", datasetId, sep = "")
  # Authentication
  authentication = paste("Bearer", token, sep=" ")
  res = httr::DELETE(url = uri, httr::add_headers(Authorization=authentication),  
                       httr::content_type("application/x-www-form-urlencoded"), httr::accept("application/json"))
    
}



############################
### Internal get feature ###
############################

.get.feature.internal<- function(BasePath, token, featID){
  # Build the uri to apply get
  uri <- paste(BasePath, "feature/", featID, sep = "")
  # Authentication
  authentication = paste("Bearer", token, sep=" ")
  params <- list( 'dataEntries'= "true",
                  'rowStart'= "0",
                  'rowMax'= '1000')
  tryCatch({
    res = httr::GET(url = uri, httr::add_headers(Authorization=authentication),  
                    httr::content_type("application/json"), httr::accept("application/json"),query=params)
    stopifnot(httr::status_code(res) < 300)
    res <- httr::content(res, "text", encoding = 'UTF-8')
    Response <- jsonlite::fromJSON(res)
  }, error = function(e) {
    e$message <-"Could not get information regarding the selected feature."
    stop(e)
  })
  return(Response)
  
}

