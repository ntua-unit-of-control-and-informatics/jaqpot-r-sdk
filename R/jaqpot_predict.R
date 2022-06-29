#' Obtain Predictions from a Jaqpot Model
#'
#' The user can provide a dataset and obtain predictions from an uploaded model given the model id.
#'  
#' @param df A dataframe containing the prediction dataset. The column names of the dataset
#' should be identical to the independent features of the model.
#' @param modelID The Jaqpot id of the model
#' @param url The base path of Jaqpot services. This argument is optional and needs 
#' to be changed only if an alternative Jaqpot installation is used.
#' @return  Model predictions for the provided dataset.
#' @details The user can obtain predictions from a model that has been uploaded on Jaqpot by providing a dataset 
#'  and the appropriate model id.
#'
#' @examples
#'  \dontrun{
#'  #df <- data.frame(x1 = c(1,2), x2 = c(0.5,0.5))
#'  #df$x3 <- list(c(1,1), c(2,2))
#'  #jaqpot.predict(df = df, modelID = "Lfz3aBdh4LlJIJxAvNqV")
#' }
#'
#' @export
#' 
jaqpot.predict <- function( df, modelID, url = 'https://api.jaqpot.org/jaqpot/'){
  
  # Make sure that the user provides a data frame
  if (class(df) != "data.frame"){
    stop("Please provide an object of class 'list'.")
  }
  
  # Make sure that the modelID is a string
  if (typeof(modelID) != "character"){
    stop("The modelID should be a string ('charachter' type).")
  }
  
  ##########
  ########
  #### Handling of NA values to be placed here
  ##
  #
  
  
  
  loginpath <- url
  # Log into Jaqpot using the LoginJaqpot helper function in utils.R and return the jwt json 
  token <- jaqpot.token
  BasePath <- paste(loginpath, "services/", sep = "")
  # Split the jwt and retrieve the user id
  userID =   jose::jwt_split(token)$payload$sub
  # Retrive the information stored in the model (gets everything except from the raw model)
  model = .get.model.internal(modelID, token, userID, BasePath)
  
  # Check if the model is pbpk
  if  ("ot:PBPK" %in% model$algorithm$ontologicalClasses){
    is.pbpk <- TRUE
  }else{
    is.pbpk <- FALSE
  }
  
  #Check if the user uses the correct names
  # Retrieve the independent feature names of the model and check if the user provided the same names
  model_names<- rep(NA,length(model$additionalInfo$independentFeature))
  for(i in 1:length(model$additionalInfo$independentFeature)){
    model_names[i] <- model$additionalInfo$independentFeature[[i]]
  }
  user_names<- colnames(df)
  if(length(setdiff(user_names, model_names)) > 0){
    stop("Please make sure you named the elements of your list according to the model independent feature names. Consider using get_model_features()")
  }
  
  # Create a list to store the dataentries
  feat_map <- list()
  # Create a list to store the feature name, key and uri
  features <- list()
  # Initialise a key counter
  key_counter <- 0
  
  print("--> Checking the dataset..")
  
  for(i in 1:length(model$additionalInfo$independentFeatures)){
    # Retrieve the feature URI
    feat_uri  <-names(model$additionalInfo$independentFeatures[i])
    # Retrieve the feature name, i.e. the value related to the corresponding uri
    value <- model$additionalInfo$independentFeatures[[i]]
    # Store in the list the mapping between key counter and value
    feat_map[value] <- key_counter
    #Initialise a FeatureInfo object
    feat_info  <- .FeatureInfo()
    # Update the FeatureInfo object attributes
    feat_info$uri =  as.character(feat_uri)
    feat_info$name =  as.character(value)
    feat_info$key =  as.character(key_counter)
    features[[key_counter+1]] <- feat_info
    key_counter <- key_counter + 1
  }
  
  
  dataset <- .Dataset()
  meta <- .MetaInfo() 
  
  meta$creators <-  list(userID)
  dataset$meta <- meta
  dataset$totalRows <- dim(df)[1]
  dataset$totalColumns <- dim(df)[2]
  
  data_entry <- .create.data.entry(df, feat_map, userID)
  dataset$dataEntry <- data_entry
  dataset$features <- features
  
  #Convert dataset to json
  jsondataset <- jsonlite::toJSON(dataset, auto_unbox = T)
  
  print("--> Sending the dataset to Jaqpot..")
  
  #Post dataset to jaqpot and obtain a dataset id
  dataset_n <- .create_dataset_sync(BasePath, token, jsondataset)
  datasetId = dataset_n$`_id`
  datasetUri = paste(BasePath, "dataset/", datasetId, sep = "")
  
  print("--> Initialising prediction..")
  
  task = .models.predict(BasePath, token, datasetUri, modelID)
  percentage = 0
  taskid = task$`_id`
  #Initialise a progress bar
  pb <- txtProgressBar(min = 0, max = 100, style = 3)
  #Stop when the percentage of the task is finished, i.e. 100% completion
  while (percentage < 100){
    # Pause for 0.5 secs before asking if the task is completed
    Sys.sleep(0.5)
    task <- .get.task(BasePath, token, taskid)
    if(!is.null(task$percentageCompleted)){
      percentage <- task$percentageCompleted
      setTxtProgressBar(pb, percentage)
    }
  }
  cat("\n")
  print("Prediction Completed!")
  predictedDatasetUri = task$'resultUri'
  # Delete here the initial dataset on the server for all models but PBPKs
  if(!is.pbpk){
    .delete.dataset(BasePath, token, datasetId)
  }else{
    print(paste0("The input dataset used in the PBPK has the following ID: ", datasetId))
  }
  predictedDataset_splitted <- strsplit(predictedDatasetUri, "/")[[1]]
  predictedDatasetID <- predictedDataset_splitted[length(predictedDataset_splitted)]
  print(paste0("The generated prediction dataset has the following ID: ", predictedDatasetID))
  dataset <- .get.dataset.internal(BasePath, token, predictedDatasetID)
  decoded <- .decode.predicted.dataset(dataset)
  
  # Keep only the output dictated by the model uploader through predicted.feats
  predicted.feats <- rep(0,  length(model$additionalInfo$predictedFeatures))
  for (i in 1:length(predicted.feats)){
    predicted.feats[i] <- model$additionalInfo$predictedFeatures[[i]]
  }
  
  # Separate predictions from input features
  final_predictions <- decoded[,predicted.feats, drop = FALSE]
  # Check which columns contain numerical values and convert the column to numeric 
  for (i in 1:dim(final_predictions)[2]){  
    if(!is.na(as.numeric(as.character(final_predictions[1,i])))){
      final_predictions[,i] <-  as.numeric(as.character(final_predictions[,i]))
    }
  }
  print(paste0("Visit the model at https://app.jaqpot.org/model/", modelID, " for more information regarding the model, its feature description and units and other relevant metadata infromation"))
  return (list(features = df, predictions = final_predictions, predictionDatasetID =predictedDatasetID, 
               inputDatasetID =ifelse(is.pbpk,datasetId,"Input included in the prediction dataset")))
  
  
}
