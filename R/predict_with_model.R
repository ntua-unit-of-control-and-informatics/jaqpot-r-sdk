#' Obtain Predictions from a Jaqpot Model
#'
#' The user can provide a dataset and obtain predictions from an uploaded model given the model id.
#'  
#' @param df A list containing the prediction dataset. The column names of the dataset
#' should be identical to the independent features of the model.
#' @param modelID The Jaqpot id of the model
#' @param url The base path of Jaqpot services. This argument is optional and needs 
#' to be changed only if an alternative Jaqpot installation is used.
#' @param envFile .env filename containing API KEY and API KEY SECRET
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
jaqpot.predict <- function( df, modelID, url = "https://api.jaqpot.org/", envFile =NULL){
  
  # Make sure that the user provides a data frame
  if (class(df) != "list"){
    stop("Please provide an object of class 'data.frame'.")
  }
  
  # Make sure that the modelID is a string
  if (typeof(modelID) != "character"){
    stop("The modelID should be a string ('charachter' type).")
  }
  
  before_sourcing <- ls()
  if (!is.null(envFile)){
    dotenv::load_dot_env(file = envFile)
    JAQPOT_API_KEY <- Sys.getenv("JAQPOT_API_KEY")
    JAQPOT_API_SECRET <- Sys.getenv("JAQPOT_API_SECRET") 
  }else{
    JAQPOT_API_KEY <- getPass::getPass("Provide JAQPOT_API_KEY: ")
    JAQPOT_API_SECRET <- getPass::getPass("Provide JAQPOT_API_SECRET: ")
  } 
 
  openapi_folder <- file.path( "./openapi")
  r_files <- list.files(openapi_folder, pattern = "\\.R$", full.names = TRUE)
  invisible(lapply(r_files, function(file) source(file, echo = FALSE, print.eval = FALSE)))
  after_sourcing <- ls()
  
  default_headers <- c("X-Api-Key" = JAQPOT_API_KEY, "X-Api-Secret" = JAQPOT_API_SECRET)
  api_client <- ApiClient$new(base_path = url,default_headers = default_headers)
  api_instance <- ModelApi$new(api_client = api_client)
  
  InputData <- R6::R6Class("InputData",
                       public = list(
                         data = NULL,
                         initialize = function(data) {
                           self$data <- data
                         }
                       )
  )
  
  input <- list(InputData$new(df))
  
  prediction_df <- Dataset$new(type = DatasetType$new("PREDICTION"), entryType = "ARRAY", input = input)
  
  response <- api_instance$PredictWithModel(model_id = modelID, dataset = prediction_df)
  dataset_id <-  sub(".*/datasets/([0-9]+).*", "\\1", response$headers$location)
  dataset_api = DatasetApi$new(api_client)
  
  # Polling Parameters
  step <- 3               # Interval between polling attempts (seconds)
  timeout <- 10 * 60      # Maximum wait time (seconds)
  start_time <- Sys.time()
  repeat {
    dataset_response <- try(
      dataset_api$GetDatasetByIdWithHttpInfo(id = dataset_id),
      silent = TRUE
    )
    
    if (dataset_response$content$status %in% c("SUCCESS", "FAILURE")) {
      message("Polling completed. Status: ", dataset_response$content$status)
      break
    }
    
    # Check if timeout exceeded
    elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (elapsed_time > timeout) {
      stop("Polling timed out while waiting for prediction result.")
    }
    
    # Wait for the next polling attempt
    Sys.sleep(step)
  }
  
  
  new_objects <- setdiff(after_sourcing, before_sourcing)
  rm(list = new_objects)
  
  return (list( predictions = dataset_response$content$result, predictionDatasetID =dataset_id))
  
  
}

