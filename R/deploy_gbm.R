#' Deploy (gbm) Tree models on Jaqpot
#'
#' Uploads trained tree tree regression model on Jaqpot given
#' a "gbm" object.
#'
#' @param object An object of either class "gbm" (base function \code{gbm()})
#' @return  The id of the uploaded model
#' @details The user can upload on Jaqpot a model that has been trained using the base
#'  function \code{gbm()}. The data used for training are deleted before the
#'  model is uploaded on the platform. Apart from the model object, the user is requested
#'  to provide further information (e.g. Jaqpot API key or credentials, model title, short
#'  description etc.) via prompt messages. If the upload process is successful,
#'  the user is given a unique model id key.
#'
#' @examples
#'  gbm.model <- gbm(y~x, data=df)
#'  deploy.tree(gbm.model)
#'
#'
#' @export
deploy.gmb <- function(object){

  # Get object class
  obj.class <- attributes(object)$class[1] # class of glm models is "glm" "lm"
  # If object not an gbm through error
  if  ( (obj.class != "gbm")){
    stop("Model should be of class 'gbm' ")
  }

  # Read the base path from the reader
  base.path <- .SelectBasePath()
  # Log into Jaqpot using the LoginJaqpot helper function in utils.R
  token <- .LoginJaqpot(base.path)
  # Ask the user for a a model title
  title <- readline("Title of the model: ")
  # Ask the user for a short model description
  description <- readline("Short description of the model: ")

  independent.vars <- object$var.names
  # Retrieve predicted variables by using set difference
  dependent.vars <- object$response.name
  print(dependent.vars)
  # Delete attributes that are not necessary in the prediction process and increase object size
  object$train.error <- NULL
  object$valid.error <- NULL
  object$fit <- NULL
  # Serialize the model in order to upload it on Jaqpot
  model <- serialize(list(MODEL=object),connection=NULL)
  # Create a list containing the information that will be uploaded on Jaqpot
  tojson <- list(rawModel=model, runtime="R-gmb", implementedWith="gmb tree in R",
                 pmmlModel=NULL, independentFeatures=independent.vars,
                 predictedFeatures=dependent.vars, dependentFeatures=dependent.vars,
                 title=title, description=description, algorithm="R/tree/gmb")
  # Convert the list to a JSON data format
  json <- jsonlite::toJSON(tojson)
  # Function that posts the model on Jaqpot
  .PostOnService(base.path, token, json)
}
