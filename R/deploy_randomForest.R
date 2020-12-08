#' Deploy Random Forest Models on Jaqpot
#'
#' Uploads trained random forest models on Jaqpot given
#' a "randomForest.formula" object (function \code{(randomForest)} 
#' of package 'randomForest').
#'
#' @param object An object of class "randomForest.formula"
#' (function \code{randomForest()} of package 'randomForest').
#' @param url The base path of Jaqpot services. This argument is optional and needs 
#' to be changed only if an alternative Jaqpot installation is used.
#' @return  The id of the uploaded model.
#' @details The user can upload on Jaqpot a model that has been trained using the
#'  function function \code{(randomForest)} 
#' of package 'randomForest'. Apart from the model object, the user is requested
#'  to provide further information (i.e. Jaqpot API key or credentials, model title and short
#'  description etc.) via prompt messages. If the upload process is successful,
#'  the user is given a unique model id key.
#'
#' @examples
#'  \dontrun{
#'  #rf.model <- randomForest::randomForest(y~x, data=df)
#'  #deploy.randomForest(rf.model)
#'  }
#'
#' @export
deploy.randomForest <- function(object, url = "https://api.jaqpot.org/"){

  # Get object class
  obj.class <- attributes(object)$class[1] # class of glm models is "glm" "lm"
  # If object not an lm or glm through error
  if (obj.class != "randomForest.formula"){
    stop("Model should be of class 'randomForest' ")
  }

  # Read the base path from the reader
  base.path <- url
  # Log into Jaqpot using the LoginJaqpot helper function in utils.R
  token <- .LoginJaqpot(base.path)
  # Ask the user for a a model title
  title <- readline("Title of the model: ")
  # Ask the user for a short model description
  description <- readline("Short description of the model: ")

  independent.vars <- array(attributes(object$terms)$term.labels)
  # Retrieve predicted variables by using set difference
  dependent.vars <- names(attributes(object$terms)$dataClasses)[[1]]

  # Delete attributes that are not necessary in the prediction process and increase object size
  object$y <- NULL
  object$mse <- NULL
  object$rsq <- NULL
  object$oob.times <- NULL
  object$proximity <- NULL
  object$inbag <- NULL
  object$localImportance <- NULL
  object$importanceSD <- NULL
  # Serialize the model in order to upload it on Jaqpot
  model <- serialize(list(MODEL=object),connection=NULL)
  # Create a list containing the information that will be uploaded on Jaqpot
  tojson <- list(rawModel=model, runtime="R-rf-tree", implementedWith="Random forest in R",
                 pmmlModel=NULL, independentFeatures=independent.vars,
                 predictedFeatures=dependent.vars, dependentFeatures=dependent.vars,
                 title=title, description=description, algorithm="R/rf")
  # Convert the list to a JSON data format
  json <- jsonlite::toJSON(tojson)
  # Function that posts the model on Jaqpot
  .PostOnService(base.path, token, json)
}






