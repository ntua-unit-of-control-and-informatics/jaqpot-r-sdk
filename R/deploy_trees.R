#' Deploy (rpart) Tree models on Jaqpot
#'
#' Uploads trained rpart tree model on Jaqpot given
#' a "tree" object.
#'
#' @param object An object of either class "" (base function \code{lm()}) or "lm" "glm"
#' (base function \code{glm()})
#' @return  The id of the uploaded model
#' @details The user can upload on Jaqpot a model that has been trained using the base
#'  function \code{lm()} or \code{glm()}. The data used for training are deleted before the
#'  model is uploaded on the platform. Apart from the model object, the user is requested
#'  to provide further information (e.g. Jaqpot API key or credentials, model title, short
#'  description etc.) via prompt messages. If the upload process is successful,
#'  the user is given a unique model id key.
#'
#' @examples
#'  tree.model <- lm(y~x, data=df)
#'  deploy.rpart.tree(lm.model)
#'
#'
#' @export
deploy.rpart.tree <- function(object){

  # Get object class
  obj.class <- attributes(object)$class[1] # class of glm models is "glm" "lm"
  # If object not an lm or glm through error
  if  ( (obj.class != "lm") && (obj.class != "glm")){
    stop("Model should be of class 'lm' or 'glm' ")
  }

  # Read the base path from the reader
  base.path <- readline("Base path of jaqpot *e.g.: https://api.jaqpot.org/ : ")
  # Log into Jaqpot using the LoginJaqpot helper function in utils.R
  token <- .LoginJaqpot(base.path)
  # Ask the user for a a model title
  title <- readline("Title of the model: ")
  # Ask the user for a short model description
  description <- readline("Short description of the model:")

  # Retrieve the independent variables of the model
  check.features <- array(names(coef(object)))
  # Create the names of the  features by excluding the "(Intercept)"
  if(check.features[1]  %in% "(Intercept)"){
    independent.vars <- check.features[!check.features  %in% "(Intercept)"]
  } else {
    independent.vars <- check.features
  }

  # Retrieve predicted variables by using set difference
  dependent.vars <- setdiff(names(object$model), independent.vars)

  # Delete attributes that are not necessary in the prediction process and increase object size
  object$residuals <- NULL
  object$model <- NULL
  object$effects<- NULL
  object$fitted.values<- NULL
  object$qr$qr <- NULL
  # Models of class "glm lm" have more unecessary attributes than simple lm models
  if (obj.class == "glm"){
    object$linear.predictors <- NULL
    object$weights <- NULL
    object$prior.weights <- NULL
    object$y <- NULL
    object$data <- NULL
  }
  # Serialize the model in order to upload it on Jaqpot
  model <- serialize(list(MODEL=object),connection=NULL)
  # Create a list containing the information that will be uploaded on Jaqpot
  tojson <- list(rawModel=model, runtime="R-lm-glm", implementedWith="lm or a glm in r",
                 pmmlModel=NULL, independentFeatures=independent.vars,
                 predictedFeatures=dependent.vars, dependentFeatures=dependent.vars,
                 title=title, description=description, algorithm="lm/glm")
  # Convert the list to a JSON data format
  json <- jsonlite::toJSON(tojson)
  # Function that posts the model on Jaqpot
  .PostOnService(base.path, token, json)
}
