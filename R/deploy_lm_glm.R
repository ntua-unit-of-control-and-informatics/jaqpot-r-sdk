#' Deploy (Generalized) Linear Models on Jaqpot
#'
#' Uploads trained linear and generalized linear models on Jaqpot given
#' an "lm" or "glm" object.
#'
#' @param object An object of class "lm" (base function \code{lm()}) or "glm"
#' (base function \code{glm()}).
#' @param replace used for NA substitution with a desired numeric value. 
#' @param url The base path of Jaqpot services. This argument is optional and needs 
#' to be changed only if an alternative Jaqpot installation is used.
#' @return  The id of the uploaded model.
#' @details The user can upload on Jaqpot a model that has been trained using the base
#'  functions \code{lm()} or \code{glm()}. Apart from the model object, the user is requested
#'  to provide further information (i.e. Jaqpot API key or credentials, model title and short
#'  description) via prompt messages. If the upload process is successful,
#'  the user is given a unique model id key. 
#'
#' @examples
#'  \dontrun{
#'  #lm.model <- lm(y~x, data=df)
#'  #deploy.lm(lm.model)
#'
#'  #glm.model <- glm(y~x, data=df, family =  "gaussian")
#'  #deploy.lm(glm.model)
#'  }
#'
#' @export
deploy.lm <- function(object, replace = NULL, url = "https://api.jaqpot.org/jaqpot/"){
  # Get object class
  obj.class <- attributes(object)$class[1] # class of glm models is "glm" "lm"
  # If object not an lm or glm through error
  if  ( (obj.class != "lm") && (obj.class != "glm") && (obj.class != "mlm")){
    stop("Model should be of class 'lm', 'mlm' or 'glm' ")
  }
  # Check if replace is provided that it is numeric
  if(!is.null(replace)){
    if ( !is.numeric(replace)){
      stop("Please provide a numeric value for NA replacement")
    }
  }
  # Read the base path from the reader
  # base.path <- readline("Base path of jaqpot *e.g.: https://api.jaqpot.org/ : ")
    base.path <- url 
  # Log into Jaqpot using the LoginJaqpot helper function in utils.R
  token <- jaqpot.token
  # Ask the user for a a model title
  title <- readline("Title of the model: ")
  # Ask the user for a short model description
  description <- readline("Short description of the model: ")

  # Extract the dependent vars from the formula
  dependent.vars <- all.vars(object$call$formula[[2]])
  # Extract the independent vars from the terms
  independent.vars <- attr(object$terms,"term.labels")

  
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
                 title=title, description=description, algorithm="lm/glm",additionalInfo = list(replace = replace))
  # Convert the list to a JSON data format
  tryCatch({
    json <- jsonlite::toJSON(tojson)
    }, error = function(e) {
          e$message <-"Failed to convert object to json. "
          stop(e)
    })
  # Function that posts the model on Jaqpot
  .PostOnService(base.path, token, json)
}
