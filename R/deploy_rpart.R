#' Deploy (rpart) Classification tree models on Jaqpot
#'
#' Uploads trained rpart tree model on Jaqpot given
#' a "tree" object.
#'
#' @param object An object of  (base function \code{rpart()}) or "rpart model"
#' (base function \code{rpart()})
#' @return  The id of the uploaded model
#' @details The user can upload on Jaqpot a classification rpart model that has been trained using the base
#'  function \code{rpart()} . The data used for training are deleted before the
#'  model is uploaded on the platform. Apart from the model object, the user is requested
#'  to provide further information (e.g. Jaqpot API key or credentials, model title, short
#'  description etc.) via prompt messages. If the upload process is successful,
#'  the user is given a unique model id key.
#'
#' @examples
#'  #rpart.model <- rpart(y~x, data=df)
#'  #deploy.rpart(rpart.model)
#'
#'
#' @export
deploy.rpart.regr.tree <- function(object){

  # Get object class
  obj.class <- attributes(object)$class[1] # class of glm models is "glm" "lm"
  # If object not an lm or glm through error
  if(obj.class != "rpart"){
    stop("Model should be of class 'rpart' ")
  }

  # Read the base path from the reader
  base.path <- .SelectBasePath()
  # Log into Jaqpot using the LoginJaqpot helper function in utils.R
  token <- .LoginJaqpot(base.path)
  # Ask the user for a a model title
  title <- readline("Title of the model: ")
  # Ask the user for a short model description
  description <- readline("Short description of the model:")

  # Retrieve the independent variables of the model
  independent.vars <- array(names(object$ordered))

  # Retrieve predicted variables by using set difference
  l <- attr(object$terms,"dataClasses")
  dependent.vars <- names(l)[1]

  # Delete attributes that are not necessary in the prediction process and increase object size
  object$where <- NULL
  object$call <- NULL
  object$cptable <- NULL
  object$control <- NULL
  object$functions <- NULL
  object$variable.importance <- NULL
  # Serialize the model in order to upload it on Jaqpot
  model <- serialize(list(MODEL=object),connection=NULL)
  # Create a list containing the information that will be uploaded on Jaqpot
  tojson <- list(rawModel=model, runtime="R-rpart", implementedWith="lm or a glm in r",
                 pmmlModel=NULL, independentFeatures=independent.vars,
                 predictedFeatures=dependent.vars, dependentFeatures=dependent.vars,
                 title=title, description=description, algorithm="lm/glm")
  # Convert the list to a JSON data format
  json <- jsonlite::toJSON(tojson)
  # Function that posts the model on Jaqpot
  .PostOnService(base.path, token, json)
}

#' Deploy (rpart) Regression tree models on Jaqpot
#'
#' Uploads trained rpart tree model on Jaqpot given
#' a "tree" object.
#'
#' @param object An object of  (base function \code{rpart()}) or "rpart model"
#' (base function \code{rpart()})
#' @return  The id of the uploaded model
#' @details The user can upload on Jaqpot a classification rpart model that has been trained using the base
#'  function \code{rpart()} . The data used for training are deleted before the
#'  model is uploaded on the platform. Apart from the model object, the user is requested
#'  to provide further information (e.g. Jaqpot API key or credentials, model title, short
#'  description etc.) via prompt messages. If the upload process is successful,
#'  the user is given a unique model id key.
#'
#' @examples
#'  #rpart.model <- rpart(y~x, data=df)
#'  #deploy.rpart.tree(rpart.model)
#'
#'
#' @export
deploy.rpart.regr.tree <- function(object){

  # Get object class
  obj.class <- attributes(object)$class[1] # class of glm models is "glm" "lm"
  # If object not an lm or glm through error
  if(obj.class != "rpart"){
    stop("Model should be of class 'rpart' ")
  }

  # Read the base path from the reader
  base.path <- .SelectBasePath()
  # Log into Jaqpot using the LoginJaqpot helper function in utils.R
  token <- .LoginJaqpot(base.path)
  # Ask the user for a a model title
  title <- readline("Title of the model: ")
  # Ask the user for a short model description
  description <- readline("Short description of the model: ")

  # Retrieve the independent variables of the model
  independent.vars <- array(names(object$ordered))

  # Retrieve predicted variables by using set difference
  l <- attr(object$terms,"dataClasses")
  dependent.vars <- names(l)[1]

  # Delete attributes that are not necessary in the prediction process and increase object size
  object$where <- NULL
  object$call <- NULL
  object$cptable <- NULL
  object$control <- NULL
  object$functions <- NULL
  object$variable.importance <- NULL
  # Serialize the model in order to upload it on Jaqpot
  model <- serialize(list(MODEL=object),connection=NULL)
  # Create a list containing the information that will be uploaded on Jaqpot
  tojson <- list(rawModel=model, runtime="R-part", implementedWith="R rpart",
                 pmmlModel=NULL, independentFeatures=independent.vars,
                 predictedFeatures=dependent.vars, dependentFeatures=dependent.vars,
                 title=title, description=description, algorithm="rpart tree")
  # Convert the list to a JSON data format
  json <- jsonlite::toJSON(tojson)
  # Function that posts the model on Jaqpot
  .PostOnService(base.path, token, json)
}

#' Deploy (rpart) Regression tree models on Jaqpot
#'
#' Uploads trained rpart tree model on Jaqpot given
#' a "tree" object.
#'
#' @param object An object of  (base function \code{rpart()}) or "rpart model"
#' (base function \code{rpart()})
#' @return  The id of the uploaded model
#' @details The user can upload on Jaqpot a classification rpart model that has been trained using the base
#'  function \code{rpart()} . The data used for training are deleted before the
#'  model is uploaded on the platform. Apart from the model object, the user is requested
#'  to provide further information (e.g. Jaqpot API key or credentials, model title, short
#'  description etc.) via prompt messages. If the upload process is successful,
#'  the user is given a unique model id key.
#'
#' @examples
#'  #rpart.model <- rpart(y~x, data=df)
#'  #deploy.rpart(rpart.model)
#'
#'
#' @export
deploy.rpart <- function(object){

  # Get object class
  obj.class <- attributes(object)$class[1] # class of glm models is "glm" "lm"
  # If object not an lm or glm through error
  if(obj.class != "rpart"){
    stop("Model should be of class 'rpart' ")
  }

  # Read the base path from the reader
  base.path <- .SelectBasePath()
  # Log into Jaqpot using the LoginJaqpot helper function in utils.R
  token <- .LoginJaqpot(base.path)
  # Ask the user for a a model title
  title <- readline("Title of the model: ")
  # Ask the user for a short model description
  description <- readline("Short description of the model: ")

  # Retrieve the independent variables of the model
  independent.vars <- array(names(object$ordered))

  # Retrieve predicted variables by using set difference
  l <- attr(object$terms,"dataClasses")
  dependent.vars <- names(l)[1]

  # Delete attributes that are not necessary in the prediction process and increase object size
  object$where <- NULL
  object$call <- NULL
  object$cptable <- NULL
  object$control <- NULL
  object$functions <- NULL
  object$variable.importance <- NULL
  # Serialize the model in order to upload it on Jaqpot
  model <- serialize(list(MODEL=object),connection=NULL)
  # Create a list containing the information that will be uploaded on Jaqpot
  tojson <- list(rawModel=model, runtime="R-rpart", implementedWith="R rpart",
                 pmmlModel=NULL, independentFeatures=independent.vars,
                 predictedFeatures=dependent.vars, dependentFeatures=dependent.vars,
                 title=title, description=description, algorithm="rpart tree")
  # Convert the list to a JSON data format
  json <- jsonlite::toJSON(tojson)
  # Function that posts the model on Jaqpot
  .PostOnService(base.path, token, json)
}
