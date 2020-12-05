#' Deploy (tree) Tree models on Jaqpot
#'
#' Uploads trained tree tree regression model on Jaqpot given
#' a "tree" object.
#'
#' @param object An object of either class "" (base function \code{tree()}) or "tree"
#' (base function \code{tree()})
#' @return  The id of the uploaded model
#' @details The user can upload on Jaqpot a model that has been trained using the base
#'  function \code{tree()}. The data used for training are deleted before the
#'  model is uploaded on the platform. Apart from the model object, the user is requested
#'  to provide further information (e.g. Jaqpot API key or credentials, model title, short
#'  description etc.) via prompt messages. If the upload process is successful,
#'  the user is given a unique model id key.
#'
#' @examples
#'  #tree.model <- tree(y~x, data=df)
#'  #deploy.tree(tree.model)
#'
#'
#' @export
deploy.tree <- function(object){

  # Get object class
  obj.class <- attributes(object)$class[1] # class of glm models is "glm" "lm"
  # If object not an lm or glm through error
  if  ( (obj.class != "tree")){
    stop("Model should be of class 'tree' ")
  }

  # Read the base path from the reader
  base.path <- .SelectBasePath()
  # Log into Jaqpot using the LoginJaqpot helper function in utils.R
  token <- .LoginJaqpot(base.path)
  # Ask the user for a a model title
  title <- readline("Title of the model: ")
  # Ask the user for a short model description
  description <- readline("Short description of the model: ")

  independent.vars <- attributes(object$terms)$term.labels
  # Retrieve predicted variables by using set difference
  dependent.vars <- as.character(attributes(object$terms)$predvars[[2]])

  # Delete attributes that are not necessary in the prediction process and increase object size
  object$where <- NULL
  object$call <- NULL
  object$y <- NULL
  object$weights <- NULL
  # Serialize the model in order to upload it on Jaqpot
  model <- serialize(list(MODEL=object),connection=NULL)
  # Create a list containing the information that will be uploaded on Jaqpot
  tojson <- list(rawModel=model, runtime="R-tree", implementedWith="tree in R",
                 pmmlModel=NULL, independentFeatures=independent.vars,
                 predictedFeatures=dependent.vars, dependentFeatures=dependent.vars,
                 title=title, description=description, algorithm="R/tree")
  # Convert the list to a JSON data format
  json <- jsonlite::toJSON(tojson)
  # Function that posts the model on Jaqpot
  .PostOnService(base.path, token, json)
}
