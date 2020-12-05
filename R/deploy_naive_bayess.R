#' Deploy Naive Bayess models on Jaqpot
#'
#' Uploads trained linear and generalized linear models on Jaqpot given
#' an "naive_bayess" object.
#'
#' @param object An object of either class "naive_bayess" (base function \code{naiveBayess()})
#' (base function \code{naiveBayess()})
#' @return  The id of the uploaded model
#' @details The user can upload on Jaqpot a model that has been trained using the base
#'  function \code{naiveBayess()}. The data used for training are deleted before the
#'  model is uploaded on the platform. Apart from the model object, the user is requested
#'  to provide further information (e.g. Jaqpot API key or credentials, model title, short
#'  description etc.) via prompt messages. If the upload process is successful,
#'  the user is given a unique model id key.
#'
#' @examples
#'  #nb.model <- naivebayes::naive_bayes(y~x, data=df)
#'  #deploy.naive.bayess(nb.model)
#'
#'
#' @export
deploy.naive.bayess <- function(object){
  # Get object class
  obj.class <- attributes(object)$class[1] # class of glm models is "glm" "lm"
  # If object not an "naive_bayes" through error
  if  (obj.class != "naive_bayes") {
    stop("Model should be of class 'naive_bayes'" )
  }

  # Read the base path from the reader
  # base.path <- readline("Base path of jaqpot *e.g.: https://api.jaqpot.org/ : ")
  base.path <- .SelectBasePath()
  # Log into Jaqpot using the LoginJaqpot helper function in utils.R
  token <- .LoginJaqpot(base.path)
  # Ask the user for a a model title
  title <- readline("Title of the model: ")
  # Ask the user for a short model description
  description <- readline("Short description of the model: ")


  independent.vars <- attributes(object$data$x)$names
  # Retrieve predicted variables by using set difference
  dependent.vars <- as.character(object$call$formula)[2]

  # Serialize the model in order to upload it on Jaqpot
  model <- serialize(list(MODEL=object),connection=NULL)
  # Create a list containing the information that will be uploaded on Jaqpot
  tojson <- list(rawModel=model, runtime="R-naivebayess", implementedWith="R naive bayess",
                 pmmlModel=NULL, independentFeatures=independent.vars,
                 predictedFeatures=dependent.vars, dependentFeatures=dependent.vars,
                 title=title, description=description, algorithm="naivebayess")
  # Convert the list to a JSON data format
  json <- jsonlite::toJSON(tojson)
  # Function that posts the model on Jaqpot
  .PostOnService(base.path, token, json)
}
