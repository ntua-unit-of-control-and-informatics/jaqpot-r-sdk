#' Deploy Naive Bayes (NB) Models on Jaqpot
#'
#' Uploads trained NB models on Jaqpot given a "naive_bayes" object.
#'
#' @param object An object of class "naive_bayes"  (function \code{naive_bayes()} of package 'naivebayes').
#' @param url The base path of Jaqpot services. This argument is optional and needs 
#' to be changed only if an alternative Jaqpot installation is used.
#' @return  The id of the uploaded model.
#' @details The user can upload on Jaqpot a model that has been trained using the 
#'  function \code{naive_bayes()} of package 'naivebayes'. Apart from the model object, the user is requested
#'  to provide further information (i.e. Jaqpot API key or credentials, model title and short
#'  description) via prompt messages. If the upload process is successful,
#'  the user is given a unique model id key.
#'
#' @examples
#'  \dontrun{
#'  #nb.model <- naivebayes::naive_bayes(y~x, data=df)
#'  #deploy.nb(nb.model)
#'  }
#'
#'
#' @export
deploy.nb <- function(object, url = "https://api.jaqpot.org/"){
  # Get object class
  obj.class <- attributes(object)$class[1] # class of glm models is "glm" "lm"
  # If object not an "naive_bayes" through error
  if  (obj.class != "naive_bayes") {
    stop("Model should be of class 'naive_bayes'" )
  }

  # Read the base path from the reader
  # base.path <- readline("Base path of jaqpot *e.g.: https://api.jaqpot.org/ : ")
   base.path <- url
  # Log into Jaqpot using the LoginJaqpot helper function in utils.R
  token <- .LoginJaqpot(base.path)
  # Ask the user for a a model title
  title <- readline("Title of the model: ")
  # Ask the user for a short model description
  description <- readline("Short description of the model: ")


  
  # Extract the dependent vars from the formula
  dependent.vars <- all.vars(object$call$formula[[2]])
  # Extract the independent vars from the terms
  independent.vars <- attr(object$terms,"term.labels")

  # Serialize the model in order to upload it on Jaqpot
  model <- serialize(list(MODEL=object),connection=NULL)
  # Create a list containing the information that will be uploaded on Jaqpot
  tojson <- list(rawModel=model, runtime="R-naivebayess", implementedWith="R naive bayess",
                 pmmlModel=NULL, independentFeatures=independent.vars,
                 predictedFeatures=dependent.vars, dependentFeatures=dependent.vars,
                 title=title, description=description, algorithm="naivebayess")
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
