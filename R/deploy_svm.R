#' Deploy Support Vector Machine (SVM) Models on Jaqpot
#'
#' Uploads trained SVM models on Jaqpot given
#' an "svm.formula" object.
#'
#' @param object An object of  class "svm.formula" (function \code{svm()} of package 'e1071'). 
#' @param replace used for NA substitution with a desired numeric value.
#' @param url The base path of Jaqpot services. This argument is optional and needs 
#' to be changed only if an alternative Jaqpot installation is used.
#' @return  The id of the uploaded model.
#' @details The user can upload on Jaqpot a model that has been trained using the function
#'   \code{svm()} of package 'e1071'. Apart from the model object, the user is requested
#'  to provide further information (i.e. Jaqpot API key or credentials, model title and short
#'  description) via prompt messages. If the upload process is successful,
#'  the user is given a unique model id key.
#'
#' @examples
#'  \dontrun{
#'  #svm.model <- e1071::svm(y~x, data=df)
#'  #deploy.svm(tree.model)
#'  }
#'
#' @export
deploy.svm <- function(object, replace = NULL, url = "https://api.jaqpot.org/jaqpot/"){

  # Get object class
  obj.class <- attributes(object)$class[1]
  # If object not an svm through error
  if  ( (obj.class != "svm.formula")){
    stop("Model should be of class 'svm' ")
  }
  # Check if replace is provided that it is numeric
  if(!is.null(replace)){
    if ( !is.numeric(replace)){
      stop("Please provide a numeric value for NA replacement")
    }
  }

  # Read the base path from the reader
    base.path <- url
  # Log into Jaqpot using the LoginJaqpot helper function in utils.R
  token <- jaqpot.token
  # Ask the user for a a model title
  title <- readline("Title of the model: ")
  # Ask the user for a short model description
  description <- readline("Short description of the model: ")

  independent.vars <- attributes(object$terms)$term.labels
  # Number of responses
  N_resp <-  attributes(object$terms)$response
  # All variables (dependent and independent)
  all_vars_init <- as.character(attributes(object$terms)$variables)
  all_vars <- all_vars_init[2:length(all_vars_init)]
  # Retrieve predicted variables by using set difference
  dependent.vars <- setdiff(all_vars, independent.vars)
 
  # Serialize the model in order to upload it on Jaqpot
  model <- serialize(list(MODEL=object),connection=NULL)
  # Create a list containing the information that will be uploaded on Jaqpot
  tojson <- list(rawModel=model, runtime="R-svm", implementedWith="e1071",
                 pmmlModel=NULL, independentFeatures=independent.vars,
                 predictedFeatures=dependent.vars, dependentFeatures=dependent.vars,
                 title=title, description=description, algorithm="r / svm",additionalInfo = list(replace = replace))
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
