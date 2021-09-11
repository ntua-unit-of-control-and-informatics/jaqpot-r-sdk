#' Deploy Tree Models on Jaqpot
#'
#' Uploads trained tree models on Jaqpot given
#' a "tree" object.
#'
#' @param object An object of class "tree" (function \code{tree()} of package 'tree'). 
#' @param replace used for NA substitution with a desired numeric value.
#' @param url The base path of Jaqpot services. This argument is optional and needs 
#' to be changed only if an alternative Jaqpot installation is used.
#' @return  The id of the uploaded model.
#' @details The user can upload on Jaqpot a model that has been trained using the
#'  function \code{tree()} of package 'tree'. Apart from the model object, the user is requested
#'  to provide further information (i.e. Jaqpot API key or credentials, model title and short
#'  description etc.) via prompt messages. If the upload process is successful,
#'  the user is given a unique model id key.
#'
#' @examples
#'  \dontrun{
#'  #tree.model <- tree(y~x, data=df)
#'  #deploy.tree(tree.model)
#' }
#'
#' @export
deploy.tree <- function(object,  replace = NULL, url = "https://api.jaqpot.org/jaqpot/"){

  # Get object class
  obj.class <- attributes(object)$class[1] # class of glm models is "glm" "lm"
  # If object not an lm or glm through error
  if  ( (obj.class != "tree")){
    stop("Model should be of class 'tree' ")
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
  token <- .LoginJaqpot(base.path)
  # Ask the user for a a model title
  title <- readline("Title of the model: ")
  # Ask the user for a short model description
  description <- readline("Short description of the model: ")

  independent.vars <- attributes(object$terms)$term.labels
  # Retrieve predicted variables by using set difference
  dependent.vars <- as.character(attributes(object$terms)$predvars[[2]])
  # Number of responses
  N_resp <-  attributes(object$terms)$response
  # All variables (dependent and independent)
  all_vars_init <- as.character(attributes(object$terms)$variables)
  all_vars <- all_vars_init[2:length(all_vars_init)]
  # Retrieve predicted variables by using set difference
  dependent.vars <- setdiff(all_vars, independent.vars)

  # Delete attributes that are not necessary in the prediction process and increase object size
  #object$where <- NULL
  #object$call <- NULL
  #object$y <- NULL
  #object$weights <- NULL
  # Serialize the model in order to upload it on Jaqpot
  model <- serialize(list(MODEL=object),connection=NULL)
  # Create a list containing the information that will be uploaded on Jaqpot
  tojson <- list(rawModel=model, runtime="R-tree", implementedWith="tree in R",
                 pmmlModel=NULL, independentFeatures=independent.vars,
                 predictedFeatures=dependent.vars, dependentFeatures=dependent.vars,
                 title=title, description=description, algorithm="R/tree",additionalInfo = list(replace = replace))
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
