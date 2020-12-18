#' Deploy Decision Tree Models on Jaqpot
#'
#' Uploads trained decision tree models on Jaqpot given
#' a "tree" object.
#'
#' @param object An object of class "tree" (function \code{ctree(()} of package 'party'). 
#' @param url The base path of Jaqpot services. This argument is optional and needs 
#' to be changed only if an alternative Jaqpot installation is used.
#'
#' @return  The id of the uploaded model.
#' @details The user can upload on Jaqpot a model that has been trained using the
#'  function \code{ctree()} of package 'party'. Apart from the model object, the user is requested
#'  to provide further information (i.e. Jaqpot API key or credentials, model title and short
#'  description etc.) via prompt messages. If the upload process is successful,
#'  the user is given a unique model id key.
#'
#' @examples
#'  \dontrun{
#'  #party.model <- party::ctree(y~x, data=df)
#'  #deploy.party(tree.model)
#' }
#'
#' @export
deploy.party <- function(object, url = "https://api.jaqpot.org/"){

  # Get object class
  #obj.class <- attributes(object)$class[1] # class of tree models is "tree"
  # If object not an lm or glm through error
  #if  ( (obj.class != "tree")){
  #  stop("Model should be of class 'tree' ")
  #}

  # Read the base path from the reader
    base.path <- url
  # Log into Jaqpot using the LoginJaqpot helper function in utils.R
  token <- .LoginJaqpot(base.path)
  # Ask the user for a a model title
  title <- readline("Title of the model: ")
  # Ask the user for a short model description
  description <- readline("Short description of the model: ")

  independent.vars.str <- as.character(object@data@formula$input)[2]
  independent.vars <- unlist(strsplit(independent.vars.str,split='+', fixed=TRUE))

  # Retrieve predicted variables by using set difference
  dependent.vars.str <- as.character(object@data@formula$response)[2]
  dependent.vars <-  unlist(strsplit(dependent.vars.str,split='+', fixed=TRUE))

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
                 title=title, description=description, algorithm="R/tree")
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
