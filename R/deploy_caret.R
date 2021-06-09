#' Deploy Decision Tree Models on Jaqpot
#'
#' Uploads trained decision tree models on Jaqpot given
#' a "tree" object.
#'  
#' @param preprocess The preprocess model 
#' @param trained.model An object of class "tree" (function \code{ctree(()} of package 'party'). 
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
deploy.caret <- function(preprocess =NULL, trained.model, url = "https://api.jaqpot.org/"){
  
  # Get trained.model class
  #obj.class <- attributes(trained.model)$class[1] # class of tree models is "tree"
  # If trained.model not an lm or glm through error
  #if  ( (obj.class != "tree")){
  #  stop("Model should be of class 'tree' ")
  #}
  
  # !!!!!!!!ATTENTION !!!!!!!!!!!!!!!!!
  # Check the model size here
  
  # Read the base path from the reader
  base.path <- url
  # Log into Jaqpot using the LoginJaqpot helper function in utils.R
  token <- .LoginJaqpot(base.path)
  # Ask the user for a a model title
  title <- readline("Title of the model: ")
  # Ask the user for a short model description
  description <- readline("Short description of the model: ")
  
  # Obtain the library on which the model was built
  library <- fitXGB2$modelInfo$library
  
  #Retrieve the independent vars
  independent.vars <- array(attributes(trained.model$terms)$term.labels)
  # Number of responses
  N_resp <-  attributes(trained.model$terms)$response
  # All variables (dependent and independent)
  all_vars_init <- as.character(attributes(trained.model$terms)$variables)
  all_vars <- all_vars_init[2:length(all_vars_init)]
  # Retrieve predicted variables by using set difference
  dependent.vars <- setdiff(all_vars, independent.vars)
  
 
  # Serialize the model in order to upload it on Jaqpot
  model <- serialize(list(MODEL=trained.model, PREPROCESS = preprocess),connection=NULL)
  
  # Create a list containing the information that will be uploaded on Jaqpot
  tojson <- list(rawModel=model, runtime="R-caret", implementedWith="caret  R",
                 pmmlModel=NULL, independentFeatures=independent.vars,
                 predictedFeatures=dependent.vars, dependentFeatures=dependent.vars,
                 title=title, description=description, algorithm="Rcaret", additionalInfo = list())
  # Convert the list to a JSON data format
  tryCatch({
    json <- jsonlite::toJSON(tojson)
  }, error = function(e) {
    e$message <-"Failed to convert trained.model to json. "
    stop(e)
  })
  # Function that posts the model on Jaqpot
  .PostOnService(base.path, token, json)
}
