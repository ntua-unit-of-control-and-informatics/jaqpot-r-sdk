#' Deploy Decision Tree Models on Jaqpot
#'
#' Uploads trained decision tree models on Jaqpot given
#' a "tree" object.
#'  
#' @param preprocess.model The preprocess model. Here it is very important not to include the response into the preprocessing phase!
#' Allowed types of preprocessing: 'preProcess' and 'dummyVars' caret functions. 
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
deploy.caret <- function(preprocess.model = NULL, trained.model, url = "https://api.jaqpot.org/jaqpot/"){
  
  # Make sure that preprocess.model is a list
  if ( !is.null(attributes(preprocess.model))){
    stop("Please enclose your preprocess model(s) in a list by using the list() function")
  }
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
  library <- trained.model$modelInfo$library
  
  if (!is.null(preprocess.model)){
    # Different handling if first preprocess object is of class dummyVars or preProcess
    if(attributes(preprocess.model[[1]])$class == "dummyVars"){
      independent.vars <- preprocess.model[[1]]$vars
    }else if (attributes(preprocess.model[[1]])$class == "preProcess"){
      # Obtain the names of the variables from the variables on the method used, combined with the ignored variables 
      independent.vars  <- list()
      for (i in 1:length(preprocess.model[[1]]$method)){
        independent.vars[[i]] <- preprocess.model[[1]]$method[[i]]
      }
      independent.vars <- unlist(independent.vars)
      independent.vars <- unique(independent.vars)
    }
    #Retrieve the model independent vars (may contain variables resulting from one-hot encoding)
    model.independent.vars <- array(attributes(trained.model$terms)$term.labels)
    # Number of responses
    N_resp <-  attributes(trained.model$terms)$response
    # All variables (dependent and independent)
    all_vars_init <- as.character(attributes(trained.model$terms)$variables)
    all_vars <- all_vars_init[2:length(all_vars_init)]
    # Retrieve predicted variables by using set difference
    dependent.vars <- setdiff(all_vars, model.independent.vars)
    
    }else{
    #Retrieve the independent vars
    independent.vars <- array(attributes(trained.model$terms)$term.labels)
    # Number of responses
    N_resp <-  attributes(trained.model$terms)$response
    # All variables (dependent and independent)
    all_vars_init <- as.character(attributes(trained.model$terms)$variables)
    all_vars <- all_vars_init[2:length(all_vars_init)]
    # Retrieve predicted variables by using set difference
    dependent.vars <- setdiff(all_vars, independent.vars)
  }
 
  # Serialize the model in order to upload it on Jaqpot
  model <- serialize(list(MODEL=trained.model, PREPROCESS = preprocess.model),connection=NULL)
  
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
