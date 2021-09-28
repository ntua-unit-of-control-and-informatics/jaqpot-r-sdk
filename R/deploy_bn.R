#' Deploy Bayesian Netowrks  on Jaqpot
#'
#' Uploads trained bayesian networks on Jaqpot
#'  
#' @param implemented.in The library the model was build on. Currently, only "bnlearn" is supported.
#' @param trained.model An object of class ""bn.fit"" (function \code{bn.fit(()} of package 'bnlearn'). 
#' @param type The type of the bayesian network, with two options: "discrete" or "gaussian" 
#' @param url The base path of Jaqpot services. This argument is optional and needs 
#' to be changed only if an alternative Jaqpot installation is used.
#' @param ... Extra arguments to be passed down the R client. This is not recommended.
#' @return  The id of the uploaded model.
#' @details The user can upload on Jaqpot a model that has been trained using the
#'  function \code{bn.fit()} of package 'bnlearn'. Apart from the model object, the user is requested
#'  to provide further information (i.e. Jaqpot API key or credentials, model title and short
#'  description etc.) via prompt messages. If the upload process is successful,
#'  the user is given a unique model id key.
#'
#' @examples
#'  \dontrun{
#'  #my_model <- bnlearn::bn.fit(x = structure, x = data, method="bayes")
#'  #deploy.bn(my_model)
#' }
#'
#' @export
deploy.bn <- function( trained.model, implemented.in = "bnlearn", type = "discrete", predict.args = list(method ="bayes-lw" , n =1000), url = "https://api.jaqpot.org/jaqpot/", ...){
  
  # Make sure that the user used bnlearn for model building
  if (!(implemented.in %in% c("bnlearn"))){
    stop("Currently, the only supported package is 'bnlearn'. ")
  }
  
  ###########################################################################################
  ### The code below should change if libraries other than 'bnlearn' are to be supported  ###
  ###########################################################################################
  
  
  # Check if the user provides the correct object
  if((class(trained.model))[1] !=  "bn.fit"){
      stop("Please provide a 'bn.fit' object.")
  }
  # Make sure that the user has provided either a discrete or a gaussian bayesian network
  if (!(type %in% c("discrete", "gaussian"))){
    stop("The type of the network should either be 'discrete' or 'gaussian'. ")
  }
  
  # Read the base path from the reader
  base.path <- url
  # Log into Jaqpot using the LoginJaqpot helper function in utils.R
  token <- .LoginJaqpot(base.path)
  # Ask the user for a a model title
  title <- readline("Title of the model: ")
  # Ask the user for a short model description
  description <- readline("Short description of the model: ")
  
  #Retrieve the independent variables
  independent.vars <- c(attributes(trained.model)$names, "query node")
  
  if( type == "discrete"){
    # Define the dependent variables of a discrete network
    dependent.vars <- c("node", "prediction class", "probability")
  }else{
    stop("Only discrete networks are currently supported.")
  }

  # Convert three dots into list
  extra.args <- list(...)
  
  # Serialize the model in order to upload it on Jaqpot
  model <- serialize(list(MODEL=trained.model,extra.args = extra.args),connection=NULL)
  
  if(implemented.in == "bnlearn" && type == "discrete"){
    set.runtime <- "R-bnlearn-discrete" 
    set.implementedWith <- "bnlearn"
    set.algorithm <- "bnlearn includes several algorithms"
  }else{
    stop("Only discrete networks of package bnlearn are currently supported. ")
  }
  # Create a list containing the information that will be uploaded on Jaqpot
  tojson <- list(rawModel=model, runtime = set.runtime, implementedWith = set.implementedWith,
                 pmmlModel=NULL, independentFeatures=independent.vars,
                 predictedFeatures=dependent.vars, dependentFeatures=dependent.vars,
                 title=title, description=description, algorithm = set.algorithm, additionalInfo = list(predict.args = predict.args))
  # Convert the list to a JSON data format
  tryCatch({
    json <- jsonlite::toJSON(tojson)
  }, error = function(e) {
    e$message <-"Failed to convert trained.model to json. "
    stop(e)
  })
  
  # Check object size
  if(object.size(json) > 5000000 ){
    stop(" The model(s) you are trying to upload exceed the maximum size that the system can currently support")
  }
  # Function that posts the model on Jaqpot
  .PostOnService(base.path, token, json)
}
