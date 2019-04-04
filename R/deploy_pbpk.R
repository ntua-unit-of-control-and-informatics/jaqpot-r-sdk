#' Deploy a pbpk model on Jaqpot.
#'
#' Uploads a PBPK mode on Jaqpot given a dataset, a set of differential equations and
#' -if applicable- a covariate model.
#'
#' @param data A list of positive numbers. It should contain the independent parameters,
#' i.e. parameters related to the individual (e.g. weight, age etc.) that will be forwarded to
#' the covariate model (if one is provided), dose, infusion_time and initial concentration in organs.
#' The naming and order of the variables included in the dataframe is very important (see Details
#' section).
#' @param cov.model A function that takes as input the individual-related parameters provided
#' in the dataframe and outputs a vector of individualised physiological parameters that will
#' be forwarded to the \code{odes()} function.
#' @param odes A function that contains the ODEs of the PBPK model. The format of the function
#' must be in line with the \code{ode())} solver of the \code{deSolve} package (see Details section.)
#' @param comp.names a string vector containing the names of the compartments of the PBPK model,
#' which should be in line with the order of the differential equations provided in \code{odes()}.
#' @return  The id of the uploaded model, if the upload process is succesful.
#' @details The indivdual related parameters must be the first parameters declared in the list,
#' followed by the dose (named 'dose'), the infusion time (named 'infusion.time') and finally the
#' initial organ concentrtions. The latter should be named as 'C0_NAME' where name is the compartment
#' name, as provided in the comp.names vector. Note that the names of the individual-related
#' parameters and the compartments cannot be further modified via the Jaqpot User Interface,
#' so the user should choose them with caution. The order of the individual-related parameters should
#' be the same as the order of input of the covariate model. The same condition holds for the output
#' of the covariate model and the parameter input of the \code{odes()} function. In other words, the
#' user should take into consideration that the parameters of the \code{odes()} function will be the
#' output of the covariate model.
#'
#' @examples
#' user_input <-data.frame(weight=70 ,gender=0, dose=10, infusion_time=0.1, C0_blood=0,
#'  C0_liver=0)
#'
#' comp_names <- c("blood", "liver")
#'
#' covariates <- function(weight, gender){
#'   Q_blood <-  #some function of weight and gender
#'   Q_liver <-  #some function of weight and gender
#'   V_blood <-  #some function of weight and gender
#'   V_liver <-   #some function of weight and gender
#'   return(Q_blood, Q_liver, V_blood, V_liver)
#' }
#'
#' odes <- function(time,C,params){
#'    dCdt<-rep(0,14)
#'    Q_blood <- params[1]
#'    Q_liver <- params[2]
#'    V_blood <- params[3]
#'    V_liver <- params[4]
#'    ...
#'    dose <- params[N-1]
#'    infusion_time <- params[N] # dose and infusion time must be the last two parameters
#'    kp= 10
#'    CL=10
#'
#'    dCdt[1]<- Q_blood * C[2] /  V_blood
#'    dCdt[2]<-(-Q_liver * C[2] / (kp * V_liver)) -CL
#'
#'    list(dCdt)
#' }
#' deploy.pbpk(data, cov.model, odes, comp.names)
#' @export

deploy.pbpk <- function(data, cov.model=NULL, odes, comp.names){
  # Read the base path from the reader
  base.path <- readline("Base path of jaqpot *e.g.: https://api.jaqpot.org/ : ")
  # Log into Jaqpot using the LoginJaqpot helper function in utils.R
  token <- .LoginJaqpot(base.path)
  # Ask the user for a a model title
  title <- readline("Title of the model: ")
  # Ask the user for a short model description
  description <- readline("Short description of the model:")
  independent.features <- c(names(data), "time.start" , "time.end", "time.by")
  cov.pars <- names(data[,1:(dim(data)[2]-2-length(comp.names))])

  libabry_in <- "deSolve"
  predicts <- comp.names
  predicts[length(predicts)+1] <- "time"
  # Serialize the model in order to upload it on Jaqpot
  model <- serialize(list(COVMODEL=cov.model, ODEMODEL=odes),connection=NULL)
  # Create a list containing the information that will be uploaded on Jaqpot
  tojson <- list(rawModel=model,runtime="pbpk-ode", implementedWith=libabry_in, pmmlModel=NULL,
                 independentFeatures=independent.features, predictedFeatures=predicts,
                 dependentFeatures=predicts, title=title, description=description,
                 algorithm="PBPK custom", additionalInfo =list(comp = comp.names, cov = cov.pars ))
  # Convert the list to a JSON data format
  json <- jsonlite::toJSON(tojson)
  # Function that posts the model on Jaqpot
  .PostOnService(base.path, token, json)
}
