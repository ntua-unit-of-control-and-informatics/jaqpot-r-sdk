#' Deploy an Ordinary Differential Equation (ODE) model on Jaqpot.
#'
#' Uploads an ODE model on Jaqpot that can be solved using the \code{ode} function of package 'deSolve' 
#' 
#' @param user.input A list of positive numbers. It should contain the independent parameters,
#' i.e. parameters related to the individual (e.g. weight, age etc.) that will be forwarded to
#' the covariate model (if one is provided), dose, infusion_time and initial concentration in organs.
#' The naming and order of the variables included in the dataframe is very important (see Details
#' section).
#' @param predicted.feats A function that takes as input the individual-related parameters provided
#' in the dataframe and outputs a vector of individualised physiological parameters that will
#' be forwarded to the \code{odes()} function.
#' @param create.params A function that contains the ODEs of the PBPK model. The format of the function
#' must be in line with the \code{ode())} solver of the \code{deSolve} package (see Details section.)
#' @param create.inits a string vector containing the names of the compartments of the PBPK model,
#' which should be in line with the order of the differential equations provided in \code{odes()}.
#' @param custom.fun The compartment through which the substance enters the system.
#' @param ode.fun The 
#' @param method
#' @param ...
#' @return  The id of the uploaded model, if the upload process is succesful.
#' @details #' The user must provide two vectors, one with the inputs that the end-user will provide on the Jaqpot 
#' User Interface (UI) and one with the predicted features that will be printed on the UI upon execution
#' of the ODE system. In addition, the user should provide five functions, all of which return
#' lists. The first function transforms the user input according to the needs of the ODE model, the 
#' second creates the initial conditions of the ODEs, the third creates the events that are forced
#' on the system, the fourth gives enables the use of custom functions inside the ODEs and
#' the last is the ODEs, with syntax compatible with package 'deSolve'. The functions
#' can be used in a nested style (see example). Note that the names of dependent and independent
#' features cannot be further modified via the Jaqpot UI,
#' so the user should choose them with caution. 
#' 
#' @examples
#' \dontrun{
#' user.input <-list("weight" = 250,"dose" = c(10,12), "administration.time" = c(0,1.5) )
#' predicted.feats <- c("Li")
#' ##########################################
#' # Function for creating parameter vector #
#' ##########################################
#' 
#' create.params <- function(input){
#'   with( as.list(input),{
#'     
#'   ############################
#'   # Physiological parameters #
#'   ############################
#'   # tissue weights (in g)
#'   W_tot <- weight # ;body weight, experimental data - g
#'   W_lu <-1.2 # weight of lungs, experimental data - g
#'   W_li <- 10.03 # weight of liver, experimental data - g
#'  
#'   W_blood <- 0.065 * W_tot
#'   W_rob <- W_tot - (W_blood + W_li + W_lu)
#'  
#'   #Regional blood flows (in mL per hour)
#'   fQl = 0.183 # fraction of cardiac output to liver, unitless
#'   fQrob = 1-fQl # fraction of cardiac output to rest of the body,  unitless
#'   Q_tot <- 4980 # cardiac output, mL/h    
#'   Q_li <- fQl*Q_tot    # blood flow to liver, mL/h
#'   Q_rob <- fQrob*Q_tot # blood flow to rest of the body, mL/h
#'  
#'   P <-1.435445 # partition coefficient tissue:blood, unitless
#'   CLE_f <- 9.958839e-05 # clearance rate to feces from liver,  mL/h
#'  
#'   return(list("W_lu" = W_lu, "W_li" = W_li, "W_rob" = W_rob, "W_blood" = W_blood, "Q_tot" =  Q_tot,
#'                 "Q_li" = Q_li, "Q_rob" = Q_rob, "P" = P,"CLE_f" = CLE_f, "dose" = dose))
#'   })
#' }
#' 
#' ### store the values
#' params <- create.params(user_input)
#' 
#' #################################################
#' # Function for creating initial values for ODEs #
#' #################################################
#' 
#' create.inits <- function(parameters){
#'   with( as.list(parameters),{
#'     Lu <- 0; Rob <- 0;Li <- 0; Art_blood <- 0; Ven_blood <- 0;
#'     
#'     return(c("Lu" = Lu, "Rob" = Rob, "Li" = Li, "Art_blood" = Art_blood,
#'              "Ven_blood" = Ven_blood))
#'   })
#' }
#' ##store the values
#' inits <- create.inits(params)
#' 
#' #################################################
#' # Function for creating events #
#' #################################################
#' create.events<- function(parameters){
#'   with( as.list(parameters),{
#'     
#'     ldose <- length(dose)
#'     ltimes <- length(administration.time)
#'     
#'     addition <- dose
#'     if (ltimes == ldose){
#'       events <- list(data = rbind(data.frame(var = "Ven_blood",  time = administration.time,
#'                                              value = addition, method = c("add")) ))
#'     }else{
#'       stop("The times when the drug is injected should be equal in number to the doses")
#'     }
#'     
#'     
#'     return(events)
#'   })
#' }
#' 
#' events <- create.events(params)
#' 
#' ###################
#' # Custom function #
#' ###################
#' custom.fun <- function(W_li){
#'   if (W_li<15){
#'       a = 10
#'   }else{
#'       a = 15
#'   }
#'   return(a)
#' }
#' 
#' #################
#' # ODEs system #
#' #################
#' 
#' ode.fun <- function(time, Initial.values, Parameters, custom.func){
#'   with( as.list(c(Initial.values, Parameters)),{
#'  
#'   #cleararance coefficient
#'   cl = custom.func(weight)
#'   # concentrations in tissues
#'   C_lu <- Lu/W_lu
#'   C_re  <-  Rob/W_re
#'   C_li  <-  Li_tissue/W_li
#'   C_art <- Art_blood/(0.2*W_blood)
#'   C_ven <- Ven_blood/(0.8*W_blood)
#'  
#'  # Lungs
#'   dlu <- Q_tot * (C_ven - C_lu/P)
#'   # Rest of the body
#'   dRob_tissue <-  Q_rob * (C_art - C_rob/P)
#'   # Liver
#'   dLi_tissue <- Q_li * (C_art - C_li/P)- CLE*cl*C_li
#'   # Arterial blood
#'   dArt_blood <- Q_tot* C_lu/P - Cart * (Q_li + Q_rob)
#'   # Venous blood
#'   dVen_blood <- Q_li*C_li/P + Q_rob*C_rob/P - Q_tot * C_ven
#'   list(c(dLu = dLu, dRob = dRob,  dLi = dLi, dArt_blood = dArt_blood, dVen_blood = dVen_blood)
#'   })
#' }
#' deploy.ode(user.input, predicted.feats, create.params, create.inits, create.events, custom.fun, ode.fun, method = "bdf", list(rtol=1e-07, atol=1e-09)
#' }
#' @export

deploy.ode <- function(user.input, predicted.feats, create.params, create.inits, create.events,
                        custom.fun, ode.fun, method = "lsodes", ...){
  # Read the base path from the reader
  base.path <- readline("Base path of jaqpot *e.g.: https://api.jaqpot.org/ : ")
  # Log into Jaqpot using the LoginJaqpot helper function in utils.R
  token <- .LoginJaqpot(base.path)
  # Ask the user for a a model title
  title <- readline("Title of the model: ")
  # Ask the user for a short model description
  description <- readline("Short description of the model:")
  # Set the time vector variables (for ODE output)
  independent.features <- c(names(user.input), "sim.start" , "sim.end", "sim.step")
  # Convert three dots into list
  extra.args <- list(...)

  # Which library is necessary for obtaining predictions from the PBPK model
  libabry_in <- "deSolve"
  # What the model output is
  predicts <- predicted.feats
  predicts[length(predicts)+1] <- "time"
  # Serialize the model in order to upload it on Jaqpot
  model <- serialize(list(create.params = create.params, create.inits = create.inits,
                          create.events = create.events, custom.func = custom.func,
                          ode.func = ode.func),connection=NULL)
  # Create a list containing the information that will be uploaded on Jaqpot
  tojson <- list(rawModel=model,runtime="pbpk-ode", implementedWith=libabry_in, pmmlModel=NULL,
                 independentFeatures=independent.features, predictedFeatures=predicts,
                 dependentFeatures=predicts, title=title, description=description,
                 algorithm="PBPK custom", additionalInfo = list("extra.args" = extra.args,
                                                                "method" = method))
  # Convert the list to a JSON data format
  json <- jsonlite::toJSON(tojson)
  # Function that posts the model on Jaqpot
  .PostOnService(base.path, token, json)
}
