#' @title Deploy an Ordinary Differential Equation (ODE) model on Jaqpot.
#'
#' @description Uploads an ODE model on Jaqpot that can be solved using the \code{ode}
#' function of package 'deSolve'.
#'
#' @param user.input A list with keys the names of the inputs that the
#' end-user will be asked to complete on the Jaqpot
#' User Interface (UI) and values expemplary input values.
#'
#' @param out.vars A vector containing the names of the state variables
#' of the ODEs to be printed on the UI. The names should be a
#' subset of the names of the state variables of the ODE system.
#'
#' @param create.params A function that receives a list of inputs (with
#' key being the name and value the corresponding value)
#' and with inner tranformations converts them to an appropriate parameter
#' vector that is used by the create.inits,
#' create.events and ode.fun. The function should return a list.
#'
#' @param create.inits A function that receives the list returned by
#' create.params and uses it to create the initial conditions of the
#' ODE system. The function should return a named vector.
#'
#' @param create.events A function that receives the list returned by
#' create.params and uses it to create the events to be forced on the
#' ODE system. The function should return a list.
#'
#' @param custom.func  A custom function that the user can call from
#' within the ode.fun
#'
#' @param ode.fun  The ODE system in a function format that is
#' appropriate for use in the 'deSolve' package solvers.
#'
#' @param method A string declaring the ODE solver to be used. The
#' user should see all available options from 'deSolve' package
#' @param url The base path of Jaqpot services. This argument is optional and needs
#' to be changed only if an alternative Jaqpot installation is used.
#'
#' @param ... Extra arguments to be passed down to the solver.
#'
#' @return  The id of the uploaded model.
#' @details The user should provide the inputs
#' that the end-user will provide on the Jaqpot
#' User Interface (UI) and the state variables of the ODE system
#' that are to be printed on the UI upon execution
#' of the ODE system. In addition, the user should provide five functions.
#' The first function transforms the user input according to the needs
#' of the ODE model, the
#' second creates the initial conditions of the ODEs, the third creates the
#' events that are forced
#' on the system, the fourth enables the use of custom functions
#' inside the ODEs and the last is the ODEs,
#' with syntax compatible with package 'deSolve'. The functions
#' are used in a nested style (see examples). Note that the names of
#' independent and dependent
#' features (i.e. user.input) cannot be further modified via the Jaqpot UI,
#' so the user should choose them with caution.
#'
#' @examples
#' \dontrun{
#'
#' ##########################################
#' # Function for creating parameter vector #
#' ##########################################
#'
#' create.params <- function(input){
#'   with( as.list(input),{
#'
#'     ############################
#'     # Physiological parameters #
#'     ############################
#'    # tissue weights (in g)
#'     W_tot <- weight # ;body weight, experimental data - g
#'     W_lu <-1.2 # weight of lungs, experimental data - g
#'     W_li <- 10.03 # weight of liver, experimental data - g
#'
#'     W_blood <- 0.065 * W_tot
#'     W_rob <- W_tot - (W_blood + W_li + W_lu)
#'
#'     #Regional blood flows (in mL per hour)
#'     fQl = 0.183 # fraction of cardiac output to liver, unitless
#'     fQrob = 1-fQl # fraction of cardiac output to rest of the body,  unitless
#'     Q_tot <- 4980 # cardiac output, mL/h
#'     Q_li <- fQl*Q_tot    # blood flow to liver, mL/h
#'     Q_rob <- fQrob*Q_tot # blood flow to rest of the body, mL/h
#'
#'     P <-1.435445 # partition coefficient tissue:blood, unitless
#'     CLE_f <- 9.958839e-05 # clearance rate to feces from liver,  mL/h
#'
#'     return(list("W_lu" = W_lu, "W_li" = W_li, "W_rob" = W_rob, "W_blood" = W_blood, "Q_tot" =  Q_tot,
#'                 "Q_li" = Q_li, "Q_rob" = Q_rob, "P" = P,"CLE_f" = CLE_f, "dose" = dose,
#'                 "administration.time" = administration.time, "weight" = weight))
#'   })
#' }
#'
#' #################################################
#' # Function for creating initial values for ODEs #
#' #################################################
#'
#' create.inits <- function(parameters){
#'   with( as.list(parameters),{
#'     Lu <- 0; Rob <- 0;Li <- 0; Mart <- 0; Mven <- 0;
#'
#'     return(c("Mlu" = Lu, "Mrob" = Rob, "Mli" = Li, "Mart" = Mart,
#'              "Mven" = Mven))
#'   })
#' }
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
#'       events <- list(data = rbind(data.frame(var = "Mven",  time = administration.time,
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
#'
#' ###################
#' # Custom function #
#' ###################
#' custom.func <- function(W_li){
#'   if (W_li<15){
#'     a = 10
#'   }else{
#'     a = 15
#'   }
#'   return(a)
#' }
#'
#' #################
#' # ODEs system #
#' #################
#'
#' ode.func <- function(time, Initial.values, Parameters, custom.func){
#'   with( as.list(c(Initial.values, Parameters)),{
#'
#'     #'cleararance coefficient
#'     CL = custom.func(weight)
#'     #' concentrations in tissues
#'     C_lu <- Mlu/W_lu
#'     C_rob  <-  Mrob/W_rob
#'     C_li  <-  Mli/W_li
#'     C_art <- Mart/(0.2*W_blood)
#'     C_ven <- Mven/(0.8*W_blood)
#'
#'     #' Lungs
#'     dMlu <- Q_tot * (C_ven - C_lu/P)
#'     #' Rest of the body
#'     dMrob <-  Q_rob * (C_art - C_rob/P)
#'     #' Liver
#'     dMli <- Q_li * (C_art - C_li/P)- CLE_f*CL*C_li
#'     #' Arterial blood
#'     dMart <- Q_tot* C_lu/P - C_art * (Q_li + Q_rob)
#'     #' Venous blood
#'     dMven <- Q_li*C_li/P + Q_rob*C_rob/P - Q_tot * C_ven
#'     list(c(dMlu = dMlu, dMrob = dMrob,  dMli = dMli, dMart = dMart, dMven = dMven))
#'   })
#' }
#'
#' user.input <-list("weight" = 250,"dose" = c(10,12), "administration.time" = c(0,1.5) )
#' out.vars <- c("Li")
#' params <- create.params(user.input)
#' inits <- create.inits(params)
#' events <- create.events(params)
#' sample_time <- seq(1,10,1)
#' solution <- data.frame(deSolve::ode(times = sample_time,  func = ode.func,
#'                                     y = inits,custom.func = custom.func, parms = params,
#'                                     events = events,
#'                                     method="lsodes",rtol = 1e-07, atol = 1e-07))
#' deploy.pbpk(user.input, out.vars, create.params, create.inits, create.events,
#'             custom.func, ode.fun, method = "bdf", list(rtol=1e-07, atol=1e-09))
#'}
#' @export
#'


deploy.pbpk <- function(user.input, out.vars, create.params, create.inits,
                        create.events,custom.func, ode.fun, method = "lsodes",
                        url = "https://api.jaqpot.org/", envFile =NULL){
  
  before_sourcing <- ls()
  
  if (!is.null(envFile)){
    dotenv::load_dot_env(file = envFile)
    JAQPOT_API_KEY <- Sys.getenv("JAQPOT_API_KEY")
    JAQPOT_API_SECRET <- Sys.getenv("JAQPOT_API_SECRET") 
  }else{
    JAQPOT_API_KEY <- getPass::getPass("Provide JAQPOT_API_KEY: ")
    JAQPOT_API_SECRET <- getPass::getPass("Provide JAQPOT_API_SECRET: ")
  } 
  openapi_folder <- file.path( "./openapi")
  r_files <- list.files(openapi_folder, pattern = "\\.R$", full.names = TRUE)
  invisible(lapply(r_files, function(file) source(file, echo = FALSE, print.eval = FALSE)))
  after_sourcing <- ls()

  

  # Ask the user for a a model title
  title <- readline("Title of the model: ")
  if (nchar(title) < 3){
    stop("Please provide a title that is more than 3 characters long")
  }
  # Ask the user for a short model description
  description  <- readline("Short description of the model:")
  if (nchar(description)!= 0 && nchar(description) < 3){
    stop("Please provide a description that is more than 3 characters long")
  }
  # Set the time vector variables (for ODE output)
  independent.features <- c(names(user.input), "sim.start" , "sim.end", "sim.step")
  # # Convert three dots into list
  # extra.args <- list(...)

  # Which library is necessary for obtaining predictions from the PBPK model
  libabry_in <- "deSolve"
  # What the model output is
  predicts <- out.vars
  predicts[length(predicts)+1] <- "time"
  # Serialize the model in order to upload it on Jaqpot
  model <- base64enc::base64encode(serialize(list(create.params = create.params, create.inits = create.inits,
                          create.events = create.events, custom.func = custom.func,
                          ode.func = ode.func),connection=NULL))

  independentFeatures <- list()
  for (i in 1:length(independent.features)){
    independentFeatures[[i]] <- Feature$new(key = independent.features[i], name = independent.features[i], 
                                            featureType = FeatureType$new(c("FLOAT")))
  }
  
  dependentFeatures <- list()
  for (i in 1:length(predicts)){
    dependentFeatures[[i]] <- Feature$new(key = predicts[i], name = predicts[i], 
                                          featureType = FeatureType$new(c("FLOAT")))
  }
  
  library <-  c(Library$new(name = "deSolve", version = "1.40" ))
  type <-  ModelType$new("R_PBPK")

  task <- ModelTask$new("REGRESSION")
  rPbpkConfig <- RPbpkConfig$new(odeSolver = method)
  visilibity <- ModelVisibility$new("PRIVATE")
  
  var_model <- Model$new(name = title, description = description,
                         independentFeatures = independentFeatures,
                         dependentFeatures = dependentFeatures,  
                         libraries =library,  jaqpotpyVersion = '0.0',
                         rawModel = model, type = type, 
                         task = task,
                         rPbpkConfig =  rPbpkConfig,
                         visibility = visilibity)

  
  default_headers <- c("X-Api-Key" = JAQPOT_API_KEY, "X-Api-Secret" = JAQPOT_API_SECRET)
  api_client <- ApiClient$new(base_path = url,default_headers = default_headers)
  api_instance <- ModelApi$new(api_client = api_client)
  #Deploy model
  response <- api_instance$CreateModel(var_model)


  new_objects <- setdiff(after_sourcing, before_sourcing)
  rm(list = new_objects)
}
