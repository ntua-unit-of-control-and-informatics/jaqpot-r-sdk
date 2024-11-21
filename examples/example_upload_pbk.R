source('../R/upload_model.R')
setwd('../R')
 ##########################################
 # Function for creating parameter vector #
 ##########################################

create.params <- function(input){
   with( as.list(input),{

   ############################
   # Physiological parameters #
   ############################
   # tissue weights (in g)
   W_tot <- weight # ;body weight, experimental data - g
   W_lu <-1.2 # weight of lungs, experimental data - g
   W_li <- 10.03 # weight of liver, experimental data - g

   W_blood <- 0.065 * W_tot
   W_rob <- W_tot - (W_blood + W_li + W_lu)
   #Regional blood flows (in mL per hour)
   fQl = 0.183 # fraction of cardiac output to liver, unitless
   fQrob = 1-fQl # fraction of cardiac output to rest of the body,  unitless
   Q_tot <- 4980 # cardiac output, mL/h
   Q_li <- fQl*Q_tot    # blood flow to liver, mL/h
   Q_rob <- fQrob*Q_tot # blood flow to rest of the body, mL/h

   P <-1.435445 # partition coefficient tissue:blood, unitless
   CLE_f <- 9.958839e-05 # clearance rate to feces from liver,  mL/h

   return(list("W_lu" = W_lu, "W_li" = W_li, "W_rob" = W_rob, "W_blood" = W_blood, "Q_tot" =  Q_tot,
                 "Q_li" = Q_li, "Q_rob" = Q_rob, "P" = P,"CLE_f" = CLE_f, "dose" = dose,
               "administration.time" = administration.time, "weight" = weight))
   })
 }

 #################################################
 # Function for creating initial values for ODEs #
 #################################################

 create.inits <- function(parameters){
   with( as.list(parameters),{
     Lu <- 0; Rob <- 0;Li <- 0; Mart <- 0; Mven <- 0;

     return(c("Mlu" = Lu, "Mrob" = Rob, "Mli" = Li, "Mart" = Mart,
              "Mven" = Mven))
   })
 }

 #################################################
 # Function for creating events #
 #################################################
 create.events<- function(parameters){
   with( as.list(parameters),{

     ldose <- length(dose)
     ltimes <- length(administration.time)

     addition <- dose
     if (ltimes == ldose){
       events <- list(data = rbind(data.frame(var = "Mven",  time = administration.time,
                                              value = addition, method = c("add")) ))
     }else{
       stop("The times when the drug is injected should be equal in number to the doses")
     }


     return(events)
   })
 }


 ###################
 # Custom function #
 ###################
custom.func <- function(W_li){
   if (W_li<15){
       a = 10
   }else{
       a = 15
   }
   return(a)
 }

 #################
 # ODEs system #
 #################

ode.func <- function(time, Initial.values, Parameters, custom.func){
   with( as.list(c(Initial.values, Parameters)),{

   #cleararance coefficient
   CL = custom.func(weight)
   # concentrations in tissues
   C_lu <- Mlu/W_lu
   C_rob  <-  Mrob/W_rob
   C_li  <-  Mli/W_li
   C_art <- Mart/(0.2*W_blood)
   C_ven <- Mven/(0.8*W_blood)

  # Lungs
   dMlu <- Q_tot * (C_ven - C_lu/P)
   # Rest of the body
   dMrob <-  Q_rob * (C_art - C_rob/P)
   # Liver
   dMli <- Q_li * (C_art - C_li/P)- CLE_f*CL*C_li
   # Arterial blood
   dMart <- Q_tot* C_lu/P - C_art * (Q_li + Q_rob)
   # Venous blood
   dMven <- Q_li*C_li/P + Q_rob*C_rob/P - Q_tot * C_ven
   list(c(dMlu = dMlu, dMrob = dMrob,  dMli = dMli, dMart = dMart, dMven = dMven))
   })
 }

user.input <-list("weight" = 250,"dose" = c(10,12), "administration.time" = c(0,1.5) )
out.vars <- c("Mli")
params <- create.params(user.input)
inits <- create.inits(params)
events <- create.events(params)
sample_time <- seq(1,10,1)
solution <- data.frame(deSolve::ode(times = sample_time,  func = ode.func,
                                    y = inits,custom.func = custom.func, parms = params,
                                    events = events,
                                    method="lsodes",rtol = 1e-07, atol = 1e-07))

deploy.pbpk(user.input, out.vars, create.params, create.inits, create.events,
             custom.func, ode.fun)
