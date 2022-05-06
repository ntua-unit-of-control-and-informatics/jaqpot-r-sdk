#' Jaqpot login using credentials with masking
#'
#' Provides the user the ability to communicate with the jaqpot server
#' 
#' @param url The base path of Jaqpot services. This argument is optional and needs 
#' to be changed only if an alternative Jaqpot installation is used.
#'
#' @return The function invisibly returns the jaqpot token, which is also written in the global environment
#' so that it can be used by other jaqpotr functions.
#' @details The user  generates a 
#' token that is used by jaqpotr functions by providing the jaqpot username and password. The difference of this
#' function with the function `login.cred` is that it receives the user input with masking. 
#' 
#' @export

login.cred.safe <- function(url = "https://api.jaqpot.org/jaqpot/"){
    # Get jaqpot username and password
    username <- readline("Username: ")
    password <- getPass::getPass(msg = "Password: ", noblank = FALSE, forcemask = FALSE)
    loginto <- paste(url, "services/aa/login/", sep = "")
    body <- list(username=username, password = password)
    httr::set_config(httr::config(ssl_verifypeer = 0L))
    
    tryCatch({
    res <-  httr::POST(loginto, body = body, encode = "form")
    stopifnot(httr::status_code(res) < 300)
    res <- httr::content(res, "text", encoding = 'UTF-8')
    authResponse <- jsonlite::fromJSON(res)
    #globally define token to be seen by other jaqpot functions 
    jaqpot.token <<- authResponse$authToken
    }, error = function(e) {
          e$message <-"http call failed. Make sure you provided the correct username and password."
          stop(e)
    })
}
    
    
#' Jaqpot login using credentials without masking
#'
#' Provides the user the ability to communicate with the jaqpot server
#' 
#' @param url The base path of Jaqpot services. This argument is optional and needs 
#' to be changed only if an alternative Jaqpot installation is used.
#'
#' @return The function invisibly returns the jaqpot token, which is also written in the global environment
#' so that it can be used by other jaqpotr functions.
#' @details The user  generates a 
#' token that is used by jaqpotr functions by providing the jaqpot username and password.
#' 
#' @export


login.cred <- function(username, password, url = "https://api.jaqpot.org/jaqpot/"){
    loginto <- paste(url, "services/aa/login/", sep = "")
    body <- list(username=username, password = password)
    httr::set_config(httr::config(ssl_verifypeer = 0L))
    
    tryCatch({
    res <-  httr::POST(loginto, body = body, encode = "form")
    stopifnot(httr::status_code(res) < 300)
    res <- httr::content(res, "text", encoding = 'UTF-8')
    authResponse <- jsonlite::fromJSON(res)
    #globally define token to be seen by other jaqpot functions 
    jaqpot.token <<- authResponse$authToken
    }, error = function(e) {
          e$message <-"http call failed. Make sure you provided the correct username and password."
          stop(e)
    })
}    
    
  
 #' Jaqpot login using api key with masking
#'
#' Provides the user the ability to communicate with the jaqpot server
#' 
#' @param url The base path of Jaqpot services. This argument is optional and needs 
#' to be changed only if an alternative Jaqpot installation is used.
#'
#' @return The function invisibly returns the jaqpot token, which is also written in the global environment
#' so that it can be used by other jaqpotr functions.
#' @details The user  generates a 
#' token that is used by jaqpotr functions by providing the jaqpot api key. The difference of this
#' function with the function `login.api` is that it receives the user input with masking. 
#' 
#' @export 
login.api.safe <- function(url = "https://api.jaqpot.org/jaqpot/"){
 
    tryCatch({
      API_key <- getPass::getPass(msg = "API Key: ", noblank = FALSE, forcemask = FALSE)
      loginto <- paste(basepath, "services/aa/validate/accesstoken", sep = "")
      httr::set_config(httr::config(ssl_verifypeer = 0L))
      res <-  httr::POST(loginto, body = API_key)
      stopifnot(httr::status_code(res) < 300)
      jaqpot.token <<- API_key
    }, error = function(e) {
      e$message <-"http call failed. Make sure you provided the correct API key."
      stop(e)
    }) 
}


 #' Jaqpot login using api key 
#'
#' Provides the user the ability to communicate with the jaqpot server
#' 
#' @param url The base path of Jaqpot services. This argument is optional and needs 
#' to be changed only if an alternative Jaqpot installation is used.
#'
#' @return The function invisibly returns the jaqpot token, which is also written in the global environment
#' so that it can be used by other jaqpotr functions.
#' @details The user  generates a 
#' token that is used by jaqpotr functions by providing the jaqpot api key. 
#'
#' @export 
login.api <- function(API_key, url = "https://api.jaqpot.org/jaqpot/"){
 
    tryCatch({
      loginto <- paste(url, "services/aa/validate/accesstoken", sep = "")
      httr::set_config(httr::config(ssl_verifypeer = 0L))
      res <-  httr::POST(loginto, body = API_key)
      stopifnot(httr::status_code(res) < 300)
      jaqpot.token <<- API_key
    }, error = function(e) {
      e$message <-"http call failed. Make sure you provided the correct API key."
      stop(e)
    }) 
}
