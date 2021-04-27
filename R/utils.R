
.LoginJaqpot <- function(basepath){
  tryCatch({
      auth.method <- readline("Please choose authentication method ([1]=login / [2]=Provide Api Key): ")
      stopifnot(auth.method %in% c(1,2))
      }, error = function(e){
            e$message <- "Invalid authentication method selected"
            stop(e)
      }
  )
  # If the user uses his Jaqpot credentials:
  if(auth.method == 1){
    # Get username and
    username <- readline("Username: ")
    password <- getPass::getPass(msg = "Password: ", noblank = FALSE, forcemask = FALSE)
    loginto <- paste(basepath, "services/aa/login/", sep = "")
    body <- list(username=username, password = password)
    httr::set_config(httr::config(ssl_verifypeer = 0L))
    
    tryCatch({
    res <-  httr::POST(loginto, body = body, encode = "form")
    stopifnot(httr::status_code(res) == 200)
    res <- httr::content(res, "text", encoding = 'UTF-8')
    authResponse <- jsonlite::fromJSON(res)
    token = authResponse$authToken
    }, error = function(e) {
          e$message <-"http call failed. Make sure you provided the correct username and password."
          stop(e)
    })
  } else if(auth.method == 2) {
    tryCatch({
      API_key <- getPass::getPass(msg = "API Key: ", noblank = FALSE, forcemask = FALSE)
      loginto <- paste(basepath, "services/aa/validate/accesstoken", sep = "")
      httr::set_config(httr::config(ssl_verifypeer = 0L))
      res <-  httr::POST(loginto, body = API_key)
      stopifnot(httr::status_code(res) < 300)
      token = API_key
    }, error = function(e) {
      e$message <-"http call failed. Make sure you provided the correct API key."
      stop(e)
    })
  
  } 
  return(token)
}

.PostOnService <- function(basepath, token, json){
  # Create a string representing the authentication method (bearer authentication)
  authentication = paste("Bearer", token, sep=" ")
  url <- paste(basepath, "services/model", sep = "")
  # Post the information to jaqpot
  res = httr::POST(url = url, httr::add_headers(Authorization=authentication),
                   httr::accept_json(), httr::content_type("application/json"), body = json, encode = "json")
  # If the model is successfully uploaded, it will receive the status '200'
  code <- httr::status_code(res)
  if(code == 200 ){
    # Read the response returned by Jaqpot API
    resp <- httr::content(res, "text", encoding = 'UTF-8')
    # Deserialize the JSON object
    respon <- jsonlite::fromJSON(resp)
    response <- paste("Model created. The id is: ", respon$modelId,
                      ". Please visit the application to further document your model.", sep=" ")
    # Inform the user about the success of the upload process and the model id
    response
  } else {
    # If not successful, print the error code
    print(code)
    print("Unsuccessful connection with the Jaqpot server. ")
  }
}
