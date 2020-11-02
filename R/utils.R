.SelectBasePath <- function(){

  method <- readline("Please choose jaqpot base path ([1]=Use main  / [2]=Use alternative): ")
  base.path <- "https://api.jaqpot.org/"
  if(method == 2){
    base.path <- readline("Base path of jaqpot *e.g.: https://api.jaqpot.org/ : ")
  }
  return(base.path)
}


.LoginJaqpot <- function(basepath){
  # Ask user to select authentication method; user credentials or API key
  method <- readline("Please choose authentication method ([1]=login / [2]=Provide Api Key): ")
  # If the user provided his Jaqpot credentials:
  if(method == 1){
    # Get username and
    username <- readline("Username: ")
    password <- getPass::getPass(msg = "PASSWORD: ", noblank = FALSE, forcemask = FALSE)
    loginto <- paste(basepath, "jaqpot/services/aa/login/", sep = "")
    body <- list(username=username, password = password)
    httr::set_config(httr::config(ssl_verifypeer = 0L))
    res <-  httr::POST(loginto, body = body, encode = "form")
    res <- httr::content(res, "text", encoding = 'UTF-8')
    authResponse <- jsonlite::fromJSON(res)
    token = authResponse$authToken
  } else if(method == 2) {
    token <- getPass::getPass(msg = "Api Key: ", noblank = FALSE, forcemask = FALSE)
  } else {
    print("Wrong input");
  }
  return(token)
}

#basepath <- "http://localhost:8080/"
.LoginJaqpot2 <- function(basepath){
  # Ask user to select authentication method; user credentials or API key
  method <- readline("Please choose authentication method ([1]=login / [2]=Provide Api Key): ")
  # If the user provided his Jaqpot credentials:
  if(method == 1){
    # Get username and
    username <- readline("Username: ")
    password <- getPass::getPass(msg = "PASSWORD: ", noblank = FALSE, forcemask = FALSE)
    loginto <- paste(basepath, "jaqpot/services/aa/login/", sep = "")
    body <- list(username=username, password = password)
    httr::set_config(httr::config(ssl_verifypeer = 0L))
    res <-  httr::POST(loginto, body = body, encode = "form")
    code <- httr::status_code(res)
    token <- ""
    if(code == 200 ){
        # Read the response returned by Jaqpot API
      res <- httr::content(res, "text", encoding = 'UTF-8')
      authResponse <- jsonlite::fromJSON(res)
      res <- httr::content(res, "text", encoding = 'UTF-8')
      token = authResponse$authToken
      return(token)
    } else {
        # If not successful, print the error code
      code
      respon <- "Could not login. Wrong credentials"
      respon
      return()
    }

  } else if(method == 2) {
    token <- getPass::getPass(msg = "Api Key: ", noblank = FALSE, forcemask = FALSE)
  } else {
    print("Wrong input");
  }
  return(token)
}

.PostOnService <- function(basepath, token, json){
  # Create a string representing the authentication method (bearer authentication)
  authentication = paste("Bearer", token, sep=" ")
  # Post the information to jaqpot
  res = httr::POST(basepath, path="jaqpot/services/model", httr::add_headers(Authorization=authentication),
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
    code
    respon <- jsonlite::fromJSON(resp)
    respon
  }
}
