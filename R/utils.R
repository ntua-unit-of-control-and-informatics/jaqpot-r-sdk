.LoginJaqpot <- function(basepath){
  # Ask user to select authentication method; user credentials or API key
  method <- readline("Please choose authentication method: [1]: login / [2]: Api Key ")
  # If the user provided his Jaqpot credentials:
  if(method == 1){
    # Get username and
    username <- readline("Username: ")
    password <- getPass::getPass(msg = "PASSWORD: ", noblank = FALSE, forcemask = FALSE)
    loginto <- paste(basepath, "jaqpot/services/aa/login/", sep = "")
    body <- list(username=username, password = password)
    httr::set_config(config(ssl_verifypeer = 0L))
    res <-  httr::POST(loginto, body = body, encode = "form")
    res <- httr::content(res, "text")
    authResponse <- jsonlite::fromJSON(res)
    token = authResponse$authToken
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
  res = httr::POST(basepath, path="jaqpot/services/model", add_headers(Authorization=authentication),
                   accept_json(), content_type("application/json"), body = json, encode = "json")
  # If the model is successfully uploaded, it will receive the status '200'
  code <- httr::status_code(res)
  if(status_code(res) == 200 ){
    # Read the response returned by Jaqpot API
    resp <- httr::content(res, "text")
    # Deserialize the JSON object
    respon <- jsonlite::fromJSON(resp)
    response <- paste("Model created. The id is: ", respon$modelId,
                      ". Please visit the application to further document your model.", sep=" ")
    # Inform the user about the success of the upload process and the model id
    response
  } else {
    # If not successful, print the error code
    code
  }
}
