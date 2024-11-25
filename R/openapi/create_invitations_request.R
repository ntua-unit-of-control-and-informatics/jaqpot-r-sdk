#' Create a new CreateInvitationsRequest
#'
#' @description
#' CreateInvitationsRequest Class
#'
#' @docType class
#' @title CreateInvitationsRequest
#' @description CreateInvitationsRequest Class
#' @format An \code{R6Class} generator object
#' @field emails List of email addresses to invite list(character) [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
CreateInvitationsRequest <- R6::R6Class(
  "CreateInvitationsRequest",
  public = list(
    `emails` = NULL,

    #' @description
    #' Initialize a new CreateInvitationsRequest class.
    #'
    #' @param emails List of email addresses to invite
    #' @param ... Other optional arguments.
    initialize = function(`emails` = NULL, ...) {
      if (!is.null(`emails`)) {
        stopifnot(is.vector(`emails`), length(`emails`) != 0)
        sapply(`emails`, function(x) stopifnot(is.character(x)))
        self$`emails` <- `emails`
      }
    },

    #' @description
    #' To JSON String
    #'
    #' @return CreateInvitationsRequest in JSON format
    toJSON = function() {
      CreateInvitationsRequestObject <- list()
      if (!is.null(self$`emails`)) {
        CreateInvitationsRequestObject[["emails"]] <-
          self$`emails`
      }
      CreateInvitationsRequestObject
    },

    #' @description
    #' Deserialize JSON string into an instance of CreateInvitationsRequest
    #'
    #' @param input_json the JSON input
    #' @return the instance of CreateInvitationsRequest
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`emails`)) {
        self$`emails` <- ApiClient$new()$deserializeObj(this_object$`emails`, "array[character]", loadNamespace("openapi"))
      }
      self
    },

    #' @description
    #' To JSON String
    #'
    #' @return CreateInvitationsRequest in JSON format
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`emails`)) {
          sprintf(
          '"emails":
             [%s]
          ',
          paste(unlist(lapply(self$`emails`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },

    #' @description
    #' Deserialize JSON string into an instance of CreateInvitationsRequest
    #'
    #' @param input_json the JSON input
    #' @return the instance of CreateInvitationsRequest
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`emails` <- ApiClient$new()$deserializeObj(this_object$`emails`, "array[character]", loadNamespace("openapi"))
      self
    },

    #' @description
    #' Validate JSON input with respect to CreateInvitationsRequest and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of CreateInvitationsRequest
    toString = function() {
      self$toJSONString()
    },

    #' @description
    #' Return true if the values in all fields are valid.
    #'
    #' @return true if the values in all fields are valid.
    isValid = function() {
      if (length(self$`emails`) > 10) {
        return(FALSE)
      }
      if (length(self$`emails`) < 1) {
        return(FALSE)
      }

      TRUE
    },

    #' @description
    #' Return a list of invalid fields (if any).
    #'
    #' @return A list of invalid fields (if any).
    getInvalidFields = function() {
      invalid_fields <- list()
      if (length(self$`emails`) > 10) {
        invalid_fields["emails"] <- "Invalid length for `emails`, number of items must be less than or equal to 10."
      }
      if (length(self$`emails`) < 1) {
        invalid_fields["emails"] <- "Invalid length for ``, number of items must be greater than or equal to 1."
      }

      invalid_fields
    },

    #' @description
    #' Print the object
    print = function() {
      print(jsonlite::prettify(self$toJSONString()))
      invisible(self)
    }
  ),
  # Lock the class to prevent modifications to the method or field
  lock_class = TRUE
)
## Uncomment below to unlock the class to allow modifications of the method or field
# CreateInvitationsRequest$unlock()
#
## Below is an example to define the print function
# CreateInvitationsRequest$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# CreateInvitationsRequest$lock()

