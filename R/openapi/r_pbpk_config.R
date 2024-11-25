#' Create a new RPbpkConfig
#'
#' @description
#' Configuration for the R PBPK models
#'
#' @docType class
#' @title RPbpkConfig
#' @description RPbpkConfig Class
#' @format An \code{R6Class} generator object
#' @field odeSolver  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
RPbpkConfig <- R6::R6Class(
  "RPbpkConfig",
  public = list(
    `odeSolver` = NULL,

    #' @description
    #' Initialize a new RPbpkConfig class.
    #'
    #' @param odeSolver odeSolver
    #' @param ... Other optional arguments.
    initialize = function(`odeSolver` = NULL, ...) {
      if (!is.null(`odeSolver`)) {
        if (!(is.character(`odeSolver`) && length(`odeSolver`) == 1)) {
          stop(paste("Error! Invalid data for `odeSolver`. Must be a string:", `odeSolver`))
        }
        self$`odeSolver` <- `odeSolver`
      }
    },

    #' @description
    #' To JSON String
    #'
    #' @return RPbpkConfig in JSON format
    toJSON = function() {
      RPbpkConfigObject <- list()
      if (!is.null(self$`odeSolver`)) {
        RPbpkConfigObject[["odeSolver"]] <-
          self$`odeSolver`
      }
      RPbpkConfigObject
    },

    #' @description
    #' Deserialize JSON string into an instance of RPbpkConfig
    #'
    #' @param input_json the JSON input
    #' @return the instance of RPbpkConfig
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`odeSolver`)) {
        self$`odeSolver` <- this_object$`odeSolver`
      }
      self
    },

    #' @description
    #' To JSON String
    #'
    #' @return RPbpkConfig in JSON format
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`odeSolver`)) {
          sprintf(
          '"odeSolver":
            "%s"
                    ',
          self$`odeSolver`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },

    #' @description
    #' Deserialize JSON string into an instance of RPbpkConfig
    #'
    #' @param input_json the JSON input
    #' @return the instance of RPbpkConfig
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`odeSolver` <- this_object$`odeSolver`
      self
    },

    #' @description
    #' Validate JSON input with respect to RPbpkConfig and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of RPbpkConfig
    toString = function() {
      self$toJSONString()
    },

    #' @description
    #' Return true if the values in all fields are valid.
    #'
    #' @return true if the values in all fields are valid.
    isValid = function() {
      TRUE
    },

    #' @description
    #' Return a list of invalid fields (if any).
    #'
    #' @return A list of invalid fields (if any).
    getInvalidFields = function() {
      invalid_fields <- list()
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
# RPbpkConfig$unlock()
#
## Below is an example to define the print function
# RPbpkConfig$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# RPbpkConfig$lock()

