#' Create a new CityBlockDoa
#'
#' @description
#' CityBlockDoa Class
#'
#' @docType class
#' @title CityBlockDoa
#' @description CityBlockDoa Class
#' @format An \code{R6Class} generator object
#' @field data  object [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
CityBlockDoa <- R6::R6Class(
  "CityBlockDoa",
  public = list(
    `data` = NULL,

    #' @description
    #' Initialize a new CityBlockDoa class.
    #'
    #' @param data data
    #' @param ... Other optional arguments.
    initialize = function(`data` = NULL, ...) {
      if (!is.null(`data`)) {
        self$`data` <- `data`
      }
    },

    #' @description
    #' To JSON String
    #'
    #' @return CityBlockDoa in JSON format
    toJSON = function() {
      CityBlockDoaObject <- list()
      if (!is.null(self$`data`)) {
        CityBlockDoaObject[["data"]] <-
          self$`data`
      }
      CityBlockDoaObject
    },

    #' @description
    #' Deserialize JSON string into an instance of CityBlockDoa
    #'
    #' @param input_json the JSON input
    #' @return the instance of CityBlockDoa
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`data`)) {
        self$`data` <- this_object$`data`
      }
      self
    },

    #' @description
    #' To JSON String
    #'
    #' @return CityBlockDoa in JSON format
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`data`)) {
          sprintf(
          '"data":
            "%s"
                    ',
          self$`data`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },

    #' @description
    #' Deserialize JSON string into an instance of CityBlockDoa
    #'
    #' @param input_json the JSON input
    #' @return the instance of CityBlockDoa
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`data` <- this_object$`data`
      self
    },

    #' @description
    #' Validate JSON input with respect to CityBlockDoa and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of CityBlockDoa
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
# CityBlockDoa$unlock()
#
## Below is an example to define the print function
# CityBlockDoa$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# CityBlockDoa$lock()

