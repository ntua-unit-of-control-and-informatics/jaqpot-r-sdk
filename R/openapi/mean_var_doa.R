#' Create a new MeanVarDoa
#'
#' @description
#' MeanVarDoa Class
#'
#' @docType class
#' @title MeanVarDoa
#' @description MeanVarDoa Class
#' @format An \code{R6Class} generator object
#' @field bounds  list(list(numeric)) [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
MeanVarDoa <- R6::R6Class(
  "MeanVarDoa",
  public = list(
    `bounds` = NULL,

    #' @description
    #' Initialize a new MeanVarDoa class.
    #'
    #' @param bounds bounds
    #' @param ... Other optional arguments.
    initialize = function(`bounds` = NULL, ...) {
      if (!is.null(`bounds`)) {
        stopifnot(is.vector(`bounds`), length(`bounds`) != 0)
        sapply(`bounds`, function(x) stopifnot(R6::is.R6(x)))
        self$`bounds` <- `bounds`
      }
    },

    #' @description
    #' To JSON String
    #'
    #' @return MeanVarDoa in JSON format
    toJSON = function() {
      MeanVarDoaObject <- list()
      if (!is.null(self$`bounds`)) {
        MeanVarDoaObject[["bounds"]] <-
          lapply(self$`bounds`, function(x) x$toJSON())
      }
      MeanVarDoaObject
    },

    #' @description
    #' Deserialize JSON string into an instance of MeanVarDoa
    #'
    #' @param input_json the JSON input
    #' @return the instance of MeanVarDoa
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`bounds`)) {
        self$`bounds` <- ApiClient$new()$deserializeObj(this_object$`bounds`, "array[array[numeric]]", loadNamespace("openapi"))
      }
      self
    },

    #' @description
    #' To JSON String
    #'
    #' @return MeanVarDoa in JSON format
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`bounds`)) {
          sprintf(
          '"bounds":
          [%s]
',
          paste(sapply(self$`bounds`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },

    #' @description
    #' Deserialize JSON string into an instance of MeanVarDoa
    #'
    #' @param input_json the JSON input
    #' @return the instance of MeanVarDoa
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`bounds` <- ApiClient$new()$deserializeObj(this_object$`bounds`, "array[array[numeric]]", loadNamespace("openapi"))
      self
    },

    #' @description
    #' Validate JSON input with respect to MeanVarDoa and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of MeanVarDoa
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
# MeanVarDoa$unlock()
#
## Below is an example to define the print function
# MeanVarDoa$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# MeanVarDoa$lock()

