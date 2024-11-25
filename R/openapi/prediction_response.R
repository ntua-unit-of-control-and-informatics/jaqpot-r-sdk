#' Create a new PredictionResponse
#'
#' @description
#' PredictionResponse Class
#'
#' @docType class
#' @title PredictionResponse
#' @description PredictionResponse Class
#' @format An \code{R6Class} generator object
#' @field predictions  list(\link{AnyType})
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
PredictionResponse <- R6::R6Class(
  "PredictionResponse",
  public = list(
    `predictions` = NULL,

    #' @description
    #' Initialize a new PredictionResponse class.
    #'
    #' @param predictions predictions
    #' @param ... Other optional arguments.
    initialize = function(`predictions`, ...) {
      if (!missing(`predictions`)) {
        stopifnot(is.vector(`predictions`), length(`predictions`) != 0)
        sapply(`predictions`, function(x) stopifnot(R6::is.R6(x)))
        self$`predictions` <- `predictions`
      }
    },

    #' @description
    #' To JSON String
    #'
    #' @return PredictionResponse in JSON format
    toJSON = function() {
      PredictionResponseObject <- list()
      if (!is.null(self$`predictions`)) {
        PredictionResponseObject[["predictions"]] <-
          lapply(self$`predictions`, function(x) x$toJSON())
      }
      PredictionResponseObject
    },

    #' @description
    #' Deserialize JSON string into an instance of PredictionResponse
    #'
    #' @param input_json the JSON input
    #' @return the instance of PredictionResponse
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`predictions`)) {
        self$`predictions` <- ApiClient$new()$deserializeObj(this_object$`predictions`, "array[AnyType]", loadNamespace("openapi"))
      }
      self
    },

    #' @description
    #' To JSON String
    #'
    #' @return PredictionResponse in JSON format
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`predictions`)) {
          sprintf(
          '"predictions":
          [%s]
',
          paste(sapply(self$`predictions`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },

    #' @description
    #' Deserialize JSON string into an instance of PredictionResponse
    #'
    #' @param input_json the JSON input
    #' @return the instance of PredictionResponse
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`predictions` <- ApiClient$new()$deserializeObj(this_object$`predictions`, "array[AnyType]", loadNamespace("openapi"))
      self
    },

    #' @description
    #' Validate JSON input with respect to PredictionResponse and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
      # check the required field `predictions`
      if (!is.null(input_json$`predictions`)) {
        stopifnot(is.vector(input_json$`predictions`), length(input_json$`predictions`) != 0)
        tmp <- sapply(input_json$`predictions`, function(x) stopifnot(R6::is.R6(x)))
      } else {
        stop(paste("The JSON input `", input, "` is invalid for PredictionResponse: the required field `predictions` is missing."))
      }
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of PredictionResponse
    toString = function() {
      self$toJSONString()
    },

    #' @description
    #' Return true if the values in all fields are valid.
    #'
    #' @return true if the values in all fields are valid.
    isValid = function() {
      # check if the required `predictions` is null
      if (is.null(self$`predictions`)) {
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
      # check if the required `predictions` is null
      if (is.null(self$`predictions`)) {
        invalid_fields["predictions"] <- "Non-nullable required field `predictions` cannot be null."
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
# PredictionResponse$unlock()
#
## Below is an example to define the print function
# PredictionResponse$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# PredictionResponse$lock()

