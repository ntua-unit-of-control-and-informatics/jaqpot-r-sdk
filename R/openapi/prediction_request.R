#' Create a new PredictionRequest
#'
#' @description
#' PredictionRequest Class
#'
#' @docType class
#' @title PredictionRequest
#' @description PredictionRequest Class
#' @format An \code{R6Class} generator object
#' @field model  \link{PredictionModel}
#' @field dataset  \link{Dataset}
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
PredictionRequest <- R6::R6Class(
  "PredictionRequest",
  public = list(
    `model` = NULL,
    `dataset` = NULL,

    #' @description
    #' Initialize a new PredictionRequest class.
    #'
    #' @param model model
    #' @param dataset dataset
    #' @param ... Other optional arguments.
    initialize = function(`model`, `dataset`, ...) {
      if (!missing(`model`)) {
        stopifnot(R6::is.R6(`model`))
        self$`model` <- `model`
      }
      if (!missing(`dataset`)) {
        stopifnot(R6::is.R6(`dataset`))
        self$`dataset` <- `dataset`
      }
    },

    #' @description
    #' To JSON String
    #'
    #' @return PredictionRequest in JSON format
    toJSON = function() {
      PredictionRequestObject <- list()
      if (!is.null(self$`model`)) {
        PredictionRequestObject[["model"]] <-
          self$`model`$toJSON()
      }
      if (!is.null(self$`dataset`)) {
        PredictionRequestObject[["dataset"]] <-
          self$`dataset`$toJSON()
      }
      PredictionRequestObject
    },

    #' @description
    #' Deserialize JSON string into an instance of PredictionRequest
    #'
    #' @param input_json the JSON input
    #' @return the instance of PredictionRequest
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`model`)) {
        `model_object` <- PredictionModel$new()
        `model_object`$fromJSON(jsonlite::toJSON(this_object$`model`, auto_unbox = TRUE, digits = NA))
        self$`model` <- `model_object`
      }
      if (!is.null(this_object$`dataset`)) {
        `dataset_object` <- Dataset$new()
        `dataset_object`$fromJSON(jsonlite::toJSON(this_object$`dataset`, auto_unbox = TRUE, digits = NA))
        self$`dataset` <- `dataset_object`
      }
      self
    },

    #' @description
    #' To JSON String
    #'
    #' @return PredictionRequest in JSON format
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`model`)) {
          sprintf(
          '"model":
          %s
          ',
          jsonlite::toJSON(self$`model`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`dataset`)) {
          sprintf(
          '"dataset":
          %s
          ',
          jsonlite::toJSON(self$`dataset`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },

    #' @description
    #' Deserialize JSON string into an instance of PredictionRequest
    #'
    #' @param input_json the JSON input
    #' @return the instance of PredictionRequest
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`model` <- PredictionModel$new()$fromJSON(jsonlite::toJSON(this_object$`model`, auto_unbox = TRUE, digits = NA))
      self$`dataset` <- Dataset$new()$fromJSON(jsonlite::toJSON(this_object$`dataset`, auto_unbox = TRUE, digits = NA))
      self
    },

    #' @description
    #' Validate JSON input with respect to PredictionRequest and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
      # check the required field `model`
      if (!is.null(input_json$`model`)) {
        stopifnot(R6::is.R6(input_json$`model`))
      } else {
        stop(paste("The JSON input `", input, "` is invalid for PredictionRequest: the required field `model` is missing."))
      }
      # check the required field `dataset`
      if (!is.null(input_json$`dataset`)) {
        stopifnot(R6::is.R6(input_json$`dataset`))
      } else {
        stop(paste("The JSON input `", input, "` is invalid for PredictionRequest: the required field `dataset` is missing."))
      }
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of PredictionRequest
    toString = function() {
      self$toJSONString()
    },

    #' @description
    #' Return true if the values in all fields are valid.
    #'
    #' @return true if the values in all fields are valid.
    isValid = function() {
      # check if the required `model` is null
      if (is.null(self$`model`)) {
        return(FALSE)
      }

      # check if the required `dataset` is null
      if (is.null(self$`dataset`)) {
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
      # check if the required `model` is null
      if (is.null(self$`model`)) {
        invalid_fields["model"] <- "Non-nullable required field `model` cannot be null."
      }

      # check if the required `dataset` is null
      if (is.null(self$`dataset`)) {
        invalid_fields["dataset"] <- "Non-nullable required field `dataset` cannot be null."
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
# PredictionRequest$unlock()
#
## Below is an example to define the print function
# PredictionRequest$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# PredictionRequest$lock()

