#' Create a new Scores
#'
#' @description
#' Scores Class
#'
#' @docType class
#' @title Scores
#' @description Scores Class
#' @format An \code{R6Class} generator object
#' @field regression  \link{RegressionScores} [optional]
#' @field binaryClassification  \link{BinaryClassificationScores} [optional]
#' @field multiclassClassification  \link{MulticlassClassificationScores} [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Scores <- R6::R6Class(
  "Scores",
  public = list(
    `regression` = NULL,
    `binaryClassification` = NULL,
    `multiclassClassification` = NULL,

    #' @description
    #' Initialize a new Scores class.
    #'
    #' @param regression regression
    #' @param binaryClassification binaryClassification
    #' @param multiclassClassification multiclassClassification
    #' @param ... Other optional arguments.
    initialize = function(`regression` = NULL, `binaryClassification` = NULL, `multiclassClassification` = NULL, ...) {
      if (!is.null(`regression`)) {
        stopifnot(R6::is.R6(`regression`))
        self$`regression` <- `regression`
      }
      if (!is.null(`binaryClassification`)) {
        stopifnot(R6::is.R6(`binaryClassification`))
        self$`binaryClassification` <- `binaryClassification`
      }
      if (!is.null(`multiclassClassification`)) {
        stopifnot(R6::is.R6(`multiclassClassification`))
        self$`multiclassClassification` <- `multiclassClassification`
      }
    },

    #' @description
    #' To JSON String
    #'
    #' @return Scores in JSON format
    toJSON = function() {
      ScoresObject <- list()
      if (!is.null(self$`regression`)) {
        ScoresObject[["regression"]] <-
          self$`regression`$toJSON()
      }
      if (!is.null(self$`binaryClassification`)) {
        ScoresObject[["binaryClassification"]] <-
          self$`binaryClassification`$toJSON()
      }
      if (!is.null(self$`multiclassClassification`)) {
        ScoresObject[["multiclassClassification"]] <-
          self$`multiclassClassification`$toJSON()
      }
      ScoresObject
    },

    #' @description
    #' Deserialize JSON string into an instance of Scores
    #'
    #' @param input_json the JSON input
    #' @return the instance of Scores
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`regression`)) {
        `regression_object` <- RegressionScores$new()
        `regression_object`$fromJSON(jsonlite::toJSON(this_object$`regression`, auto_unbox = TRUE, digits = NA))
        self$`regression` <- `regression_object`
      }
      if (!is.null(this_object$`binaryClassification`)) {
        `binaryclassification_object` <- BinaryClassificationScores$new()
        `binaryclassification_object`$fromJSON(jsonlite::toJSON(this_object$`binaryClassification`, auto_unbox = TRUE, digits = NA))
        self$`binaryClassification` <- `binaryclassification_object`
      }
      if (!is.null(this_object$`multiclassClassification`)) {
        `multiclassclassification_object` <- MulticlassClassificationScores$new()
        `multiclassclassification_object`$fromJSON(jsonlite::toJSON(this_object$`multiclassClassification`, auto_unbox = TRUE, digits = NA))
        self$`multiclassClassification` <- `multiclassclassification_object`
      }
      self
    },

    #' @description
    #' To JSON String
    #'
    #' @return Scores in JSON format
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`regression`)) {
          sprintf(
          '"regression":
          %s
          ',
          jsonlite::toJSON(self$`regression`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`binaryClassification`)) {
          sprintf(
          '"binaryClassification":
          %s
          ',
          jsonlite::toJSON(self$`binaryClassification`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`multiclassClassification`)) {
          sprintf(
          '"multiclassClassification":
          %s
          ',
          jsonlite::toJSON(self$`multiclassClassification`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },

    #' @description
    #' Deserialize JSON string into an instance of Scores
    #'
    #' @param input_json the JSON input
    #' @return the instance of Scores
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`regression` <- RegressionScores$new()$fromJSON(jsonlite::toJSON(this_object$`regression`, auto_unbox = TRUE, digits = NA))
      self$`binaryClassification` <- BinaryClassificationScores$new()$fromJSON(jsonlite::toJSON(this_object$`binaryClassification`, auto_unbox = TRUE, digits = NA))
      self$`multiclassClassification` <- MulticlassClassificationScores$new()$fromJSON(jsonlite::toJSON(this_object$`multiclassClassification`, auto_unbox = TRUE, digits = NA))
      self
    },

    #' @description
    #' Validate JSON input with respect to Scores and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Scores
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
# Scores$unlock()
#
## Below is an example to define the print function
# Scores$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Scores$lock()

