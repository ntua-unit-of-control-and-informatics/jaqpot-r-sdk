#' Create a new ModelScores
#'
#' @description
#' ModelScores Class
#'
#' @docType class
#' @title ModelScores
#' @description ModelScores Class
#' @format An \code{R6Class} generator object
#' @field train  list(\link{Scores}) [optional]
#' @field test  list(\link{Scores}) [optional]
#' @field crossValidation  list(\link{Scores}) [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
ModelScores <- R6::R6Class(
  "ModelScores",
  public = list(
    `train` = NULL,
    `test` = NULL,
    `crossValidation` = NULL,

    #' @description
    #' Initialize a new ModelScores class.
    #'
    #' @param train train
    #' @param test test
    #' @param crossValidation crossValidation
    #' @param ... Other optional arguments.
    initialize = function(`train` = NULL, `test` = NULL, `crossValidation` = NULL, ...) {
      if (!is.null(`train`)) {
        stopifnot(is.vector(`train`), length(`train`) != 0)
        sapply(`train`, function(x) stopifnot(R6::is.R6(x)))
        self$`train` <- `train`
      }
      if (!is.null(`test`)) {
        stopifnot(is.vector(`test`), length(`test`) != 0)
        sapply(`test`, function(x) stopifnot(R6::is.R6(x)))
        self$`test` <- `test`
      }
      if (!is.null(`crossValidation`)) {
        stopifnot(is.vector(`crossValidation`), length(`crossValidation`) != 0)
        sapply(`crossValidation`, function(x) stopifnot(R6::is.R6(x)))
        self$`crossValidation` <- `crossValidation`
      }
    },

    #' @description
    #' To JSON String
    #'
    #' @return ModelScores in JSON format
    toJSON = function() {
      ModelScoresObject <- list()
      if (!is.null(self$`train`)) {
        ModelScoresObject[["train"]] <-
          lapply(self$`train`, function(x) x$toJSON())
      }
      if (!is.null(self$`test`)) {
        ModelScoresObject[["test"]] <-
          lapply(self$`test`, function(x) x$toJSON())
      }
      if (!is.null(self$`crossValidation`)) {
        ModelScoresObject[["crossValidation"]] <-
          lapply(self$`crossValidation`, function(x) x$toJSON())
      }
      ModelScoresObject
    },

    #' @description
    #' Deserialize JSON string into an instance of ModelScores
    #'
    #' @param input_json the JSON input
    #' @return the instance of ModelScores
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`train`)) {
        self$`train` <- ApiClient$new()$deserializeObj(this_object$`train`, "array[Scores]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`test`)) {
        self$`test` <- ApiClient$new()$deserializeObj(this_object$`test`, "array[Scores]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`crossValidation`)) {
        self$`crossValidation` <- ApiClient$new()$deserializeObj(this_object$`crossValidation`, "array[Scores]", loadNamespace("openapi"))
      }
      self
    },

    #' @description
    #' To JSON String
    #'
    #' @return ModelScores in JSON format
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`train`)) {
          sprintf(
          '"train":
          [%s]
',
          paste(sapply(self$`train`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`test`)) {
          sprintf(
          '"test":
          [%s]
',
          paste(sapply(self$`test`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`crossValidation`)) {
          sprintf(
          '"crossValidation":
          [%s]
',
          paste(sapply(self$`crossValidation`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },

    #' @description
    #' Deserialize JSON string into an instance of ModelScores
    #'
    #' @param input_json the JSON input
    #' @return the instance of ModelScores
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`train` <- ApiClient$new()$deserializeObj(this_object$`train`, "array[Scores]", loadNamespace("openapi"))
      self$`test` <- ApiClient$new()$deserializeObj(this_object$`test`, "array[Scores]", loadNamespace("openapi"))
      self$`crossValidation` <- ApiClient$new()$deserializeObj(this_object$`crossValidation`, "array[Scores]", loadNamespace("openapi"))
      self
    },

    #' @description
    #' Validate JSON input with respect to ModelScores and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of ModelScores
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
# ModelScores$unlock()
#
## Below is an example to define the print function
# ModelScores$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# ModelScores$lock()

