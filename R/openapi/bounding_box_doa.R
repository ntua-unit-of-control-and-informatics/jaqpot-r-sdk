#' Create a new BoundingBoxDoa
#'
#' @description
#' BoundingBoxDoa Class
#'
#' @docType class
#' @title BoundingBoxDoa
#' @description BoundingBoxDoa Class
#' @format An \code{R6Class} generator object
#' @field boundingBox  list(list(numeric)) [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
BoundingBoxDoa <- R6::R6Class(
  "BoundingBoxDoa",
  public = list(
    `boundingBox` = NULL,

    #' @description
    #' Initialize a new BoundingBoxDoa class.
    #'
    #' @param boundingBox boundingBox
    #' @param ... Other optional arguments.
    initialize = function(`boundingBox` = NULL, ...) {
      if (!is.null(`boundingBox`)) {
        stopifnot(is.vector(`boundingBox`), length(`boundingBox`) != 0)
        sapply(`boundingBox`, function(x) stopifnot(R6::is.R6(x)))
        self$`boundingBox` <- `boundingBox`
      }
    },

    #' @description
    #' To JSON String
    #'
    #' @return BoundingBoxDoa in JSON format
    toJSON = function() {
      BoundingBoxDoaObject <- list()
      if (!is.null(self$`boundingBox`)) {
        BoundingBoxDoaObject[["boundingBox"]] <-
          lapply(self$`boundingBox`, function(x) x$toJSON())
      }
      BoundingBoxDoaObject
    },

    #' @description
    #' Deserialize JSON string into an instance of BoundingBoxDoa
    #'
    #' @param input_json the JSON input
    #' @return the instance of BoundingBoxDoa
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`boundingBox`)) {
        self$`boundingBox` <- ApiClient$new()$deserializeObj(this_object$`boundingBox`, "array[array[numeric]]", loadNamespace("openapi"))
      }
      self
    },

    #' @description
    #' To JSON String
    #'
    #' @return BoundingBoxDoa in JSON format
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`boundingBox`)) {
          sprintf(
          '"boundingBox":
          [%s]
',
          paste(sapply(self$`boundingBox`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },

    #' @description
    #' Deserialize JSON string into an instance of BoundingBoxDoa
    #'
    #' @param input_json the JSON input
    #' @return the instance of BoundingBoxDoa
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`boundingBox` <- ApiClient$new()$deserializeObj(this_object$`boundingBox`, "array[array[numeric]]", loadNamespace("openapi"))
      self
    },

    #' @description
    #' Validate JSON input with respect to BoundingBoxDoa and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of BoundingBoxDoa
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
# BoundingBoxDoa$unlock()
#
## Below is an example to define the print function
# BoundingBoxDoa$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# BoundingBoxDoa$lock()

