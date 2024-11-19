#' Create a new FeaturePossibleValue
#'
#' @description
#' FeaturePossibleValue Class
#'
#' @docType class
#' @title FeaturePossibleValue
#' @description FeaturePossibleValue Class
#' @format An \code{R6Class} generator object
#' @field key  character
#' @field value  character
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
FeaturePossibleValue <- R6::R6Class(
  "FeaturePossibleValue",
  public = list(
    `key` = NULL,
    `value` = NULL,

    #' @description
    #' Initialize a new FeaturePossibleValue class.
    #'
    #' @param key key
    #' @param value value
    #' @param ... Other optional arguments.
    initialize = function(`key`, `value`, ...) {
      if (!missing(`key`)) {
        if (!(is.character(`key`) && length(`key`) == 1)) {
          stop(paste("Error! Invalid data for `key`. Must be a string:", `key`))
        }
        self$`key` <- `key`
      }
      if (!missing(`value`)) {
        if (!(is.character(`value`) && length(`value`) == 1)) {
          stop(paste("Error! Invalid data for `value`. Must be a string:", `value`))
        }
        self$`value` <- `value`
      }
    },

    #' @description
    #' To JSON String
    #'
    #' @return FeaturePossibleValue in JSON format
    toJSON = function() {
      FeaturePossibleValueObject <- list()
      if (!is.null(self$`key`)) {
        FeaturePossibleValueObject[["key"]] <-
          self$`key`
      }
      if (!is.null(self$`value`)) {
        FeaturePossibleValueObject[["value"]] <-
          self$`value`
      }
      FeaturePossibleValueObject
    },

    #' @description
    #' Deserialize JSON string into an instance of FeaturePossibleValue
    #'
    #' @param input_json the JSON input
    #' @return the instance of FeaturePossibleValue
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`key`)) {
        self$`key` <- this_object$`key`
      }
      if (!is.null(this_object$`value`)) {
        self$`value` <- this_object$`value`
      }
      self
    },

    #' @description
    #' To JSON String
    #'
    #' @return FeaturePossibleValue in JSON format
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`key`)) {
          sprintf(
          '"key":
            "%s"
                    ',
          self$`key`
          )
        },
        if (!is.null(self$`value`)) {
          sprintf(
          '"value":
            "%s"
                    ',
          self$`value`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },

    #' @description
    #' Deserialize JSON string into an instance of FeaturePossibleValue
    #'
    #' @param input_json the JSON input
    #' @return the instance of FeaturePossibleValue
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`key` <- this_object$`key`
      self$`value` <- this_object$`value`
      self
    },

    #' @description
    #' Validate JSON input with respect to FeaturePossibleValue and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
      # check the required field `key`
      if (!is.null(input_json$`key`)) {
        if (!(is.character(input_json$`key`) && length(input_json$`key`) == 1)) {
          stop(paste("Error! Invalid data for `key`. Must be a string:", input_json$`key`))
        }
      } else {
        stop(paste("The JSON input `", input, "` is invalid for FeaturePossibleValue: the required field `key` is missing."))
      }
      # check the required field `value`
      if (!is.null(input_json$`value`)) {
        if (!(is.character(input_json$`value`) && length(input_json$`value`) == 1)) {
          stop(paste("Error! Invalid data for `value`. Must be a string:", input_json$`value`))
        }
      } else {
        stop(paste("The JSON input `", input, "` is invalid for FeaturePossibleValue: the required field `value` is missing."))
      }
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of FeaturePossibleValue
    toString = function() {
      self$toJSONString()
    },

    #' @description
    #' Return true if the values in all fields are valid.
    #'
    #' @return true if the values in all fields are valid.
    isValid = function() {
      # check if the required `key` is null
      if (is.null(self$`key`)) {
        return(FALSE)
      }

      # check if the required `value` is null
      if (is.null(self$`value`)) {
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
      # check if the required `key` is null
      if (is.null(self$`key`)) {
        invalid_fields["key"] <- "Non-nullable required field `key` cannot be null."
      }

      # check if the required `value` is null
      if (is.null(self$`value`)) {
        invalid_fields["value"] <- "Non-nullable required field `value` cannot be null."
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
# FeaturePossibleValue$unlock()
#
## Below is an example to define the print function
# FeaturePossibleValue$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# FeaturePossibleValue$lock()

