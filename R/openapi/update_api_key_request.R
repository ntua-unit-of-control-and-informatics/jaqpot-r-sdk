#' Create a new UpdateApiKeyRequest
#'
#' @description
#' UpdateApiKeyRequest Class
#'
#' @docType class
#' @title UpdateApiKeyRequest
#' @description UpdateApiKeyRequest Class
#' @format An \code{R6Class} generator object
#' @field note Updated description for the API key character [optional]
#' @field enabled Set to `false` to disable the API key character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
UpdateApiKeyRequest <- R6::R6Class(
  "UpdateApiKeyRequest",
  public = list(
    `note` = NULL,
    `enabled` = NULL,

    #' @description
    #' Initialize a new UpdateApiKeyRequest class.
    #'
    #' @param note Updated description for the API key
    #' @param enabled Set to `false` to disable the API key
    #' @param ... Other optional arguments.
    initialize = function(`note` = NULL, `enabled` = NULL, ...) {
      if (!is.null(`note`)) {
        if (!(is.character(`note`) && length(`note`) == 1)) {
          stop(paste("Error! Invalid data for `note`. Must be a string:", `note`))
        }
        self$`note` <- `note`
      }
      if (!is.null(`enabled`)) {
        if (!(is.logical(`enabled`) && length(`enabled`) == 1)) {
          stop(paste("Error! Invalid data for `enabled`. Must be a boolean:", `enabled`))
        }
        self$`enabled` <- `enabled`
      }
    },

    #' @description
    #' To JSON String
    #'
    #' @return UpdateApiKeyRequest in JSON format
    toJSON = function() {
      UpdateApiKeyRequestObject <- list()
      if (!is.null(self$`note`)) {
        UpdateApiKeyRequestObject[["note"]] <-
          self$`note`
      }
      if (!is.null(self$`enabled`)) {
        UpdateApiKeyRequestObject[["enabled"]] <-
          self$`enabled`
      }
      UpdateApiKeyRequestObject
    },

    #' @description
    #' Deserialize JSON string into an instance of UpdateApiKeyRequest
    #'
    #' @param input_json the JSON input
    #' @return the instance of UpdateApiKeyRequest
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`note`)) {
        self$`note` <- this_object$`note`
      }
      if (!is.null(this_object$`enabled`)) {
        self$`enabled` <- this_object$`enabled`
      }
      self
    },

    #' @description
    #' To JSON String
    #'
    #' @return UpdateApiKeyRequest in JSON format
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`note`)) {
          sprintf(
          '"note":
            "%s"
                    ',
          self$`note`
          )
        },
        if (!is.null(self$`enabled`)) {
          sprintf(
          '"enabled":
            %s
                    ',
          tolower(self$`enabled`)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },

    #' @description
    #' Deserialize JSON string into an instance of UpdateApiKeyRequest
    #'
    #' @param input_json the JSON input
    #' @return the instance of UpdateApiKeyRequest
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`note` <- this_object$`note`
      self$`enabled` <- this_object$`enabled`
      self
    },

    #' @description
    #' Validate JSON input with respect to UpdateApiKeyRequest and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of UpdateApiKeyRequest
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
# UpdateApiKeyRequest$unlock()
#
## Below is an example to define the print function
# UpdateApiKeyRequest$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# UpdateApiKeyRequest$lock()

