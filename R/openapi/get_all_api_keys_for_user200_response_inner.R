#' Create a new GetAllApiKeysForUser200ResponseInner
#'
#' @description
#' GetAllApiKeysForUser200ResponseInner Class
#'
#' @docType class
#' @title GetAllApiKeysForUser200ResponseInner
#' @description GetAllApiKeysForUser200ResponseInner Class
#' @format An \code{R6Class} generator object
#' @field clientKey The API key character [optional]
#' @field note Description of the API key character [optional]
#' @field createdAt Creation timestamp of the API key character [optional]
#' @field expiresAt Expiration timestamp of the API key (optional) character [optional]
#' @field enabled Whether the API key is active or disabled character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
GetAllApiKeysForUser200ResponseInner <- R6::R6Class(
  "GetAllApiKeysForUser200ResponseInner",
  public = list(
    `clientKey` = NULL,
    `note` = NULL,
    `createdAt` = NULL,
    `expiresAt` = NULL,
    `enabled` = NULL,

    #' @description
    #' Initialize a new GetAllApiKeysForUser200ResponseInner class.
    #'
    #' @param clientKey The API key
    #' @param note Description of the API key
    #' @param createdAt Creation timestamp of the API key
    #' @param expiresAt Expiration timestamp of the API key (optional)
    #' @param enabled Whether the API key is active or disabled
    #' @param ... Other optional arguments.
    initialize = function(`clientKey` = NULL, `note` = NULL, `createdAt` = NULL, `expiresAt` = NULL, `enabled` = NULL, ...) {
      if (!is.null(`clientKey`)) {
        if (!(is.character(`clientKey`) && length(`clientKey`) == 1)) {
          stop(paste("Error! Invalid data for `clientKey`. Must be a string:", `clientKey`))
        }
        self$`clientKey` <- `clientKey`
      }
      if (!is.null(`note`)) {
        if (!(is.character(`note`) && length(`note`) == 1)) {
          stop(paste("Error! Invalid data for `note`. Must be a string:", `note`))
        }
        self$`note` <- `note`
      }
      if (!is.null(`createdAt`)) {
        if (!is.character(`createdAt`)) {
          stop(paste("Error! Invalid data for `createdAt`. Must be a string:", `createdAt`))
        }
        self$`createdAt` <- `createdAt`
      }
      if (!is.null(`expiresAt`)) {
        if (!is.character(`expiresAt`)) {
          stop(paste("Error! Invalid data for `expiresAt`. Must be a string:", `expiresAt`))
        }
        self$`expiresAt` <- `expiresAt`
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
    #' @return GetAllApiKeysForUser200ResponseInner in JSON format
    toJSON = function() {
      GetAllApiKeysForUser200ResponseInnerObject <- list()
      if (!is.null(self$`clientKey`)) {
        GetAllApiKeysForUser200ResponseInnerObject[["clientKey"]] <-
          self$`clientKey`
      }
      if (!is.null(self$`note`)) {
        GetAllApiKeysForUser200ResponseInnerObject[["note"]] <-
          self$`note`
      }
      if (!is.null(self$`createdAt`)) {
        GetAllApiKeysForUser200ResponseInnerObject[["createdAt"]] <-
          self$`createdAt`
      }
      if (!is.null(self$`expiresAt`)) {
        GetAllApiKeysForUser200ResponseInnerObject[["expiresAt"]] <-
          self$`expiresAt`
      }
      if (!is.null(self$`enabled`)) {
        GetAllApiKeysForUser200ResponseInnerObject[["enabled"]] <-
          self$`enabled`
      }
      GetAllApiKeysForUser200ResponseInnerObject
    },

    #' @description
    #' Deserialize JSON string into an instance of GetAllApiKeysForUser200ResponseInner
    #'
    #' @param input_json the JSON input
    #' @return the instance of GetAllApiKeysForUser200ResponseInner
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`clientKey`)) {
        self$`clientKey` <- this_object$`clientKey`
      }
      if (!is.null(this_object$`note`)) {
        self$`note` <- this_object$`note`
      }
      if (!is.null(this_object$`createdAt`)) {
        self$`createdAt` <- this_object$`createdAt`
      }
      if (!is.null(this_object$`expiresAt`)) {
        self$`expiresAt` <- this_object$`expiresAt`
      }
      if (!is.null(this_object$`enabled`)) {
        self$`enabled` <- this_object$`enabled`
      }
      self
    },

    #' @description
    #' To JSON String
    #'
    #' @return GetAllApiKeysForUser200ResponseInner in JSON format
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`clientKey`)) {
          sprintf(
          '"clientKey":
            "%s"
                    ',
          self$`clientKey`
          )
        },
        if (!is.null(self$`note`)) {
          sprintf(
          '"note":
            "%s"
                    ',
          self$`note`
          )
        },
        if (!is.null(self$`createdAt`)) {
          sprintf(
          '"createdAt":
            "%s"
                    ',
          self$`createdAt`
          )
        },
        if (!is.null(self$`expiresAt`)) {
          sprintf(
          '"expiresAt":
            "%s"
                    ',
          self$`expiresAt`
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
    #' Deserialize JSON string into an instance of GetAllApiKeysForUser200ResponseInner
    #'
    #' @param input_json the JSON input
    #' @return the instance of GetAllApiKeysForUser200ResponseInner
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`clientKey` <- this_object$`clientKey`
      self$`note` <- this_object$`note`
      self$`createdAt` <- this_object$`createdAt`
      self$`expiresAt` <- this_object$`expiresAt`
      self$`enabled` <- this_object$`enabled`
      self
    },

    #' @description
    #' Validate JSON input with respect to GetAllApiKeysForUser200ResponseInner and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of GetAllApiKeysForUser200ResponseInner
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
# GetAllApiKeysForUser200ResponseInner$unlock()
#
## Below is an example to define the print function
# GetAllApiKeysForUser200ResponseInner$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# GetAllApiKeysForUser200ResponseInner$lock()

