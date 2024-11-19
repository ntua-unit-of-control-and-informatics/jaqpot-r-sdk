#' Create a new User
#'
#' @description
#' User Class
#'
#' @docType class
#' @title User
#' @description User Class
#' @format An \code{R6Class} generator object
#' @field id  character
#' @field username  character [optional]
#' @field firstName  character [optional]
#' @field lastName  character [optional]
#' @field email  character [optional]
#' @field emailVerified  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
User <- R6::R6Class(
  "User",
  public = list(
    `id` = NULL,
    `username` = NULL,
    `firstName` = NULL,
    `lastName` = NULL,
    `email` = NULL,
    `emailVerified` = NULL,

    #' @description
    #' Initialize a new User class.
    #'
    #' @param id id
    #' @param username username
    #' @param firstName firstName
    #' @param lastName lastName
    #' @param email email
    #' @param emailVerified emailVerified
    #' @param ... Other optional arguments.
    initialize = function(`id`, `username` = NULL, `firstName` = NULL, `lastName` = NULL, `email` = NULL, `emailVerified` = NULL, ...) {
      if (!missing(`id`)) {
        if (!(is.character(`id`) && length(`id`) == 1)) {
          stop(paste("Error! Invalid data for `id`. Must be a string:", `id`))
        }
        self$`id` <- `id`
      }
      if (!is.null(`username`)) {
        if (!(is.character(`username`) && length(`username`) == 1)) {
          stop(paste("Error! Invalid data for `username`. Must be a string:", `username`))
        }
        self$`username` <- `username`
      }
      if (!is.null(`firstName`)) {
        if (!(is.character(`firstName`) && length(`firstName`) == 1)) {
          stop(paste("Error! Invalid data for `firstName`. Must be a string:", `firstName`))
        }
        self$`firstName` <- `firstName`
      }
      if (!is.null(`lastName`)) {
        if (!(is.character(`lastName`) && length(`lastName`) == 1)) {
          stop(paste("Error! Invalid data for `lastName`. Must be a string:", `lastName`))
        }
        self$`lastName` <- `lastName`
      }
      if (!is.null(`email`)) {
        if (!(is.character(`email`) && length(`email`) == 1)) {
          stop(paste("Error! Invalid data for `email`. Must be a string:", `email`))
        }
        self$`email` <- `email`
      }
      if (!is.null(`emailVerified`)) {
        if (!(is.logical(`emailVerified`) && length(`emailVerified`) == 1)) {
          stop(paste("Error! Invalid data for `emailVerified`. Must be a boolean:", `emailVerified`))
        }
        self$`emailVerified` <- `emailVerified`
      }
    },

    #' @description
    #' To JSON String
    #'
    #' @return User in JSON format
    toJSON = function() {
      UserObject <- list()
      if (!is.null(self$`id`)) {
        UserObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`username`)) {
        UserObject[["username"]] <-
          self$`username`
      }
      if (!is.null(self$`firstName`)) {
        UserObject[["firstName"]] <-
          self$`firstName`
      }
      if (!is.null(self$`lastName`)) {
        UserObject[["lastName"]] <-
          self$`lastName`
      }
      if (!is.null(self$`email`)) {
        UserObject[["email"]] <-
          self$`email`
      }
      if (!is.null(self$`emailVerified`)) {
        UserObject[["emailVerified"]] <-
          self$`emailVerified`
      }
      UserObject
    },

    #' @description
    #' Deserialize JSON string into an instance of User
    #'
    #' @param input_json the JSON input
    #' @return the instance of User
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`username`)) {
        self$`username` <- this_object$`username`
      }
      if (!is.null(this_object$`firstName`)) {
        self$`firstName` <- this_object$`firstName`
      }
      if (!is.null(this_object$`lastName`)) {
        self$`lastName` <- this_object$`lastName`
      }
      if (!is.null(this_object$`email`)) {
        self$`email` <- this_object$`email`
      }
      if (!is.null(this_object$`emailVerified`)) {
        self$`emailVerified` <- this_object$`emailVerified`
      }
      self
    },

    #' @description
    #' To JSON String
    #'
    #' @return User in JSON format
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`id`)) {
          sprintf(
          '"id":
            "%s"
                    ',
          self$`id`
          )
        },
        if (!is.null(self$`username`)) {
          sprintf(
          '"username":
            "%s"
                    ',
          self$`username`
          )
        },
        if (!is.null(self$`firstName`)) {
          sprintf(
          '"firstName":
            "%s"
                    ',
          self$`firstName`
          )
        },
        if (!is.null(self$`lastName`)) {
          sprintf(
          '"lastName":
            "%s"
                    ',
          self$`lastName`
          )
        },
        if (!is.null(self$`email`)) {
          sprintf(
          '"email":
            "%s"
                    ',
          self$`email`
          )
        },
        if (!is.null(self$`emailVerified`)) {
          sprintf(
          '"emailVerified":
            %s
                    ',
          tolower(self$`emailVerified`)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },

    #' @description
    #' Deserialize JSON string into an instance of User
    #'
    #' @param input_json the JSON input
    #' @return the instance of User
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`username` <- this_object$`username`
      self$`firstName` <- this_object$`firstName`
      self$`lastName` <- this_object$`lastName`
      self$`email` <- this_object$`email`
      self$`emailVerified` <- this_object$`emailVerified`
      self
    },

    #' @description
    #' Validate JSON input with respect to User and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
      # check the required field `id`
      if (!is.null(input_json$`id`)) {
        if (!(is.character(input_json$`id`) && length(input_json$`id`) == 1)) {
          stop(paste("Error! Invalid data for `id`. Must be a string:", input_json$`id`))
        }
      } else {
        stop(paste("The JSON input `", input, "` is invalid for User: the required field `id` is missing."))
      }
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of User
    toString = function() {
      self$toJSONString()
    },

    #' @description
    #' Return true if the values in all fields are valid.
    #'
    #' @return true if the values in all fields are valid.
    isValid = function() {
      # check if the required `id` is null
      if (is.null(self$`id`)) {
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
      # check if the required `id` is null
      if (is.null(self$`id`)) {
        invalid_fields["id"] <- "Non-nullable required field `id` cannot be null."
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
# User$unlock()
#
## Below is an example to define the print function
# User$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# User$lock()

