#' Create a new OrganizationUser
#'
#' @description
#' OrganizationUser Class
#'
#' @docType class
#' @title OrganizationUser
#' @description OrganizationUser Class
#' @format An \code{R6Class} generator object
#' @field id  integer [optional]
#' @field userId  character
#' @field username  character [optional]
#' @field email  character [optional]
#' @field associationType  \link{OrganizationUserAssociationType}
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
OrganizationUser <- R6::R6Class(
  "OrganizationUser",
  public = list(
    `id` = NULL,
    `userId` = NULL,
    `username` = NULL,
    `email` = NULL,
    `associationType` = NULL,

    #' @description
    #' Initialize a new OrganizationUser class.
    #'
    #' @param userId userId
    #' @param associationType associationType
    #' @param id id
    #' @param username username
    #' @param email email
    #' @param ... Other optional arguments.
    initialize = function(`userId`, `associationType`, `id` = NULL, `username` = NULL, `email` = NULL, ...) {
      if (!missing(`userId`)) {
        if (!(is.character(`userId`) && length(`userId`) == 1)) {
          stop(paste("Error! Invalid data for `userId`. Must be a string:", `userId`))
        }
        self$`userId` <- `userId`
      }
      if (!missing(`associationType`)) {
        if (!(`associationType` %in% c())) {
          stop(paste("Error! \"", `associationType`, "\" cannot be assigned to `associationType`. Must be .", sep = ""))
        }
        stopifnot(R6::is.R6(`associationType`))
        self$`associationType` <- `associationType`
      }
      if (!is.null(`id`)) {
        if (!(is.numeric(`id`) && length(`id`) == 1)) {
          stop(paste("Error! Invalid data for `id`. Must be an integer:", `id`))
        }
        self$`id` <- `id`
      }
      if (!is.null(`username`)) {
        if (!(is.character(`username`) && length(`username`) == 1)) {
          stop(paste("Error! Invalid data for `username`. Must be a string:", `username`))
        }
        self$`username` <- `username`
      }
      if (!is.null(`email`)) {
        if (!(is.character(`email`) && length(`email`) == 1)) {
          stop(paste("Error! Invalid data for `email`. Must be a string:", `email`))
        }
        self$`email` <- `email`
      }
    },

    #' @description
    #' To JSON String
    #'
    #' @return OrganizationUser in JSON format
    toJSON = function() {
      OrganizationUserObject <- list()
      if (!is.null(self$`id`)) {
        OrganizationUserObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`userId`)) {
        OrganizationUserObject[["userId"]] <-
          self$`userId`
      }
      if (!is.null(self$`username`)) {
        OrganizationUserObject[["username"]] <-
          self$`username`
      }
      if (!is.null(self$`email`)) {
        OrganizationUserObject[["email"]] <-
          self$`email`
      }
      if (!is.null(self$`associationType`)) {
        OrganizationUserObject[["associationType"]] <-
          self$`associationType`$toJSON()
      }
      OrganizationUserObject
    },

    #' @description
    #' Deserialize JSON string into an instance of OrganizationUser
    #'
    #' @param input_json the JSON input
    #' @return the instance of OrganizationUser
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`userId`)) {
        self$`userId` <- this_object$`userId`
      }
      if (!is.null(this_object$`username`)) {
        self$`username` <- this_object$`username`
      }
      if (!is.null(this_object$`email`)) {
        self$`email` <- this_object$`email`
      }
      if (!is.null(this_object$`associationType`)) {
        `associationtype_object` <- OrganizationUserAssociationType$new()
        `associationtype_object`$fromJSON(jsonlite::toJSON(this_object$`associationType`, auto_unbox = TRUE, digits = NA))
        self$`associationType` <- `associationtype_object`
      }
      self
    },

    #' @description
    #' To JSON String
    #'
    #' @return OrganizationUser in JSON format
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`id`)) {
          sprintf(
          '"id":
            %d
                    ',
          self$`id`
          )
        },
        if (!is.null(self$`userId`)) {
          sprintf(
          '"userId":
            "%s"
                    ',
          self$`userId`
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
        if (!is.null(self$`email`)) {
          sprintf(
          '"email":
            "%s"
                    ',
          self$`email`
          )
        },
        if (!is.null(self$`associationType`)) {
          sprintf(
          '"associationType":
          %s
          ',
          jsonlite::toJSON(self$`associationType`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },

    #' @description
    #' Deserialize JSON string into an instance of OrganizationUser
    #'
    #' @param input_json the JSON input
    #' @return the instance of OrganizationUser
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`userId` <- this_object$`userId`
      self$`username` <- this_object$`username`
      self$`email` <- this_object$`email`
      self$`associationType` <- OrganizationUserAssociationType$new()$fromJSON(jsonlite::toJSON(this_object$`associationType`, auto_unbox = TRUE, digits = NA))
      self
    },

    #' @description
    #' Validate JSON input with respect to OrganizationUser and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
      # check the required field `userId`
      if (!is.null(input_json$`userId`)) {
        if (!(is.character(input_json$`userId`) && length(input_json$`userId`) == 1)) {
          stop(paste("Error! Invalid data for `userId`. Must be a string:", input_json$`userId`))
        }
      } else {
        stop(paste("The JSON input `", input, "` is invalid for OrganizationUser: the required field `userId` is missing."))
      }
      # check the required field `associationType`
      if (!is.null(input_json$`associationType`)) {
        stopifnot(R6::is.R6(input_json$`associationType`))
      } else {
        stop(paste("The JSON input `", input, "` is invalid for OrganizationUser: the required field `associationType` is missing."))
      }
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of OrganizationUser
    toString = function() {
      self$toJSONString()
    },

    #' @description
    #' Return true if the values in all fields are valid.
    #'
    #' @return true if the values in all fields are valid.
    isValid = function() {
      # check if the required `userId` is null
      if (is.null(self$`userId`)) {
        return(FALSE)
      }

      # check if the required `associationType` is null
      if (is.null(self$`associationType`)) {
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
      # check if the required `userId` is null
      if (is.null(self$`userId`)) {
        invalid_fields["userId"] <- "Non-nullable required field `userId` cannot be null."
      }

      # check if the required `associationType` is null
      if (is.null(self$`associationType`)) {
        invalid_fields["associationType"] <- "Non-nullable required field `associationType` cannot be null."
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
# OrganizationUser$unlock()
#
## Below is an example to define the print function
# OrganizationUser$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# OrganizationUser$lock()

