#' Create a new OrganizationInvitation
#'
#' @description
#' OrganizationInvitation Class
#'
#' @docType class
#' @title OrganizationInvitation
#' @description OrganizationInvitation Class
#' @format An \code{R6Class} generator object
#' @field id ID of the invitation character [optional]
#' @field userId The user id associated with that invitation character [optional]
#' @field userEmail Email address of the invited user character
#' @field status Status of the invitation character
#' @field expirationDate Expiration date of the invitation character
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
OrganizationInvitation <- R6::R6Class(
  "OrganizationInvitation",
  public = list(
    `id` = NULL,
    `userId` = NULL,
    `userEmail` = NULL,
    `status` = NULL,
    `expirationDate` = NULL,

    #' @description
    #' Initialize a new OrganizationInvitation class.
    #'
    #' @param userEmail Email address of the invited user
    #' @param status Status of the invitation
    #' @param expirationDate Expiration date of the invitation
    #' @param id ID of the invitation
    #' @param userId The user id associated with that invitation
    #' @param ... Other optional arguments.
    initialize = function(`userEmail`, `status`, `expirationDate`, `id` = NULL, `userId` = NULL, ...) {
      if (!missing(`userEmail`)) {
        if (!(is.character(`userEmail`) && length(`userEmail`) == 1)) {
          stop(paste("Error! Invalid data for `userEmail`. Must be a string:", `userEmail`))
        }
        self$`userEmail` <- `userEmail`
      }
      if (!missing(`status`)) {
        if (!(`status` %in% c("PENDING", "REJECTED", "ACCEPTED"))) {
          stop(paste("Error! \"", `status`, "\" cannot be assigned to `status`. Must be \"PENDING\", \"REJECTED\", \"ACCEPTED\".", sep = ""))
        }
        if (!(is.character(`status`) && length(`status`) == 1)) {
          stop(paste("Error! Invalid data for `status`. Must be a string:", `status`))
        }
        self$`status` <- `status`
      }
      if (!missing(`expirationDate`)) {
        if (!(is.character(`expirationDate`) && length(`expirationDate`) == 1)) {
          stop(paste("Error! Invalid data for `expirationDate`. Must be a string:", `expirationDate`))
        }
        self$`expirationDate` <- `expirationDate`
      }
      if (!is.null(`id`)) {
        if (!(is.character(`id`) && length(`id`) == 1)) {
          stop(paste("Error! Invalid data for `id`. Must be a string:", `id`))
        }
        self$`id` <- `id`
      }
      if (!is.null(`userId`)) {
        if (!(is.character(`userId`) && length(`userId`) == 1)) {
          stop(paste("Error! Invalid data for `userId`. Must be a string:", `userId`))
        }
        self$`userId` <- `userId`
      }
    },

    #' @description
    #' To JSON String
    #'
    #' @return OrganizationInvitation in JSON format
    toJSON = function() {
      OrganizationInvitationObject <- list()
      if (!is.null(self$`id`)) {
        OrganizationInvitationObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`userId`)) {
        OrganizationInvitationObject[["userId"]] <-
          self$`userId`
      }
      if (!is.null(self$`userEmail`)) {
        OrganizationInvitationObject[["userEmail"]] <-
          self$`userEmail`
      }
      if (!is.null(self$`status`)) {
        OrganizationInvitationObject[["status"]] <-
          self$`status`
      }
      if (!is.null(self$`expirationDate`)) {
        OrganizationInvitationObject[["expirationDate"]] <-
          self$`expirationDate`
      }
      OrganizationInvitationObject
    },

    #' @description
    #' Deserialize JSON string into an instance of OrganizationInvitation
    #'
    #' @param input_json the JSON input
    #' @return the instance of OrganizationInvitation
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`userId`)) {
        self$`userId` <- this_object$`userId`
      }
      if (!is.null(this_object$`userEmail`)) {
        self$`userEmail` <- this_object$`userEmail`
      }
      if (!is.null(this_object$`status`)) {
        if (!is.null(this_object$`status`) && !(this_object$`status` %in% c("PENDING", "REJECTED", "ACCEPTED"))) {
          stop(paste("Error! \"", this_object$`status`, "\" cannot be assigned to `status`. Must be \"PENDING\", \"REJECTED\", \"ACCEPTED\".", sep = ""))
        }
        self$`status` <- this_object$`status`
      }
      if (!is.null(this_object$`expirationDate`)) {
        self$`expirationDate` <- this_object$`expirationDate`
      }
      self
    },

    #' @description
    #' To JSON String
    #'
    #' @return OrganizationInvitation in JSON format
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
        if (!is.null(self$`userId`)) {
          sprintf(
          '"userId":
            "%s"
                    ',
          self$`userId`
          )
        },
        if (!is.null(self$`userEmail`)) {
          sprintf(
          '"userEmail":
            "%s"
                    ',
          self$`userEmail`
          )
        },
        if (!is.null(self$`status`)) {
          sprintf(
          '"status":
            "%s"
                    ',
          self$`status`
          )
        },
        if (!is.null(self$`expirationDate`)) {
          sprintf(
          '"expirationDate":
            "%s"
                    ',
          self$`expirationDate`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },

    #' @description
    #' Deserialize JSON string into an instance of OrganizationInvitation
    #'
    #' @param input_json the JSON input
    #' @return the instance of OrganizationInvitation
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`userId` <- this_object$`userId`
      self$`userEmail` <- this_object$`userEmail`
      if (!is.null(this_object$`status`) && !(this_object$`status` %in% c("PENDING", "REJECTED", "ACCEPTED"))) {
        stop(paste("Error! \"", this_object$`status`, "\" cannot be assigned to `status`. Must be \"PENDING\", \"REJECTED\", \"ACCEPTED\".", sep = ""))
      }
      self$`status` <- this_object$`status`
      self$`expirationDate` <- this_object$`expirationDate`
      self
    },

    #' @description
    #' Validate JSON input with respect to OrganizationInvitation and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
      # check the required field `userEmail`
      if (!is.null(input_json$`userEmail`)) {
        if (!(is.character(input_json$`userEmail`) && length(input_json$`userEmail`) == 1)) {
          stop(paste("Error! Invalid data for `userEmail`. Must be a string:", input_json$`userEmail`))
        }
      } else {
        stop(paste("The JSON input `", input, "` is invalid for OrganizationInvitation: the required field `userEmail` is missing."))
      }
      # check the required field `status`
      if (!is.null(input_json$`status`)) {
        if (!(is.character(input_json$`status`) && length(input_json$`status`) == 1)) {
          stop(paste("Error! Invalid data for `status`. Must be a string:", input_json$`status`))
        }
      } else {
        stop(paste("The JSON input `", input, "` is invalid for OrganizationInvitation: the required field `status` is missing."))
      }
      # check the required field `expirationDate`
      if (!is.null(input_json$`expirationDate`)) {
        if (!(is.character(input_json$`expirationDate`) && length(input_json$`expirationDate`) == 1)) {
          stop(paste("Error! Invalid data for `expirationDate`. Must be a string:", input_json$`expirationDate`))
        }
      } else {
        stop(paste("The JSON input `", input, "` is invalid for OrganizationInvitation: the required field `expirationDate` is missing."))
      }
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of OrganizationInvitation
    toString = function() {
      self$toJSONString()
    },

    #' @description
    #' Return true if the values in all fields are valid.
    #'
    #' @return true if the values in all fields are valid.
    isValid = function() {
      # check if the required `userEmail` is null
      if (is.null(self$`userEmail`)) {
        return(FALSE)
      }

      # check if the required `status` is null
      if (is.null(self$`status`)) {
        return(FALSE)
      }

      # check if the required `expirationDate` is null
      if (is.null(self$`expirationDate`)) {
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
      # check if the required `userEmail` is null
      if (is.null(self$`userEmail`)) {
        invalid_fields["userEmail"] <- "Non-nullable required field `userEmail` cannot be null."
      }

      # check if the required `status` is null
      if (is.null(self$`status`)) {
        invalid_fields["status"] <- "Non-nullable required field `status` cannot be null."
      }

      # check if the required `expirationDate` is null
      if (is.null(self$`expirationDate`)) {
        invalid_fields["expirationDate"] <- "Non-nullable required field `expirationDate` cannot be null."
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
# OrganizationInvitation$unlock()
#
## Below is an example to define the print function
# OrganizationInvitation$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# OrganizationInvitation$lock()

