#' Create a new Lead
#'
#' @description
#' Lead Class
#'
#' @docType class
#' @title Lead
#' @description Lead Class
#' @format An \code{R6Class} generator object
#' @field id  integer [optional]
#' @field email  character [optional]
#' @field name  character [optional]
#' @field status  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Lead <- R6::R6Class(
  "Lead",
  public = list(
    `id` = NULL,
    `email` = NULL,
    `name` = NULL,
    `status` = NULL,

    #' @description
    #' Initialize a new Lead class.
    #'
    #' @param id id
    #' @param email email
    #' @param name name
    #' @param status status
    #' @param ... Other optional arguments.
    initialize = function(`id` = NULL, `email` = NULL, `name` = NULL, `status` = NULL, ...) {
      if (!is.null(`id`)) {
        if (!(is.numeric(`id`) && length(`id`) == 1)) {
          stop(paste("Error! Invalid data for `id`. Must be an integer:", `id`))
        }
        self$`id` <- `id`
      }
      if (!is.null(`email`)) {
        if (!(is.character(`email`) && length(`email`) == 1)) {
          stop(paste("Error! Invalid data for `email`. Must be a string:", `email`))
        }
        self$`email` <- `email`
      }
      if (!is.null(`name`)) {
        if (!(is.character(`name`) && length(`name`) == 1)) {
          stop(paste("Error! Invalid data for `name`. Must be a string:", `name`))
        }
        self$`name` <- `name`
      }
      if (!is.null(`status`)) {
        if (!(`status` %in% c("PENDING", "APPROVED", "DENIED"))) {
          stop(paste("Error! \"", `status`, "\" cannot be assigned to `status`. Must be \"PENDING\", \"APPROVED\", \"DENIED\".", sep = ""))
        }
        if (!(is.character(`status`) && length(`status`) == 1)) {
          stop(paste("Error! Invalid data for `status`. Must be a string:", `status`))
        }
        self$`status` <- `status`
      }
    },

    #' @description
    #' To JSON String
    #'
    #' @return Lead in JSON format
    toJSON = function() {
      LeadObject <- list()
      if (!is.null(self$`id`)) {
        LeadObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`email`)) {
        LeadObject[["email"]] <-
          self$`email`
      }
      if (!is.null(self$`name`)) {
        LeadObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`status`)) {
        LeadObject[["status"]] <-
          self$`status`
      }
      LeadObject
    },

    #' @description
    #' Deserialize JSON string into an instance of Lead
    #'
    #' @param input_json the JSON input
    #' @return the instance of Lead
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`email`)) {
        self$`email` <- this_object$`email`
      }
      if (!is.null(this_object$`name`)) {
        self$`name` <- this_object$`name`
      }
      if (!is.null(this_object$`status`)) {
        if (!is.null(this_object$`status`) && !(this_object$`status` %in% c("PENDING", "APPROVED", "DENIED"))) {
          stop(paste("Error! \"", this_object$`status`, "\" cannot be assigned to `status`. Must be \"PENDING\", \"APPROVED\", \"DENIED\".", sep = ""))
        }
        self$`status` <- this_object$`status`
      }
      self
    },

    #' @description
    #' To JSON String
    #'
    #' @return Lead in JSON format
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
        if (!is.null(self$`email`)) {
          sprintf(
          '"email":
            "%s"
                    ',
          self$`email`
          )
        },
        if (!is.null(self$`name`)) {
          sprintf(
          '"name":
            "%s"
                    ',
          self$`name`
          )
        },
        if (!is.null(self$`status`)) {
          sprintf(
          '"status":
            "%s"
                    ',
          self$`status`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },

    #' @description
    #' Deserialize JSON string into an instance of Lead
    #'
    #' @param input_json the JSON input
    #' @return the instance of Lead
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`email` <- this_object$`email`
      self$`name` <- this_object$`name`
      if (!is.null(this_object$`status`) && !(this_object$`status` %in% c("PENDING", "APPROVED", "DENIED"))) {
        stop(paste("Error! \"", this_object$`status`, "\" cannot be assigned to `status`. Must be \"PENDING\", \"APPROVED\", \"DENIED\".", sep = ""))
      }
      self$`status` <- this_object$`status`
      self
    },

    #' @description
    #' Validate JSON input with respect to Lead and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Lead
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
# Lead$unlock()
#
## Below is an example to define the print function
# Lead$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Lead$lock()

