#' Create a new Doa
#'
#' @description
#' Doa Class
#'
#' @docType class
#' @title Doa
#' @description Doa Class
#' @format An \code{R6Class} generator object
#' @field id  integer [optional]
#' @field method  \link{DoaMethod}
#' @field data  named list(\link{AnyType})
#' @field createdAt The date and time when the feature was created. character [optional]
#' @field updatedAt The date and time when the feature was last updated. character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Doa <- R6::R6Class(
  "Doa",
  public = list(
    `id` = NULL,
    `method` = NULL,
    `data` = NULL,
    `createdAt` = NULL,
    `updatedAt` = NULL,

    #' @description
    #' Initialize a new Doa class.
    #'
    #' @param method method
    #' @param data data
    #' @param id id
    #' @param createdAt The date and time when the feature was created.
    #' @param updatedAt The date and time when the feature was last updated.
    #' @param ... Other optional arguments.
    initialize = function(`method`, `data`, `id` = NULL, `createdAt` = NULL, `updatedAt` = NULL, ...) {
      if (!missing(`method`)) {
#         if (!(`method` %in% c())) {
#           stop(paste("Error! \"", `method`, "\" cannot be assigned to `method`. Must be .", sep = ""))
#         }
        stopifnot(R6::is.R6(`method`))
        self$`method` <- `method`
      }
      if (!missing(`data`)) {
        stopifnot(is.vector(`data`), length(`data`) != 0)
        sapply(`data`, function(x) stopifnot(R6::is.R6(x)))
        self$`data` <- `data`
      }
      if (!is.null(`id`)) {
        if (!(is.numeric(`id`) && length(`id`) == 1)) {
          stop(paste("Error! Invalid data for `id`. Must be an integer:", `id`))
        }
        self$`id` <- `id`
      }
      if (!is.null(`createdAt`)) {
        if (!is.character(`createdAt`)) {
          stop(paste("Error! Invalid data for `createdAt`. Must be a string:", `createdAt`))
        }
        self$`createdAt` <- `createdAt`
      }
      if (!is.null(`updatedAt`)) {
        if (!is.character(`updatedAt`)) {
          stop(paste("Error! Invalid data for `updatedAt`. Must be a string:", `updatedAt`))
        }
        self$`updatedAt` <- `updatedAt`
      }
    },

    #' @description
    #' To JSON String
    #'
    #' @return Doa in JSON format
    toJSON = function() {
      DoaObject <- list()
      if (!is.null(self$`id`)) {
        DoaObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`method`)) {
        DoaObject[["method"]] <-
          self$`method`$toJSON()
      }
      if (!is.null(self$`data`)) {
        DoaObject[["data"]] <-
          lapply(self$`data`, function(x) x$toJSON())
      }
      if (!is.null(self$`createdAt`)) {
        DoaObject[["createdAt"]] <-
          self$`createdAt`
      }
      if (!is.null(self$`updatedAt`)) {
        DoaObject[["updatedAt"]] <-
          self$`updatedAt`
      }
      DoaObject
    },

    #' @description
    #' Deserialize JSON string into an instance of Doa
    #'
    #' @param input_json the JSON input
    #' @return the instance of Doa
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`method`)) {
        `method_object` <- DoaMethod$new()
        `method_object`$fromJSON(jsonlite::toJSON(this_object$`method`, auto_unbox = TRUE, digits = NA))
        self$`method` <- `method_object`
      }
      if (!is.null(this_object$`data`)) {
        self$`data` <- ApiClient$new()$deserializeObj(this_object$`data`, "map(AnyType)", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`createdAt`)) {
        self$`createdAt` <- this_object$`createdAt`
      }
      if (!is.null(this_object$`updatedAt`)) {
        self$`updatedAt` <- this_object$`updatedAt`
      }
      self
    },

    #' @description
    #' To JSON String
    #'
    #' @return Doa in JSON format
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
        if (!is.null(self$`method`)) {
          sprintf(
          '"method":
          %s
          ',
          jsonlite::toJSON(self$`method`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`data`)) {
          sprintf(
          '"data":
          %s
',
          jsonlite::toJSON(lapply(self$`data`, function(x){ x$toJSON() }), auto_unbox = TRUE, digits = NA)
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
        if (!is.null(self$`updatedAt`)) {
          sprintf(
          '"updatedAt":
            "%s"
                    ',
          self$`updatedAt`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },

    #' @description
    #' Deserialize JSON string into an instance of Doa
    #'
    #' @param input_json the JSON input
    #' @return the instance of Doa
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`method` <- DoaMethod$new()$fromJSON(jsonlite::toJSON(this_object$`method`, auto_unbox = TRUE, digits = NA))
      self$`data` <- ApiClient$new()$deserializeObj(this_object$`data`, "map(AnyType)", loadNamespace("openapi"))
      self$`createdAt` <- this_object$`createdAt`
      self$`updatedAt` <- this_object$`updatedAt`
      self
    },

    #' @description
    #' Validate JSON input with respect to Doa and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
      # check the required field `method`
      if (!is.null(input_json$`method`)) {
        stopifnot(R6::is.R6(input_json$`method`))
      } else {
        stop(paste("The JSON input `", input, "` is invalid for Doa: the required field `method` is missing."))
      }
      # check the required field `data`
      if (!is.null(input_json$`data`)) {
        stopifnot(is.vector(input_json$`data`), length(input_json$`data`) != 0)
        tmp <- sapply(input_json$`data`, function(x) stopifnot(R6::is.R6(x)))
      } else {
        stop(paste("The JSON input `", input, "` is invalid for Doa: the required field `data` is missing."))
      }
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Doa
    toString = function() {
      self$toJSONString()
    },

    #' @description
    #' Return true if the values in all fields are valid.
    #'
    #' @return true if the values in all fields are valid.
    isValid = function() {
      # check if the required `method` is null
      if (is.null(self$`method`)) {
        return(FALSE)
      }

      # check if the required `data` is null
      if (is.null(self$`data`)) {
        return(FALSE)
      }

      if (length(self$`data`) > 20) {
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
      # check if the required `method` is null
      if (is.null(self$`method`)) {
        invalid_fields["method"] <- "Non-nullable required field `method` cannot be null."
      }

      # check if the required `data` is null
      if (is.null(self$`data`)) {
        invalid_fields["data"] <- "Non-nullable required field `data` cannot be null."
      }

      if (length(self$`data`) > 20) {
        invalid_fields["data"] <- "Invalid length for `data`, number of items must be less than or equal to 20."
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
# Doa$unlock()
#
## Below is an example to define the print function
# Doa$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Doa$lock()

