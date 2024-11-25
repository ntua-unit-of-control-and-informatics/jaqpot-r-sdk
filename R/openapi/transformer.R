#' Create a new Transformer
#'
#' @description
#' A preprocessor for the model
#'
#' @docType class
#' @title Transformer
#' @description Transformer Class
#' @format An \code{R6Class} generator object
#' @field id  integer [optional]
#' @field name  character
#' @field config  named list(\link{AnyType})
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
Transformer <- R6::R6Class(
  "Transformer",
  public = list(
    `id` = NULL,
    `name` = NULL,
    `config` = NULL,

    #' @description
    #' Initialize a new Transformer class.
    #'
    #' @param name name
    #' @param config config
    #' @param id id
    #' @param ... Other optional arguments.
    initialize = function(`name`, `config`, `id` = NULL, ...) {
      if (!missing(`name`)) {
        if (!(is.character(`name`) && length(`name`) == 1)) {
          stop(paste("Error! Invalid data for `name`. Must be a string:", `name`))
        }
        self$`name` <- `name`
      }
      if (!missing(`config`)) {
        stopifnot(is.vector(`config`), length(`config`) != 0)
        sapply(`config`, function(x) stopifnot(R6::is.R6(x)))
        self$`config` <- `config`
      }
      if (!is.null(`id`)) {
        if (!(is.numeric(`id`) && length(`id`) == 1)) {
          stop(paste("Error! Invalid data for `id`. Must be an integer:", `id`))
        }
        self$`id` <- `id`
      }
    },

    #' @description
    #' To JSON String
    #'
    #' @return Transformer in JSON format
    toJSON = function() {
      TransformerObject <- list()
      if (!is.null(self$`id`)) {
        TransformerObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`name`)) {
        TransformerObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`config`)) {
        TransformerObject[["config"]] <-
          lapply(self$`config`, function(x) x$toJSON())
      }
      TransformerObject
    },

    #' @description
    #' Deserialize JSON string into an instance of Transformer
    #'
    #' @param input_json the JSON input
    #' @return the instance of Transformer
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`name`)) {
        self$`name` <- this_object$`name`
      }
      if (!is.null(this_object$`config`)) {
        self$`config` <- ApiClient$new()$deserializeObj(this_object$`config`, "map(AnyType)", loadNamespace("openapi"))
      }
      self
    },

    #' @description
    #' To JSON String
    #'
    #' @return Transformer in JSON format
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
        if (!is.null(self$`name`)) {
          sprintf(
          '"name":
            "%s"
                    ',
          self$`name`
          )
        },
        if (!is.null(self$`config`)) {
          sprintf(
          '"config":
          %s
',
          jsonlite::toJSON(lapply(self$`config`, function(x){ x$toJSON() }), auto_unbox = TRUE, digits = NA)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },

    #' @description
    #' Deserialize JSON string into an instance of Transformer
    #'
    #' @param input_json the JSON input
    #' @return the instance of Transformer
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`name` <- this_object$`name`
      self$`config` <- ApiClient$new()$deserializeObj(this_object$`config`, "map(AnyType)", loadNamespace("openapi"))
      self
    },

    #' @description
    #' Validate JSON input with respect to Transformer and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
      # check the required field `name`
      if (!is.null(input_json$`name`)) {
        if (!(is.character(input_json$`name`) && length(input_json$`name`) == 1)) {
          stop(paste("Error! Invalid data for `name`. Must be a string:", input_json$`name`))
        }
      } else {
        stop(paste("The JSON input `", input, "` is invalid for Transformer: the required field `name` is missing."))
      }
      # check the required field `config`
      if (!is.null(input_json$`config`)) {
        stopifnot(is.vector(input_json$`config`), length(input_json$`config`) != 0)
        tmp <- sapply(input_json$`config`, function(x) stopifnot(R6::is.R6(x)))
      } else {
        stop(paste("The JSON input `", input, "` is invalid for Transformer: the required field `config` is missing."))
      }
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Transformer
    toString = function() {
      self$toJSONString()
    },

    #' @description
    #' Return true if the values in all fields are valid.
    #'
    #' @return true if the values in all fields are valid.
    isValid = function() {
      # check if the required `name` is null
      if (is.null(self$`name`)) {
        return(FALSE)
      }

      # check if the required `config` is null
      if (is.null(self$`config`)) {
        return(FALSE)
      }

      if (length(self$`config`) > 20) {
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
      # check if the required `name` is null
      if (is.null(self$`name`)) {
        invalid_fields["name"] <- "Non-nullable required field `name` cannot be null."
      }

      # check if the required `config` is null
      if (is.null(self$`config`)) {
        invalid_fields["config"] <- "Non-nullable required field `config` cannot be null."
      }

      if (length(self$`config`) > 20) {
        invalid_fields["config"] <- "Invalid length for `config`, number of items must be less than or equal to 20."
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
# Transformer$unlock()
#
## Below is an example to define the print function
# Transformer$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Transformer$lock()

