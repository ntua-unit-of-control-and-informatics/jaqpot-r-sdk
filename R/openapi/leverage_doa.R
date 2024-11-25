#' Create a new LeverageDoa
#'
#' @description
#' LeverageDoa Class
#'
#' @docType class
#' @title LeverageDoa
#' @description LeverageDoa Class
#' @format An \code{R6Class} generator object
#' @field hStar  numeric [optional]
#' @field doaMatrix  list(list(numeric)) [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
LeverageDoa <- R6::R6Class(
  "LeverageDoa",
  public = list(
    `hStar` = NULL,
    `doaMatrix` = NULL,

    #' @description
    #' Initialize a new LeverageDoa class.
    #'
    #' @param hStar hStar
    #' @param doaMatrix doaMatrix
    #' @param ... Other optional arguments.
    initialize = function(`hStar` = NULL, `doaMatrix` = NULL, ...) {
      if (!is.null(`hStar`)) {
        if (!(is.numeric(`hStar`) && length(`hStar`) == 1)) {
          stop(paste("Error! Invalid data for `hStar`. Must be a number:", `hStar`))
        }
        self$`hStar` <- `hStar`
      }
      if (!is.null(`doaMatrix`)) {
        stopifnot(is.vector(`doaMatrix`), length(`doaMatrix`) != 0)
        sapply(`doaMatrix`, function(x) stopifnot(R6::is.R6(x)))
        self$`doaMatrix` <- `doaMatrix`
      }
    },

    #' @description
    #' To JSON String
    #'
    #' @return LeverageDoa in JSON format
    toJSON = function() {
      LeverageDoaObject <- list()
      if (!is.null(self$`hStar`)) {
        LeverageDoaObject[["hStar"]] <-
          self$`hStar`
      }
      if (!is.null(self$`doaMatrix`)) {
        LeverageDoaObject[["doaMatrix"]] <-
          lapply(self$`doaMatrix`, function(x) x$toJSON())
      }
      LeverageDoaObject
    },

    #' @description
    #' Deserialize JSON string into an instance of LeverageDoa
    #'
    #' @param input_json the JSON input
    #' @return the instance of LeverageDoa
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`hStar`)) {
        self$`hStar` <- this_object$`hStar`
      }
      if (!is.null(this_object$`doaMatrix`)) {
        self$`doaMatrix` <- ApiClient$new()$deserializeObj(this_object$`doaMatrix`, "array[array[numeric]]", loadNamespace("openapi"))
      }
      self
    },

    #' @description
    #' To JSON String
    #'
    #' @return LeverageDoa in JSON format
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`hStar`)) {
          sprintf(
          '"hStar":
            %d
                    ',
          self$`hStar`
          )
        },
        if (!is.null(self$`doaMatrix`)) {
          sprintf(
          '"doaMatrix":
          [%s]
',
          paste(sapply(self$`doaMatrix`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },

    #' @description
    #' Deserialize JSON string into an instance of LeverageDoa
    #'
    #' @param input_json the JSON input
    #' @return the instance of LeverageDoa
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`hStar` <- this_object$`hStar`
      self$`doaMatrix` <- ApiClient$new()$deserializeObj(this_object$`doaMatrix`, "array[array[numeric]]", loadNamespace("openapi"))
      self
    },

    #' @description
    #' Validate JSON input with respect to LeverageDoa and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of LeverageDoa
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
# LeverageDoa$unlock()
#
## Below is an example to define the print function
# LeverageDoa$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# LeverageDoa$lock()

