#' @docType class
#' @title ModelTask
#' @description ModelTask Class
#' @format An \code{R6Class} generator object
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
ModelTask <- R6::R6Class(
  "ModelTask",
  public = list(

    #' @description
    #' Initialize a new ModelTask class.
    #'
    #' @param ... Optional arguments.
    initialize = function(...) {
      local.optional.var <- list(...)
      val <- unlist(local.optional.var)
      enumvec <- .parse_ModelTask()

      if (length(val) == 0L) {
        val = "DUMMY_ENUM"
      } else {
        stopifnot(length(val) == 1L)
      }

      if (!val %in% enumvec) {
        if (!(val=="DUMMY_ENUM")) {
          stop("Use one of the valid values: ",
            paste0(enumvec, collapse = ", "))
        }
        warning("Initializing ModelTask with DUMMY_ENUM. Use one of the valid values: ",
          paste0(enumvec, collapse = ", "),
          ". If you did not manually initialize ModelTask, this may already be overwritten by an enum loaded from a JSON config.")
      }
      private$value <- val
    },

    #' @description
    #' To JSON String
    #'
    #' @return ModelTask in JSON format
    toJSON = function() {
        jsonlite::toJSON(private$value, auto_unbox = TRUE)
    },
    
    #' @description
    #' Get string value
    #'
    #' @return string
    getValue = function() {
      private$value
    },

    #' @description
    #' Deserialize JSON string into an instance of ModelTask
    #'
    #' @param input_json the JSON input
    #'
    #' @return the instance of ModelTask
    fromJSON = function(input_json) {
      private$value <- jsonlite::fromJSON(input_json,
          simplifyVector = FALSE)
      self
    },

    #' @description
    #' To JSON String
    #'
    #' @return ModelTask in JSON format
    toJSONString = function() {
      as.character(jsonlite::toJSON(private$value,
          auto_unbox = TRUE))
    },

    #' @description
    #' Deserialize JSON string into an instance of ModelTask
    #'
    #' @param input_json the JSON input
    #'
    #' @return the instance of ModelTask
    fromJSONString = function(input_json) {
      private$value <- jsonlite::fromJSON(input_json,
          simplifyVector = FALSE)
      self
    }
  ),
  private = list(
    value = NULL
  )
)

# add to utils.R
.parse_ModelTask <- function(vals) {
  res <- gsub("^\\[|\\]$", "", "[REGRESSION, BINARY_CLASSIFICATION, MULTICLASS_CLASSIFICATION]")
  unlist(strsplit(res, ", "))
}

