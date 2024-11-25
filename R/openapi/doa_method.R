#' @docType class
#' @title DoaMethod
#' @description DoaMethod Class
#' @format An \code{R6Class} generator object
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
DoaMethod <- R6::R6Class(
  "DoaMethod",
  public = list(

    #' @description
    #' Initialize a new DoaMethod class.
    #'
    #' @param ... Optional arguments.
    initialize = function(...) {
      local.optional.var <- list(...)
      val <- unlist(local.optional.var)
      enumvec <- .parse_DoaMethod()

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
        warning("Initializing DoaMethod with DUMMY_ENUM. Use one of the valid values: ",
          paste0(enumvec, collapse = ", "),
          ". If you did not manually initialize DoaMethod, this may already be overwritten by an enum loaded from a JSON config.")
      }
      private$value <- val
    },

    #' @description
    #' To JSON String
    #'
    #' @return DoaMethod in JSON format
    toJSON = function() {
        jsonlite::toJSON(private$value, auto_unbox = TRUE)
    },

    #' @description
    #' Deserialize JSON string into an instance of DoaMethod
    #'
    #' @param input_json the JSON input
    #'
    #' @return the instance of DoaMethod
    fromJSON = function(input_json) {
      private$value <- jsonlite::fromJSON(input_json,
          simplifyVector = FALSE)
      self
    },

    #' @description
    #' To JSON String
    #'
    #' @return DoaMethod in JSON format
    toJSONString = function() {
      as.character(jsonlite::toJSON(private$value,
          auto_unbox = TRUE))
    },

    #' @description
    #' Deserialize JSON string into an instance of DoaMethod
    #'
    #' @param input_json the JSON input
    #'
    #' @return the instance of DoaMethod
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
.parse_DoaMethod <- function(vals) {
  res <- gsub("^\\[|\\]$", "", "[LEVERAGE, BOUNDING_BOX, KERNEL_BASED, MEAN_VAR, MAHALANOBIS, CITY_BLOCK]")
  unlist(strsplit(res, ", "))
}

