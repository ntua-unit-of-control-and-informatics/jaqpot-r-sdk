#' Create a new RegressionScores
#'
#' @description
#' RegressionScores Class
#'
#' @docType class
#' @title RegressionScores
#' @description RegressionScores Class
#' @format An \code{R6Class} generator object
#' @field yName  character
#' @field folds  integer [optional]
#' @field r2  numeric [optional]
#' @field mae  numeric [optional]
#' @field rmse  numeric [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
RegressionScores <- R6::R6Class(
  "RegressionScores",
  public = list(
    `yName` = NULL,
    `folds` = NULL,
    `r2` = NULL,
    `mae` = NULL,
    `rmse` = NULL,

    #' @description
    #' Initialize a new RegressionScores class.
    #'
    #' @param yName yName
    #' @param folds folds
    #' @param r2 r2
    #' @param mae mae
    #' @param rmse rmse
    #' @param ... Other optional arguments.
    initialize = function(`yName`, `folds` = NULL, `r2` = NULL, `mae` = NULL, `rmse` = NULL, ...) {
      if (!missing(`yName`)) {
        if (!(is.character(`yName`) && length(`yName`) == 1)) {
          stop(paste("Error! Invalid data for `yName`. Must be a string:", `yName`))
        }
        self$`yName` <- `yName`
      }
      if (!is.null(`folds`)) {
        if (!(is.numeric(`folds`) && length(`folds`) == 1)) {
          stop(paste("Error! Invalid data for `folds`. Must be an integer:", `folds`))
        }
        self$`folds` <- `folds`
      }
      if (!is.null(`r2`)) {
        if (!(is.numeric(`r2`) && length(`r2`) == 1)) {
          stop(paste("Error! Invalid data for `r2`. Must be a number:", `r2`))
        }
        self$`r2` <- `r2`
      }
      if (!is.null(`mae`)) {
        if (!(is.numeric(`mae`) && length(`mae`) == 1)) {
          stop(paste("Error! Invalid data for `mae`. Must be a number:", `mae`))
        }
        self$`mae` <- `mae`
      }
      if (!is.null(`rmse`)) {
        if (!(is.numeric(`rmse`) && length(`rmse`) == 1)) {
          stop(paste("Error! Invalid data for `rmse`. Must be a number:", `rmse`))
        }
        self$`rmse` <- `rmse`
      }
    },

    #' @description
    #' To JSON String
    #'
    #' @return RegressionScores in JSON format
    toJSON = function() {
      RegressionScoresObject <- list()
      if (!is.null(self$`yName`)) {
        RegressionScoresObject[["yName"]] <-
          self$`yName`
      }
      if (!is.null(self$`folds`)) {
        RegressionScoresObject[["folds"]] <-
          self$`folds`
      }
      if (!is.null(self$`r2`)) {
        RegressionScoresObject[["r2"]] <-
          self$`r2`
      }
      if (!is.null(self$`mae`)) {
        RegressionScoresObject[["mae"]] <-
          self$`mae`
      }
      if (!is.null(self$`rmse`)) {
        RegressionScoresObject[["rmse"]] <-
          self$`rmse`
      }
      RegressionScoresObject
    },

    #' @description
    #' Deserialize JSON string into an instance of RegressionScores
    #'
    #' @param input_json the JSON input
    #' @return the instance of RegressionScores
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`yName`)) {
        self$`yName` <- this_object$`yName`
      }
      if (!is.null(this_object$`folds`)) {
        self$`folds` <- this_object$`folds`
      }
      if (!is.null(this_object$`r2`)) {
        self$`r2` <- this_object$`r2`
      }
      if (!is.null(this_object$`mae`)) {
        self$`mae` <- this_object$`mae`
      }
      if (!is.null(this_object$`rmse`)) {
        self$`rmse` <- this_object$`rmse`
      }
      self
    },

    #' @description
    #' To JSON String
    #'
    #' @return RegressionScores in JSON format
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`yName`)) {
          sprintf(
          '"yName":
            "%s"
                    ',
          self$`yName`
          )
        },
        if (!is.null(self$`folds`)) {
          sprintf(
          '"folds":
            %d
                    ',
          self$`folds`
          )
        },
        if (!is.null(self$`r2`)) {
          sprintf(
          '"r2":
            %d
                    ',
          self$`r2`
          )
        },
        if (!is.null(self$`mae`)) {
          sprintf(
          '"mae":
            %d
                    ',
          self$`mae`
          )
        },
        if (!is.null(self$`rmse`)) {
          sprintf(
          '"rmse":
            %d
                    ',
          self$`rmse`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },

    #' @description
    #' Deserialize JSON string into an instance of RegressionScores
    #'
    #' @param input_json the JSON input
    #' @return the instance of RegressionScores
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`yName` <- this_object$`yName`
      self$`folds` <- this_object$`folds`
      self$`r2` <- this_object$`r2`
      self$`mae` <- this_object$`mae`
      self$`rmse` <- this_object$`rmse`
      self
    },

    #' @description
    #' Validate JSON input with respect to RegressionScores and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
      # check the required field `yName`
      if (!is.null(input_json$`yName`)) {
        if (!(is.character(input_json$`yName`) && length(input_json$`yName`) == 1)) {
          stop(paste("Error! Invalid data for `yName`. Must be a string:", input_json$`yName`))
        }
      } else {
        stop(paste("The JSON input `", input, "` is invalid for RegressionScores: the required field `yName` is missing."))
      }
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of RegressionScores
    toString = function() {
      self$toJSONString()
    },

    #' @description
    #' Return true if the values in all fields are valid.
    #'
    #' @return true if the values in all fields are valid.
    isValid = function() {
      # check if the required `yName` is null
      if (is.null(self$`yName`)) {
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
      # check if the required `yName` is null
      if (is.null(self$`yName`)) {
        invalid_fields["yName"] <- "Non-nullable required field `yName` cannot be null."
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
# RegressionScores$unlock()
#
## Below is an example to define the print function
# RegressionScores$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# RegressionScores$lock()

