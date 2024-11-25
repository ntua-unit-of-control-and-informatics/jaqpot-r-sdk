#' Create a new BinaryClassificationScores
#'
#' @description
#' BinaryClassificationScores Class
#'
#' @docType class
#' @title BinaryClassificationScores
#' @description BinaryClassificationScores Class
#' @format An \code{R6Class} generator object
#' @field labels  list(character) [optional]
#' @field yName  character
#' @field folds  integer [optional]
#' @field accuracy  numeric [optional]
#' @field balancedAccuracy  numeric [optional]
#' @field precision  list(numeric) [optional]
#' @field recall  list(numeric) [optional]
#' @field f1Score  list(numeric) [optional]
#' @field jaccard  list(numeric) [optional]
#' @field matthewsCorrCoef  numeric [optional]
#' @field confusionMatrix  list(list(numeric)) [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @keywords internal
BinaryClassificationScores <- R6::R6Class(
  "BinaryClassificationScores",
  public = list(
    `labels` = NULL,
    `yName` = NULL,
    `folds` = NULL,
    `accuracy` = NULL,
    `balancedAccuracy` = NULL,
    `precision` = NULL,
    `recall` = NULL,
    `f1Score` = NULL,
    `jaccard` = NULL,
    `matthewsCorrCoef` = NULL,
    `confusionMatrix` = NULL,

    #' @description
    #' Initialize a new BinaryClassificationScores class.
    #'
    #' @param yName yName
    #' @param labels labels
    #' @param folds folds
    #' @param accuracy accuracy
    #' @param balancedAccuracy balancedAccuracy
    #' @param precision precision
    #' @param recall recall
    #' @param f1Score f1Score
    #' @param jaccard jaccard
    #' @param matthewsCorrCoef matthewsCorrCoef
    #' @param confusionMatrix confusionMatrix
    #' @param ... Other optional arguments.
    initialize = function(`yName`, `labels` = NULL, `folds` = NULL, `accuracy` = NULL, `balancedAccuracy` = NULL, `precision` = NULL, `recall` = NULL, `f1Score` = NULL, `jaccard` = NULL, `matthewsCorrCoef` = NULL, `confusionMatrix` = NULL, ...) {
      if (!missing(`yName`)) {
        if (!(is.character(`yName`) && length(`yName`) == 1)) {
          stop(paste("Error! Invalid data for `yName`. Must be a string:", `yName`))
        }
        self$`yName` <- `yName`
      }
      if (!is.null(`labels`)) {
        stopifnot(is.vector(`labels`), length(`labels`) != 0)
        sapply(`labels`, function(x) stopifnot(is.character(x)))
        self$`labels` <- `labels`
      }
      if (!is.null(`folds`)) {
        if (!(is.numeric(`folds`) && length(`folds`) == 1)) {
          stop(paste("Error! Invalid data for `folds`. Must be an integer:", `folds`))
        }
        self$`folds` <- `folds`
      }
      if (!is.null(`accuracy`)) {
        if (!(is.numeric(`accuracy`) && length(`accuracy`) == 1)) {
          stop(paste("Error! Invalid data for `accuracy`. Must be a number:", `accuracy`))
        }
        self$`accuracy` <- `accuracy`
      }
      if (!is.null(`balancedAccuracy`)) {
        if (!(is.numeric(`balancedAccuracy`) && length(`balancedAccuracy`) == 1)) {
          stop(paste("Error! Invalid data for `balancedAccuracy`. Must be a number:", `balancedAccuracy`))
        }
        self$`balancedAccuracy` <- `balancedAccuracy`
      }
      if (!is.null(`precision`)) {
        stopifnot(is.vector(`precision`), length(`precision`) != 0)
        sapply(`precision`, function(x) stopifnot(is.character(x)))
        self$`precision` <- `precision`
      }
      if (!is.null(`recall`)) {
        stopifnot(is.vector(`recall`), length(`recall`) != 0)
        sapply(`recall`, function(x) stopifnot(is.character(x)))
        self$`recall` <- `recall`
      }
      if (!is.null(`f1Score`)) {
        stopifnot(is.vector(`f1Score`), length(`f1Score`) != 0)
        sapply(`f1Score`, function(x) stopifnot(is.character(x)))
        self$`f1Score` <- `f1Score`
      }
      if (!is.null(`jaccard`)) {
        stopifnot(is.vector(`jaccard`), length(`jaccard`) != 0)
        sapply(`jaccard`, function(x) stopifnot(is.character(x)))
        self$`jaccard` <- `jaccard`
      }
      if (!is.null(`matthewsCorrCoef`)) {
        if (!(is.numeric(`matthewsCorrCoef`) && length(`matthewsCorrCoef`) == 1)) {
          stop(paste("Error! Invalid data for `matthewsCorrCoef`. Must be a number:", `matthewsCorrCoef`))
        }
        self$`matthewsCorrCoef` <- `matthewsCorrCoef`
      }
      if (!is.null(`confusionMatrix`)) {
        stopifnot(is.vector(`confusionMatrix`), length(`confusionMatrix`) != 0)
        sapply(`confusionMatrix`, function(x) stopifnot(R6::is.R6(x)))
        self$`confusionMatrix` <- `confusionMatrix`
      }
    },

    #' @description
    #' To JSON String
    #'
    #' @return BinaryClassificationScores in JSON format
    toJSON = function() {
      BinaryClassificationScoresObject <- list()
      if (!is.null(self$`labels`)) {
        BinaryClassificationScoresObject[["labels"]] <-
          self$`labels`
      }
      if (!is.null(self$`yName`)) {
        BinaryClassificationScoresObject[["yName"]] <-
          self$`yName`
      }
      if (!is.null(self$`folds`)) {
        BinaryClassificationScoresObject[["folds"]] <-
          self$`folds`
      }
      if (!is.null(self$`accuracy`)) {
        BinaryClassificationScoresObject[["accuracy"]] <-
          self$`accuracy`
      }
      if (!is.null(self$`balancedAccuracy`)) {
        BinaryClassificationScoresObject[["balancedAccuracy"]] <-
          self$`balancedAccuracy`
      }
      if (!is.null(self$`precision`)) {
        BinaryClassificationScoresObject[["precision"]] <-
          self$`precision`
      }
      if (!is.null(self$`recall`)) {
        BinaryClassificationScoresObject[["recall"]] <-
          self$`recall`
      }
      if (!is.null(self$`f1Score`)) {
        BinaryClassificationScoresObject[["f1Score"]] <-
          self$`f1Score`
      }
      if (!is.null(self$`jaccard`)) {
        BinaryClassificationScoresObject[["jaccard"]] <-
          self$`jaccard`
      }
      if (!is.null(self$`matthewsCorrCoef`)) {
        BinaryClassificationScoresObject[["matthewsCorrCoef"]] <-
          self$`matthewsCorrCoef`
      }
      if (!is.null(self$`confusionMatrix`)) {
        BinaryClassificationScoresObject[["confusionMatrix"]] <-
          lapply(self$`confusionMatrix`, function(x) x$toJSON())
      }
      BinaryClassificationScoresObject
    },

    #' @description
    #' Deserialize JSON string into an instance of BinaryClassificationScores
    #'
    #' @param input_json the JSON input
    #' @return the instance of BinaryClassificationScores
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`labels`)) {
        self$`labels` <- ApiClient$new()$deserializeObj(this_object$`labels`, "array[character]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`yName`)) {
        self$`yName` <- this_object$`yName`
      }
      if (!is.null(this_object$`folds`)) {
        self$`folds` <- this_object$`folds`
      }
      if (!is.null(this_object$`accuracy`)) {
        self$`accuracy` <- this_object$`accuracy`
      }
      if (!is.null(this_object$`balancedAccuracy`)) {
        self$`balancedAccuracy` <- this_object$`balancedAccuracy`
      }
      if (!is.null(this_object$`precision`)) {
        self$`precision` <- ApiClient$new()$deserializeObj(this_object$`precision`, "array[numeric]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`recall`)) {
        self$`recall` <- ApiClient$new()$deserializeObj(this_object$`recall`, "array[numeric]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`f1Score`)) {
        self$`f1Score` <- ApiClient$new()$deserializeObj(this_object$`f1Score`, "array[numeric]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`jaccard`)) {
        self$`jaccard` <- ApiClient$new()$deserializeObj(this_object$`jaccard`, "array[numeric]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`matthewsCorrCoef`)) {
        self$`matthewsCorrCoef` <- this_object$`matthewsCorrCoef`
      }
      if (!is.null(this_object$`confusionMatrix`)) {
        self$`confusionMatrix` <- ApiClient$new()$deserializeObj(this_object$`confusionMatrix`, "array[array[numeric]]", loadNamespace("openapi"))
      }
      self
    },

    #' @description
    #' To JSON String
    #'
    #' @return BinaryClassificationScores in JSON format
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`labels`)) {
          sprintf(
          '"labels":
             [%s]
          ',
          paste(unlist(lapply(self$`labels`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
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
        if (!is.null(self$`accuracy`)) {
          sprintf(
          '"accuracy":
            %d
                    ',
          self$`accuracy`
          )
        },
        if (!is.null(self$`balancedAccuracy`)) {
          sprintf(
          '"balancedAccuracy":
            %d
                    ',
          self$`balancedAccuracy`
          )
        },
        if (!is.null(self$`precision`)) {
          sprintf(
          '"precision":
             [%s]
          ',
          paste(unlist(lapply(self$`precision`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`recall`)) {
          sprintf(
          '"recall":
             [%s]
          ',
          paste(unlist(lapply(self$`recall`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`f1Score`)) {
          sprintf(
          '"f1Score":
             [%s]
          ',
          paste(unlist(lapply(self$`f1Score`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`jaccard`)) {
          sprintf(
          '"jaccard":
             [%s]
          ',
          paste(unlist(lapply(self$`jaccard`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`matthewsCorrCoef`)) {
          sprintf(
          '"matthewsCorrCoef":
            %d
                    ',
          self$`matthewsCorrCoef`
          )
        },
        if (!is.null(self$`confusionMatrix`)) {
          sprintf(
          '"confusionMatrix":
          [%s]
',
          paste(sapply(self$`confusionMatrix`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },

    #' @description
    #' Deserialize JSON string into an instance of BinaryClassificationScores
    #'
    #' @param input_json the JSON input
    #' @return the instance of BinaryClassificationScores
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`labels` <- ApiClient$new()$deserializeObj(this_object$`labels`, "array[character]", loadNamespace("openapi"))
      self$`yName` <- this_object$`yName`
      self$`folds` <- this_object$`folds`
      self$`accuracy` <- this_object$`accuracy`
      self$`balancedAccuracy` <- this_object$`balancedAccuracy`
      self$`precision` <- ApiClient$new()$deserializeObj(this_object$`precision`, "array[numeric]", loadNamespace("openapi"))
      self$`recall` <- ApiClient$new()$deserializeObj(this_object$`recall`, "array[numeric]", loadNamespace("openapi"))
      self$`f1Score` <- ApiClient$new()$deserializeObj(this_object$`f1Score`, "array[numeric]", loadNamespace("openapi"))
      self$`jaccard` <- ApiClient$new()$deserializeObj(this_object$`jaccard`, "array[numeric]", loadNamespace("openapi"))
      self$`matthewsCorrCoef` <- this_object$`matthewsCorrCoef`
      self$`confusionMatrix` <- ApiClient$new()$deserializeObj(this_object$`confusionMatrix`, "array[array[numeric]]", loadNamespace("openapi"))
      self
    },

    #' @description
    #' Validate JSON input with respect to BinaryClassificationScores and throw an exception if invalid
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
        stop(paste("The JSON input `", input, "` is invalid for BinaryClassificationScores: the required field `yName` is missing."))
      }
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of BinaryClassificationScores
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

      if (length(self$`precision`) > 1000) {
        return(FALSE)
      }

      if (length(self$`recall`) > 1000) {
        return(FALSE)
      }

      if (length(self$`f1Score`) > 1000) {
        return(FALSE)
      }

      if (length(self$`jaccard`) > 1000) {
        return(FALSE)
      }

      if (length(self$`confusionMatrix`) > 100) {
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

      if (length(self$`precision`) > 1000) {
        invalid_fields["precision"] <- "Invalid length for `precision`, number of items must be less than or equal to 1000."
      }

      if (length(self$`recall`) > 1000) {
        invalid_fields["recall"] <- "Invalid length for `recall`, number of items must be less than or equal to 1000."
      }

      if (length(self$`f1Score`) > 1000) {
        invalid_fields["f1Score"] <- "Invalid length for `f1Score`, number of items must be less than or equal to 1000."
      }

      if (length(self$`jaccard`) > 1000) {
        invalid_fields["jaccard"] <- "Invalid length for `jaccard`, number of items must be less than or equal to 1000."
      }

      if (length(self$`confusionMatrix`) > 100) {
        invalid_fields["confusionMatrix"] <- "Invalid length for `confusionMatrix`, number of items must be less than or equal to 100."
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
# BinaryClassificationScores$unlock()
#
## Below is an example to define the print function
# BinaryClassificationScores$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# BinaryClassificationScores$lock()

