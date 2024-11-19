#' Create a new PredictionModel
#'
#' @description
#' PredictionModel Class
#'
#' @docType class
#' @title PredictionModel
#' @description PredictionModel Class
#' @format An \code{R6Class} generator object
#' @field id Unique identifier for the prediction model integer [optional]
#' @field dependentFeatures List of dependent features for the model list(\link{Feature})
#' @field independentFeatures List of independent features for the model list(\link{Feature})
#' @field type  \link{ModelType}
#' @field rawModel Raw model data in serialized format character
#' @field rawPreprocessor Raw preprocessor data in serialized format character [optional]
#' @field doas List of Domain of Applicability (DoA) configurations list(\link{PredictionDoa}) [optional]
#' @field selectedFeatures List of feature names selected for the model list(character) [optional]
#' @field task  \link{ModelTask}
#' @field featurizers List of featurizer configurations applied to the model list(\link{Transformer}) [optional]
#' @field preprocessors List of preprocessor configurations applied to the model list(\link{Transformer}) [optional]
#' @field torchConfig Torch configuration settings, optional named list(\link{AnyType}) [optional]
#' @field rPbpkOdeSolver  character [optional]
#' @field legacyAdditionalInfo Legacy additional information settings, optional named list(\link{AnyType}) [optional]
#' @field legacyPredictionService Legacy prediction service information, if available character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
PredictionModel <- R6::R6Class(
  "PredictionModel",
  public = list(
    `id` = NULL,
    `dependentFeatures` = NULL,
    `independentFeatures` = NULL,
    `type` = NULL,
    `rawModel` = NULL,
    `rawPreprocessor` = NULL,
    `doas` = NULL,
    `selectedFeatures` = NULL,
    `task` = NULL,
    `featurizers` = NULL,
    `preprocessors` = NULL,
    `torchConfig` = NULL,
    `rPbpkOdeSolver` = NULL,
    `legacyAdditionalInfo` = NULL,
    `legacyPredictionService` = NULL,

    #' @description
    #' Initialize a new PredictionModel class.
    #'
    #' @param dependentFeatures List of dependent features for the model
    #' @param independentFeatures List of independent features for the model
    #' @param type type
    #' @param rawModel Raw model data in serialized format
    #' @param task task
    #' @param id Unique identifier for the prediction model
    #' @param rawPreprocessor Raw preprocessor data in serialized format
    #' @param doas List of Domain of Applicability (DoA) configurations
    #' @param selectedFeatures List of feature names selected for the model
    #' @param featurizers List of featurizer configurations applied to the model
    #' @param preprocessors List of preprocessor configurations applied to the model
    #' @param torchConfig Torch configuration settings, optional
    #' @param rPbpkOdeSolver rPbpkOdeSolver
    #' @param legacyAdditionalInfo Legacy additional information settings, optional
    #' @param legacyPredictionService Legacy prediction service information, if available
    #' @param ... Other optional arguments.
    initialize = function(`dependentFeatures`, `independentFeatures`, `type`, `rawModel`, `task`, `id` = NULL, `rawPreprocessor` = NULL, `doas` = NULL, `selectedFeatures` = NULL, `featurizers` = NULL, `preprocessors` = NULL, `torchConfig` = NULL, `rPbpkOdeSolver` = NULL, `legacyAdditionalInfo` = NULL, `legacyPredictionService` = NULL, ...) {
      if (!missing(`dependentFeatures`)) {
        stopifnot(is.vector(`dependentFeatures`), length(`dependentFeatures`) != 0)
        sapply(`dependentFeatures`, function(x) stopifnot(R6::is.R6(x)))
        self$`dependentFeatures` <- `dependentFeatures`
      }
      if (!missing(`independentFeatures`)) {
        stopifnot(is.vector(`independentFeatures`), length(`independentFeatures`) != 0)
        sapply(`independentFeatures`, function(x) stopifnot(R6::is.R6(x)))
        self$`independentFeatures` <- `independentFeatures`
      }
      if (!missing(`type`)) {
        if (!(`type` %in% c())) {
          stop(paste("Error! \"", `type`, "\" cannot be assigned to `type`. Must be .", sep = ""))
        }
        stopifnot(R6::is.R6(`type`))
        self$`type` <- `type`
      }
      if (!missing(`rawModel`)) {
        if (!(is.character(`rawModel`) && length(`rawModel`) == 1)) {
          stop(paste("Error! Invalid data for `rawModel`. Must be a string:", `rawModel`))
        }
        self$`rawModel` <- `rawModel`
      }
      if (!missing(`task`)) {
        if (!(`task` %in% c())) {
          stop(paste("Error! \"", `task`, "\" cannot be assigned to `task`. Must be .", sep = ""))
        }
        stopifnot(R6::is.R6(`task`))
        self$`task` <- `task`
      }
      if (!is.null(`id`)) {
        if (!(is.numeric(`id`) && length(`id`) == 1)) {
          stop(paste("Error! Invalid data for `id`. Must be an integer:", `id`))
        }
        self$`id` <- `id`
      }
      if (!is.null(`rawPreprocessor`)) {
        if (!(is.character(`rawPreprocessor`) && length(`rawPreprocessor`) == 1)) {
          stop(paste("Error! Invalid data for `rawPreprocessor`. Must be a string:", `rawPreprocessor`))
        }
        self$`rawPreprocessor` <- `rawPreprocessor`
      }
      if (!is.null(`doas`)) {
        stopifnot(is.vector(`doas`), length(`doas`) != 0)
        sapply(`doas`, function(x) stopifnot(R6::is.R6(x)))
        self$`doas` <- `doas`
      }
      if (!is.null(`selectedFeatures`)) {
        stopifnot(is.vector(`selectedFeatures`), length(`selectedFeatures`) != 0)
        sapply(`selectedFeatures`, function(x) stopifnot(is.character(x)))
        self$`selectedFeatures` <- `selectedFeatures`
      }
      if (!is.null(`featurizers`)) {
        stopifnot(is.vector(`featurizers`), length(`featurizers`) != 0)
        sapply(`featurizers`, function(x) stopifnot(R6::is.R6(x)))
        self$`featurizers` <- `featurizers`
      }
      if (!is.null(`preprocessors`)) {
        stopifnot(is.vector(`preprocessors`), length(`preprocessors`) != 0)
        sapply(`preprocessors`, function(x) stopifnot(R6::is.R6(x)))
        self$`preprocessors` <- `preprocessors`
      }
      if (!is.null(`torchConfig`)) {
        stopifnot(is.vector(`torchConfig`), length(`torchConfig`) != 0)
        sapply(`torchConfig`, function(x) stopifnot(R6::is.R6(x)))
        self$`torchConfig` <- `torchConfig`
      }
      if (!is.null(`rPbpkOdeSolver`)) {
        if (!(is.character(`rPbpkOdeSolver`) && length(`rPbpkOdeSolver`) == 1)) {
          stop(paste("Error! Invalid data for `rPbpkOdeSolver`. Must be a string:", `rPbpkOdeSolver`))
        }
        self$`rPbpkOdeSolver` <- `rPbpkOdeSolver`
      }
      if (!is.null(`legacyAdditionalInfo`)) {
        stopifnot(is.vector(`legacyAdditionalInfo`), length(`legacyAdditionalInfo`) != 0)
        sapply(`legacyAdditionalInfo`, function(x) stopifnot(R6::is.R6(x)))
        self$`legacyAdditionalInfo` <- `legacyAdditionalInfo`
      }
      if (!is.null(`legacyPredictionService`)) {
        if (!(is.character(`legacyPredictionService`) && length(`legacyPredictionService`) == 1)) {
          stop(paste("Error! Invalid data for `legacyPredictionService`. Must be a string:", `legacyPredictionService`))
        }
        self$`legacyPredictionService` <- `legacyPredictionService`
      }
    },

    #' @description
    #' To JSON String
    #'
    #' @return PredictionModel in JSON format
    toJSON = function() {
      PredictionModelObject <- list()
      if (!is.null(self$`id`)) {
        PredictionModelObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`dependentFeatures`)) {
        PredictionModelObject[["dependentFeatures"]] <-
          lapply(self$`dependentFeatures`, function(x) x$toJSON())
      }
      if (!is.null(self$`independentFeatures`)) {
        PredictionModelObject[["independentFeatures"]] <-
          lapply(self$`independentFeatures`, function(x) x$toJSON())
      }
      if (!is.null(self$`type`)) {
        PredictionModelObject[["type"]] <-
          self$`type`$toJSON()
      }
      if (!is.null(self$`rawModel`)) {
        PredictionModelObject[["rawModel"]] <-
          self$`rawModel`
      }
      if (!is.null(self$`rawPreprocessor`)) {
        PredictionModelObject[["rawPreprocessor"]] <-
          self$`rawPreprocessor`
      }
      if (!is.null(self$`doas`)) {
        PredictionModelObject[["doas"]] <-
          lapply(self$`doas`, function(x) x$toJSON())
      }
      if (!is.null(self$`selectedFeatures`)) {
        PredictionModelObject[["selectedFeatures"]] <-
          self$`selectedFeatures`
      }
      if (!is.null(self$`task`)) {
        PredictionModelObject[["task"]] <-
          self$`task`$toJSON()
      }
      if (!is.null(self$`featurizers`)) {
        PredictionModelObject[["featurizers"]] <-
          lapply(self$`featurizers`, function(x) x$toJSON())
      }
      if (!is.null(self$`preprocessors`)) {
        PredictionModelObject[["preprocessors"]] <-
          lapply(self$`preprocessors`, function(x) x$toJSON())
      }
      if (!is.null(self$`torchConfig`)) {
        PredictionModelObject[["torchConfig"]] <-
          lapply(self$`torchConfig`, function(x) x$toJSON())
      }
      if (!is.null(self$`rPbpkOdeSolver`)) {
        PredictionModelObject[["rPbpkOdeSolver"]] <-
          self$`rPbpkOdeSolver`
      }
      if (!is.null(self$`legacyAdditionalInfo`)) {
        PredictionModelObject[["legacyAdditionalInfo"]] <-
          lapply(self$`legacyAdditionalInfo`, function(x) x$toJSON())
      }
      if (!is.null(self$`legacyPredictionService`)) {
        PredictionModelObject[["legacyPredictionService"]] <-
          self$`legacyPredictionService`
      }
      PredictionModelObject
    },

    #' @description
    #' Deserialize JSON string into an instance of PredictionModel
    #'
    #' @param input_json the JSON input
    #' @return the instance of PredictionModel
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`dependentFeatures`)) {
        self$`dependentFeatures` <- ApiClient$new()$deserializeObj(this_object$`dependentFeatures`, "array[Feature]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`independentFeatures`)) {
        self$`independentFeatures` <- ApiClient$new()$deserializeObj(this_object$`independentFeatures`, "array[Feature]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`type`)) {
        `type_object` <- ModelType$new()
        `type_object`$fromJSON(jsonlite::toJSON(this_object$`type`, auto_unbox = TRUE, digits = NA))
        self$`type` <- `type_object`
      }
      if (!is.null(this_object$`rawModel`)) {
        self$`rawModel` <- this_object$`rawModel`
      }
      if (!is.null(this_object$`rawPreprocessor`)) {
        self$`rawPreprocessor` <- this_object$`rawPreprocessor`
      }
      if (!is.null(this_object$`doas`)) {
        self$`doas` <- ApiClient$new()$deserializeObj(this_object$`doas`, "array[PredictionDoa]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`selectedFeatures`)) {
        self$`selectedFeatures` <- ApiClient$new()$deserializeObj(this_object$`selectedFeatures`, "array[character]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`task`)) {
        `task_object` <- ModelTask$new()
        `task_object`$fromJSON(jsonlite::toJSON(this_object$`task`, auto_unbox = TRUE, digits = NA))
        self$`task` <- `task_object`
      }
      if (!is.null(this_object$`featurizers`)) {
        self$`featurizers` <- ApiClient$new()$deserializeObj(this_object$`featurizers`, "array[Transformer]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`preprocessors`)) {
        self$`preprocessors` <- ApiClient$new()$deserializeObj(this_object$`preprocessors`, "array[Transformer]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`torchConfig`)) {
        self$`torchConfig` <- ApiClient$new()$deserializeObj(this_object$`torchConfig`, "map(AnyType)", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`rPbpkOdeSolver`)) {
        self$`rPbpkOdeSolver` <- this_object$`rPbpkOdeSolver`
      }
      if (!is.null(this_object$`legacyAdditionalInfo`)) {
        self$`legacyAdditionalInfo` <- ApiClient$new()$deserializeObj(this_object$`legacyAdditionalInfo`, "map(AnyType)", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`legacyPredictionService`)) {
        self$`legacyPredictionService` <- this_object$`legacyPredictionService`
      }
      self
    },

    #' @description
    #' To JSON String
    #'
    #' @return PredictionModel in JSON format
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
        if (!is.null(self$`dependentFeatures`)) {
          sprintf(
          '"dependentFeatures":
          [%s]
',
          paste(sapply(self$`dependentFeatures`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`independentFeatures`)) {
          sprintf(
          '"independentFeatures":
          [%s]
',
          paste(sapply(self$`independentFeatures`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`type`)) {
          sprintf(
          '"type":
          %s
          ',
          jsonlite::toJSON(self$`type`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`rawModel`)) {
          sprintf(
          '"rawModel":
            "%s"
                    ',
          self$`rawModel`
          )
        },
        if (!is.null(self$`rawPreprocessor`)) {
          sprintf(
          '"rawPreprocessor":
            "%s"
                    ',
          self$`rawPreprocessor`
          )
        },
        if (!is.null(self$`doas`)) {
          sprintf(
          '"doas":
          [%s]
',
          paste(sapply(self$`doas`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`selectedFeatures`)) {
          sprintf(
          '"selectedFeatures":
             [%s]
          ',
          paste(unlist(lapply(self$`selectedFeatures`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`task`)) {
          sprintf(
          '"task":
          %s
          ',
          jsonlite::toJSON(self$`task`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`featurizers`)) {
          sprintf(
          '"featurizers":
          [%s]
',
          paste(sapply(self$`featurizers`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`preprocessors`)) {
          sprintf(
          '"preprocessors":
          [%s]
',
          paste(sapply(self$`preprocessors`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`torchConfig`)) {
          sprintf(
          '"torchConfig":
          %s
',
          jsonlite::toJSON(lapply(self$`torchConfig`, function(x){ x$toJSON() }), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`rPbpkOdeSolver`)) {
          sprintf(
          '"rPbpkOdeSolver":
            "%s"
                    ',
          self$`rPbpkOdeSolver`
          )
        },
        if (!is.null(self$`legacyAdditionalInfo`)) {
          sprintf(
          '"legacyAdditionalInfo":
          %s
',
          jsonlite::toJSON(lapply(self$`legacyAdditionalInfo`, function(x){ x$toJSON() }), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`legacyPredictionService`)) {
          sprintf(
          '"legacyPredictionService":
            "%s"
                    ',
          self$`legacyPredictionService`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },

    #' @description
    #' Deserialize JSON string into an instance of PredictionModel
    #'
    #' @param input_json the JSON input
    #' @return the instance of PredictionModel
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`dependentFeatures` <- ApiClient$new()$deserializeObj(this_object$`dependentFeatures`, "array[Feature]", loadNamespace("openapi"))
      self$`independentFeatures` <- ApiClient$new()$deserializeObj(this_object$`independentFeatures`, "array[Feature]", loadNamespace("openapi"))
      self$`type` <- ModelType$new()$fromJSON(jsonlite::toJSON(this_object$`type`, auto_unbox = TRUE, digits = NA))
      self$`rawModel` <- this_object$`rawModel`
      self$`rawPreprocessor` <- this_object$`rawPreprocessor`
      self$`doas` <- ApiClient$new()$deserializeObj(this_object$`doas`, "array[PredictionDoa]", loadNamespace("openapi"))
      self$`selectedFeatures` <- ApiClient$new()$deserializeObj(this_object$`selectedFeatures`, "array[character]", loadNamespace("openapi"))
      self$`task` <- ModelTask$new()$fromJSON(jsonlite::toJSON(this_object$`task`, auto_unbox = TRUE, digits = NA))
      self$`featurizers` <- ApiClient$new()$deserializeObj(this_object$`featurizers`, "array[Transformer]", loadNamespace("openapi"))
      self$`preprocessors` <- ApiClient$new()$deserializeObj(this_object$`preprocessors`, "array[Transformer]", loadNamespace("openapi"))
      self$`torchConfig` <- ApiClient$new()$deserializeObj(this_object$`torchConfig`, "map(AnyType)", loadNamespace("openapi"))
      self$`rPbpkOdeSolver` <- this_object$`rPbpkOdeSolver`
      self$`legacyAdditionalInfo` <- ApiClient$new()$deserializeObj(this_object$`legacyAdditionalInfo`, "map(AnyType)", loadNamespace("openapi"))
      self$`legacyPredictionService` <- this_object$`legacyPredictionService`
      self
    },

    #' @description
    #' Validate JSON input with respect to PredictionModel and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
      # check the required field `dependentFeatures`
      if (!is.null(input_json$`dependentFeatures`)) {
        stopifnot(is.vector(input_json$`dependentFeatures`), length(input_json$`dependentFeatures`) != 0)
        tmp <- sapply(input_json$`dependentFeatures`, function(x) stopifnot(R6::is.R6(x)))
      } else {
        stop(paste("The JSON input `", input, "` is invalid for PredictionModel: the required field `dependentFeatures` is missing."))
      }
      # check the required field `independentFeatures`
      if (!is.null(input_json$`independentFeatures`)) {
        stopifnot(is.vector(input_json$`independentFeatures`), length(input_json$`independentFeatures`) != 0)
        tmp <- sapply(input_json$`independentFeatures`, function(x) stopifnot(R6::is.R6(x)))
      } else {
        stop(paste("The JSON input `", input, "` is invalid for PredictionModel: the required field `independentFeatures` is missing."))
      }
      # check the required field `type`
      if (!is.null(input_json$`type`)) {
        stopifnot(R6::is.R6(input_json$`type`))
      } else {
        stop(paste("The JSON input `", input, "` is invalid for PredictionModel: the required field `type` is missing."))
      }
      # check the required field `rawModel`
      if (!is.null(input_json$`rawModel`)) {
        if (!(is.character(input_json$`rawModel`) && length(input_json$`rawModel`) == 1)) {
          stop(paste("Error! Invalid data for `rawModel`. Must be a string:", input_json$`rawModel`))
        }
      } else {
        stop(paste("The JSON input `", input, "` is invalid for PredictionModel: the required field `rawModel` is missing."))
      }
      # check the required field `task`
      if (!is.null(input_json$`task`)) {
        stopifnot(R6::is.R6(input_json$`task`))
      } else {
        stop(paste("The JSON input `", input, "` is invalid for PredictionModel: the required field `task` is missing."))
      }
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of PredictionModel
    toString = function() {
      self$toJSONString()
    },

    #' @description
    #' Return true if the values in all fields are valid.
    #'
    #' @return true if the values in all fields are valid.
    isValid = function() {
      # check if the required `dependentFeatures` is null
      if (is.null(self$`dependentFeatures`)) {
        return(FALSE)
      }

      # check if the required `independentFeatures` is null
      if (is.null(self$`independentFeatures`)) {
        return(FALSE)
      }

      # check if the required `type` is null
      if (is.null(self$`type`)) {
        return(FALSE)
      }

      # check if the required `rawModel` is null
      if (is.null(self$`rawModel`)) {
        return(FALSE)
      }

      # check if the required `task` is null
      if (is.null(self$`task`)) {
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
      # check if the required `dependentFeatures` is null
      if (is.null(self$`dependentFeatures`)) {
        invalid_fields["dependentFeatures"] <- "Non-nullable required field `dependentFeatures` cannot be null."
      }

      # check if the required `independentFeatures` is null
      if (is.null(self$`independentFeatures`)) {
        invalid_fields["independentFeatures"] <- "Non-nullable required field `independentFeatures` cannot be null."
      }

      # check if the required `type` is null
      if (is.null(self$`type`)) {
        invalid_fields["type"] <- "Non-nullable required field `type` cannot be null."
      }

      # check if the required `rawModel` is null
      if (is.null(self$`rawModel`)) {
        invalid_fields["rawModel"] <- "Non-nullable required field `rawModel` cannot be null."
      }

      # check if the required `task` is null
      if (is.null(self$`task`)) {
        invalid_fields["task"] <- "Non-nullable required field `task` cannot be null."
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
# PredictionModel$unlock()
#
## Below is an example to define the print function
# PredictionModel$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# PredictionModel$lock()

