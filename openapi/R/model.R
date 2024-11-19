#' Create a new Model
#'
#' @description
#' Model Class
#'
#' @docType class
#' @title Model
#' @description Model Class
#' @format An \code{R6Class} generator object
#' @field id  integer [optional]
#' @field name  character
#' @field description  character [optional]
#' @field type  \link{ModelType}
#' @field jaqpotpyVersion  character
#' @field doas  list(\link{Doa}) [optional]
#' @field libraries  list(\link{Library})
#' @field dependentFeatures  list(\link{Feature})
#' @field independentFeatures  list(\link{Feature})
#' @field sharedWithOrganizations  list(\link{Organization}) [optional]
#' @field visibility  \link{ModelVisibility}
#' @field task  \link{ModelTask}
#' @field torchConfig  named list(\link{AnyType}) [optional]
#' @field preprocessors  list(\link{Transformer}) [optional]
#' @field featurizers  list(\link{Transformer}) [optional]
#' @field rawPreprocessor A base64 representation of the raw preprocessor. character [optional]
#' @field rawModel A base64 representation of the raw model. character
#' @field creator  \link{User} [optional]
#' @field canEdit If the current user can edit the model character [optional]
#' @field isAdmin  character [optional]
#' @field selectedFeatures  list(character) [optional]
#' @field tags  character [optional]
#' @field legacyPredictionService  character [optional]
#' @field scores  \link{ModelScores} [optional]
#' @field rPbpkConfig  \link{RPbpkConfig} [optional]
#' @field createdAt The date and time when the feature was created. character [optional]
#' @field updatedAt The date and time when the feature was last updated. character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Model <- R6::R6Class(
  "Model",
  public = list(
    `id` = NULL,
    `name` = NULL,
    `description` = NULL,
    `type` = NULL,
    `jaqpotpyVersion` = NULL,
    `doas` = NULL,
    `libraries` = NULL,
    `dependentFeatures` = NULL,
    `independentFeatures` = NULL,
    `sharedWithOrganizations` = NULL,
    `visibility` = NULL,
    `task` = NULL,
    `torchConfig` = NULL,
    `preprocessors` = NULL,
    `featurizers` = NULL,
    `rawPreprocessor` = NULL,
    `rawModel` = NULL,
    `creator` = NULL,
    `canEdit` = NULL,
    `isAdmin` = NULL,
    `selectedFeatures` = NULL,
    `tags` = NULL,
    `legacyPredictionService` = NULL,
    `scores` = NULL,
    `rPbpkConfig` = NULL,
    `createdAt` = NULL,
    `updatedAt` = NULL,

    #' @description
    #' Initialize a new Model class.
    #'
    #' @param name name
    #' @param type type
    #' @param jaqpotpyVersion jaqpotpyVersion
    #' @param libraries libraries
    #' @param dependentFeatures dependentFeatures
    #' @param independentFeatures independentFeatures
    #' @param visibility visibility
    #' @param task task
    #' @param rawModel A base64 representation of the raw model.
    #' @param id id
    #' @param description description
    #' @param doas doas
    #' @param sharedWithOrganizations sharedWithOrganizations
    #' @param torchConfig torchConfig
    #' @param preprocessors preprocessors
    #' @param featurizers featurizers
    #' @param rawPreprocessor A base64 representation of the raw preprocessor.
    #' @param creator creator
    #' @param canEdit If the current user can edit the model
    #' @param isAdmin isAdmin
    #' @param selectedFeatures selectedFeatures
    #' @param tags tags
    #' @param legacyPredictionService legacyPredictionService
    #' @param scores scores
    #' @param rPbpkConfig rPbpkConfig
    #' @param createdAt The date and time when the feature was created.
    #' @param updatedAt The date and time when the feature was last updated.
    #' @param ... Other optional arguments.
    initialize = function(`name`, `type`, `jaqpotpyVersion`, `libraries`, `dependentFeatures`, `independentFeatures`, `visibility`, `task`, `rawModel`, `id` = NULL, `description` = NULL, `doas` = NULL, `sharedWithOrganizations` = NULL, `torchConfig` = NULL, `preprocessors` = NULL, `featurizers` = NULL, `rawPreprocessor` = NULL, `creator` = NULL, `canEdit` = NULL, `isAdmin` = NULL, `selectedFeatures` = NULL, `tags` = NULL, `legacyPredictionService` = NULL, `scores` = NULL, `rPbpkConfig` = NULL, `createdAt` = NULL, `updatedAt` = NULL, ...) {
      if (!missing(`name`)) {
        if (!(is.character(`name`) && length(`name`) == 1)) {
          stop(paste("Error! Invalid data for `name`. Must be a string:", `name`))
        }
        self$`name` <- `name`
      }
      if (!missing(`type`)) {
        if (!(`type` %in% c())) {
          stop(paste("Error! \"", `type`, "\" cannot be assigned to `type`. Must be .", sep = ""))
        }
        stopifnot(R6::is.R6(`type`))
        self$`type` <- `type`
      }
      if (!missing(`jaqpotpyVersion`)) {
        if (!(is.character(`jaqpotpyVersion`) && length(`jaqpotpyVersion`) == 1)) {
          stop(paste("Error! Invalid data for `jaqpotpyVersion`. Must be a string:", `jaqpotpyVersion`))
        }
        self$`jaqpotpyVersion` <- `jaqpotpyVersion`
      }
      if (!missing(`libraries`)) {
        stopifnot(is.vector(`libraries`), length(`libraries`) != 0)
        sapply(`libraries`, function(x) stopifnot(R6::is.R6(x)))
        self$`libraries` <- `libraries`
      }
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
      if (!missing(`visibility`)) {
        if (!(`visibility` %in% c())) {
          stop(paste("Error! \"", `visibility`, "\" cannot be assigned to `visibility`. Must be .", sep = ""))
        }
        stopifnot(R6::is.R6(`visibility`))
        self$`visibility` <- `visibility`
      }
      if (!missing(`task`)) {
        if (!(`task` %in% c())) {
          stop(paste("Error! \"", `task`, "\" cannot be assigned to `task`. Must be .", sep = ""))
        }
        stopifnot(R6::is.R6(`task`))
        self$`task` <- `task`
      }
      if (!missing(`rawModel`)) {
        self$`rawModel` <- `rawModel`
      }
      if (!is.null(`id`)) {
        if (!(is.numeric(`id`) && length(`id`) == 1)) {
          stop(paste("Error! Invalid data for `id`. Must be an integer:", `id`))
        }
        self$`id` <- `id`
      }
      if (!is.null(`description`)) {
        if (!(is.character(`description`) && length(`description`) == 1)) {
          stop(paste("Error! Invalid data for `description`. Must be a string:", `description`))
        }
        self$`description` <- `description`
      }
      if (!is.null(`doas`)) {
        stopifnot(is.vector(`doas`), length(`doas`) != 0)
        sapply(`doas`, function(x) stopifnot(R6::is.R6(x)))
        self$`doas` <- `doas`
      }
      if (!is.null(`sharedWithOrganizations`)) {
        stopifnot(is.vector(`sharedWithOrganizations`), length(`sharedWithOrganizations`) != 0)
        sapply(`sharedWithOrganizations`, function(x) stopifnot(R6::is.R6(x)))
        self$`sharedWithOrganizations` <- `sharedWithOrganizations`
      }
      if (!is.null(`torchConfig`)) {
        stopifnot(is.vector(`torchConfig`), length(`torchConfig`) != 0)
        sapply(`torchConfig`, function(x) stopifnot(R6::is.R6(x)))
        self$`torchConfig` <- `torchConfig`
      }
      if (!is.null(`preprocessors`)) {
        stopifnot(is.vector(`preprocessors`), length(`preprocessors`) != 0)
        sapply(`preprocessors`, function(x) stopifnot(R6::is.R6(x)))
        self$`preprocessors` <- `preprocessors`
      }
      if (!is.null(`featurizers`)) {
        stopifnot(is.vector(`featurizers`), length(`featurizers`) != 0)
        sapply(`featurizers`, function(x) stopifnot(R6::is.R6(x)))
        self$`featurizers` <- `featurizers`
      }
      if (!is.null(`rawPreprocessor`)) {
        self$`rawPreprocessor` <- `rawPreprocessor`
      }
      if (!is.null(`creator`)) {
        stopifnot(R6::is.R6(`creator`))
        self$`creator` <- `creator`
      }
      if (!is.null(`canEdit`)) {
        if (!(is.logical(`canEdit`) && length(`canEdit`) == 1)) {
          stop(paste("Error! Invalid data for `canEdit`. Must be a boolean:", `canEdit`))
        }
        self$`canEdit` <- `canEdit`
      }
      if (!is.null(`isAdmin`)) {
        if (!(is.logical(`isAdmin`) && length(`isAdmin`) == 1)) {
          stop(paste("Error! Invalid data for `isAdmin`. Must be a boolean:", `isAdmin`))
        }
        self$`isAdmin` <- `isAdmin`
      }
      if (!is.null(`selectedFeatures`)) {
        stopifnot(is.vector(`selectedFeatures`), length(`selectedFeatures`) != 0)
        sapply(`selectedFeatures`, function(x) stopifnot(is.character(x)))
        self$`selectedFeatures` <- `selectedFeatures`
      }
      if (!is.null(`tags`)) {
        if (!(is.character(`tags`) && length(`tags`) == 1)) {
          stop(paste("Error! Invalid data for `tags`. Must be a string:", `tags`))
        }
        self$`tags` <- `tags`
      }
      if (!is.null(`legacyPredictionService`)) {
        if (!(is.character(`legacyPredictionService`) && length(`legacyPredictionService`) == 1)) {
          stop(paste("Error! Invalid data for `legacyPredictionService`. Must be a string:", `legacyPredictionService`))
        }
        self$`legacyPredictionService` <- `legacyPredictionService`
      }
      if (!is.null(`scores`)) {
        stopifnot(R6::is.R6(`scores`))
        self$`scores` <- `scores`
      }
      if (!is.null(`rPbpkConfig`)) {
        stopifnot(R6::is.R6(`rPbpkConfig`))
        self$`rPbpkConfig` <- `rPbpkConfig`
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
    #' @return Model in JSON format
    toJSON = function() {
      ModelObject <- list()
      if (!is.null(self$`id`)) {
        ModelObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`name`)) {
        ModelObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`description`)) {
        ModelObject[["description"]] <-
          self$`description`
      }
      if (!is.null(self$`type`)) {
        ModelObject[["type"]] <-
          self$`type`$toJSON()
      }
      if (!is.null(self$`jaqpotpyVersion`)) {
        ModelObject[["jaqpotpyVersion"]] <-
          self$`jaqpotpyVersion`
      }
      if (!is.null(self$`doas`)) {
        ModelObject[["doas"]] <-
          lapply(self$`doas`, function(x) x$toJSON())
      }
      if (!is.null(self$`libraries`)) {
        ModelObject[["libraries"]] <-
          lapply(self$`libraries`, function(x) x$toJSON())
      }
      if (!is.null(self$`dependentFeatures`)) {
        ModelObject[["dependentFeatures"]] <-
          lapply(self$`dependentFeatures`, function(x) x$toJSON())
      }
      if (!is.null(self$`independentFeatures`)) {
        ModelObject[["independentFeatures"]] <-
          lapply(self$`independentFeatures`, function(x) x$toJSON())
      }
      if (!is.null(self$`sharedWithOrganizations`)) {
        ModelObject[["sharedWithOrganizations"]] <-
          lapply(self$`sharedWithOrganizations`, function(x) x$toJSON())
      }
      if (!is.null(self$`visibility`)) {
        ModelObject[["visibility"]] <-
          self$`visibility`$toJSON()
      }
      if (!is.null(self$`task`)) {
        ModelObject[["task"]] <-
          self$`task`$toJSON()
      }
      if (!is.null(self$`torchConfig`)) {
        ModelObject[["torchConfig"]] <-
          lapply(self$`torchConfig`, function(x) x$toJSON())
      }
      if (!is.null(self$`preprocessors`)) {
        ModelObject[["preprocessors"]] <-
          lapply(self$`preprocessors`, function(x) x$toJSON())
      }
      if (!is.null(self$`featurizers`)) {
        ModelObject[["featurizers"]] <-
          lapply(self$`featurizers`, function(x) x$toJSON())
      }
      if (!is.null(self$`rawPreprocessor`)) {
        ModelObject[["rawPreprocessor"]] <-
          self$`rawPreprocessor`
      }
      if (!is.null(self$`rawModel`)) {
        ModelObject[["rawModel"]] <-
          self$`rawModel`
      }
      if (!is.null(self$`creator`)) {
        ModelObject[["creator"]] <-
          self$`creator`$toJSON()
      }
      if (!is.null(self$`canEdit`)) {
        ModelObject[["canEdit"]] <-
          self$`canEdit`
      }
      if (!is.null(self$`isAdmin`)) {
        ModelObject[["isAdmin"]] <-
          self$`isAdmin`
      }
      if (!is.null(self$`selectedFeatures`)) {
        ModelObject[["selectedFeatures"]] <-
          self$`selectedFeatures`
      }
      if (!is.null(self$`tags`)) {
        ModelObject[["tags"]] <-
          self$`tags`
      }
      if (!is.null(self$`legacyPredictionService`)) {
        ModelObject[["legacyPredictionService"]] <-
          self$`legacyPredictionService`
      }
      if (!is.null(self$`scores`)) {
        ModelObject[["scores"]] <-
          self$`scores`$toJSON()
      }
      if (!is.null(self$`rPbpkConfig`)) {
        ModelObject[["rPbpkConfig"]] <-
          self$`rPbpkConfig`$toJSON()
      }
      if (!is.null(self$`createdAt`)) {
        ModelObject[["createdAt"]] <-
          self$`createdAt`
      }
      if (!is.null(self$`updatedAt`)) {
        ModelObject[["updatedAt"]] <-
          self$`updatedAt`
      }
      ModelObject
    },

    #' @description
    #' Deserialize JSON string into an instance of Model
    #'
    #' @param input_json the JSON input
    #' @return the instance of Model
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`name`)) {
        self$`name` <- this_object$`name`
      }
      if (!is.null(this_object$`description`)) {
        self$`description` <- this_object$`description`
      }
      if (!is.null(this_object$`type`)) {
        `type_object` <- ModelType$new()
        `type_object`$fromJSON(jsonlite::toJSON(this_object$`type`, auto_unbox = TRUE, digits = NA))
        self$`type` <- `type_object`
      }
      if (!is.null(this_object$`jaqpotpyVersion`)) {
        self$`jaqpotpyVersion` <- this_object$`jaqpotpyVersion`
      }
      if (!is.null(this_object$`doas`)) {
        self$`doas` <- ApiClient$new()$deserializeObj(this_object$`doas`, "array[Doa]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`libraries`)) {
        self$`libraries` <- ApiClient$new()$deserializeObj(this_object$`libraries`, "array[Library]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`dependentFeatures`)) {
        self$`dependentFeatures` <- ApiClient$new()$deserializeObj(this_object$`dependentFeatures`, "array[Feature]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`independentFeatures`)) {
        self$`independentFeatures` <- ApiClient$new()$deserializeObj(this_object$`independentFeatures`, "array[Feature]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`sharedWithOrganizations`)) {
        self$`sharedWithOrganizations` <- ApiClient$new()$deserializeObj(this_object$`sharedWithOrganizations`, "array[Organization]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`visibility`)) {
        `visibility_object` <- ModelVisibility$new()
        `visibility_object`$fromJSON(jsonlite::toJSON(this_object$`visibility`, auto_unbox = TRUE, digits = NA))
        self$`visibility` <- `visibility_object`
      }
      if (!is.null(this_object$`task`)) {
        `task_object` <- ModelTask$new()
        `task_object`$fromJSON(jsonlite::toJSON(this_object$`task`, auto_unbox = TRUE, digits = NA))
        self$`task` <- `task_object`
      }
      if (!is.null(this_object$`torchConfig`)) {
        self$`torchConfig` <- ApiClient$new()$deserializeObj(this_object$`torchConfig`, "map(AnyType)", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`preprocessors`)) {
        self$`preprocessors` <- ApiClient$new()$deserializeObj(this_object$`preprocessors`, "array[Transformer]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`featurizers`)) {
        self$`featurizers` <- ApiClient$new()$deserializeObj(this_object$`featurizers`, "array[Transformer]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`rawPreprocessor`)) {
        self$`rawPreprocessor` <- this_object$`rawPreprocessor`
      }
      if (!is.null(this_object$`rawModel`)) {
        self$`rawModel` <- this_object$`rawModel`
      }
      if (!is.null(this_object$`creator`)) {
        `creator_object` <- User$new()
        `creator_object`$fromJSON(jsonlite::toJSON(this_object$`creator`, auto_unbox = TRUE, digits = NA))
        self$`creator` <- `creator_object`
      }
      if (!is.null(this_object$`canEdit`)) {
        self$`canEdit` <- this_object$`canEdit`
      }
      if (!is.null(this_object$`isAdmin`)) {
        self$`isAdmin` <- this_object$`isAdmin`
      }
      if (!is.null(this_object$`selectedFeatures`)) {
        self$`selectedFeatures` <- ApiClient$new()$deserializeObj(this_object$`selectedFeatures`, "array[character]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`tags`)) {
        self$`tags` <- this_object$`tags`
      }
      if (!is.null(this_object$`legacyPredictionService`)) {
        self$`legacyPredictionService` <- this_object$`legacyPredictionService`
      }
      if (!is.null(this_object$`scores`)) {
        `scores_object` <- ModelScores$new()
        `scores_object`$fromJSON(jsonlite::toJSON(this_object$`scores`, auto_unbox = TRUE, digits = NA))
        self$`scores` <- `scores_object`
      }
      if (!is.null(this_object$`rPbpkConfig`)) {
        `rpbpkconfig_object` <- RPbpkConfig$new()
        `rpbpkconfig_object`$fromJSON(jsonlite::toJSON(this_object$`rPbpkConfig`, auto_unbox = TRUE, digits = NA))
        self$`rPbpkConfig` <- `rpbpkconfig_object`
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
    #' @return Model in JSON format
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
        if (!is.null(self$`description`)) {
          sprintf(
          '"description":
            "%s"
                    ',
          self$`description`
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
        if (!is.null(self$`jaqpotpyVersion`)) {
          sprintf(
          '"jaqpotpyVersion":
            "%s"
                    ',
          self$`jaqpotpyVersion`
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
        if (!is.null(self$`libraries`)) {
          sprintf(
          '"libraries":
          [%s]
',
          paste(sapply(self$`libraries`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
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
        if (!is.null(self$`sharedWithOrganizations`)) {
          sprintf(
          '"sharedWithOrganizations":
          [%s]
',
          paste(sapply(self$`sharedWithOrganizations`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`visibility`)) {
          sprintf(
          '"visibility":
          %s
          ',
          jsonlite::toJSON(self$`visibility`$toJSON(), auto_unbox = TRUE, digits = NA)
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
        if (!is.null(self$`torchConfig`)) {
          sprintf(
          '"torchConfig":
          %s
',
          jsonlite::toJSON(lapply(self$`torchConfig`, function(x){ x$toJSON() }), auto_unbox = TRUE, digits = NA)
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
        if (!is.null(self$`featurizers`)) {
          sprintf(
          '"featurizers":
          [%s]
',
          paste(sapply(self$`featurizers`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
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
        if (!is.null(self$`rawModel`)) {
          sprintf(
          '"rawModel":
            "%s"
                    ',
          self$`rawModel`
          )
        },
        if (!is.null(self$`creator`)) {
          sprintf(
          '"creator":
          %s
          ',
          jsonlite::toJSON(self$`creator`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`canEdit`)) {
          sprintf(
          '"canEdit":
            %s
                    ',
          tolower(self$`canEdit`)
          )
        },
        if (!is.null(self$`isAdmin`)) {
          sprintf(
          '"isAdmin":
            %s
                    ',
          tolower(self$`isAdmin`)
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
        if (!is.null(self$`tags`)) {
          sprintf(
          '"tags":
            "%s"
                    ',
          self$`tags`
          )
        },
        if (!is.null(self$`legacyPredictionService`)) {
          sprintf(
          '"legacyPredictionService":
            "%s"
                    ',
          self$`legacyPredictionService`
          )
        },
        if (!is.null(self$`scores`)) {
          sprintf(
          '"scores":
          %s
          ',
          jsonlite::toJSON(self$`scores`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`rPbpkConfig`)) {
          sprintf(
          '"rPbpkConfig":
          %s
          ',
          jsonlite::toJSON(self$`rPbpkConfig`$toJSON(), auto_unbox = TRUE, digits = NA)
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
    #' Deserialize JSON string into an instance of Model
    #'
    #' @param input_json the JSON input
    #' @return the instance of Model
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`name` <- this_object$`name`
      self$`description` <- this_object$`description`
      self$`type` <- ModelType$new()$fromJSON(jsonlite::toJSON(this_object$`type`, auto_unbox = TRUE, digits = NA))
      self$`jaqpotpyVersion` <- this_object$`jaqpotpyVersion`
      self$`doas` <- ApiClient$new()$deserializeObj(this_object$`doas`, "array[Doa]", loadNamespace("openapi"))
      self$`libraries` <- ApiClient$new()$deserializeObj(this_object$`libraries`, "array[Library]", loadNamespace("openapi"))
      self$`dependentFeatures` <- ApiClient$new()$deserializeObj(this_object$`dependentFeatures`, "array[Feature]", loadNamespace("openapi"))
      self$`independentFeatures` <- ApiClient$new()$deserializeObj(this_object$`independentFeatures`, "array[Feature]", loadNamespace("openapi"))
      self$`sharedWithOrganizations` <- ApiClient$new()$deserializeObj(this_object$`sharedWithOrganizations`, "array[Organization]", loadNamespace("openapi"))
      self$`visibility` <- ModelVisibility$new()$fromJSON(jsonlite::toJSON(this_object$`visibility`, auto_unbox = TRUE, digits = NA))
      self$`task` <- ModelTask$new()$fromJSON(jsonlite::toJSON(this_object$`task`, auto_unbox = TRUE, digits = NA))
      self$`torchConfig` <- ApiClient$new()$deserializeObj(this_object$`torchConfig`, "map(AnyType)", loadNamespace("openapi"))
      self$`preprocessors` <- ApiClient$new()$deserializeObj(this_object$`preprocessors`, "array[Transformer]", loadNamespace("openapi"))
      self$`featurizers` <- ApiClient$new()$deserializeObj(this_object$`featurizers`, "array[Transformer]", loadNamespace("openapi"))
      self$`rawPreprocessor` <- this_object$`rawPreprocessor`
      self$`rawModel` <- this_object$`rawModel`
      self$`creator` <- User$new()$fromJSON(jsonlite::toJSON(this_object$`creator`, auto_unbox = TRUE, digits = NA))
      self$`canEdit` <- this_object$`canEdit`
      self$`isAdmin` <- this_object$`isAdmin`
      self$`selectedFeatures` <- ApiClient$new()$deserializeObj(this_object$`selectedFeatures`, "array[character]", loadNamespace("openapi"))
      self$`tags` <- this_object$`tags`
      self$`legacyPredictionService` <- this_object$`legacyPredictionService`
      self$`scores` <- ModelScores$new()$fromJSON(jsonlite::toJSON(this_object$`scores`, auto_unbox = TRUE, digits = NA))
      self$`rPbpkConfig` <- RPbpkConfig$new()$fromJSON(jsonlite::toJSON(this_object$`rPbpkConfig`, auto_unbox = TRUE, digits = NA))
      self$`createdAt` <- this_object$`createdAt`
      self$`updatedAt` <- this_object$`updatedAt`
      self
    },

    #' @description
    #' Validate JSON input with respect to Model and throw an exception if invalid
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
        stop(paste("The JSON input `", input, "` is invalid for Model: the required field `name` is missing."))
      }
      # check the required field `type`
      if (!is.null(input_json$`type`)) {
        stopifnot(R6::is.R6(input_json$`type`))
      } else {
        stop(paste("The JSON input `", input, "` is invalid for Model: the required field `type` is missing."))
      }
      # check the required field `jaqpotpyVersion`
      if (!is.null(input_json$`jaqpotpyVersion`)) {
        if (!(is.character(input_json$`jaqpotpyVersion`) && length(input_json$`jaqpotpyVersion`) == 1)) {
          stop(paste("Error! Invalid data for `jaqpotpyVersion`. Must be a string:", input_json$`jaqpotpyVersion`))
        }
      } else {
        stop(paste("The JSON input `", input, "` is invalid for Model: the required field `jaqpotpyVersion` is missing."))
      }
      # check the required field `libraries`
      if (!is.null(input_json$`libraries`)) {
        stopifnot(is.vector(input_json$`libraries`), length(input_json$`libraries`) != 0)
        tmp <- sapply(input_json$`libraries`, function(x) stopifnot(R6::is.R6(x)))
      } else {
        stop(paste("The JSON input `", input, "` is invalid for Model: the required field `libraries` is missing."))
      }
      # check the required field `dependentFeatures`
      if (!is.null(input_json$`dependentFeatures`)) {
        stopifnot(is.vector(input_json$`dependentFeatures`), length(input_json$`dependentFeatures`) != 0)
        tmp <- sapply(input_json$`dependentFeatures`, function(x) stopifnot(R6::is.R6(x)))
      } else {
        stop(paste("The JSON input `", input, "` is invalid for Model: the required field `dependentFeatures` is missing."))
      }
      # check the required field `independentFeatures`
      if (!is.null(input_json$`independentFeatures`)) {
        stopifnot(is.vector(input_json$`independentFeatures`), length(input_json$`independentFeatures`) != 0)
        tmp <- sapply(input_json$`independentFeatures`, function(x) stopifnot(R6::is.R6(x)))
      } else {
        stop(paste("The JSON input `", input, "` is invalid for Model: the required field `independentFeatures` is missing."))
      }
      # check the required field `visibility`
      if (!is.null(input_json$`visibility`)) {
        stopifnot(R6::is.R6(input_json$`visibility`))
      } else {
        stop(paste("The JSON input `", input, "` is invalid for Model: the required field `visibility` is missing."))
      }
      # check the required field `task`
      if (!is.null(input_json$`task`)) {
        stopifnot(R6::is.R6(input_json$`task`))
      } else {
        stop(paste("The JSON input `", input, "` is invalid for Model: the required field `task` is missing."))
      }
      # check the required field `rawModel`
      if (!is.null(input_json$`rawModel`)) {
      } else {
        stop(paste("The JSON input `", input, "` is invalid for Model: the required field `rawModel` is missing."))
      }
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Model
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

      if (nchar(self$`name`) > 255) {
        return(FALSE)
      }
      if (nchar(self$`name`) < 3) {
        return(FALSE)
      }

      if (nchar(self$`description`) > 50000) {
        return(FALSE)
      }
      if (nchar(self$`description`) < 3) {
        return(FALSE)
      }

      # check if the required `type` is null
      if (is.null(self$`type`)) {
        return(FALSE)
      }

      # check if the required `jaqpotpyVersion` is null
      if (is.null(self$`jaqpotpyVersion`)) {
        return(FALSE)
      }

      if (length(self$`doas`) > 50) {
        return(FALSE)
      }

      # check if the required `libraries` is null
      if (is.null(self$`libraries`)) {
        return(FALSE)
      }

      if (length(self$`libraries`) > 1000) {
        return(FALSE)
      }

      # check if the required `dependentFeatures` is null
      if (is.null(self$`dependentFeatures`)) {
        return(FALSE)
      }

      if (length(self$`dependentFeatures`) > 1000) {
        return(FALSE)
      }

      # check if the required `independentFeatures` is null
      if (is.null(self$`independentFeatures`)) {
        return(FALSE)
      }

      if (length(self$`independentFeatures`) > 1000) {
        return(FALSE)
      }

      # check if the required `visibility` is null
      if (is.null(self$`visibility`)) {
        return(FALSE)
      }

      # check if the required `task` is null
      if (is.null(self$`task`)) {
        return(FALSE)
      }

      if (length(self$`torchConfig`) > 20) {
        return(FALSE)
      }

      if (length(self$`preprocessors`) > 50) {
        return(FALSE)
      }

      if (length(self$`featurizers`) > 50) {
        return(FALSE)
      }

      if (nchar(self$`rawPreprocessor`) > 10000000) {
        return(FALSE)
      }

      # check if the required `rawModel` is null
      if (is.null(self$`rawModel`)) {
        return(FALSE)
      }

      if (nchar(self$`rawModel`) > 10000000) {
        return(FALSE)
      }

      if (length(self$`selectedFeatures`) > 1000) {
        return(FALSE)
      }

      if (nchar(self$`tags`) > 1000) {
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

      if (nchar(self$`name`) > 255) {
        invalid_fields["name"] <- "Invalid length for `name`, must be smaller than or equal to 255."
      }
      if (nchar(self$`name`) < 3) {
        invalid_fields["name"] <- "Invalid length for `name`, must be bigger than or equal to 3."
      }

      if (nchar(self$`description`) > 50000) {
        invalid_fields["description"] <- "Invalid length for `description`, must be smaller than or equal to 50000."
      }
      if (nchar(self$`description`) < 3) {
        invalid_fields["description"] <- "Invalid length for `description`, must be bigger than or equal to 3."
      }

      # check if the required `type` is null
      if (is.null(self$`type`)) {
        invalid_fields["type"] <- "Non-nullable required field `type` cannot be null."
      }

      # check if the required `jaqpotpyVersion` is null
      if (is.null(self$`jaqpotpyVersion`)) {
        invalid_fields["jaqpotpyVersion"] <- "Non-nullable required field `jaqpotpyVersion` cannot be null."
      }

      if (length(self$`doas`) > 50) {
        invalid_fields["doas"] <- "Invalid length for `doas`, number of items must be less than or equal to 50."
      }

      # check if the required `libraries` is null
      if (is.null(self$`libraries`)) {
        invalid_fields["libraries"] <- "Non-nullable required field `libraries` cannot be null."
      }

      if (length(self$`libraries`) > 1000) {
        invalid_fields["libraries"] <- "Invalid length for `libraries`, number of items must be less than or equal to 1000."
      }

      # check if the required `dependentFeatures` is null
      if (is.null(self$`dependentFeatures`)) {
        invalid_fields["dependentFeatures"] <- "Non-nullable required field `dependentFeatures` cannot be null."
      }

      if (length(self$`dependentFeatures`) > 1000) {
        invalid_fields["dependentFeatures"] <- "Invalid length for `dependentFeatures`, number of items must be less than or equal to 1000."
      }

      # check if the required `independentFeatures` is null
      if (is.null(self$`independentFeatures`)) {
        invalid_fields["independentFeatures"] <- "Non-nullable required field `independentFeatures` cannot be null."
      }

      if (length(self$`independentFeatures`) > 1000) {
        invalid_fields["independentFeatures"] <- "Invalid length for `independentFeatures`, number of items must be less than or equal to 1000."
      }

      # check if the required `visibility` is null
      if (is.null(self$`visibility`)) {
        invalid_fields["visibility"] <- "Non-nullable required field `visibility` cannot be null."
      }

      # check if the required `task` is null
      if (is.null(self$`task`)) {
        invalid_fields["task"] <- "Non-nullable required field `task` cannot be null."
      }

      if (length(self$`torchConfig`) > 20) {
        invalid_fields["torchConfig"] <- "Invalid length for `torchConfig`, number of items must be less than or equal to 20."
      }

      if (length(self$`preprocessors`) > 50) {
        invalid_fields["preprocessors"] <- "Invalid length for `preprocessors`, number of items must be less than or equal to 50."
      }

      if (length(self$`featurizers`) > 50) {
        invalid_fields["featurizers"] <- "Invalid length for `featurizers`, number of items must be less than or equal to 50."
      }

      if (nchar(self$`rawPreprocessor`) > 10000000) {
        invalid_fields["rawPreprocessor"] <- "Invalid length for `rawPreprocessor`, must be smaller than or equal to 10000000."
      }

      # check if the required `rawModel` is null
      if (is.null(self$`rawModel`)) {
        invalid_fields["rawModel"] <- "Non-nullable required field `rawModel` cannot be null."
      }

      if (nchar(self$`rawModel`) > 10000000) {
        invalid_fields["rawModel"] <- "Invalid length for `rawModel`, must be smaller than or equal to 10000000."
      }

      if (length(self$`selectedFeatures`) > 1000) {
        invalid_fields["selectedFeatures"] <- "Invalid length for `selectedFeatures`, number of items must be less than or equal to 1000."
      }

      if (nchar(self$`tags`) > 1000) {
        invalid_fields["tags"] <- "Invalid length for `tags`, must be smaller than or equal to 1000."
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
# Model$unlock()
#
## Below is an example to define the print function
# Model$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Model$lock()

