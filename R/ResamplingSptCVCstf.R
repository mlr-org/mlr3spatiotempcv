#' @title Create spatio-temporal Folds Using Predefined Groups
#'
#' @template rox_sptcv_cstf
#' @export
#' @examples
#' library(mlr3)
#' task = tsk("ecuador")
#'
#' # Instantiate Resampling
#' rcv = rsmp("spcv-cstf", folds = 5)
#' rcv$instantiate(task, time_var = "Date", space_var = "SOURCEID")
#'
#' # Individual sets:
#' rcv$train_set(1)
#' rcv$test_set(1)
#' # check that no obs are in both sets
#' intersect(rcv$train_set(1), rcv$test_set(1)) # good!
#'
#' # Internal storage:
#' rcv$instance # table
ResamplingSptCVCstf = R6Class("ResamplingSptCVCstf",
  inherit = mlr3::Resampling,

  public = list(
    #' @description
    #' Create a "Spacetime Folds" resampling instance.
    #' @param id `character(1)`\cr
    #'   Identifier for the resampling strategy.
    initialize = function(id = "spcv-cstf") {
      ps = ParamSet$new(params = list(
        ParamInt$new("folds", lower = 1L, default = 10L, tags = "required")
      ))
      ps$values = list(folds = 10L)
      super$initialize(
        id = id,
        param_set = ps
      )
    },

    #' @description
    #'  Materializes fixed training and test splits for a given task.
    #' @param task [Task]\cr
    #'   A task to instantiate.
    #' @param space_var `[character]`\cr
    #'   Column name identifying the spatial units.
    #' @param time_var `[character]`\cr
    #'   Column name identifying the temporal units.
    #' @param class `[character]`\cr
    #'   Column name identifying a class unit (e.g. land cover).
    instantiate = function(task, space_var = NULL, time_var = NULL,
      class = NULL) {

      assert_task(task)
      groups = task$groups

      if (!is.null(groups)) {
        stopf("Grouping is not supported for spatial resampling methods")
      }

      private$.sample(task, space_var, time_var, class)

      self$task_hash = task$hash
      invisible(self)
    }
  ),

  active = list(
    #' @field iters `integer(1)`\cr
    #'   Returns the number of resampling iterations, depending on the
    #'   values stored in the `param_set`.
    iters = function() {
      self$param_set$values$folds
    }
  ),

  private = list(
    .sample = function(task, space_var, time_var, class) {

      k = self$param_set$values$folds
      data = task$data()

      # if classification is used, make sure that classes are equally
      # distributed across folds
      if (!is.null(class)) {
        unit = unique(data[, c(..space_var, ..class)])
        # unit needs to be a data.frame here
        unit$cstf_fold = caret::createFolds(
          as.data.frame(unit)[, which(names(unit) == class)],
          k = k, list = FALSE)
        unit$cstf_fold = as.factor(as.character(unit$cstf_fold))
        data = merge(data, unit,
          by.x = c(space_var, class),
          by.y = c(space_var, class), all.x = TRUE, sort = FALSE)
        space_var = "cstf_fold"
      }

      if (!is.null(space_var)) {
        checkmate::assert_factor(data[[space_var]], null.ok = TRUE)
        if (k > uniqueN(data[[space_var]])) {
          k = uniqueN(data[[space_var]])
          cli::cli_alert_warning("Number of folds is higher than number of
            unique locations.
            Setting folds to the maximum amount of unique levels of variable
            {space_var} which is {k}.", wrap = TRUE)
        }
      }
      if (!is.null(time_var)) {
        checkmate::assert_factor(data[[time_var]], null.ok = TRUE)
        if (k > uniqueN(data[[time_var]])) {
          k = uniqueN(data[[time_var]])
          cli::cli_alert_warning("Number of folds is higher than number of
            unique points in time.
            Setting folds to the maximum amount of unique levels of variable
            {time_var} which is {k}.", wrap = TRUE)
        }
      }

      seed = sample(1:1000, 1)

      # split space into k folds
      if (!is.null(space_var)) {
        set.seed(seed)
        spacefolds = lapply(caret::createFolds(
          1:uniqueN(data[[space_var]]),
          k), function(y) {
          unique(data[[space_var]])[y]
        })
      }
      # split time into k folds
      if (!is.null(time_var)) {
        set.seed(seed)
        timefolds = lapply(caret::createFolds(
          1:uniqueN(data[[time_var]]),
          k), function(y) {
          unique(data[[time_var]])[y]
        })
      }

      # combine space and time folds
      for (i in 1:k) {
        if (!is.null(time_var) & !is.null(space_var)) {
          self$instance$test[[i]] = which(data[[space_var]] %in%
            spacefolds[[i]] &
            data[[time_var]] %in% timefolds[[i]])
          self$instance$train[[i]] = which(!data[[space_var]] %in%
            spacefolds[[i]] &
            data[[time_var]] %in% timefolds[[i]])
        } else if (is.null(time_var) & !is.null(space_var)) {
          self$instance$test[[i]] = which(data[[space_var]] %in%
            spacefolds[[i]])
          self$instance$train[[i]] = which(!data[[space_var]] %in%
            spacefolds[[i]])
        } else if (!is.null(time_var) & is.null(space_var)) {
          self$instance$test[[i]] = which(data[[time_var]] %in%
            timefolds[[i]])
          self$instance$train[[i]] = which(!data[[time_var]] %in%
            timefolds[[i]])
        }
      }
      invisible(self)
    },

    # private get funs for train and test which are used by
    # Resampling$.get_set()
    .get_train = function(i) {
      self$instance$train[[i]]
    },

    .get_test = function(i) {
      self$instance$test[[i]]
    }
  )
)
