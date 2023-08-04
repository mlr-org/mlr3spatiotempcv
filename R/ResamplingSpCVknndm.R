#' @title (CAST) K-fold Nearest Neighbour Distance Matching
#'
#' @template rox_sptcv_knndm
#' @name mlr_resamplings_spcv_knndm
#'
#' @references
#' `r format_bib("linnenbrink2023")`
#'
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3spatial)
#' set.seed(42)
#' simarea = list(matrix(c(0, 0, 0, 100, 100, 100, 100, 0, 0, 0), ncol = 2, byrow = TRUE))
#' simarea = sf::st_polygon(simarea)
#' train_points = sf::st_sample(simarea, 1000, type = "random")
#' train_points = sf::st_as_sf(train_points)
#' train_points$target = as.factor(sample(c("TRUE", "FALSE"), 1000, replace = TRUE))
#' pred_points = sf::st_sample(simarea, 1000, type = "regular")
#'
#' task = mlr3spatial::as_task_classif_st(sf::st_as_sf(train_points), "target", positive = "TRUE")
#'
#' cv_knndm = rsmp("spcv_knndm", ppoints = pred_points)
#' cv_knndm$instantiate(task)

#' #' ### Individual sets:
#' # cv_knndm$train_set(1)
#' # cv_knndm$test_set(1)
#' # check that no obs are in both sets
#' intersect(cv_knndm$train_set(1), cv_knndm$test_set(1)) # good!
#'
#' # Internal storage:
#' # cv_knndm$instance # table
ResamplingSpCVKnndm = R6Class("ResamplingSpCVKnndm",
  inherit = mlr3::Resampling,
  public = list(

    #' @description
    #' Create a "K-fold Nearest Neighbour Distance Matching" resampling instance.
    #' @param id `character(1)`\cr
    #'   Identifier for the resampling strategy.
    initialize = function(id = "spcv_knndm") {
      ps = ParamSet$new(params = list(
        ParamUty$new("modeldomain", default = NULL,
          custom_check = function(x) {
            checkmate::check_class(x, "SpatRaster",
              null.ok = TRUE)
          }
        ),
        ParamUty$new("ppoints", default = NULL,
          custom_check = function(x) {
            checkmate::check_class(x, "sfc_POINT",
              null.ok = TRUE)
          }
        ),
        ParamFct$new("space", levels = c("geographical"), default = "geographical"),
        ParamInt$new("folds", default = 10, lower = 2),
        ParamDbl$new("maxp", default = 0.5, lower = 0, upper = 1),
        ParamFct$new("clustering", default = "hierarchical",
          levels = c("hierarchical", "kmeans"), tags = "required"),
        ParamUty$new("linkf", default = "ward.D2"),
        ParamInt$new("samplesize"),
        ParamFct$new("sampling", levels = c("random", "hexagonal", "regular", "Fibonacci"))
      ))
      ps$values = list(folds = 10L)

      super$initialize(
        id = id,
        param_set = ps,
        label = "Spatial 'K-fold Nearest Neighbour Distance Matching",
        man = "mlr3spatiotempcv::mlr_resamplings_spcv_knndm"
      )
    },

    #' @description
    #'  Materializes fixed training and test splits for a given task.
    #' @param task [Task]\cr
    #'   A task to instantiate.
    instantiate = function(task) {
      task = assert_task(task)
      strata = task$strata
      groups = task$groups

      if (!is.null(groups)) {
        stopf("Grouping is not supported for spatial resampling methods.")
      }

      pv = self$param_set$values

      if (is.null(pv$modeldomain) && is.null(pv$ppoints)) {
        stopf("Either 'modeldomain' or 'ppoints' need to be set.")
      }

      if (!is.null(groups)) {
        stopf("Grouping is not supported for spatial resampling methods.")
      }

      if (!is.null(strata)) {
        stopf("Stratified sampling is not supported for spatial resampling methods.")
      }


      # Set values to default if missing
      if (is.null(pv$space)) {
        self$param_set$values$space = self$param_set$default[["space"]] # nocov
      }
      if (is.null(pv$maxp)) {
        self$param_set$values$maxp = self$param_set$default[["maxp"]] # nocov
      }
      if (is.null(pv$linkf)) {
        self$param_set$values$linkf = self$param_set$default[["linkf"]]
      }
      if (is.null(pv$clustering)) {
        self$param_set$values$clustering = self$param_set$default[["clustering"]]
      }

      instance = private$.sample(
        task$row_ids,
        task$coordinates(),
        task$crs
      )

      self$instance = instance
      self$task_hash = task$hash
      self$task_nrow = task$nrow
      invisible(self)
    }
  ),
  active = list(
    #' @field iters `integer(1)`\cr
    #'   Returns the number of resampling iterations, depending on the
    #'   values stored in the `param_set`.
    iters = function() {
      as.integer(self$param_set$values$folds)
    }
  ),


  private = list(
    .sample = function(ids, coords, crs) {

      points = sf::st_as_sf(coords,
        coords = colnames(coords),
        crs = crs
      )

      inds = CAST::knndm(
        tpoints = points,
        modeldomain = self$param_set$values$modeldomain,
        ppoints = self$param_set$values$ppoints,
        k = self$param_set$values$folds,
        maxp = self$param_set$values$maxp,
        clustering = self$param_set$values$clustering,
        linkf = self$param_set$values$linkf,
        samplesize = self$param_set$values$samplesize,
        sampling = self$param_set$values$sampling
      )

      list(train = inds$indx_train, test = inds$indx_test)

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
