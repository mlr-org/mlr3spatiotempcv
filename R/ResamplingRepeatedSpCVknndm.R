#' @title (CAST) Repeated K-fold Nearest Neighbour Distance Matching
#'
#' @template rox_sptcv_knndm
#' @name mlr_resamplings_repeated_spcv_knndm
#'
#' @section Parameters:
#'
#' * `repeats` (`integer(1)`)\cr
#'   Number of repeats.
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
#' cv_knndm = rsmp("repeated_spcv_knndm", ppoints = pred_points, repeats = 2)
#' cv_knndm$instantiate(task)

#' #' ### Individual sets:
#' # cv_knndm$train_set(1)
#' # cv_knndm$test_set(1)
#' # check that no obs are in both sets
#' intersect(cv_knndm$train_set(1), cv_knndm$test_set(1)) # good!
#'
#' # Internal storage:
#' # cv_knndm$instance # table
ResamplingRepeatedSpCVKnndm = R6Class("ResamplingRepeatedSpCVKnndm",
  inherit = mlr3::Resampling,
  public = list(
    #' @description
    #' Create a "K-fold Nearest Neighbour Distance Matching" resampling instance.
    #'
    #' @param id `character(1)`\cr
    #'   Identifier for the resampling strategy.
    initialize = function(id = "repeated_spcv_knndm") {
      ps = ps(
        modeldomain = p_uty(default = NULL,
          custom_check = crate(function(x) {
            checkmate::check_class(x, "SpatRaster",
              null.ok = TRUE)
          })
        ),
        ppoints = p_uty(default = NULL,
          custom_check = crate(function(x) {
            checkmate::check_class(x, "sfc_POINT",
              null.ok = TRUE)
          })
        ),
        space = p_fct(levels = "geographical", default = "geographical"),
        folds = p_int(default = 10, lower = 2),
        maxp = p_dbl(default = 0.5, lower = 0, upper = 1),
        clustering = p_fct(default = "hierarchical",
          levels = c("hierarchical", "kmeans"), tags = "required"),
        linkf = p_uty(default = "ward.D2"),
        samplesize = p_int(),
        sampling = p_fct(levels = c("random", "hexagonal", "regular", "Fibonacci")),
        repeats = p_int(lower = 1, tags = "required")
      )
      ps$values = list(repeats = 1, folds = 10)

      super$initialize(
        id = id,
        param_set = ps,
        label = "Repeated Spatial 'K-fold Nearest Neighbour Distance Matching",
        man = "mlr3spatiotempcv::mlr_resamplings_repeated_spcv_knndm"
      )
    },

    #' @description Translates iteration numbers to fold number.
    #' @param iters `integer()`\cr
    #'   Iteration number.
    folds = function(iters) {
      iters = assert_integerish(iters, any.missing = FALSE, coerce = TRUE)
      ((iters - 1L) %% as.integer(self$param_set$values$folds)) + 1L
    },

    #' @description Translates iteration numbers to repetition number.
    #' @param iters `integer()`\cr
    #'   Iteration number.
    repeats = function(iters) {
      iters = assert_integerish(iters, any.missing = FALSE, coerce = TRUE)
      ((iters - 1L) %/% as.integer(self$param_set$values$folds)) + 1L
    },

    #' @description
    #'  Materializes fixed training and test splits for a given task.
    #' @param task [Task]\cr
    #'  A task to instantiate.
    instantiate = function(task) {

      mlr3::assert_task(task)
      assert_spatial_task(task)
      groups = task$groups

      pv = self$param_set$values

      # Set values to default if missing
      mlr3misc::map(
        c("modeldomain", "ppoints", "space", "maxp", "folds", "clustering",
          "linkf", "samplesize", "sampling"),
        function(x) private$.set_default_param_values(x)
      )

      if (is.null(pv$modeldomain) && is.null(pv$ppoints)) {
        stopf("Either 'modeldomain' or 'ppoints' need to be set.")
      }

      if (!is.null(groups)) {
        stopf("Grouping is not supported for spatial resampling methods.")
      }

      if (!is.null(groups)) {
        stopf("Grouping is not supported for spatial resampling methods")
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
      pv = self$param_set$values
      # hack for autoplot
      if (!is.null(names(self$instance))) {
        as.integer(pv$repeats) * as.integer(length(self$instance$train))
      } else {
        as.integer(pv$repeats) * as.integer(length(self$instance[[1]]$test))
      }
    }
  ),
  private = list(
    .sample = function(ids, coords, crs) {

      points = sf::st_as_sf(coords,
        coords = colnames(coords),
        crs = crs
      )

      pv = self$param_set$values
      map(seq_len(pv$repeats), function(i) {
        inds = CAST::knndm(tpoints = points,
          modeldomain = self$param_set$values$modeldomain,
          ppoints = self$param_set$values$ppoints,
          k = self$param_set$values$folds,
          maxp = self$param_set$values$maxp,
          clustering = self$param_set$values$clustering,
          linkf = self$param_set$values$linkf,
          samplesize = self$param_set$values$samplesize,
          sampling = self$param_set$values$sampling)

        list(train = inds$indx_train, test = inds$indx_test)
      })

    },
    .set_default_param_values = function(param) {
      if (is.null(self$param_set$values[[param]])) {
        self$param_set$values[[param]] = self$param_set$default[[param]]
      }
    },
    .get_train = function(i) {
      i = as.integer(i) - 1L
      folds = as.integer(self$param_set$values$folds)
      rep = i %/% folds + 1L
      fold = i %% folds + 1L
      self$instance[[rep]]$train[[fold]]
    },
    .get_test = function(i) {
      i = as.integer(i) - 1L
      folds = as.integer(self$param_set$values$folds)
      rep = i %/% folds + 1L
      fold = i %% folds + 1L
      self$instance[[rep]]$test[[fold]]
    }
  )
)

#' @include aaa.R
resamplings[["repeated_spcv_knndm"]] = ResamplingRepeatedSpCVKnndm
