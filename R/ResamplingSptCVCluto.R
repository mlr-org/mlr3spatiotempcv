#' @title Spatio-Temporal Cluster Resampling
#'
#' @import mlr3
#'
#' @description Spatio-temporal cluster partitioning via the `vcluster`
#'   executable of the
#'   [CLUTO](http://glaros.dtc.umn.edu/gkhome/cluto/cluto/overview) clustering
#'   application.
#'
#' This partitioning method relies on the external CLUTO library.
#' To use it, CLUTO's executables need to be downloaded and installed into
#' this package.
#'
#' See \url{https://gist.github.com/pat-s/6430470cf817050e27d26c43c0e9be72} for an
#' installation approach that should work on Windows and Linux.
#' macOS is not supported by CLUTO.
#'
#' Before using this method, please check the restrictive
#' [copyright](http://glaros.dtc.umn.edu/gkhome/cluto/cluto/download) shown
#' below.
#'
#' @details
#' By default, `-clmethod='direct'` is passed to the `vcluster` executable in
#' contrast to the upstream default `-clmethod='rb'`.
#' There is no evidence or research that this method is the best among the
#' available ones ("rb", "rbr", "direct", "agglo", "graph", "bagglo"). Also,
#' various other parameters can be set via argument `cluto_parameters` to
#' achieve different clustering results.
#'
#' Parameter `-clusterfile` is handled by \CRANpkg{skmeans} and cannot be
#' changed.
#'
#' @section Copyright:
#'
#' CLUTO's copyright is as follows:
#'
#' The CLUTO package is copyrighted by the Regents of the University of
#' Minnesota.
#' It can be freely used for educational and research purposes by non-profit
#' institutions and US government agencies only.
#' Other organizations are allowed to use CLUTO only for evaluation purposes,
#' and any further uses will require prior approval.
#' The software may not be sold or redistributed without prior approval.
#' One may make copies of the software for their use provided that the copies,
#' are not sold or distributed, are used under the same terms and conditions.
#' As unestablished research software, this code is provided on an “as is” basis
#' without warranty of any kind, either expressed or implied.
#' The downloading, or executing any part of this software constitutes an
#' implicit agreement to these terms. These terms and conditions are subject to
#' change at any time without prior notice.
#'
#' @references
#' FIXME: correct?
#' Ying Zhao and George Karypis. 11th Conference of Information and Knowledge
#' Management (CIKM), pp. 515-524, 2002.
#'
#' @export
#' @examples
#' \dontrun{
#' library(mlr3)
#' library(mlr3spatiotempcv)
#' task = tsk("cookfarm")
#'
#' # Instantiate Resampling
#' rcv = rsmp("spcv-cluto", folds = 5)
#' rcv$instantiate(task, "Date")
#'
#' # Individual sets:
#' rcv$train_set(1)
#' rcv$test_set(1)
#' # check that no obs are in both sets
#' intersect(rcv$train_set(1), rcv$test_set(1)) # good!
#'
#' # Internal storage:
#' rcv$instance # table
#' }
ResamplingSptCVCluto = R6Class("ResamplingSptCVCluto",
  inherit = mlr3::Resampling,

  public = list(
    #' @description
    #' FIXME.
    #' @param id `character(1)`\cr
    #'   Identifier for the resampling strategy.
    initialize = function(id = "spcv-cluto") {
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
    #'  A task to instantiate.
    #' @param time_var [character]\cr
    #'  The name of the variable which represents the time dimension.
    #'  Must be of type numeric.
    #' @param clmethod [character]\cr
    #'   Name of the clustering method to use within `vcluster`.
    #'   See Details for more information.
    #' @param cluto_parameters [character]\cr
    #'   Additional parameters to pass to `vcluster`.
    #'   Must be given as a single character string, e.g.
    #'   `"param1='value1'param2='value2'"`.
    #'   See the CLUTO documentation for a full list of supported parameters.
    #' @param verbose [logical]\cr
    #'   Whether to show `vcluster` progress and summary output.
    instantiate = function(task, time_var, clmethod = "direct",
      cluto_parameters = NULL, verbose = TRUE) {

      requireNamespace("skmeans", quietly = TRUE)

      assert_task(task)
      groups = task$groups

      if (!is.null(groups)) {
        stopf("Grouping is not supported for spatial resampling methods")
      }

      time = as.POSIXct(task$data()[[time_var]])
      # time in seconds since 1/1/1970
      time_num = as.numeric(time)

      data_matrix = data.matrix(data.frame(task$coordinates(), time_num))
      colnames(data_matrix) = c("x", "y", "z")

      instance = private$.sample(
        task$row_ids, data_matrix, clmethod,
        cluto_parameters, verbose)

      self$instance = instance
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
    .sample = function(ids, data_matrix, clmethod, cluto_parameters, verbose) {
      if (is.null(cluto_parameters)) {
        control_cluto = sprintf("-clmethod='%s'", clmethod)
      } else {
        control_cluto = sprintf("-clmethod='%s''%s'", clmethod, cluto_parameters)
      }

      vcluster_loc = switch(Sys.info()[["sysname"]],
        "Windows" = {
          if (!file.exists(system.file("vcluster.exe",
            package = "mlr3spatiotempcv"))) {
            cli::cli_alert_danger("{.file vcluster.exe} not found. Please
              install CLUTO first. See {.code ?ResamplingSptCVCluto} for
              instructions.", wrap = TRUE)
            stop("CLUTO executable not found.")
          }
          system.file("vcluster.exe",
            package = "mlr3spatiotempcv")
        },
        "Linux" = {
          if (!file.exists(system.file("vcluster",
            package = "mlr3spatiotempcv"))) {
            cli::cli_alert_danger("{.file vcluster} not found. Please install
              CLUTO first. See {.code ?ResamplingSptCVCluto} for
              instructions.", wrap = TRUE)
            stop("CLUTO executable not found.")
          }
          system.file("vcluster",
            package = "mlr3spatiotempcv")
        },
        "Darwin" = stop("macOS is not supported by CLUTO.")
      )
      inds = skmeans::skmeans(data_matrix,
        k = self$param_set$values$folds,
        method = "CLUTO",
        control = list(
          vcluster = vcluster_loc,
          verbose = verbose,
          control = control_cluto)
      )

      data.table(
        row_id = ids,
        fold = inds$cluster,
        key = "fold"
      )
    },

    # private get funs for train and test which are used by
    # Resampling$.get_set()
    .get_train = function(i) {
      self$instance[!list(i), "row_id", on = "fold"][[1L]]
    },

    .get_test = function(i) {
      self$instance[list(i), "row_id", on = "fold"][[1L]]
    }
  )
)
