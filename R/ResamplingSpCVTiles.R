#' @title (sperrorest) Spatial "Tiles" resampling
#'
#' @template rox_spcv_tiles
#'
#' @references
#' `r format_bib("brenning2012")`
#'
#' @export
#' @examples
#' if (mlr3misc::require_namespaces("sperrorest", quietly = TRUE)) {
#'   library(mlr3)
#'   task = tsk("ecuador")
#'
#'   # Instantiate Resampling
#'   rcv = rsmp("spcv_tiles", nsplit = c(4L, 3L), reassign = FALSE)
#'   rcv$instantiate(task)
#'
#'   # Individual sets:
#'   rcv$train_set(1)
#'   rcv$test_set(1)
#'   # check that no obs are in both sets
#'   intersect(rcv$train_set(1), rcv$test_set(1)) # good!
#'
#'   # Internal storage:
#'   rcv$instance # table
#' }
ResamplingSpCVTiles = R6Class("ResamplingSpCVTiles",
  inherit = mlr3::Resampling,
  public = list(
    #' @description
    #' Create a "Spatial 'Tiles' resampling" resampling instance.
    #' @param id `character(1)`\cr
    #'   Identifier for the resampling strategy.
    #' For a list of available arguments, please see
    #' [sperrorest::partition_tiles].
    initialize = function(id = "spcv_Tiles") {
      ps = ParamSet$new(params = list(
        ParamUty$new("dsplit", custom_check = function(x) checkmate::assert_integer(x, len = 2)),
        ParamUty$new("nsplit", custom_check = function(x) checkmate::assert_integer(x, len = 2)),
        ParamFct$new("rotation", levels = c("none", "random", "user"), default = "none"),
        ParamUty$new("user_rotation", default = NULL),
        ParamFct$new("offset", levels = c("none", "random", "user"), default = "none"),
        ParamUty$new("user_offset", default = NULL),
        ParamLgl$new("reassign", default = TRUE),
        ParamDbl$new("min_frac", default = 0.025, lower = 0, upper = 1),
        ParamInt$new("min_n", default = 5, lower = 0),
        ParamInt$new("iterate", default = 1, lower = 1)
      ))
      super$initialize(
        id = id,
        param_set = ps
      )
    },

    #' @description
    #'  Materializes fixed training and test splits for a given task.
    #' @param task [Task]\cr
    #'  A task to instantiate.
    instantiate = function(task) {

      mlr3::assert_task(task)
      checkmate::assert_multi_class(task, c("TaskClassifST", "TaskRegrST"))
      groups = task$groups

      pv = self$param_set$values

      if (!is.null(self$param_set$values$user_rotation) && !self$param_set$values$rotation == "user") {
        stopf("When setting 'user_rotation = TRUE', 'rotation' needs to be set to 'user'.")
      }
      if (!is.null(self$param_set$values$user_offset) && !self$param_set$values$offset == "user") {
        stopf("When setting 'user_offset = TRUE', 'offset' needs to be set to 'user'.")
      }
      if (is.null(pv$dsplit) && is.null(pv$nsplit)) {
        stopf("One of 'dsplit' or 'nsplit' must be set.")
      }

      # Set values to default if missing
      mlr3misc::map(
        c("rotation", "user_rotation", "offset", "user_offset", "reassign", "min_frac",
          "min_n", "iterate"),
        function(x) private$.set_default_param_values(x)
      )

      if (!is.null(groups)) {
        stopf("Grouping is not supported for spatial resampling methods")
      }

      private$.sample(task$row_ids, task$coordinates())

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
      length(self$instance$train)
    }
  ),
  private = list(
    .sample = function(ids, coords) {

      pv = self$param_set$values

      if (is.null(pv$repeats)) {
        pv$repeats = 1
      }

      if (pv$rotation == "none") {
        phi = rep(0, length(seq_len(pv$repeats)))
      } else if (pv$rotation == "random") {
        phi = stats::runif(-45, 45, n = length(seq_len(pv$repeats)))
      } else if (pv$rotation == "user") {
        if (length(pv$user_rotation) == 1) {
          pv$user_rotation = rep(pv$user_rotation, length(seq_len(pv$repeats)))
        }
        stopifnot(length(pv$user_rotation) == length(seq_len(pv$repeats)))
        phi = pv$user_rotation
      }
      names(phi) = as.character(seq_len(pv$repeats))

      coords[[1]] = coords[[1]] - mean(coords[[1]])
      coords[[2]] = coords[[2]] - mean(coords[[2]])

      if (pv$offset == "none") {
        x_shift = y_shift = rep(0, length(seq_len(pv$repeats)))
      } else if (pv$offset == "random") {
        x_shift = stats::runif(0, 1, n = length(seq_len(pv$repeats)))
        y_shift = stats::runif(0, 1, n = length(seq_len(pv$repeats)))
      } else if (pv$offset == "user") {
        if (is.vector(pv$user_offset) && length(pv$user_offset) == 2) {
          pv$user_offset = list(pv$user_offset[1], pv$user_offset[2])
        }
        stopifnot(is.list(pv$user_offset) && length(pv$user_offset) == 2)
        # Recycle offsets as needed:
        if (length(pv$user_offset[[1]]) == 1) {
          pv$user_offset[[1]] = rep(pv$user_offset[[1]], length(seq_len(pv$repeats)))
        }
        if (length(pv$user_offset[[2]]) == 1) {
          pv$user_offset[[2]] = rep(pv$user_offset[[2]], length(seq_len(pv$repeats)))
        }
        # Got enough user_offsets?
        stopifnot(length(pv$user_offset[[1]]) == length(seq_len(pv$repeats)))
        stopifnot(length(pv$user_offset[[2]]) == length(seq_len(pv$repeats)))
        # Valid range, [0,1]?

        stopifnot(min(pv$user_offset[[1]]) >= 0 & max(pv$user_offset[[1]]) <= 1)
        stopifnot(min(pv$user_offset[[2]]) >= 0 & max(pv$user_offset[[2]]) <= 1)
        x_shift = pv$user_offset[[1]]
        y_shift = pv$user_offset[[2]]
      }
      names(x_shift) = as.character(seq_len(pv$repeats))
      names(y_shift) = as.character(seq_len(pv$repeats))

      if (!is.null(pv$nsplit)) {
        if (length(pv$nsplit) == 1) {
          pv$nsplit = c(pv$nsplit, pv$nsplit)
        }
      }
      if (!is.null(pv$dsplit)) {
        if (length(pv$dsplit) == 1) {
          pv$dsplit = c(pv$dsplit, pv$dsplit)
        }
      }

      for (cnt in seq_len(pv$repeats)) {
        if (pv$rotation != "none") {
          r = phi[as.character(cnt)] * 180 / pi
          r = matrix(c(cos(r), -sin(r), sin(r), cos(r)), ncol = 2)
          xy = r %*% t(coords)
          x = xy[1, ]
          y = xy[2, ]
        } else {
          x = coords[[1]]
          y = coords[[2]]
        }

        x_range = range(x)
        y_range = range(y)

        if (!is.null(pv$nsplit)) {
          x_delta = diff(x_range) / pv$nsplit[1]
          y_delta = diff(y_range) / pv$nsplit[2]
          my_nsplit = pv$nsplit
        } else {
          # if !is.null(dsplit)
          x_delta = pv$dsplit[1]
          y_delta = pv$dsplit[2]
        }
        # Apply offsets:
        if (pv$offset != "none") {
          # Widen the range and increase nsplit to allow for 'lurking' tiles:
          x_range[2] = x_range[2] + x_delta
          y_range[2] = y_range[2] + y_delta
          x_range = x_range - x_delta * (x_shift[as.character(cnt)])
          y_range = y_range - y_delta * (y_shift[as.character(cnt)])
          if (is.null(pv$dsplit)) {
            my_nsplit = my_nsplit + 1
          }
        }

        # Calculate x and y splits:
        if (is.null(pv$dsplit)) {
          x_split = seq(x_range[1], x_range[2], length = my_nsplit[1] + 1)
          y_split = seq(y_range[1], y_range[2], length = my_nsplit[2] + 1)
        } else {
          x_split = seq(x_range[1], x_range[2] + x_delta, by = x_delta)
          y_split = seq(y_range[1], y_range[2] + y_delta, by = y_delta)
          my_nsplit = c(length(x_split) - 1, length(y_split) - 1)
        }

        # Group data into tiles, i.e. assign tile labels to samples:
        tile = rep(NA, nrow(coords))

        for (ix in 1:my_nsplit[1]) {
          # Intervals are normally open to the left, except the first one:
          if (ix == 1) {
            sel_x = (x >= x_split[ix]) & (x <= x_split[ix + 1])
          } else {
            sel_x = (x > x_split[ix]) & (x <= x_split[ix + 1])
          }
          for (iy in 1:my_nsplit[2]) {
            if (iy == 1) {
              sel_y = (y >= y_split[iy]) & (y <= y_split[iy + 1])
            } else {
              sel_y = (y > y_split[iy]) & (y <= y_split[iy + 1])
            }
            # Assign tile name to samples:
            if (any(sel_x & sel_y)) {
              tile[sel_x & sel_y] = as.character(sperrorest::as.tilename(c(ix, iy)))
            }
          }
        }
        tile = factor(tile)

        # Identify and process small tiles:
        s_tiles = sperrorest::get_small_tiles(tile, min_n = pv$min_n, min_frac = pv$min_frac)

        if (length(s_tiles) > 0) {
          # any small tiles?
          if (pv$reassign) {
            # Merge small tiles with neighbors:
            ignore = c()
            # Repeat until no small tiles are left:
            while ((length(s_tiles) > 0) & (length(levels(tile)) > 1)) {
              # Start with smallest small tile:
              nbrs = sperrorest::tile_neighbors(s_tiles[1],
                tileset = levels(tile),
                iterate = pv$iterate
              )
              if (length(nbrs) == 0) {
                ignore = c(ignore, as.character(s_tiles[1]))
              } else {
                # Merge tile with smallest neighbour to keep tile sizes balanced:
                n_tile = tapply(tile, tile, length)
                s_nbr = nbrs[which.min(n_tile[nbrs])]
                tile[tile == s_tiles[1]] = s_nbr
                tile = factor(as.character(tile))
              }
              # Update small tiles list:
              s_tiles = sperrorest::get_small_tiles(tile,
                min_n = pv$min_n, min_frac = pv$min_frac,
                ignore = pv$ignore
              )
            }
          } else {
            # Just eliminate small tiles:
            tile[tile %in% s_tiles] = NA
            if (all(is.na(tile))) {
              stopf("No observations left in folds - 'min_n' too high?")
            }
            tile = factor(as.character(tile))
          }
        }
      }
      tile = sperrorest::as.resampling(tile)

      class(tile) == "list"
      train_inds = lapply(tile, function(x) x$train)
      test_inds = lapply(tile, function(x) x$test)

      names(train_inds) = 1:length(train_inds)
      names(test_inds) = 1:length(test_inds)

      self$instance = list(train = train_inds, test = test_inds)

      return(invisible(self))
    },
    .set_default_param_values = function(param) {
      if (is.null(self$param_set$values[[param]])) {
        self$param_set$values[[param]] = self$param_set$default[[param]]
      }
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
