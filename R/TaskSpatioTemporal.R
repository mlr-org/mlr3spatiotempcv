library(mlr3)
library(checkmate)

# let mlr3 know about the new task type and the new column role
ee <- mlr_reflections
ee$task_types <- union(ee$task_types, "spatiotemporal")
ee$task_col_roles <- union(ee$task_col_roles, "coordinate")


TaskSpatioTemporal <- R6::R6Class("TaskSpatioTemporal",
  inherit = TaskSupervised,
  public = list(
    task_type = "spatiotemporal",

    initialize = function(id, backend, target, coordinates) {
      super$initialize(id = id, backend = backend, target = target)

      # check coordinates
      assert_names(self$backend$colnames, must.include = coordinates)
      for (coord in coordinates) {
        assert_numeric(self$data(cols = coord)[[1L]], any.missing = FALSE)
      }

      # mark columns as coordinate
      # set exclusive to TRUE to remove from features
      self$set_col_role(coordinates, "coordinate", exclusive = FALSE)

      self$measures <- list(mlr_measures$get("mse")) # default measure
    },

    truth = function(row_ids = NULL) {
      self$data(row_ids, cols = self$target_names)[[1L]]
    },

    coordinates = function(row_ids = NULL) {
      if (is.null(row_ids)) {
        row_ids <- self$row_ids
      }
      self$backend$data(rows = row_ids, cols = self$coordinate_names)
    }
  ),

  active = list(
    coordinate_names = function() {
      self$col_roles$coordinate
    }
  )
)

if (FALSE) {

  n <- 30
  data <- data.table::data.table(
    x = runif(n), y = runif(n), z = runif(n),
    a = sample(letters[1:3], n, replace = TRUE)
  )

  b <- as_data_backend(data)
  task <- TaskSpatioTemporal$new("example", backend = b, target = "z", coordinates = c("x", "y"))
  task$task_type
  task$target_names
  task$coordinate_names
  task$feature_names
  task$formula
  task$truth()
  task$coordinates()
  task

}
