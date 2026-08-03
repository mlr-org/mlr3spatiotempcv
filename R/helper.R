# formats an sf object into a data.frame to be used with for Task creation
format_sf = function(data) {
  geometries = as.character(unique(sf::st_geometry_type(data)))
  if (!test_names(geometries, identical.to = "POINT")) {
    stop(
      "Simple feature may not contain geometries of type '%s'",
      str_collapse(setdiff(geometries, "POINT"))
    )
  }

  # extract spatial meta data
  coordinates = as.data.frame(sf::st_coordinates(data))

  # convert sf to data.frame
  data[[attr(data, "sf_column")]] = NULL
  attr(data, "sf_column") = NULL
  data = as.data.frame(data)

  # add coordinates
  data = cbind(data, coordinates)
  return(data)
}

# Name of the column which carries the task target into the 'sf' object handed
# to blockCV::cv_spatial(). Fixed (and not the target name) to rule out a clash
# with the geometry column.
blockcv_col_name = "mlr3_target"

# Extracts the task target for response-aware fold balancing in
# blockCV::cv_spatial(). Returns `NULL` when balancing on the target was not
# requested, in which case blockCV balances the total number of records.
blockcv_response = function(task, balance_on_target, presence_bg) {
  if (!isTRUE(balance_on_target)) {
    if (isTRUE(presence_bg)) {
      stopf("'presence_bg = TRUE' requires 'balance_on_target = TRUE': blockCV
        needs the task target to tell presences from background points.",
        wrap = TRUE)
    }
    return(NULL)
  }

  values = task$data(rows = task$row_ids, cols = task$target_names)[[1L]]

  if (!isTRUE(presence_bg)) {
    return(values)
  }

  # blockCV expects presence-background data as a binary numeric column with 1s
  # for presences and 0s for background points. Translate two-class factor
  # targets via the positive class of the task.
  if (is.factor(values)) {
    if (nlevels(values) != 2L) {
      stopf("'presence_bg = TRUE' requires a two-class target, but target '%s'
        has %i levels.", task$target_names, nlevels(values), wrap = TRUE)
    }
    values = as.integer(values == task$positive)
  }

  values
}
