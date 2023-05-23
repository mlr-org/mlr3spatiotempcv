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
