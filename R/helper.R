check_cluto_path = function() {
  if (!Sys.getenv("CLUTO_PATH") == "") {
    vcluster_loc = normalizePath(Sys.getenv("CLUTO_PATH"))
  } else {
    messagef("CLUTO executable path not set via env var CLUTO_PATH.
      Trying to find it in mlr3spatiotempcv.", wrap = TRUE)

    vcluster_loc = switch(Sys.info()[["sysname"]],
      "Windows" = {
        if (!file.exists(system.file("vcluster.exe", # nocov start
          package = "mlr3spatiotempcv"
        ))) {
          stopf("vcluster.exe not found. Please install CLUTO first.
            See ?ResamplingSptCVCluto for instructions.", wrap = TRUE)
        }
        normalizePath(system.file("vcluster.exe",
          package = "mlr3spatiotempcv"
        ))
      }, # nocov end
      "Linux" = {
        if (Sys.info()[["machine"]] != "x86_64") {
          stopf("CLUTO on Linux only works on x86_64 architecture.")
        }
        if (!file.exists(system.file("vcluster",
          package = "mlr3spatiotempcv"
        ))) {
          stopf("vcluster executable not found. Please install CLUTO first.
            See ?ResamplingSptCVCluto for instructions.", wrap = TRUE)
        }
        # nocov start
        system.file("vcluster",
          package = "mlr3spatiotempcv"
        )
        # nocov end
      },
      "Darwin" = stopf("macOS is not supported by method 'CLUTO'.")
    )
  }
  return(vcluster_loc)
}

catn = function(..., file = "") {
  cat(paste0(..., collapse = "\n"), "\n", sep = "", file = file)
}

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
