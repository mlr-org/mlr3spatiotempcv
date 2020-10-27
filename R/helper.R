check_cluto_path = function() {
  if (!Sys.getenv("CLUTO_PATH") == "") {
    vcluster_loc = normalizePath(Sys.getenv("CLUTO_PATH"))
  } else {
    messagef("CLUTO executable path not set via env var CLUTO_PATH.
      Trying to find it in mlr3spatiotempcv.", wrap = TRUE)

    vcluster_loc = switch(Sys.info()[["sysname"]],
      "Windows" = {
        if (!file.exists(system.file("vcluster.exe", # nocov start
          package = "mlr3spatiotempcv"))) {
          stopf("vcluster.exe not found. Please install CLUTO first.
            See ?ResamplingSptCVCluto for instructions.", wrap = TRUE)
        }
        normalizePath(system.file("vcluster.exe",
          package = "mlr3spatiotempcv"))
      }, # nocov end
      "Linux" = {
        if (!file.exists(system.file("vcluster",
          package = "mlr3spatiotempcv"))) {
          stopf("vcluster.exe not found. Please install CLUTO first.
            See ?ResamplingSptCVCluto for instructions.", wrap = TRUE)
        }
        # nocov start
        system.file("vcluster",
          package = "mlr3spatiotempcv")
        # nocov end
      },
      "Darwin" = stopf("macOS is not supported by method 'CLUTO'.")
    )
  }
  return(vcluster_loc)
}
