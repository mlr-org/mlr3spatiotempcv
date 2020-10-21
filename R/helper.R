check_cluto_path = function() {
  if (!Sys.getenv("CLUTO_PATH") == "") {
    vcluster_loc = normalizePath(Sys.getenv("CLUTO_PATH"))
  } else {
    cli::cli_alert_info("CLUTO executable path not set via env var
          {.var CLUTO_PATH}.
          Trying to find it in {.pkg mlr3spatiotempcv}.", wrap = TRUE)

    vcluster_loc = switch(Sys.info()[["sysname"]],
      "Windows" = {
        if (!file.exists(system.file("vcluster.exe",
          package = "mlr3spatiotempcv"))) {
          cli::cli_alert_danger("{.file vcluster.exe} not found. Please
              install CLUTO first. See {.code ?ResamplingSptCVCluto} for
              instructions.", wrap = TRUE)
          stopf("'CLUTO' executable not found.")
        }
        normalizePath(system.file("vcluster.exe",
          package = "mlr3spatiotempcv"))
      },
      "Linux" = {
        if (!file.exists(system.file("vcluster",
          package = "mlr3spatiotempcv"))) {
          cli::cli_alert_danger("{.file vcluster} not found. Please install
              CLUTO first. See {.code ?ResamplingSptCVCluto} for
              instructions.", wrap = TRUE)
          stopf("'CLUTO' executable not found.")
        }
        system.file("vcluster",
          package = "mlr3spatiotempcv")
      },
      "Darwin" = stop("macOS is not supported by method 'CLUTO'.")
    )
  }
  return(vcluster_loc)
}
