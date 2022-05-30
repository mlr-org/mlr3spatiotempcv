# install CLUTO executable into $PROJ_ROOT/inst/ for testthat to work
# system.file() is shimed in testthat and pkgload::load_all()
if (Sys.getenv("NOT_CRAN") != "" && Sys.getenv("R_COVR") == "") {
  gist_url = "https://gist.githubusercontent.com/pat-s/6430470cf817050e27d26c43c0e9be72/raw/9a1bee360880be5c2ab8dc5168b4ff9543e499d6/install-cluto.R"
  if (Sys.info()[["sysname"]] == "Linux") {
    if (!file.exists(system.file("vcluster", package = "mlr3spatiotempcv"))) {
      source(gist_url)
    }
  }
  if (Sys.info()[["sysname"]] == "Windows") {
    if (!file.exists(system.file("vcluster.exe", package = "mlr3spatiotempcv"))) {
      source(gist_url)
    }
  }
}
