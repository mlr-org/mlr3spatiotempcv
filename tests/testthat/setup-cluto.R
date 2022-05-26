# install CLUTO executable into $PROJ_ROOT/inst/ for testthat to work
# system.file() is shimed in testthat and pkgload::load_all()
if (Sys.getenv("NOT_CRAN") != "") {
  if (Sys.info()[["sysname"]] == "Linux") {
    if (!file.exists(system.file("vcluster", package = "mlr3spatiotempcv"))) {
      source("https://gist.githubusercontent.com/pat-s/6430470cf817050e27d26c43c0e9be72/raw/c57cd29c7aeaad55e3f9fca850632a31f4a23280/install-cluto.R")
    }
  }
  if (Sys.info()[["sysname"]] == "Windows") {
    if (!file.exists(system.file("vcluster.exe", package = "mlr3spatiotempcv"))) {
      source("https://gist.githubusercontent.com/pat-s/6430470cf817050e27d26c43c0e9be72/raw/fee1895540f25386123bf1fb6e0aca6719c72f53/install-cluto.R")
    }
  }
}
