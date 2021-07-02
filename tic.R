do_package_checks(error_on = "warning")

get_stage("install") %>%
  add_step(step_install_github("ropensci/tic@fledge-supp"))

if (ci_on_ghactions() && ci_has_env("BUILD_PKGDOWN")) {
  # creates pkgdown site and pushes to gh-pages branch
  # only for the runner with the "BUILD_PKGDOWN" env var set
  get_stage("install") %>%
    add_step(step_install_github("mlr-org/mlr3pkgdowntemplate"))
  do_pkgdown()
}

# write .Renviron with CLUTO path
if (Sys.info()[["sysname"]] == "Windows") {
  get_stage("before_script") %>%
    add_code_step(writeLines(sprintf("CLUTO_PATH=%s/cluto/vcluster.exe", getwd()), ".Renviron"))
} else if (Sys.info()[["sysname"]] == "Linux") {
  get_stage("before_script") %>%
    add_code_step(writeLines(sprintf("CLUTO_PATH=%s/cluto/vcluster", getwd()), ".Renviron"))
}
