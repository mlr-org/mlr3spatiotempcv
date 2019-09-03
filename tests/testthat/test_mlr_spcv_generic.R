context("mlr_resampling_spcv-coords")

# init objects -----------------------------------------------------------------

spcv_rsp = mlr_resamplings$mget(
  as.data.table(mlr_resamplings)[map_lgl(key, stringr::str_detect, pattern = "spcv"), key]
)
# because 'spcv-buffer' has no fold argument, we need to create an extra group
# for some tests
spcv_rsp_no_buffer = spcv_rsp
spcv_rsp_no_buffer$`spcv-buffer` = NULL

task = mlr_tasks$get("ecuador")

# create custom data for grouping variable
b <- as.data.table(readRDS(system.file("extdata", "ecuador.rda",
  package = "mlr3spatiotemporal"
)))

# add grouping variable
data <- insert_named(b[1:150, ], list(grp = rep_len(letters[1:10], 150)))
task_grp <- TaskClassifST$new("ecuador-grp", as_data_backend(data), target = "slides",
  coordinates = c("x", "y"))
task_grp$set_col_role("grp", "groups")

# run tests --------------------------------------------------------------------

test_that("spcv has no duplicated ids", {
  for (i in spcv_rsp) {
    expect_identical(i$duplicated_ids, FALSE)
  }
})

test_that("stratification throws errors", {
  for (i in spcv_rsp_no_buffer) {
    i$param_set$values <- list(folds = 5, stratify = TRUE)
    expect_error(i$instantiate(task))
  }
  spcv_rsp$`spcv-buffer`$param_set$values = list(stratify = TRUE, range = 100)
  expect_error(spcv_rsp$`spcv-buffer`$instantiate(task))
})

test_that("grouping throws errors when 'groups' is set", {
  spcv_rsp = mlr_resamplings$mget(
    as.data.table(mlr_resamplings)[map_lgl(key, stringr::str_detect, pattern = "spcv"), key]
  )
  spcv_rsp_no_buffer = spcv_rsp
  spcv_rsp_no_buffer$`spcv-buffer` = NULL

  for (i in spcv_rsp) {
    expect_error(i$instantiate(task_grp), "Grouping is not supported for spatial resampling methods")
  }
})

test_that("grouping throws errors when 'groups' and 'stratify' is set", {
  spcv_rsp = mlr_resamplings$mget(
    as.data.table(mlr_resamplings)[map_lgl(key, stringr::str_detect, pattern = "spcv"), key]
  )
  spcv_rsp_no_buffer = spcv_rsp
  spcv_rsp_no_buffer$`spcv-buffer` = NULL

  for (i in spcv_rsp_no_buffer) {
    i$param_set$values <- list(folds = 5, stratify = TRUE)
    expect_error(i$instantiate(task_grp), "Grouping is not supported for spatial resampling methods")
  }

  spcv_rsp$`spcv-buffer`$param_set$values = list(stratify = TRUE, range = 100)
  expect_error(spcv_rsp$`spcv-buffer`$instantiate(task_grp), "Grouping is not supported for spatial resampling methods")
})
