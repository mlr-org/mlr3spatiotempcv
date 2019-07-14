context("mlr_resampling_spcv-kmeans")

test_that("spcv has no duplicated ids", {
  r <- mlr_resamplings$get("spcv-kmeans")
  expect_identical(r$duplicated_ids, FALSE)
})

test_that("stratification", {
  task <- mlr_tasks$get("ecuador")
  r <- mlr_resamplings$get("spcv-kmeans")
  r$param_set$values <- list(folds = 5, stratify = TRUE)
  expect_error(r$inskantiate(task))
})

test_that("grouping", {
  # select resampling
  r <- mlr_resamplings$get("spcv-kmeans")

  # create custom data for grouping variable
  b <- as.data.table(readRDS(system.file("extdata", "ecuador.rda",
    package = "mlr3spatiotemporal"
  )))

  # add grouping variable
  data <- insert_named(b[1:150, ], list(grp = rep_len(letters[1:10], 150)))
  task <- TaskClassifST$new("ecuador-grp", as_data_backend(data), target = "slides",
    coordinates = c("x", "y"))
  browser()
  task$set_col_role("grp", "groups")

  # grouping only
  r$param_set$values <- list(folds = 5)
  expect_error(r$instantiate(task))

  # grouping and stratify = TRUE
  r$param_set$values <- list(folds = 5, stratify = TRUE)
  expect_error(r$instantiate(task))
})
