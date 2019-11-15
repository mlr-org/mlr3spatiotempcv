context("mlr_resampling_spcv-buffer")

# init objects -----------------------------------------------------------------

task = mlr_tasks$get("ecuador")

### create custom data for grouping variable
b = as.data.table(readRDS(system.file("extdata", "ecuador.rda",
                                       package = "mlr3spatiotempcv"
)))
# add grouping variable
data = insert_named(b[1:150, ], list(grp = rep_len(letters[1:10], 150)))
task_grp = TaskClassifST$new("ecuador-grp", as_data_backend(data), target = "slides",
                              coordinates = c("x", "y"))
task_grp$col_roles$group = "grp"

# run tests --------------------------------------------------------------------

test_that("stratification throws errors", {
  spcv_rsp_buffer = mlr_resamplings$get("spcv-buffer")

  spcv_rsp_buffer$param_set$values = list(stratify = TRUE, range = 100)
  expect_error(spcv_rsp_buffer$instantiate(task))
})

test_that("grouping throws errors when 'groups' is set", {

  spcv_rsp_buffer = mlr_resamplings$get("spcv-buffer")

  expect_error(spcv_rsp_buffer$instantiate(task_grp),
               "Grouping is not supported for spatial resampling methods")
})

test_that("grouping throws errors when 'groups' and 'stratify' is set", {
  spcv_rsp_buffer = mlr_resamplings$get("spcv-buffer")

    expect_error(spcv_rsp_buffer$instantiate(task_grp),
                 "Grouping is not supported for spatial resampling methods")
})

test_that("train and test set getter functions are working", {
  spcv_rsp_buffer = mlr_resamplings$get("spcv-buffer")

  spcv_rsp_buffer$instantiate(task)
  expect_silent(spcv_rsp_buffer$train_set(1))
  expect_silent(spcv_rsp_buffer$test_set(1))
})

test_that("cloning works", {
  spcv_rsp = mlr_resamplings$mget(
    as.data.table(mlr_resamplings)[map_lgl(key, grepl, pattern = "spcv"), key]
  )

  for (i in spcv_rsp) {
    clone = i$clone(deep = TRUE)
    expect_true(all.equal(i, clone))
  }
})
