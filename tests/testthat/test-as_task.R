# classif ----------------------------------------------------------------------

test_that("as_task_classif_st.TaskClassifST works", {
  task = tsk("ecuador")
  new_task = as_task_classif_st(task)

  expect_equal(task, new_task)
})

test_that("as_task_classif_st.data.frame works", {
  data("ecuador", package = "mlr3spatiotempcv")
  new_task = as_task_classif_st(ecuador, target = "slides", positive = "TRUE",
    coords_as_features = FALSE,
    crs = "+proj=utm +zone=17 +south +datum=WGS84 +units=m +no_defs",
    coordinate_names = c("x", "y"))

  expect_class(new_task, "TaskClassifST")
})

test_that("as_task_classif_st.sf works", {
  data("ecuador", package = "mlr3spatiotempcv")
  ecuador_sf = sf::st_as_sf(ecuador, coords = c("x", "y"), crs = "epsg:32717")
  new_task = as_task_classif_st(ecuador_sf, target = "slides", positive = "TRUE")

  expect_class(new_task, "TaskClassifST")
  expect_equal(new_task$extra_args$crs, "epsg:32717")
  expect_equal(new_task$extra_args$coordinate_names, c("X", "Y"))
})

test_that("as_task_classif_st.DataBackendSf works", {
  data("ecuador", package = "mlr3spatiotempcv")
  ecuador_sf = sf::st_as_sf(ecuador, coords = c("x", "y"), crs = "epsg:32717")
  library(mlr3spatial)
  backend_sf = DataBackendSf$new(ecuador_sf)
  new_task = as_task_classif_st(backend_sf, target = "slides", positive = "TRUE")

  expect_class(new_task, "TaskClassifST")
  expect_equal(new_task$extra_args$crs, "epsg:32717")
  expect_equal(new_task$extra_args$coordinate_names, NULL)
})

# test_that("as_task_classif_st.DataBackendSpatRaster works", {
#   library(mlr3spatial)
#   stack_classif = demo_stack_spatraster(size = 1, layers = 5)
#   value = data.table(ID = c(0, 1), y = c("negative", "positive"))
#   terra::setCats(stack_classif, layer = "y", value = value)
#   colnames = c(names(stack_classif), "..row_id")
#   backend_terra = DataBackendSpatRaster$new(stack_classif)
#
#   new_task = as_task_classif_st(backend_terra, target = "y", positive = "positive")
#
#   expect_class(new_task, "TaskClassifST")
#   expect_class(new_task$extra_args$crs, "crs")
#   expect_equal(new_task$extra_args$crs$input, "epsg:32717")
# })

# regr -------------------------------------------------------------------------

test_that("as_task_regr_st.TaskRegrST works", {
  task = tsk("cookfarm")
  new_task = as_task_regr_st(task)

  expect_equal(task, new_task)
})

test_that("as_task_regr_st.data.rame works", {
  data("cookfarm_sample", package = "mlr3spatiotempcv")
  new_task = as_task_regr_st(cookfarm_sample, target = "PHIHOX",
    id = "cookfarm",
    coords_as_features = FALSE,
    crs = 26911,
    coordinate_names = c("x", "y"))

  expect_class(new_task, "TaskRegrST")
})

test_that("as_task_regr_st.sf works", {
  data("cookfarm_sample", package = "mlr3spatiotempcv")
  cookfarm_sf = sf::st_as_sf(cookfarm_sample, coords = c("x", "y"), crs = 26911)
  new_task = as_task_regr_st(cookfarm_sf, target = "PHIHOX")

  expect_class(new_task, "TaskRegrST")
  expect_equal(new_task$extra_args$crs, "EPSG:26911")
  expect_equal(new_task$extra_args$coordinate_names, c("X", "Y"))
})

# conversion to non-ST task ----------------------------------------------------

test_that("as_task_classif.TaskClassifST works", {
  task = tsk("ecuador")
  new_task = as_task_classif(task)

  expect_class(new_task, "TaskClassif")
})

test_that("as_task_regr.TaskRegrST works", {
  task = tsk("cookfarm")
  new_task = as_task_regr(task)

  expect_class(new_task, "TaskRegr")
})
