# classif ----------------------------------------------------------------------

test_that("as_task_classif_st.TaskClassifST works", {
  task = tsk("ecuador")
  new_task = as_task_classif_st(task)

  expect_equal(task, new_task)
})

test_that("as_task_classif_st.data.rame works", {
  task = tsk("ecuador")
  data("ecuador", package = "mlr3spatiotempcv")
  new_task = as_task_classif_st(ecuador, target = "slides", positive = "TRUE",
    coords_as_features = FALSE,
    crs = "+proj=utm +zone=17 +south +datum=WGS84 +units=m +no_defs",
    coordinate_names = c("x", "y"))

  expect_equal(task, new_task)
})

test_that("as_task_classif_st.sf works", {
  data("ecuador", package = "mlr3spatiotempcv")
  ecuador_sf = sf::st_as_sf(ecuador, coords = c("x", "y"), crs = 4326)
  new_task = as_task_classif_st(ecuador_sf, target = "slides", positive = "TRUE")

  # ST tasks from sf objects will have different fields than the example task
  # hence we do not test against the example task
  expect_equal(new_task$extra_args$crs, "EPSG:4326")
  expect_equal(new_task$extra_args$coordinate_names, c("X", "Y"))
})

# regr -------------------------------------------------------------------------

test_that("as_task_regr_st.TaskClassifST works", {
  task = tsk("cookfarm")
  new_task = as_task_regr_st(task)

  expect_equal(task, new_task)
})

test_that("as_task_regr_st.data.rame works", {
  task = tsk("cookfarm")
  data("cookfarm_sample", package = "mlr3spatiotempcv")
  new_task = as_task_regr_st(cookfarm_sample, target = "PHIHOX",
    id = "cookfarm",
    coords_as_features = FALSE,
    crs = 26911,
    coordinate_names = c("x", "y"))

  expect_equal(task, new_task)
})

test_that("as_task_regr_st.sf works", {
  data("cookfarm_sample", package = "mlr3spatiotempcv")
  cookfarm_sf = sf::st_as_sf(cookfarm_sample, coords = c("x", "y"), crs = 26911)
  new_task = as_task_regr_st(cookfarm_sf, target = "PHIHOX")

  # ST tasks from sf objects will have different fields than the example task
  # hence we do not test against the example task
  expect_equal(new_task$extra_args$crs, "EPSG:26911")
  expect_equal(new_task$extra_args$coordinate_names, c("X", "Y"))
})
