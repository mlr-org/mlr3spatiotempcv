context("TaskClassifST")

test_that("coordinates can be used as features", {

  b = as_data_backend(readRDS(system.file("extdata", "ecuador.rda",
                                          package = "mlr3spatiotemporal")))
  b$hash = "_mlr3_tasks_ecuador_"
  task = TaskClassifST$new("test", b, target = "slides", positive = "TRUE",
                           coordinates = c("x", "y"), coords_as_features = TRUE,
                           crs = "+proj=utm +zone=17 +south +datum=WGS84 +units=m +no_defs")

  expect_names(c("x", "y"), subset.of = task$feature_names)
}
)

test_that("printing works", {
  task = mlr_tasks$get("ecuador")
  expect_output(print(task))

  expect_data_table(task$coordinates())
})
