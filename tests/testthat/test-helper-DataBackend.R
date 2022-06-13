test_that("`assert_spatial_task()` works", {

  skip_if_not_installed("mlr3spatial")
  skip_if_not_installed("sf")

  sf = sf::st_as_sf(ecuador, coords = c("x", "y"),
    crs = "+proj=utm +zone=17 +south +datum=WGS84 +units=m +no_defs")
  task_vec = as_task_classif_st(sf, target = "slides")

  assert_spatial_task(task_vec)
})
