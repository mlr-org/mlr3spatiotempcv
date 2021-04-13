## code to prepare `cookfarm_sample` dataset goes here

# mlr3misc::require_namespaces("GSIF", quietly = TRUE)
# library(GSIF)
# moved to package https://github.com/envirometrix/landmap after GSIF was archived on CRAN in 2021-03
data(cookfarm)
# saveRDS(cookfarm, "R/sysdata.rda", version = 2)
# cookfarm = readRDS("R/sysdata.rda")
set.seed(42)

cookfarm_profiles = cookfarm$profiles

cookfarm_mlr3_sf = cookfarm_profiles %>%
  dplyr::left_join(cookfarm$readings) %>%
  # dplyr::select("Date", "SOURCEID", by = "SOURCEID") %>%
  sf::st_as_sf(coords = c("Easting", "Northing"), crs = 26911) %>%
  dplyr::mutate(x = sf::st_coordinates(.)[, "X"]) %>%
  dplyr::mutate(y = sf::st_coordinates(.)[, "Y"]) %>%
  dplyr::mutate(Date = as.character(Date))

cookfarm_sample = cookfarm_mlr3_sf %>%
  sf::st_set_geometry(NULL) %>%
  stats::na.omit() %>%
  dplyr::sample_n(500)

# mapview::mapview(cookfarm_sample)

### unique timestamps
# length(unique(cookfarm_mlr3_sf$Date))
# = 365 (out of 500)

### unique coordinates
# sf::st_coordinates(cookfarm_mlr3_sf) %>%
#   as.data.frame() %>%
#   dplyr::group_by(X, Y) %>%
#   unique()
# = 44

usethis::use_data(cookfarm_sample, overwrite = TRUE)
