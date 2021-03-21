mlr3misc::require_namespaces("GSIF", quietly = TRUE)
data(cookfarm)
set.seed(42)

cookfarm_profiles = cookfarm$profiles

cookfarm_mlr3 = cookfarm_profiles %>%
  dplyr::left_join(cookfarm$readings %>%
    dplyr::select("Date", "SOURCEID"), by = "SOURCEID") %>%
  sf::st_as_sf(coords = c("Easting", "Northing"), crs = 26911) %>%
  dplyr::mutate(x = sf::st_coordinates(.)[, "X"]) %>%
  dplyr::mutate(y = sf::st_coordinates(.)[, "Y"]) %>%
  dplyr::mutate(Date = as.character(Date)) %>%
  sf::st_set_geometry(NULL) %>%
  na.omit() %>%
  dplyr::sample_n(500)

saveRDS(cookfarm_mlr3, "inst/extdata/cookfarm.rda")

# mapview::mapview(cookfarm_mlr3)

### unique timestamps
# length(unique(cookfarm_mlr3$Date))
# = 365 (out of 500)

### unique coordinates
# st_coordinates(cookfarm_mlr3) %>%
#   as.data.frame() %>%
#   group_by(X, Y) %>%
#   unique()
# = 41
