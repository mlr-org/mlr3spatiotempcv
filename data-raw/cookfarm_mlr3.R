## code to prepare `cookfarm_sample` dataset goes here

# retrieved from package https://github.com/envirometrix/landmap after GSIF was archived on CRAN in 2021-03
set.seed(42)

cookfarm = readRDS("data-raw/cookfarm.rds")
cookfarm_profiles = cookfarm$profiles

cookfarm_mlr3_sf = cookfarm_profiles %>%
  dplyr::left_join(cookfarm$readings) %>%
  # dplyr::select("Date", "SOURCEID", by = "SOURCEID") %>%
  sf::st_as_sf(coords = c("Easting", "Northing"), crs = 26911) %>%
  dplyr::mutate(x = sf::st_coordinates(.)[, "X"]) %>%
  dplyr::mutate(y = sf::st_coordinates(.)[, "Y"]) %>%
  dplyr::mutate(Date = as.character(Date))

cookfarm_mlr3 = cookfarm_mlr3_sf %>%
  sf::st_set_geometry(NULL) # %>%
# stats::na.omit()

# mapview::mapview(cookfarm_mlr3)

usethis::use_data(cookfarm_mlr3, overwrite = TRUE)
