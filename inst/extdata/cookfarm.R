library(dplyr)
library(sf)
library(GSIF)
data(cookfarm)
set.seed(42)

cookfarm_profiles = cookfarm$profiles

cookfarm_mlr3 = cookfarm_profiles %>%
  left_join(cookfarm$readings %>%
    select("Date", "SOURCEID"), by = "SOURCEID") %>%
  st_as_sf(coords = c("Easting", "Northing"), crs = 26911) %>%
  mutate(x = st_coordinates(.)[, "X"]) %>%
  mutate(y = st_coordinates(.)[, "Y"]) %>%
  mutate(Date = as.character(Date)) %>%
  st_set_geometry(NULL) %>%
  na.omit() %>%
  sample_n(500)

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
