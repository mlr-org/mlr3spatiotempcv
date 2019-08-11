library(sf)
library(ggplot2)

set.seed(123)

task = mlr_tasks$get("ecuador")
resampling_cv = mlr_resamplings$get("spcv-kmeans", param_vals = list(folds = 3))
resampling_cv$instantiate(task)

coords = task$coordinates()
coords$row_id = task$row_ids

coords_resamp = merge(coords, resampling_cv$instance, by = "row_id")

sf_df = st_as_sf(coords_resamp, coords = c("x", "y"))
sf_df$fold = as.factor(as.character(sf_df$fold))

ggplot() +
  geom_sf(data = sf_df, aes(color = fold)) +
  scale_color_viridis_d()


