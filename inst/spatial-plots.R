library(sf)
library(ggplot2)

set.seed(123)

# kmeans and block

task = mlr_tasks$get("ecuador")
resampling_cv = rsmp("spcv-block", folds = 4)
resampling_cv$instantiate(task)

coords = task$coordinates()
coords$row_id = task$row_ids

coords_resamp = merge(coords, resampling_cv$instance, by = "row_id")

sf_df = st_as_sf(coords_resamp, coords = c("x", "y"))
sf_df$fold = as.factor(as.character(sf_df$fold))

ggplot() +
  geom_sf(data = sf_df, aes(color = fold)) +
  scale_color_viridis_d()


# buffer

task = mlr_tasks$get("ecuador")
resampling_cv = rsmp("spcv-buffer", range = 3000)
resampling_cv$instantiate(task)

coords = task$coordinates()
coords$row_id = task$row_ids

# Select set
i = 1

coords_train = coords[row_id %in% resampling_cv$instance[[i]]$train]
coords_test = coords[row_id %in% resampling_cv$instance[[i]]$test]

coords_train$indicator = "train"
coords_test$indicator = "test"

coords_df = rbind(coords_train, coords_test)

sf_df = st_as_sf(coords_df, coords = c("x", "y"))

ggplot() +
  geom_sf(data = sf_df, aes(color = indicator)) +
  scale_color_viridis_d()
