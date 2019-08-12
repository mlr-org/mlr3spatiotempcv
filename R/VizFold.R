#' @title Plot for Spatial Resampling
#'
#' @description
#' Generates plots for [mlr3spatiotemporal::ResamplingSpCVBlock].
#'
#' @param object ([mlr3spatiotemporal::ResamplingSpCVBlock]).
#'
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3)
#' task <- mlr3::mlr_tasks$get("ecuador")
#' resampling <- ResamplingSpCVBlock$new()
#' resampling$param_set$values <- list(folds = 4)
#' resampling$instantiate(task)
#' plot(resampling, task)
plot.ResamplingSpCVBlock = function(object, task) {
  coords = task$coordinates()
  coords$row_id = task$row_ids

  coords_resamp = merge(coords, object$instance, by = "row_id")

  sf_df = sf::st_as_sf(coords_resamp, coords = c("x", "y"), crs=task$crs)
  sf_df$fold = as.factor(as.character(sf_df$fold))

  ggplot2::ggplot() +
    ggplot2::geom_sf(data = sf_df, ggplot2::aes(color = fold)) +
    ggplot2::scale_color_viridis_d()
}

#' @title Plot for Spatial Resampling
#'
#' @description
#' Generates plots for [mlr3spatiotemporal::ResamplingSpCVBuffer].
#'
#' @param object ([mlr3spatiotemporal::ResamplingSpCVBuffer]).
#'
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3)
#' task <- mlr3::mlr_tasks$get("ecuador")
#' resampling <- ResamplingSpCVBuffer$new()
#' resampling$param_set$values <- list(range = 1000)
#' resampling$instantiate(task)
#' plot(resampling, task, 1)
plot.ResamplingSpCVBuffer = function(object, task, fold) {
  coords = task$coordinates()
  coords$row_id = task$row_ids

  coords_train = coords[row_id %in% object$instance[[fold]]$train]
  coords_test = coords[row_id %in% object$instance[[fold]]$test]

  coords_train$indicator = "train"
  coords_test$indicator = "test"

  coords_df = rbind(coords_train, coords_test)

  sf_df = sf::st_as_sf(coords_df, coords = c("x", "y"))

  ggplot2::ggplot() +
    ggplot2::geom_sf(data = sf_df, ggplot2::aes(color = indicator)) +
    ggplot2::scale_color_viridis_d()
}

#' @title Plot for Spatial Resampling
#'
#' @description
#' Generates plots for [mlr3spatiotemporal::ResamplingSpCVEnv].
#'
#' @param object ([mlr3spatiotemporal::ResamplingSpCVEnv]).
#'
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3)
#' task <- mlr3::mlr_tasks$get("ecuador")
#' resampling <- ResamplingSpCVEnv$new()
#' resampling$param_set$values <- list(folds = 4, features=c("dem"))
#' resampling$instantiate(task)
#' plot(resampling, task)
plot.ResamplingSpCVEnv = function(object, task) {
  coords = task$coordinates()
  coords$row_id = task$row_ids

  coords_resamp = merge(coords, object$instance, by = "row_id")

  sf_df = sf::st_as_sf(coords_resamp, coords = c("x", "y"), crs=task$crs)
  sf_df$fold = as.factor(as.character(sf_df$fold))

  ggplot2::ggplot() +
    ggplot2::geom_sf(data = sf_df, ggplot2::aes(color = fold)) +
    ggplot2::scale_color_viridis_d()
}

#' @title Plot for Spatial Resampling
#'
#' @description
#' Generates plots for [mlr3spatiotemporal::ResamplingSpCVKmeans].
#'
#' @param object ([mlr3spatiotemporal::ResamplingSpCVKmeans]).
#'
#' @return [ggplot2::ggplot()] object.
#' @export
#' @examples
#' library(mlr3)
#' task <- mlr3::mlr_tasks$get("ecuador")
#' resampling <- ResamplingSpCVKmeans$new()
#' resampling$param_set$values <- list(folds = 4)
#' resampling$instantiate(task)
#' plot(resampling, task)
plot.ResamplingSpCVKmeans= function(object, task) {
  coords = task$coordinates()
  coords$row_id = task$row_ids

  coords_resamp = merge(coords, object$instance, by = "row_id")

  sf_df = sf::st_as_sf(coords_resamp, coords = c("x", "y"), crs=task$crs)
  sf_df$fold = as.factor(as.character(sf_df$fold))

  ggplot2::ggplot() +
    ggplot2::geom_sf(data = sf_df, ggplot2::aes(color = fold)) +
    ggplot2::scale_color_viridis_d()
}
