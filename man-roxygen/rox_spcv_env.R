#' @description Environmental Block Cross Validation. This strategy uses k-means
#'   clustering to specify blocks of similar environmental conditions. Only
#'   numeric features can be used. The `features` used for building blocks can
#'   be specified in the `param_set`. By default, all numeric features are used.
