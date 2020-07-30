#' @import mlr3
#'
#' @description Spatial Buffer Cross validation implemented by the `blockCV`
#' package.
#'
#' @note
#' However, the default settings allow to conduct a leave-one-out cross
#' validation for two-class, multi-class and continuous response data, where
#' each observation is one test set. For each test, all observations outside the
#' buffer around the test observation are included in the training set.
#'
#' The parameter `spDataType = PB` and `addBG` are designed for
#' presence-background data in species distribution modelling. If `spDataType =
#' PB`, test sets are only created for each presence observation
#' (`task\$positive`). The option `addBG = TRUE` adds the background data inside
#' the buffer to the corresponding test sets. For each test set, all
#' observations outside the buffer around the test observation are included in
#' the training set.
