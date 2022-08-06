#' @param coords_as_features (`logical(1)`)\cr
#'  If `TRUE`, coordinates are used as features.
#'  This is a shortcut for
#'  `task$set_col_roles(c("x", "y"), role = "feature")` with the assumption
#'  that the coordinates in the data are named `"x"` and `"y"`.
