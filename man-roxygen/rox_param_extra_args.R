#' @param extra_args `[named list]`\cr
#'   Additional task arguments set during construction. Required for
#'   [convert_task()].
#'   - crs `[character(1)]`\cr
#'     Coordinate reference system. Either a PROJ string or an
#'     [EPSG](https://epsg.io/) code.
#'
#'   - coords_as_features `[logical(1)]`\cr
#'     Whether the coordinates should also be used as features.
#'   - coordinate_names `[character(2)]`\cr
#'     The variables names of the coordinates in the data.
