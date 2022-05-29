#' @description
#' Spatial partitioning using rectangular tiles.
#' Small partitions can optionally be merged into adjacent ones to avoid
#' partitions with too few observations.
#' This method is similar to `ResamplingSpCVBlock` by making use of
#' rectangular zones in the coordinate space.
#' See the upstream implementation at `sperrorest::partition_disc()` and
#' Brenning (2012) for further information.
#'
#' @section Parameters:
#' * `dsplit` (`integer(2)`)\cr
#'   Equidistance of splits in (possibly rotated) x direction (`dsplit[1]``) and y direction (`dsplit[2]``) used to define tiles.
#'   If dsplit is of length 1, its value is recycled.
#'   Either `dsplit` or `nsplit` must be specified.
#' * `nsplit` (`integer(2)`)\cr
#'   Number of splits in (possibly rotated) x direction (`nsplit[1]`) and y direction (`nsplit[2]`) used to define tiles.
#'   If `nsplit` is of length 1, its value is recycled.
#' * `rotation` (`character(1)`)\cr
#'   Whether and how the rectangular grid should be rotated; random rotation is only possible between -45 and +45 degrees.
#'   Accepted values: One of `c("none", "random", "user")`.
#' * `user_rotation` (`character(1)`)\cr
#'   Only used when `rotation = "user"`.
#'   Angle(s) (in degrees) by which the rectangular grid is to be rotated in
#'   each repetition.
#'   Either a vector of same length as `repeats`, or a single number that
#'   will be replicated `length(repeats)` times.
#' * `offset` (`logical(1)`)\cr
#'   Whether and how the rectangular grid should be shifted by an offset.
#'   Accepted values: One of `c("none", "random", "user")`.
#' * `user_offset` (`logical(1)`)\cr
#'   Only used when `offset = "user"`.
#'   A list (or vector) of two components specifying a shift of the rectangular
#'   grid in (possibly rotated) x and y direction.
#'   The offset values are relative values, a value of 0.5 resulting in a
#'   one-half tile shift towards the left, or upward.
#'   If this is a list, its first (second) component refers to the rotated
#'   x (y) direction, and both components must have same length as `repeats`
#'   (or length 1).
#'   If a vector of length 2 (or list components have length 1), the two values
#'    will be interpreted as relative shifts in (rotated) x and y direction,
#'   respectively, and will therefore be recycled as needed (`length(repeats)`
#'   times each).
#' * `reassign` (`logical(1)`)\cr
#'   If `TRUE`, 'small' tiles (as per `min_frac` and `min_n`) are merged with
#'   (smallest) adjacent tiles.
#'   If `FALSE`, small tiles are 'eliminated', i.e., set to `NA.`
#' * `min_frac` (`numeric(1)`)\cr
#'   Value must be >=0, <1.
#'   Minimum relative size of partition as percentage of sample.
#' * `min_n` (`integer(1)`)\cr
#'   Minimum number of samples per partition.
#' * `iterate` (`integer(1)`)\cr
#'   Passed down to [sperrorest::tile_neighbors()].
#'
#' @seealso ResamplingSpCVBlock
