#' @importFrom mlr3 as_data_backend
#' @rdname as_data_backend
#' @export as_data_backend.sf
#' @exportS3Method
as_data_backend.sf = function(data, primary_key = NULL, ...) {
  data = format_sf(data)

  if (is.character(primary_key)) {
    assert_string(primary_key)
    assert_choice(primary_key, colnames(data))
    assert_integer(data[[primary_key]], any.missing = FALSE, unique = TRUE)
  } else {
    if (is.null(primary_key)) {
      row_ids = seq_row(data)
      compact_seq = TRUE
    } else if (is.integer(primary_key)) {
      row_ids = assert_integer(primary_key, len = nrow(data), any.missing = FALSE, unique = TRUE)
    } else {
      stopf("Argument 'primary_key' must be NULL, a column name or a vector of ids")
    }

    primary_key = "..row_id"
    data = insert_named(data, list("..row_id" = row_ids))
  }

  compact_seq = FALSE

  b = DataBackendDataTable$new(data, primary_key)
  b$compact_seq = compact_seq

  return(b)
}
