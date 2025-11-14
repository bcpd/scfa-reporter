#' Normalize SCFA measurements
#'
#' @param data Tibble that contains SCFA measurements along with metadata.
#' @param analytes Character vector with the SCFA column names that should be
#'   normalized.
#' @param basis Character string that documents the normalization strategy.
#' @param multiplier Numeric scalar applied to every analyte (defaults to `1`).
#' @param transform_fn Optional function that receives the numeric analyte vector
#'   and returns the normalized values. When supplied it takes precedence over
#'   `multiplier`.
#' @param fill Value that replaces `NA` entries after normalization.
#'
#' @return A tibble with normalized analyte columns and the same metadata as the
#'   input `data`. The returned tibble carries `basis` and `multiplier`
#'   attributes for downstream reporting.
#' @export
#'
#' @examples
#' \dontrun{
#' normalized <- normalize_scfa(merged, analytes = default_analytes, multiplier = 7)
#' attr(normalized, "basis")
#' }
normalize_scfa <- function(data,
                           analytes,
                           basis = c("as_is", "mmol_per_L_plasma", "mg_per_kg"),
                           multiplier = 1,
                           transform_fn = NULL,
                           fill = 0) {
  basis <- match.arg(basis)
  stopifnot(is.numeric(multiplier), length(multiplier) == 1)
  if (!all(analytes %in% names(data))) {
    missing <- analytes[!analytes %in% names(data)]
    stop("The following analytes were not found: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  res <- data
  res <- dplyr::mutate(
    res,
    dplyr::across(
      dplyr::all_of(analytes),
      ~ {
        values <- suppressWarnings(as.numeric(.x))
        if (is.function(transform_fn)) {
          values <- transform_fn(values)
        } else {
          values <- values * multiplier
        }
        if (!is.null(fill)) {
          values[is.na(values)] <- fill
        }
        values
      }
    )
  )
  attr(res, "basis") <- basis
  attr(res, "multiplier") <- multiplier
  res
}
