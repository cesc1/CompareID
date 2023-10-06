
#' Check if a tibble column has duplicates
#'
#' @param tibble
#'
#' @return Boolean: T if duplicates, F if unique.
#' @export
#'
#' @examples is_duplicate(table2, "municipi")
is_duplicate <- function(x, colname = "id") {
  x[, colname] |>
    duplicated() |>
    any()
}
