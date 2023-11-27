
#' Check if a tibble has duplicates
#'
#' @param taula Tibble with 1 column, or multiple columns with one named "n"
#'
#' @return Boolean: T if duplicates, F if unique.
#' @export
#'
#' @examples is_duplicate(table2, "municipi")
#'
is_duplicate <- function(taula) {
  # Type check
  stopifnot(checkvar(taula, "tibble"))

    # Id table
  if (ncol(taula) == 1) {
    result <- taula[[1]] |>
      duplicated() |>
      any()
    return(result)

  } # Counts table
  else if ("n" %in% names(taula)) {
    result <- taula |>
      dplyr::filter(n > 1) |>
      nrow() > 0
    return(result)
  }

  stop("Table format not correct")
}


#' Create stats tables, adding calculated columns of counts (n) and comparison (match)
#'
#' @param ids List of 2 tables, created using create_id()
#'
#' @return List of 2 tables, but with added columns (counts and matches)
#' @export
#'
#' @examples add_stats(ids)
#'
add_stats <- function(ids) {

  if(!check_var(ids, "list", len = 2)) stop("Need a list of 2 id tables")

  result <- lapply(1:2, function(taula_id) { # Pass the index of the table
    # Identify the index of the other table
    taula_other_id = ifelse(taula_id == 1, 2, 1)

    ids[[taula_id]] |>
      dplyr::count(id) |>
      dplyr::mutate(dub = n > 1) |>
      dplyr::mutate(match = id %in% ids[[taula_other_id]]$id)
  })
  names(result) <- names(ids)

  return(result)
}
