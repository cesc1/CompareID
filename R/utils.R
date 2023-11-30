
#' Check if a "CompareID" object has duplicates
#'
#' @importFrom dplyr mutate filter count
#' @param objecte CompareID
#' @param view Boolean. If we want to view witch values are duplicates
#'
#' @return Boolean vector. Tells if id1 or id2 has duplicates
#' @export
#'
#' @examples is_duplicate(CompareID$new(table1, table2, "municipi"))
#'
is_duplicate <- function(objecte, view = FALSE) {
  # Type check
  stopifnot(inherits(objecte, "CompareID"))

  dup1 <- objecte$id1 |>
    dplyr::count(id) |>
    dplyr::filter(n > 1)
  dup2 <- objecte$id2 |>
    dplyr::count(id) |>
    dplyr::filter(n > 1)

  result <- c("id1" = nrow(dup1) > 0,
              "id2" = nrow(dup2) > 0)

  if (view) {
    return(
      list(id1 = dup1,
           id2 = dup2)[result])
  }

  return(result)
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
