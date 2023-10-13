

#' Select which column pick as id
#'
#' @param table1 A dataframe
#' @param table2 A dataframe
#' @param id The column id to select. Must be a vector of length 1 or 2
#' @param names The names of the datasets. Must be a vector of length 2. If
#'        NULL, the names are "table1" and "table2"
#'
#' @return A list with the 2 columns, in vector format
#' @export
#'
#' @examples
select_id <- function(table1, table2, id = "id", table_names = NULL) {
  # Check length id
  if(length_id(id) == 1) { # will stop if not 1 or 2
    id[2] <- id
  }

  # Create list
  result <- list(
    table1 = dplyr::select(table1, all_of(id[1])),
    table2 = dplyr::select(table2, all_of(id[2]))
  )

  # Rename feature
  if(!is.null(table_names)) {
    names(result) <- table_names
  }

  return(result)
}


#' Utils check for select id
#'
#' @param id The column id to select. Must be a char, factor, num or int
#' of length 1 or 2 (from the two tables).
#'
#' @return Length of the id (1, 2 or error)
#' @export
#'
#' @examples
length_id <- function(id) {
  # Check type
  if(!(is.numeric(id) ||
       is.integer(id) ||
       is.factor(id)  ||
       is.character(id)
    )) {
    stop("wrong type id")
  }

  # Check length
  result <- switch(length(id),
         "1" = 1,
         "2" = 2,
         stop("wrong length id")
  )

  return(result)
}
