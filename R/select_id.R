

#' Select which column pick as id
#'
#' @param table1 A data.frame
#' @param table2 A data.frame
#' @param id The column id to select. Must be a vector of length 1 or 2
#'
#' @return A list with the 2 columns, in vector format
#' @export
#'
#' @examples
select_id <- function(table1, table2, id) {
  len <- length_id(id) # will stop if not 1 or 2

  if(len == 1) {
    id[2] <- id
  }

  result <- list(
    table1 = table1[id[1]],
    table2 = table2[id[2]]
  )

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
