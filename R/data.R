#' Basic testing table1
#'
#' A small table, used on test and the Basic vignette.
#' It will be used on the left part of the left join
#'
#' @format ## `table1`
#' A data frame with 9 rows and 2 columns:
#'
#' \describe{
#'   \item{municipi}{The city name, or the id that we want to match}
#'   \item{info}{Some random info, in this case the row position}
#' }
"table1"


#' Basic testing table2
#'
#' A small table, used on test and the Basic vignette.
#' It will be used on the right part of the left join. It has duplicate values.
#'
#' @format ## `table1`
#' A data frame with 13 rows and 2 columns:
#'
#' \describe{
#'   \item{municipi}{The city name, or the id that we want to match}
#'   \item{info}{Some random info, in this case the row position}
#' }
"table2"


#' Complex example, table_complex1
#'
#' A table with some municipalities of Catalonia
#' It will be used on the left part of the left join.
#'
#' @format ## `table_complex1`
#' A data frame with 928 rows and 2 columns:
#'
#' \describe{
#'   \item{municipi}{The city name, or the id that we want to match}
#'   \item{info}{Some random info, in this case a number}
#' }
"table_complex1"


#' Complex example, table_complex2
#'
#' A table with all the municipalities of Catalonia, and with some more
#' geografic information
#' It will be used on the right part of the left join, so we can add information
#' to the base dataset. It has some duplicates.
#'
#' @format ## `table_complex2`
#' A data frame with 956 rows and 5 columns:
#'
#' \describe{
#'   \item{municipi}{The municipality name, or the id that we want to match}
#'   \item{provincia}{The province of the municipality}
#'   \item{comarques}{Region name}
#'   \item{areaPolicial}{Smaller police area}
#'   \item{regioPolicial}{Larger police area}
#' }
#' @source <https://www.idescat.cat/codis/?id=50&n=9>
"table_complex2"






