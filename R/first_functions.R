# General unorganized functions to start development asap


#' Load data
#'
#' @param path A character vector with one element. Path to the file.
#' @param format Read rio::import()
#' @param setclass Read rio::import()
#' @param which Read rio::import()
#' @param ... Read rio::import()
#'
#' @return A data.frame
#' @export
#'
#' @examples
#' path = paste0("https://www.stats.govt.nz/assets/Uploads/Annual-enterprise-survey",
#'               "/Annual-enterprise-survey-2021-financial-year-provisional",
#'               "/Download-data/annual-enterprise-survey-2021-financial-year-provisional-csv.csv")
#' load_data(path)
load_data <- function(path, format, setclass = getOption("rio.import.class", "data.frame"), which, ...) {
  rio::import(path, format, setclass, which, ...)
}
