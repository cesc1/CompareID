## code to prepare the vignette data

path = file.path("data-raw", "Dades vignette.xlsx")
table1 <- rio::import(path, which = "Hoja1")
table2 <- rio::import(path, which = "Hoja2")

# Type equivalence check

check_equivalences <- dplyr::tribble(
  ~type_informal, ~check,
  "char",       function(object) {is.character(object) || is.factor(object)},
  "carac",      function(object) {is.character(object) || is.factor(object)},
  "str",        function(object) {is.character(object) || is.factor(object)},
  "fact",       function(object) {is.character(object) || is.factor(object)},
  "num",        function(object) {is.numeric(object) || is.integer(object)},
  "int",        function(object) {is.numeric(object) || is.integer(object)},
  "dataframe",  function(object) {is.data.frame(object) || tibble::is_tibble(object)},
  "data.frame", function(object) {is.data.frame(object) || tibble::is_tibble(object)},
  "df",         function(object) {is.data.frame(object) || tibble::is_tibble(object)},
  "tibble",     function(object) {is.data.frame(object) || tibble::is_tibble(object)},
  "list",       function(object) {is.list(object)}
)

usethis::use_data(table1, overwrite = TRUE)
usethis::use_data(table2, overwrite = TRUE)
usethis::use_data(check_equivalences, overwrite = TRUE)
