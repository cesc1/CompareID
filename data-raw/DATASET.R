## code to prepare the vignette data

path = file.path("data-raw", "Dades vignette.xlsx")
table1 <- rio::import(path, which = "Hoja1")
table2 <- rio::import(path, which = "Hoja2")

path1_1 = file.path("data-raw", "altres", "Dades_municipals_base.csv")
path1_2 = file.path("data-raw", "altres", "Dades_municipals_idescat.csv")
table_complex1 <- rio::import(path1_1) |>
  mutate(municipi = enc2utf8(municipi))
table_complex2 <- rio::import(path1_2) |>
  mutate(municipi = enc2utf8(municipi),
         provincia = enc2utf8(provincia),
         comarques = enc2utf8(comarques),
         areaPolicial = enc2utf8(areaPolicial),
         regioPolicial = enc2utf8(regioPolicial))
# Type equivalence check

check_equivalences <- dplyr::tribble(
  ~type_informal, ~check,
  "char",       function(object) {is.character(object) || is.factor(object)},
  "carac",      function(object) {is.character(object) || is.factor(object)},
  "str",        function(object) {is.character(object) || is.factor(object)},
  "fact",       function(object) {is.character(object) || is.factor(object)},
  "num",        function(object) {is.numeric(object) || is.integer(object)},
  "int",        function(object) {is.numeric(object) || is.integer(object)},
  "bool",       function(object) {is.logical(object)},
  "logical",    function(object) {is.logical(object)},
  "dataframe",  function(object) {is.data.frame(object) || tibble::is_tibble(object)},
  "data.frame", function(object) {is.data.frame(object) || tibble::is_tibble(object)},
  "df",         function(object) {is.data.frame(object) || tibble::is_tibble(object)},
  "tibble",     function(object) {is.data.frame(object) || tibble::is_tibble(object)},
  "list",       function(object) {is.list(object)},
  "fun",        function(object) {is.function(object)}
)

usethis::use_data(table1, overwrite = TRUE)
usethis::use_data(table2, overwrite = TRUE)

usethis::use_data(check_equivalences, overwrite = TRUE, internal = TRUE)

usethis::use_data(table_complex1, overwrite = TRUE)
usethis::use_data(table_complex2, overwrite = TRUE)
