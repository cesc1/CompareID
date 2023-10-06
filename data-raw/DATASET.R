## code to prepare the vignette data

path = file.path("data-raw", "Dades vignette.xlsx")
table1 <- load_data(path, which = "Hoja1")
table2 <- load_data(path, which = "Hoja2")

usethis::use_data(table1, overwrite = TRUE)
usethis::use_data(table2, overwrite = TRUE)
