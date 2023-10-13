
# Test load_data: Càrrega de dades

test_that("load_data: used with my data", {
  path = test_path("data test", "Dades vignette.xlsx")

  data_check <- dplyr::tibble(
    municipi = c("Abrera", "Aguilar", "Alélla", "Alpans",
                 "Ametlla Vallès", "Arenys Mar", "Espunyola, l'",
                 "Terrassa", "Barcelona"), # Import delete spaces in start-end
    info = c(1:9)
  )

  expect_equal(load_data(path, which = "Hoja1"), data_check)
})
