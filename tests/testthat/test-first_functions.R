test_that("load_data: used with my data", {
  data_load <- load_data(
    test_path("test_data", "Dades prova.xlsx")
  )

  data_check <- data.frame(
    municipi = c("Abrera", "Aguilar", "Alélla", "Alpans",
                 "Ametlla Vallès", "Arenys Mar", "Espunyola, l'",
                 "Terrassa", "Barcelona"), # Import delete spaces in start-end
    info = c(1:9)
  )

  expect_equal(data_load, data_check)
})
