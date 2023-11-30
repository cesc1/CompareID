test_that("is_duplicated", {
  objecte <- CompareID$new(table1, table2, "municipi")
  expect_false(is_duplicate(objecte)["id1"])
  expect_true(is_duplicate(objecte)["id2"])
  expect_identical(is_duplicate(objecte, view = TRUE)[["id2"]]$id, c("Alpans", "Barcelona"))
})


