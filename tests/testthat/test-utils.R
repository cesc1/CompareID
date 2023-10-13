test_that("is_duplicated", {
  tables <- select_id(table1,
                      table2,
                      id = "municipi")

  expect_false( is_duplicate( tables$table1 ))
  expect_true ( is_duplicate( tables$table2 ))
})
