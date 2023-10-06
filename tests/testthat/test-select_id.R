test_that("length_id", {
  # null
  expect_error(length_id(NULL), "type")


  # char vector
  expect_equal(length_id("col1"), 1)
  expect_equal(length_id(c("col1", "col2")), 2)
  expect_error(length_id(c("col1", "col2", "col3")), "length")

  # factor vector
  expect_equal(length_id(factor("col1")), 1)
  expect_equal(length_id(factor(c("col1", "col2"))), 2)
  expect_error(length_id(factor(c("col1", "col2", "col3"))), "length")

  # numeric vector
  expect_equal(length_id(1), 1)
  expect_equal(length_id(c(11, 12)), 2)
  expect_error(length_id(c(11, 12, 13)), "length")

  # int vector
  expect_equal(length_id(1L), 1)
  expect_equal(length_id(c(11L, 12L)), 2)
  expect_error(length_id(c(11L, 12L, 13L)), "length")
})


test_that("select_id data rows", {
  data <- select_id(table1, table2, "municipi")

  expect_equal(nrow(data$table1), nrow(table1))
  expect_equal(nrow(data$table2), nrow(table2))
})
