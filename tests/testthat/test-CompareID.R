# CompareID class
  # Constructor: Saving fields, arg errors


test_that("constructor", {
  # fields
  exemple <- CompareID$new(table1, table2, "municipi")
  expect_identical(table1$municipi, exemple$id1$id)
  expect_identical(table2$municipi, exemple$id2$id)
  expect_identical("left", exemple$join_type)

  # field id_name
  expect_identical(rep("municipi", 2), exemple$id_name)
  id_name_test = c("municipi", "municipi")
  expect_identical(id_name_test, CompareID$new(table1, table2, id_name_test)$id_name)

  # field error
  expect_error(CompareID$new(1, table2, "municipi"))
  expect_error(CompareID$new(table1, 2, "municipi"))
  expect_error(CompareID$new(table1, table2))
  expect_error(CompareID$new(table1, table2, 1))
  expect_error(CompareID$new(table1, table2, c("a", "b", "c")))
  expect_error(CompareID$new(table1, table2, c("municipi", "unaltrestring")))
  expect_error(CompareID$new(table1, table2, "municipi", 1))
  expect_error(CompareID$new(table1, table2, "municipi", "a"))
})

test_that("join", {
  exemple <- CompareID$new(table1, table2, "municipi")
  expect_equal(exemple$join()$join_result,
               dplyr::left_join(exemple$id1["id"],
                                exemple$id2,
                                "id"))
})

test_that("join_miss", {
  exemple <- CompareID$new(table1, table2, "municipi")
  checker <- dplyr::left_join(exemple$id1["id"],
                              exemple$id2,
                              "id") |>
    filter(is.na(original))
  expect_equal(exemple$join()$join_miss,
               checker)
})

test_that("join_match", {
  exemple <- CompareID$new(table1, table2, "municipi")
  checker <- dplyr::left_join(exemple$id1["id"],
                              exemple$id2,
                              "id") |>
    filter(!is.na(original))
  expect_equal(exemple$join()$join_match,
               checker)
})
