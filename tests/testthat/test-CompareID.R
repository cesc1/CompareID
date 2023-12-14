# CompareID class
  # Constructor: Saving fields, arg errors


test_that("constructor", {
  # fields
  exemple <- CompareID$new(table1, table2, "municipi")
  expect_identical(exemple$id1$id, table1$municipi)
  expect_identical(exemple$id2$id, table2$municipi)
  expect_identical(exemple$join_type, "left")

  # field id_name
  expect_identical(exemple$id_name, rep("municipi", 2))
  id_name_test = c("municipi", "municipi")
  expect_identical(CompareID$new(table1, table2, id_name_test)$id_name,
                   id_name_test)

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
