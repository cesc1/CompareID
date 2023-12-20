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
               dplyr::left_join(exemple$id1,
                                exemple$id2,
                                "id") |> dplyr::distinct())
})

test_that("join_data", {
  exemple <- CompareID$new(table1, table2, "municipi")
  res <- exemple$join_data(table1, table2)

  exemple <- CompareID$new(table1, table2, "municipi", "right")
  res <- exemple$join_data(table1, table2)
})



test_that("join_miss", {
  # left join
  exemple <- CompareID$new(table1, table2, "municipi")
  checker <- dplyr::left_join(exemple$id1,
                              exemple$id2,
                              "id") |> filter(is.na(original.y))
  expect_equal(exemple$join()$join_miss,
               checker)

  # right join
  exemple <- CompareID$new(table1, table2, "municipi", join_type = "right")
  checker <- dplyr::right_join(exemple$id1,
                              exemple$id2,
                              "id") |> filter(is.na(original.x))
  expect_equal(exemple$join()$join_miss,
               checker)
})

test_that("join_match", {
  exemple <- CompareID$new(table1, table2, "municipi")
  checker <- dplyr::left_join(exemple$id1,
                              exemple$id2,
                              "id") |>
    dplyr::distinct() |>
    filter(!is.na(original.y))
  expect_equal(exemple$join()$join_match,
               checker)
})

test_that("add_Step", {
  exemple <- CompareID$new(table1, table2, "municipi")
  exemple$add_step("lower")
  exemple$add_step("replace", "l'")
  exemple$join()
  #print(exemple, show_id = T)
})

test_that("is_duplicate", {
  exemple <- CompareID$new(table1, table2, "municipi")

  # Basic test
  expect_equal(exemple$is_duplicate(), c(id1 = FALSE, id2 = TRUE))

  # Test View
  expect_equal(length(exemple$is_duplicate(view = T)), 1) # length
  expect_equal(exemple$is_duplicate(TRUE)[["id2"]]$id, c("Alpans", "Barcelona"))
  #print(exemple, show_id = T)
})
