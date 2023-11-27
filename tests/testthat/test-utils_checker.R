# Tests for class VARIABLE CHECKER
  # Small tests for wrapper checkvar()
  # TRUE/FALSE for all, type, len
  # get_values dim, null constructor, error constructor
  # Check all the types


test_that("checkvar() wrapper", {
  expect_true(checkvar(1:2, type = "integer", len = 2, len_operator = "=="))
  expect_false(checkvar("ap", type = "character", len = 2))
  expect_equal(checkvar(1:2, type = "int", len = 2, len_operator = "<="),
               Check_variable$new(1:2, type = "int", len = 2, len_operator = "<=")$all)
})


test_that("Check_variable general checks", {
  expect_true(Check_variable$new(1:2, type = "integer", len = 2, len_operator = "==")$all)
  expect_false(Check_variable$new(1, type = "integer", len = 2, len_operator = "==")$all)

  expect_true(Check_variable$new(1:2, type = "integer", len = 2, len_operator = "==")$len)
  expect_false(Check_variable$new(1, type = "integer", len = 2, len_operator = "==")$len)

  expect_true(Check_variable$new(1:2, type = "integer", len = 2, len_operator = "==")$type)
  expect_false(Check_variable$new(1:2, type = "character", len = 2, len_operator = "==")$type)
})

test_that("Check_variable fields", {
  expect_equal(Check_variable$new(1, type = "integer", len = 1)$get_values |> dim(),
               c(1, 3))
  expect_equal(Check_variable$new(1, type = "integer")$get_values |> dim(),
               c(1, 2))
  expect_null(Check_variable$new(1, type = "integer")$len)
  expect_error(Check_variable$new(1))
})

test_that("Check_variable types", {
  # Error en paràmetre type
  expect_error( Check_variable$new(1, type = 15))

  # integer
  expect_true(Check_variable$new(1, type = "int", len = 1)$type)
  expect_true(Check_variable$new(1, type = "integer", len = 1)$type)
  expect_true(Check_variable$new(1, type = "num", len = 1)$type)
  expect_true(Check_variable$new(1, type = "numeric", len = 1)$type)
  expect_false(Check_variable$new(1, type = "character", len = 1)$type)

  # character
  expect_true(Check_variable$new("test", type = "char", len = 1)$type)
  expect_true(Check_variable$new("test", type = "character", len = 1)$type)
  expect_true(Check_variable$new("test", type = "str", len = 1)$type)
  expect_true(Check_variable$new("test", type = "string", len = 1)$type)
  expect_true(Check_variable$new("test", type = "factor", len = 1)$type)


  # data.frame
  expect_true(Check_variable$new(data.frame(x=1), type = "data.frame", len = 1)$type)
  expect_true(Check_variable$new(dplyr::tibble(x=1), type = "dataframe", len = 1)$type)
  expect_true(Check_variable$new(dplyr::tibble(x=1), type = "tibble", len = 1)$type)
  expect_true(Check_variable$new(dplyr::tibble(x=1), type = "df", len = 1)$type)
  expect_false(Check_variable$new(dplyr::tibble(x=1), type = "integer", len = 1)$type)

  # list
  expect_true(Check_variable$new(list(1), type = "list", len = 1)$type)
  expect_false(Check_variable$new(list(1), type = "int", len = 1)$type)
  expect_false(Check_variable$new("test", type = "data.frame", len = 1)$type)
})
