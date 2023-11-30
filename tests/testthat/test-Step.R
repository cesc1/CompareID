# Step class
# Constructor: Saving fields, arg errors

test_that("Constructor", {
  expect_no_error(Step$new(type = "replace", pattern = "asdf"))
  expect_error(Step$new(type = "hola"))
})

test_that("step replace", {
  taula <- tibble(id = c("saaba"))
  expect_equal(Step$new(type = "replace", pattern = "a", position = "all")$.step(taula),
               tibble(id = "sb"))
  expect_equal(Step$new(type = "replace", pattern = "a", position = "one")$.step(taula),
               tibble(id = "saba"))
  expect_equal(Step$new(type = "replace", pattern = "s", position = "start")$.step(taula),
               tibble(id = "aaba"))
  expect_equal(Step$new(type = "replace", pattern = "a", position = "start")$.step(taula),
               tibble(id = "saaba"))
  expect_equal(Step$new(type = "replace", pattern = "a", position = "end")$.step(taula),
               tibble(id = "saab"))
  expect_equal(Step$new(type = "replace", pattern = "b", position = "end")$.step(taula),
               tibble(id = "saaba"))
  expect_equal(Step$new(type = "replace", pattern = "a", position = "all", replace = "x")$.step(taula),
               tibble(id = "sxxbx"))

})

test_that("step to_lower", {
  taula <- tibble(id = c("AAAAbbbbAAAAbbbAAAA", "FRANKY"))
  expect_equal(Step$new(type = "lower")$.step(taula),
               taula |> mutate(id = tolower(id)))
})
