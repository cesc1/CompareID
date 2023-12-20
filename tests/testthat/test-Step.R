# Step class
# Constructor: Saving fields, arg errors

test_that("Constructor", {
  expect_no_error(Step$new(type = "replace", pattern = "asdf"))
  expect_error(Step$new(type = "hola"))
  expect_error(Step$new())
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
  expect_equal(Step$new(type = "replace", pattern = c("a", "s"), position = "all", replace = "")$.step(taula),
               tibble(id = "b"))

})


test_that("step to_lower", {
  taula <- tibble(id = c("AAAAbbbbAAAAbbbAAAA", "FRANKY"))
  expect_equal(Step$new(type = "lower")$.step(taula),
               taula |> mutate(id = tolower(id)))
})


test_that("step manual", {
  taula <- dplyr::tibble(id = c(
    "hola",
    "sabe",
    "que pasa"
  ))
  taula_res <- dplyr::tibble(id = c(
    "hola",
    "sape",
    "adeu"
  ))
  llista_equival <- list(
    "sabe" = "sape",
    "que pasa" = "adeu"
  )
  vector_equival <- c(
    "sabe" = "sape",
    "que pasa" = "adeu"
  )

  expect_equal(Step$new(type = "manual", match = llista_equival)$.step(taula),
               taula_res)
  expect_equal(Step$new(type = "manual", match = vector_equival)$.step(taula),
               taula_res)
  expect_error(Step$new(type = "manual", match = c(1:2))$.step(taula))
})

test_that("step_function", {
  taula <- dplyr::tibble(id = c("Hola", "COMP", "ESTAS", "BaBE", "scak"))

  res <- Step$new(type = "function", function(dades) {
    dplyr::mutate(dades, id = stringr::str_to_lower(id))
  })$.step(taula)

  expect_equal(res, dplyr::tibble(id = stringr::str_to_lower(taula$id)))
})
