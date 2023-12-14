test_that("Test a process", {
  manager <- Step_manager$new()

  # Adding steps
  manager$add_step(Step$new("lower"))
  expect_equal(manager$info$type, c("lower"))
  manager$add_step(Step$new("replace", "a"))
  expect_equal(manager$info$type, c("lower", "replace"))
  manager$add_step(Step$new("replace", c(", el", ", la")))
  expect_equal(manager$info$type, c("lower", "replace", "replace"))

  # Deleting steps
  expect_error(manager$remove_step(0))
  expect_error(manager$remove_step(4))
  expect_error(manager$remove_step("a"))

  manager$remove_step(3)
  expect_equal(manager$info$type, c("lower", "replace"))
  manager$remove_step(1)
  expect_equal(manager$info$type, c("replace"))
  manager$add_step(Step$new("lower"))
  manager$remove_step(1)
  expect_equal(manager$info$type, c("lower"))

  manager$remove_step(1) # 0 steps
  expect_equal(manager$number, 0)
  expect_error(manager$remove_step(1))
  manager$add_step(Step$new("lower"))
  expect_equal(manager$info$type, c("lower"))


  # Test move
  manager <- Step_manager$new()
  manager$add_step(Step$new("lower"))
  manager$add_step(Step$new("replace", "a"))
  manager$add_step(Step$new("replace",
                            pattern = c(", el", ", la"),
                            replace = "l'",
                            position = "start"))
  manager$add_step(Step$new("manual", match = c("a" = "b")))
  expect_equal(manager$info$type, c("lower", "replace", "replace", "manual"))

  manager$move_step(from = 4, to = 2)
  expect_equal(manager$info$type, c("lower", "manual", "replace", "replace"))
  manager$move_step(from = 1, to = 4)
  expect_equal(manager$info$type, c("manual", "replace", "replace", "lower"))
  manager$move_step(from = 3, to = 3)
  expect_equal(manager$info$type, c("manual", "replace", "replace", "lower"))

  expect_error(manager$move_step(from = 0, to = 3))
  expect_error(manager$move_step(from = 2, to = 6))
  expect_error(manager$move_step(from = 2, to = 0))

  # Test do_steps
  dades_test <- tibble(id = c("Hola", "de Lujo de", "De lujo"))
  dades_solu <- tibble(id = c("hola", " lujo ", " lujo"))
  manager <- Step_manager$new()
  manager$add_step(Step$new("lower"))
  manager$add_step(Step$new("replace", pattern = "de"))
  expect_equal(manager$do_steps(dades_test), dades_solu)

  expect_error(manager$do_steps(table1))
})
