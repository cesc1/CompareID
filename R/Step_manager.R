#' R6 Class Step_manager, will help manage the steps: add, remove, sort...
#'
#' @docType class
#' @importFrom R6 R6Class
#'

Step_manager <- R6::R6Class(
  "Step_manager",

  public = list(
    # FIELDS

    #' @field .steps Tibble. A table containing the steps,
    #' and the order which they are applied
    #'
    .steps = NULL,


    # CONSTRUCTOR

    #' @description
    #' Create object of the class.
    #' @return A Step_manager object.
    #' @examples
    #' Step_manager$new()
    #'
    initialize = function() {
      self$.steps <- dplyr::tibble(step  = list())

      return(invisible(self))
    },


    # METHODS

    #' @description
    #' Adds a step at the end of the process
    #'
    add_step = function(step) {
      stopifnot(inherits(step, "Step"))

      self$.steps <- self$.steps |>
        dplyr::add_row(step = list(step))

      return(invisible(self))
    },

    #' @description
    #' Remove one or more steps, based on the index of the table.
    #' @param row_number Int, of length one or more
    remove_step = function(row_number) {
      stopifnot(checkvar(row_number, "integer", 1))
      stopifnot(row_number > 0 &&
                row_number <= self$number)

      self$.steps <- self$.steps |>
        dplyr::filter(!(dplyr::row_number() %in% row_number))

      return(invisible(self))
    },

    #' @description
    #' Move a step, from one row to another.
    #' @param from int, row that we want to move
    #' @param to int, destination of the row that we want to move
    move_step = function(from, to) {
      # Check variable
      stopifnot(checkvar(  from, type = "int", len = 1))
      stopifnot(checkvar(    to, type = "int", len = 1))
      stopifnot(from <= self$number)
      stopifnot(to   <= self$number)
      stopifnot(from > 0)
      stopifnot(to   > 0)

      # Copy and remove step
      target <- self$.steps$step[from]
      self$remove_step(from)

      # Add step at to
      self$.steps <- self$.steps |>
        dplyr::add_row(step = target, .before = to)

      return(invisible(self))
    },


    #' @description
    #' Execute the steps in order.
    #' @param dades tibble. It needs a column with name "id".
    do_steps = function(dades) {
      for(step in self$.steps$step) {
        dades = dades |>
          step$.step()
      }
      return(dades)
    },


    #' @description
    #' Print the info method
    #' @param dades tibble. It needs a column with name "id".

    print = function() {
      cat("Class: Step_manager\n")
      cat("-------------------\n")
      print(self$info)
    }

  ),

  active = list(

    #' @description
    #' Displays the number of steps of the process.
    #' @return An integer.
    #'
    number = function() {
      result <- nrow(self$.steps)
      return(result)
    },

    #' @description
    #' Show basic info
    #' @return A vector with the info
    #'
    info = function() {
      self$.steps$step |>
        lapply(function(step) {
          step$info
        }) |>
        dplyr::bind_rows()
    }
  )
)
