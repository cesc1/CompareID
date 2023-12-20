
#' R6 Class Step, will help adding a step on the preprocessing process
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom dplyr mutate tibble add_row
#' @importFrom stringr str_replace str_replace_all
#'

Step <- R6::R6Class(
  "Step",

  public = list(
    # FIELDS

    #' @field type char. The type of step to apply.
    #' Can be: c("replace", "lower", "manual", "function")
    #'
    .type = NULL,
    #' @field .step function. A step to aply to CompareID
    #'
    .step = NULL,


    # CONSTRUCTOR

    #' @description
    #' Create object of the class.
    #' @param type string. Select the type of step/function to apply.
    #' @param ... Other arguments of the function that will be applied.
    #' - replace: pattern, position
    #' - manual: match
    #' @return A Step object.
    #'
    initialize = function(type = list(NULL, "replace", "lower", "manual", "function"),
                          ...) {
      # Add type
      type = type[[1]]
      stopifnot(checkvar(type, "character"))
      self$.type <- type

      # Add step
      self$.step <- private$choose_step_type(...)
    },


    #' @description
    #' Print the class
    #' @return Void
    #'
    print = function() {
      cat("Class: Step\n")
      cat("-----------\n")
      print(self$info)
    }

  ),

  private = list(
    # FIELDS

    #' @field .pattern char. The pattern to apply to some steps
    #'
    .pattern = NULL,
    #' @field .position char. The position to apply the pattern.
    #' Can be: c("all", "one", "start", "end")
    #'
    .position = NULL,

    #' @field .replace char. The replacement for the pattern found.
    #'
    .replace = NULL,

    #' @field .match tibble. Columns: search and replace.
    #' It uses exact matches. To create, list("search" = "replace")
    #'
    .match = NULL,


    # METHODS

    #' @description
    #' Will help choosing the function step to apply (equivalence table from self$.type)
    #'
    choose_step_type = function(...) {
      result <- switch(self$.type,
        "replace"  = private$step_replace_pat(...),
        "lower"    = private$step_to_lower(),
        "manual"   = private$step_manual(...),
        "function" = private$step_function(...),
        stop("type not found.")
      )
      return(result)
    },

    #' @description
    #' If type = "replace", will create a step that removes or replaces a
    #' pattern from a dataset.
    #' @param pattern string. Regex to apply. If length > 1, it paste it together
    #' with "|"
    #' @param position  string. Where to apply the pattern.
    #' Can be: c("all", "one", "start", "end")
    #' @param replace string. The replacement of the pattern
    #' @return function
    #'
    step_replace_pat = function(pattern,
                                position = c("all", "one", "start", "end"),
                                replace = "") {
      position = position[1]
      stopifnot(checkvar(pattern, "character", len = 1, len_operator = ">="))
      stopifnot(checkvar(position, "character"))
      stopifnot(checkvar(replace, "character"))
      stopifnot(position %in% c("all", "one", "start", "end"))

      # Preprocessing pattern
      pattern <- switch(position,
                        "all" =   pattern,
                        "one" =   pattern,
                        "start" = paste0("^", pattern),
                        "end" =   paste0(pattern, "$")
      )
      pattern <- ifelse(length(pattern) > 1,
             paste(pattern, collapse = "|"),
             pattern)

      # Add fields
      private$.position = position
      private$.replace = replace
      private$.pattern <- pattern

      result <- ifelse(
        position == "all",
        function(dades) {
          dplyr::mutate(dades, id = stringr::str_replace_all(id,
                                                            private$.pattern,
                                                            private$.replace))},
        function(dades) {
          dplyr::mutate(dades, id = stringr::str_replace(id,
                                                        private$.pattern,
                                                        private$.replace))}
      )
      return(result)
    },


    #' @description
    #' If type = "lower", will create a step that transform the string
    #' to lower from a dataset.
    #' @return function
    #'
    step_to_lower = function() {
      result <- function(dades) {
        dplyr::mutate(dades, id = tolower(id))
      }
      return(result)
    },


    #' @description
    #' If type = "manual", will create a step to replace an exact
    #' match from a dataset.
    #' @param match list or char named vector. Example: c(search = "replace")
    #' @return function
    #'
    step_manual = function(match) {
      # Check match
      stopifnot(checkvar(match, type = "list") ||
                checkvar(match, type = "char"))
      stopifnot(length(names(match)) == length(match))

      # Preprocessing match
      private$.match <- tibble(
        search = names(match),
        replace = unlist(match)
      )
      # Result
      result <- function(dades) {
        idx_match <- match(private$.match$search,
                           dades$id)

        if (any(is.na(idx_match))) # If match not found, stop!
          stop("Equivalence not found! Try the manual step again changing the table.")

        dades |>
          dplyr::mutate(
            id = replace(id,
                         idx_match,
                         private$.match$replace))
      }
      return(result)
    },


    #' @description
    #' If type = "function", will create a step with a custom function.
    #' The custom function uses "dades", and this data.frame has a column id
    #' that you can transform, and return the next table.
    #' @param fun function.
    #' @return function
    #'
    step_function = function(fun) {
      stopifnot(checkvar(fun, type = "function"))

      return(fun)
    }

  ),

  active = list(

    #' @description
    #' Info: Show basic info
    #' @return A vector with the info
    #'
    info = function() {
      taula <- dplyr::tibble(type = character(),
                             pattern = character(),
                             replace = character(),
                             position = character(),
                             match = list())
      taula |>
        dplyr::add_row(
          type = self$.type,
          pattern = private$.pattern,
          replace = private$.replace,
          position = private$.position,
          match = list(private$.match)
        )
    }
  )
)
