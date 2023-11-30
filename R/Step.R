
#' R6 Class Step, will help adding a step on the preprocessing process
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom dplyr mutate
#' @importFrom stringr str_replace str_replace_all
#'

Step <- R6::R6Class(
  "Step",

  public = list(
    # FIELDS

    #' @field type char. The type of step to apply.
    #' Can be: c("replace", lower)
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
    #' -
    #' @return A Step object.
    #' @examples
    #' Step$new(type = "replace", pattern = "a")
    #'
    initialize = function(type, ...) {

      # Add type type
      stopifnot(checkvar(type, "character"))
      self$.type <- type

      # Add step
      self$.step <- private$choose_step_type(...)
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


    # METHODS

    #' @description
    #' Will help choosing the function step to apply (equivalence table from self$.type)
    #'
    choose_step_type = function(...) {
      result <- switch(self$.type,
        "replace" = private$step_replace_pat(...),
        "lower"  = private$step_to_lower(),
        stop("type not found.")
      )
      return(result)
    },

    #' @description
    #' If type = "replace", will remove or replace a pattern from a dataset.
    #' @param pattern string. Regex to apply
    #' @param position  string. Where to apply the pattern.
    #' Can be: c("all", "one", "start", "end")
    #' @param replace string. The replacement of the pattern
    #' @return function
    #'
    step_replace_pat = function(pattern,
                                position = c("all", "one", "start", "end"),
                                replace = "") {
      position = position[1]
      stopifnot(checkvar(pattern, "character"))
      stopifnot(checkvar(position, "character"))
      stopifnot(checkvar(replace, "character"))

      stopifnot(position %in% c("all", "one", "start", "end"))

      # Add fields
      private$.position = position
      private$.replace = replace
      private$.pattern <- switch( position,
        "all" =   pattern,
        "one" =   pattern,
        "start" = paste0("^", pattern),
        "end" =   paste0(pattern, "$")
      )

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
    #' If type = "lower, will transform the string to lower from a dataset.
    #' @return function
    #'
    step_to_lower = function() {
      result <- function(dades) {
        dplyr::mutate(dades, id = tolower(id))
      }
      return(result)
    }

  )
)
