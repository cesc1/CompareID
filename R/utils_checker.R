
#' Check the type of a basic variable.
#'
#' It is used internaly on the package, for consistency. It's a wrap up of the
#' `Check_variable` class.
#'
#' @param object The object to check the type off.
#' @param type Single string. It's the type to check. It does not have to be the
#' exact string of the type to check, but it has to be close (ex: for integer
#' type, at least the string has to have "int" in it).
#' Basic types: char num int data.frame tibble list
#' @param len Integer. The length of the object to check.
#' @param len_operator Single string. It's the operator that we want to do the
#' length check.
#'
#' @return Boolean
#'
#' @export
#'
#' @examples
#' # Check the type
#' checkvar(123, type = "integer")
#'
#' # The type can be any string, if it contains "int"
#' checkvar(123, type = "int")
#'
#' # Check the length
#' checkvar(1:2, len = 2)
#'
#' # We can change the operator to check
#' checkvar(1:2, len = 5, len_operator = "<")
#'
#' # We could check everything, and the type and length needs to match
#' checkvar(1:2, type = "int", len = 2)
#'
checkvar <- function(object, type = NULL, len = NULL, len_operator = "==") {
  Check_variable$new(object, type, len, len_operator)$all
}


#' R6 Class helper for checking types and length of variables
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom dplyr filter pull tibble
#' @importFrom stringr str_detect

Check_variable <- R6::R6Class(
  "Check_variable",

  public = list(

    # CONSTRUCTOR

    #' @description
    #' Create object of the class. It needs to have at least a type or len arguments.
    #' @param object The object to check the type off.
    #' @param type Single string. It's the type to check. It does not have to be the
    #' exact string of the type to check, but it has to be close (ex: for integer
    #' type, at least the string has to have "int" in it).
    #' Basic types: char num int data.frame tibble list
    #' @param len Integer. The length of the object to check.
    #' @param len_operator Single string. It's the operator that we want to do the
    #' length check.
    #' @return A checker object
    initialize = function(object, type = NULL, len = NULL, len_operator = "==") {
      private$.object = object
      private$.type   = type
      private$.len    = len

      private$.check_all = private$check_var(len_operator)
      return(invisible(self))
    },


    #' @description
    #' Prints get_values, and the constructor inputs.
    print = function() {
      result <- list(
        Object = private$.object,
        parameters = c(Type = private$.type,
                       Length = private$.len),
        results = self$get_values
      )
      print(result)
    }
  ),

  private = list(
    # FIELDS

    #' @field .object The object that we want to check
    .object = NULL,

    #' @field .type The type to check. String with the name of the class.
    #'              To do the check, it needs to match at least a part of a string
    #'              (ex: for integer type, at least the string has to have "int" in it).
    .type   = NULL,

    #' @field len The length of the object to check.
    .len    = NULL,

    #' @field .check_all Result of a global check, it has to match what it checked (len, or type, or both).
    .check_all  = NULL,

    #' @field .check_type Result of a type check.
    .check_type = NULL,

    #' @field .check_len Result of a length check.
    .check_len  = NULL,

    # METHODS

    #' @description
    #' It will coompare length, type or both fields, if provided on the constructor.
    #' @return Boolean. Both type and length need to be true.
    check_var = function(len_operator = "==") {

      stopifnot(!(is.null(private$.type) && is.null(private$.len))) # General argument check

      if (is.null(private$.type)) { # Check type
       result_type <- TRUE
      }
      else {
       result_type <- private$check_type()
      }

      if (is.null(private$.len)) { # Check length
       result_test <- TRUE
      }
      else {
       result_test <- private$check_length(len_operator)
      }

      # Return
      return (result_type && result_test)
    },


    #'  @description
    #'  It will compare the type of the field object, with the fild type.
    #'  Then update the check_type field.
    #'  @return Boolean
    check_type = function() {

      # Arg check
      stopifnot(is.character(private$.type))

      # General checks with table

      final_table <- check_equivalences |>
        dplyr::filter(stringr::str_detect(private$.type, type_informal)) |>
        dplyr::pull(check)

      stopifnot(length(final_table) <= 1)

      if (length(final_table) == 0) {
        result <- FALSE
      }
      result <- ifelse(length(final_table) == 0,
                       FALSE,
                       final_table[[1]](private$.object))

      private$.check_type <- result
      return(result)
    },


    #' @description
    #' It will compare the length of the field object, and use
    #' the compare_operator to do the comparison. Then update the check_len field.
    #' @param compare_operator Unique string. It gives more options to compare the
    #' .len field of our class.
    #' @return Boolean
    check_length = function(compare_operator = "==") {

      string_expr <- paste(length(private$.object),
                           compare_operator,
                           private$.len)
      result <- sapply(string_expr, function(x){
        eval(parse(text = x))
      }) |>
        any()

      private$.check_len <- result
      return(result)
    }
  ),

  active = list(
    # METHODS

    #' @description
    #' Show the 3 results of the comparison.
    #' @return tibble
    get_values = function() {
      dplyr::tibble(all    = private$.check_all,
                    type   = private$.check_type,
                    length = private$.check_len)
    },

    #' @description
    #' Show the global results of the comparison
    all = function() {
      private$.check_all
    },

    #' @description
    #' Show the type results of the comparison
    type = function() {
      private$.check_type
    },

    #' @description
    #' Show the length results of the comparison
    len= function() {
      private$.check_len
    }
  )
)








