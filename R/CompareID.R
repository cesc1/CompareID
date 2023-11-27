
#' R6 Class. Principal class, to preprocess the tables.
#'
#' @docType class
#' @importFrom R6 R6Class

CompareID <- R6::R6Class(
  "CompareID",

  # PUBLIC
  public = list(
    # FIELDS

    #' @field id1 Column identifier of the first table
    id1 = NULL,

    #' @field id2 Column identifier of the second table
    id2 = NULL,

    #' @field id_name Character. The name of the column identifiers. Can be
    #' a vector of length 1 or 2, depending if the name is the same or not.
    id_name = NULL,

    #' @field join-type String. Type of join to use (left, right, full, inner)
    join_type = NULL,

    join_result = NULL,

    # CONSTRUCTOR

    #' @description
    #' Create object of the class.
    #' @param dades1 data.frame. Taula en la cual volem extreure el primer identificador
    #' @param dades2 data.frame. Taula en la cual volem extreure el segon identificador
    #' @return A CompareID object
    #' @examples
    #' CompareID$new(table1, table2, "municipi")
    initialize = function(dades1, dades2, id, join_type = "left") {

      # CHECK
        # dades1 dades2
      stopifnot( checkvar(dades1, type = "tibble"))
      stopifnot( checkvar(dades2, type = "tibble"))

        # id_name
      if(missing(id)) {
        self$id_name <- c(
          names(dades1)[1],
          names(dades2)[1]
        )
      }
      stopifnot( checkvar(id, type = "character", len = c(1, 2)))

        # join_type
      stopifnot( checkvar(join_type, type = "character", len = 1))
      stopifnot(join_type %in% c("left", "right", "full", "inner"))

      # PROCESS
        # id_name
      self$id_name <- ifelse(length(id) == 1,
                             list(c(id, id)),
                             list(c(id))) |> unlist()

        # id1, id2
      self$id1 <- dplyr::select(dades1, dplyr::all_of(self$id_name[1])) |>
        dplyr::mutate(checker = TRUE)
      self$id2 <- dplyr::select(dades2, dplyr::all_of(self$id_name[2])) |>
        dplyr::mutate(checker = TRUE)

      colnames(self$id1) <- c("id", "checker") # Rename column ids
      colnames(self$id2) <- c("id", "checker")

        # join_type
      self$join_type <- join_type

      return(invisible(self))
    },

    # METHODS

    #' @description
    #' Do a join with id1 and id2. The type of join is chose by join_type.
    #' Can add more parameters (see ?dplyr::left_join)
    #' TODO: Add the preprocessing step.
    #' @return Updates self$join_result, and returns invisible(self).
    #' @examples
    #' CompareID$new(table1, table2, "municipi")$join()$join_result
    join = function(...) {
      do_join = switch(self$join_type,
        "left" = dplyr::left_join,
        "right" = dplyr::right_join,
        "full" = dplyr::full_join,
        "inner" = dplyr::inner_join,
        stop("join_type not found.")
      )

      self$join_result <- do_join(
        self$id1["id"],
        self$id2,
        by = "id",
        ...
      )

      return(invisible(self))
    }

  ),

  # ACTIVE
  active = list(
    #' @description
    #' Return the missing matches of the join
    #' @return data.frame
    #' @examples
    #' CompareID$new(table1, table2, "municipi")$join()$join_miss
    join_miss = function() {
      result <- self$join_result |>
        dplyr::filter(is.na(checker))

      return(result)
    },

    #' @description
    #' Return the missing matches of the join
    #' @return data.frame
    #' @examples
    #' CompareID$new(table1, table2, "municipi")$join()$join_miss
    join_match = function() {
      result <- self$join_result |>
        dplyr::filter(!is.na(checker))

      return(result)
    }

  )

)