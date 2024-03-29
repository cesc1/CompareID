
#' R6 Class. Principal class, to preprocess the tables.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom dplyr select all_of left_join right_join full_join inner_join filter
#'              tibble distinct rename count mutate
#' @export

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

    #' @field join_type
    #' . Type of join to use (left, right, full, inner)
    join_type = NULL,

    #' @field join_result Tibble. The result of the join
    join_result = NULL,

    #' @field steps_id1 Step_manager. Steps of id1
    steps_id1 = NULL,

    #' @field steps_id2 Step_manager. Steps of id2
    steps_id2 = NULL,


    # CONSTRUCTOR

    #' @description
    #' Create object of the class.
    #' @param dades1 data.frame. Taula en la cual volem extreure el primer identificador
    #' @param dades2 data.frame. Taula en la cual volem extreure el segon identificador
    #' @return A CompareID object
    #' @examples
    #' CompareID$new(table1, table2, "municipi")
    initialize = function(dades1, dades2, id, join_type = c("left", "right", "full", "inner")) {

      # CHECK
        # dades1 dades2
      stopifnot( checkvar(dades1, type = "tibble"))
      stopifnot( checkvar(dades2, type = "tibble"))

        # id_name
      if(missing(id)) {
        id <- c(
          names(dades1)[1],
          names(dades2)[1]
        )
      }
      stopifnot( checkvar(id, type = "character", len = c(1, 2)))

        # join_type
      join_type <- join_type[1]
      stopifnot(checkvar(join_type, type = "character", len = 1))
      stopifnot(join_type %in% c("left", "right", "full", "inner"))

      # PROCESS
        # id_name
      self$id_name <- ifelse(length(id) == 1,
                             list(c(id, id)),
                             list(c(id))) |> unlist()
      rm(id) # delete to avoid confusions with column name "id"

        # id1, id2
      self$id1 <- dplyr::select(dades1, dplyr::all_of(self$id_name[1])) |>
        dplyr::tibble()
      self$id2 <- dplyr::select(dades2, dplyr::all_of(self$id_name[2])) |>
        dplyr::tibble()

      colnames(self$id1) <- "id" # Rename column ids
      colnames(self$id2) <- "id"
      self$id1 <- self$id1 |> dplyr::mutate(original = id)
      self$id2 <- self$id2 |> dplyr::mutate(original = id)

        # join_type
      self$join_type <- join_type

        # steps_id
      self$steps_id1 <- Step_manager$new()
      self$steps_id2 <- Step_manager$new()

      return(invisible(self))
    },

    # METHODS

    #' @description
    #' Do a join with id1 and id2. The type of join is chose by join_type.
    #' Can add more parameters (see ?dplyr::left_join)
    #' @return Updates self$join_result, and returns invisible(self).
    #' @examples
    #' CompareID$new(table1, table2, "municipi")$join()$join_result
    join = function(...) {

      # Reset preprocessing steps
      private$reset_steps()

      # Apply preprocessing steps
      private$do_steps()

      # Join ids
      do_join = switch(self$join_type, # choose a function
        "left" = dplyr::left_join,
        "right" = dplyr::right_join,
        "full" = dplyr::full_join,
        "inner" = dplyr::inner_join,
        stop("join_type not found.")
      )

      self$join_result <- do_join(
        self$id1,
        self$id2,
        by = "id",
        ...
      ) |>
        dplyr::distinct()

      return(invisible(self))
    },


    #' @description
    #' Do a join with dades1 and dades2, based on the transformations and
    #' parameters of the object.
    #' @return Updates self$join_result, and returns invisible(self).
    #'
    join_data = function(dades1, dades2, ...) {

      self$join(...)

      dades1 <- dades1 |>
        dplyr::rename("original.x" = self$id_name[1])
      dades2 <- dades2 |>
        dplyr::rename("original.y" = self$id_name[2])

      result <-
        dplyr::left_join(
          self$join_result,
          dades1,
          by = "original.x"
        ) |>
        dplyr::left_join(
          dades2,
          by = "original.y"
        ) |>
        dplyr::distinct()

      # Rename original columns
      new_names <- ifelse(self$id_name[1] == self$id_name[2],
                          list(c(paste0(self$id_name[1], c(".x", ".y")))),
                          list(self$id_name)) |> unlist()
      names(result)[names(result) == "original.x"] <- new_names[1]
      names(result)[names(result) == "original.y"] <- new_names[2]

      return(result)
    },


    #' @description
    #' Add a step to id1, id2 or both
    #'
    #' @param type string. Select the type of step/function to apply.
    #' @param ... Other arguments of the function that will be applied.
    #' - replace: pattern, position, replace
    #' - manual: match
    #' @param which character. Which id add the step (all, id1 or id2)
    #' @param func function. A function step to process the tibble "dades".
    #' @return Updates self$steps_id1 and self$steps_id2
    #'
    add_step = function(type = list(NULL, "replace", "lower", "manual", "function"),
                        ...,
                        which = c("all", "id1", "id2")) {

      which = which[1]
      stopifnot(which %in% c("all", "id1", "id2"))

      if (which == "id1" || which == "all") {
        Step$new(type, ...) |>
          self$steps_id1$add_step()
      }

      if (which == "id2" || which == "all") {
        Step$new(type, ...) |>
          self$steps_id2$add_step()
      }

      return(invisible(self))
    },


    #' @description
    #' Print some personalized info in the class
    #' @param show_fields bool. If we want to show all the fields of the class.
    #' @param show_steps bool. If we want to show the steps applied at the moment.
    #' @param show_result bool. If we want to show the result
    #' @param show_misses bool. If we want to show the miss matches of the result.
    #' @param show_matches bool. If we want to show the matches of the result.
    #' @param show_dup bool. If we want to show if id1 or id2 have duplicates.
    #'
    #' @return Void
    #'
    print = function(show_fields  = FALSE,
                     show_steps   = TRUE,
                     show_result  = FALSE,
                     show_misses  = TRUE,
                     show_matches = FALSE,
                     show_dup = TRUE) {
      cat("Class: Step_manager\n")
      cat("-------------------\n\n")

      if (show_dup) {
        dup <- self$is_duplicate()
        cat("\nDUPLICATES\n")
        cat("----------")
        cat("\nId1: "); cat(ifelse(dup["id1"], "SI", "NO"))
        cat("\nId2: "); cat(ifelse(dup["id2"], "SI", "NO"))
      }

      if (show_steps) {
        cat("\n")
        cat("Steps id1:\n")
        print(self$steps_id1$info)
        cat("Steps id2:\n")
        print(self$steps_id2$info)
      }

      if (show_fields) {
        cat("\nID 1:\n")
        print(self$id1)
        cat("\nID 2:\n")
        print(self$id2)

        cat("\nID name:\n")
        print(self$id_name)

        cat("\nJoin type:\n")
        print(self$join_type)
      }

      if (show_result) {
        cat("\nJoin result:\n")
        print(self$join_result)
      }

      if (show_misses) {
        cat("\n   MISS\n")
        cat("---------\n")
        print(self$join_miss)
      }

      if (show_matches) {
        cat("\n   MATCH\n")
        cat("---------\n")
        print(self$join_match)
      }
    },


    #' @description
    #' Return the missing matches of the secondary table of the join
    #' @return if view = T, list of tibbles. if view = F, named vector of length 2
    #' @examples
    #' CompareID$new(table1, table2, "municipi")$join()$join_search
    is_duplicate = function(view = FALSE) {
      # Type check

      dup1 <- self$id1 |>
        dplyr::count(id) |>
        dplyr::filter(n > 1)
      dup2 <- self$id2 |>
        dplyr::count(id) |>
        dplyr::filter(n > 1)

      result <- c("id1" = nrow(dup1) > 0,
                  "id2" = nrow(dup2) > 0)

      if (view) {
        return(
          list(id1 = dup1,
               id2 = dup2)[result])
      }

      return(result)
    }

  ),

  # PRIVATE
  private = list(

    #' @description
    #' Apply the preprocessing steps stored in steps_ids, to its respective ids.
    #' @return Updates self$id1 and self$id2 with the steps applied
    #'
    do_steps = function() {

      self$id1 <- self$id1 |>
        self$steps_id1$do_steps()

      self$id2 <- self$id2 |>
        self$steps_id2$do_steps()

      return(invisible(self))
    },


    #' @description
    #' Reset the preprocessing steps applied to the data. Used at the start of join.
    #' @return Updates self$id1 and self$id2 with the originals
    #'
    reset_steps = function() {
      self$id1 <- self$id1 |>
        dplyr::mutate(id = original)

      self$id2 <- self$id2 |>
        dplyr::mutate(id = original)

      return(invisible(self))
    }
  ),

  # ACTIVE
  active = list(

    #' @description
    #' Return the missing matches of the main table of the join
    #' @return data.frame
    #' @examples
    #' CompareID$new(table1, table2, "municipi")$join()$join_miss
    join_miss = function() {

      self$join()

      # Depends on join_type, we search NAs in diferent columns (original.x/y)
      j_res <- self$join_result
      NA_search <- switch(self$join_type,
                          "left"  = is.na(j_res$original.y),
                          "right" = is.na(j_res$original.x),
                          "inner" = is.na(j_res$id), # can't be NA
                          "full"  = is.na(j_res$original.x) | is.na(j_res$original.y))

      result <- self$join_result |>
        dplyr::filter(NA_search)

      return(result)
    },

    #' @description
    #' Return the matches of the join
    #' @return data.frame
    #' @examples
    #' CompareID$new(table1, table2, "municipi")$join()$join_match
    join_match = function() {
      self$join()

      result <- self$join_result |>
        dplyr::filter(!is.na(original.y))

      return(result)
    },


    #' @description
    #' Return the missing matches of the secondary table of the join
    #' @return data.frame
    #' @examples
    #' CompareID$new(table1, table2, "municipi")$join()$join_search
    join_search = function() {
      new_checker <- self$clone()
      new_checker$join_type = switch(self$join_type,
                                     "left"  = "right",
                                     "right" = "left",
                                     "inner" = "inner",
                                     "full"  = "full")
      result <- new_checker$join_miss

      return(result)
    }
  )
)
