% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CompareID.R
\docType{class}
\name{CompareID}
\alias{CompareID}
\title{R6 Class. Principal class, to preprocess the tables.}
\arguments{
\item{dades1}{data.frame. Taula en la cual volem extreure el primer identificador}

\item{dades2}{data.frame. Taula en la cual volem extreure el segon identificador}

\item{type}{string. Select the type of step/function to apply.}

\item{...}{Other arguments of the function that will be applied.
\itemize{
\item replace: pattern, position, replace
\item manual: match
}}

\item{which}{character. Which id add the step (all, id1 or id2)}

\item{func}{function. A function step to process the tibble "dades".}

\item{show_fields}{bool. If we want to show all the fields of the class.}

\item{show_steps}{bool. If we want to show the steps applied at the moment.}

\item{show_result}{bool. If we want to show the result}

\item{show_misses}{bool. If we want to show the miss matches of the result.}

\item{show_matches}{bool. If we want to show the matches of the result.}

\item{show_dup}{bool. If we want to show if id1 or id2 have duplicates.}
}
\value{
A CompareID object

Updates self$join_result, and returns invisible(self).

Updates self$join_result, and returns invisible(self).

Updates self$steps_id1 and self$steps_id2

Void

if view = T, list of tibbles. if view = F, named vector of length 2

Updates self$id1 and self$id2 with the steps applied

Updates self$id1 and self$id2 with the originals

data.frame

data.frame

data.frame
}
\description{
Create object of the class.

Do a join with id1 and id2. The type of join is chose by join_type.
Can add more parameters (see ?dplyr::left_join)

Do a join with dades1 and dades2, based on the transformations and
parameters of the object.

Add a step to id1, id2 or both

Print some personalized info in the class

Return the missing matches of the secondary table of the join

Apply the preprocessing steps stored in steps_ids, to its respective ids.

Reset the preprocessing steps applied to the data. Used at the start of join.

Return the missing matches of the main table of the join

Return the matches of the join

Return the missing matches of the secondary table of the join
}
\section{Fields}{

\describe{
\item{\code{id1}}{Column identifier of the first table}

\item{\code{id2}}{Column identifier of the second table}

\item{\code{id_name}}{Character. The name of the column identifiers. Can be
a vector of length 1 or 2, depending if the name is the same or not.}

\item{\code{join_type}}{. Type of join to use (left, right, full, inner)}

\item{\code{join_result}}{Tibble. The result of the join}

\item{\code{steps_id1}}{Step_manager. Steps of id1}

\item{\code{steps_id2}}{Step_manager. Steps of id2}
}}

\examples{
CompareID$new(table1, table2, "municipi")
CompareID$new(table1, table2, "municipi")$join()$join_result
CompareID$new(table1, table2, "municipi")$join()$join_search
CompareID$new(table1, table2, "municipi")$join()$join_miss
CompareID$new(table1, table2, "municipi")$join()$join_match
CompareID$new(table1, table2, "municipi")$join()$join_search
}
