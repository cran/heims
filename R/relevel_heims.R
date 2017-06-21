#' @title Relevel categorical variables
#' @description Changes categorical variables in a data.table to levels with a sensible reference level
#' @param DT A \code{data.table} post \code{\link{decode_heims}}.
#' @return The same data.table with character vectors changed to factors whose first level is
#' the level intended.
#' @importFrom stats relevel
#' @export
#'

relevel_heims <- function(DT){
  # CRAN NOTE avoidance: colname of first_levels
  Variable <- NULL

  for (j in seq_along(DT)){
    nom <- names(DT)[j]
    if (nom %in% first_levels$Variable){
      if (is.character(DT[[j]])){
        set(DT, j = j, value = as.factor(DT[[j]]))
        set(DT, j = j, value = relevel(DT[[j]], ref = first_levels[Variable == nom][["First_level"]]))
      }
    }
  }
  DT
}


