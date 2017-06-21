#' Browse elements for description
#' @param pattern A case-insensitive perl expression or expressions to match in the long name of \code{\link{heims_data_dict}}.
#' @return A \code{data.table} of all element-long name combinations matching the perl regular expression.
#' @examples browse_elements(c("ProViDer", "Maj"))
#' @export browse_elements

browse_elements <- function(pattern){
  if (length(pattern) > 1) {
    pattern <- paste0("(", paste0(pattern, collapse = ")|("), ")")
  }

  long_name <- NULL

  lapply(heims_data_dict, function(x){
    data.table(long_name = unlist(x[names(x) == "long_name"]),
               orig_name = unlist(x[names(x) == "orig_name"]))
  }) %>%
    rbindlist(use.names = TRUE, fill = TRUE) %>%
    .[grepl(pattern, long_name, perl = TRUE, ignore.case = TRUE)]
}
