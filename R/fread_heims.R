#' Read raw HEIMS file
#' @param filename A text-delimited file, passed to \code{fread} from \code{data.table}.
#' @return A \code{data.table} with column names in ascending (lexicographical) order and
#' any columns starting with \code{e} will be uppercase.
#' @details The strings \code{"" "NA" "?" "." "*" "**"} are treated as missing, as well as \code{ZZZZZZZZZZ}
#' (so students without a CHESSN will be marked with the \code{integer64} missing value).
#' @export

fread_heims <- function(filename){
  file <- gsub("^.*((enrol)|(completions)|(load)).*$", "\\1", filename)

  fread(filename,
        na.strings = c("", "NA", "?", ".", "*", "**",
                             # CHESSN
                             "ZZZZZZZZZZ"),
        # course file does not contain "E313"
        colClasses = switch(file,
                            "enrol" = list(character = c("E313", "E347"),
                                           numeric = c("ASGC_remote", "remote")),
                            "completions" = list(character = c("E313", "E347")),
                            "load" = list(character = c("E313")),
                            NULL)) %>%
    setnames(grep("^e", names(.), value = TRUE),
             gsub("^e", "E", grep("^e", names(.), value = TRUE))) %>%
    setcolorder(sort(names(.)))
}
