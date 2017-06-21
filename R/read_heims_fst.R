#' Read HEIMS data from decoded fst files
#'
#' @param filename File path to \code{.fst} file of a decoded HEIMS file (\code{\link{decode_heims}})
#' produced by \code{fst::write.fst}.
#' @return A \code{data.table} with appropriate attributes.
#' @export

read_heims_fst <- function(filename){
  file <- gsub("^.*((enrol)|(completions)|(course)|(load)).*$", "\\1", filename)

  out <- fst::read.fst(filename, as.data.table = TRUE)

  noms <- names(out)

  if ("CHESSN" %in% noms) {
    setattr(out$CHESSN, "class", "integer64")
  }

  Course_commencement_date <- NULL
  if ("Course_commencement_date" %in% noms) {
    setattr(out$Course_commencement_date, "class", "Date")
    out[, "Course_commencement_year" := year(Course_commencement_date)]
  }

  Course_start_date <- NULL
  if ("Course_start_date" %in% noms) {
    setattr(out$Course_start_date, "class", "Date")
    out[, "Cohort" := year(Course_start_date)]
  }

  Census_date <- NULL
  if ("Census_date" %in% noms) {
    setattr(out$Census_date, "class", "Date")
    out[, "Semester" := (month(Census_date) - 1L) %/% 6 + 1L]
  }

  if ("DOB" %in% noms) {
    setattr(out$DOB, "class", "Date")
  }

  major_id <- paste0(file, "_row_id")
  if (major_id %in% noms) {
    setkeyv(out, major_id)
  }

  # Put 'id' columns first
  setcolorder(out,
              c(intersect(noms,
                          c(major_id, "CHESSN", "Cohort",
                            "Ref_year", "row_id",
                            "HE_Provider_name", "Student_id", "Course_cd")),
                setdiff(noms,
                        c(major_id, "CHESSN", "Cohort",
                          "Ref_year", "row_id",
                          "HE_Provider_name", "Student_id", "Course_cd"))))

  out
}


