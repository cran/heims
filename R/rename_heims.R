#' Make HEIMS element nos human-readable
#' @name element_decoders
#' @param DT The data table with original names
#' @param v A vector of element names.
#' @return \code{DT} with the new names or the vector with the names translated.
#' @details See \code{\link{heims_data_dict}}. Note that \code{\link{decode_heims}} is generally better,
#' as it decodes the variable if a decoder is present in the dictionary.
#'
#' \code{element2name} is the inverse of \code{\link{browse_elements}}:
#' given an element like \code{E306}, it returns
#' the name (\code{HE_Provider_cd}.)


#' @rdname element_decoders
#' @export
rename_heims <- function(DT){
  DTnoms <- names(DT)
  long_name <- orig_name <- NULL
  decoder <-
    lapply(heims_data_dict, function(x) data.table(long_name = unlist(x[names(x) == "long_name"]),
                                                   orig_name = unlist(x[names(x) == "orig_name"]))) %>%
    rbindlist(use.names = TRUE, fill = TRUE) %>%
    .[orig_name %in% DTnoms] %>%
    .[, long_name := if_else(long_name %fin% DTnoms, paste0(long_name, "_orig"), long_name)] %>%
    .[, long_name := if_else(long_name == "suburb", "Suburb_ABS", long_name)]

  setnames(DT,
           old = decoder[["orig_name"]],
           new = decoder[["long_name"]])
  DT
}

#' @rdname element_decoders
#' @export element2name
element2name <- function(v){
  long_name <- v. <- NULL

  v <- gsub("^e([0-9]+)$", "E\\1", v)

  v_nosuffix <-
   v %>%
    gsub("_[12]$", "", .) %>%
    gsub("A$", "", .)

  decoder <-
    lapply(heims_data_dict, function(x){
      data.table(long_name = unlist(x[names(x) == "long_name"]),
                 v. = unlist(x[names(x) == "orig_name"]))
    }) %>%
    rbindlist(use.names = TRUE, fill = TRUE) %>%
    .[v. %in% v_nosuffix]

  is_initial <- semester <- NULL
  input <-
    data.table(v. = v) %>%
    # A at the end means 'initial'
    .[, is_initial := grepl("^E[0-9]+A$", v., perl = TRUE)] %>%
    .[, semester := if_else(grepl("^E[0-9]+_[12]$", v., perl = TRUE),
                            gsub("^.*_([12])$", "\\1", v., perl = TRUE),
                            NA_character_)] %>%
    .[, v. := gsub("^(E[0-9]+)A$", "\\1", v., perl = TRUE)] %>%
    .[, v. := gsub("_[12]", "", v., perl = TRUE)]

  decoder %>%
    .[input, on = "v."] %>%
    # Retain the name if join unsuccessful
    .[, long_name := if_else(is.na(long_name), v., long_name)] %>%
    # Use _init if suffix was A
    .[, long_name := if_else(!is_initial, long_name, paste0(long_name, "_init"))] %>%
    # if suffix _1 or _2, reuse in long_name
    .[, long_name := if_else(is.na(semester), long_name, paste0(long_name, "_", semester))] %>%
    .[["long_name"]]
}




