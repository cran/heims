.onLoad <- function(libname = find.package("heims"), pkgname = "heims"){
  if (getRversion() >= "2.15.1"){
    utils::globalVariables(c("heims_data_dict",
                             "first_levels",
                             "."))
  }
}
