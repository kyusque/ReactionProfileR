
#' Get energy value of the entry path
#'
#' @param dirpath entry path
#'
#' @return numeric the energy value
#' @importFrom dplyr %>%
#'
get_energy_by <- function(dirpath){
  dirpath %>%
    paste0("/file") %>%
    readLines %>%
    as.numeric
}
