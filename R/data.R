## how to document datasets: you need to specify @docType and @name; do not
## forget NULL in the end

#' Some example data
#'
#' A example of isotope dataset.
#' @docType data
#' @name isotope
#' @examples data(isotope)
#' ANCOVAplot(isotope$d13C, isotope$d15N, isotope$area, isotope)
NULL
