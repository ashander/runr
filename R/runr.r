#' runr: a tidy simulation runner
#'
#' runr provides an opinionated wrapper to run simulations and recover tidy output.
#' 
#' The packages exports one function, `run`, which runs a function with some
#' varying parameters, and some fixed parameters. `run`, expects the varying
#' parameters in a data.frame and the fixed parameters in a list or environment.
#' It returns the results in an data.frame that can be `cbind`-ed to the input
#' data.frame.
#'
#' @docType package
#' @name runr
#' @import assertthat
NULL
