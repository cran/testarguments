#' @title \code{'testargs'} class
#' @description This is the central class definition of the \code{testarguments} package, containing all information from a call to \code{\link{test_arguments}}
#'
#' @slot diagnostics_df a \code{data.frame} containing the diagnostics for each combination of the supplied arguments
#' @slot arg_names the argument names
#' @slot diagnostic_names the diagnostic names
setClass("testargs",
         representation(diagnostics_df = "data.frame",
                        arg_names = "character",
                        diagnostic_names = "character"))

