#' @title Find the optimal combinations of arguments for each diagnostic
#'
#' @description The measure of optimality is typically diagnostic dependent; for example,
#' we wish to minimise the RMSE and run time, but we want coverage
#' to be as close to the purported value as possible. Hence,
#' \code{optimal_arguments()} allows one to set the optimality criteria individually for each diagnostic.
#'
#' @param object an object of class \code{'testargs'}
#' @param optimality_criterion a function (or list of functions) that defines the optimality criterion for each diagnostic.
#' Each function should return a single positive integer indicating the index of the optimal argument combination.
#' If a named list is provided with less elements than the number of diagnostic scores, unspecified diagnostics are assumed to be negatively oriented (i.e., assigned optimality criterion \code{which.min})
#' @return A \code{data.frame}; each row corresponds to one of the diagnostics (specified by the row names), and the columns contain the argument values that optimise the corresponding diagnostic. The diagnostics at each of these optimal argument combinations are also included
#' @export
#' @examples
#' ## See ?test_arguments
optimal_arguments <- function(object, optimality_criterion = which.min) {

  if(!is(object, "testargs"))
    stop("object should be of class testargs")

  if (is.list(optimality_criterion)) {

    if(length(optimality_criterion) != length(object@diagnostic_names)) {
      ## The argument optimality_criterion can be a named list, possibly with
      ## less elements than the number of diagnostic scores:
      ## unspecified diagnostics are assumed to be negatively oriented (i.e., assigned
      ## optimality criterion which.min)
      if (!all(names(optimality_criterion) %in% object@diagnostic_names))
        stop("optimality_criterion is a named list: Its names should be in the given diagnostic names")

      ## Define an optimality criterion for the unspecified diagnostic names
      idx <- which(!(object@diagnostic_names %in% names(optimality_criterion)))
      for (i in idx) {
        optimality_criterion[[object@diagnostic_names[i]]] <- which.min
      }
    } else {
      if(is.null(names(optimality_criterion))) {
        ## Name the elements of the list according to the diagnostic names
        warning("optimality_criterion is an unnamed list: Assuming that the order of optimality_criterion is the same as that of object@diagnostic_names")
        names(optimality_criterion) <- object@diagnostic_names
      } else if (!all(names(optimality_criterion) %in% object@diagnostic_names)){
          stop("optimality_criterion is a named list: Its names should be in the given diagnostic names")
      }
    }

    optimal_idx <- sapply(object@diagnostic_names,
                          function(i) {
                            x <- object@diagnostics_df[, i, drop = T]
                            optimality_criterion[[i]](x)
                          })
  } else {
    optimal_idx <- sapply(object@diagnostics_df[, object@diagnostic_names, drop = F],
                          function(x) optimality_criterion(x))
  }

  out <- object@diagnostics_df[optimal_idx, ]
  which_diagnostic_optimal <- object@diagnostic_names
  rownames(out) <- which_diagnostic_optimal

  ## Reorder the data.frame so that the argument columns appear before the diagnostic columns
  out <- out[, c(object@arg_names, object@diagnostic_names)]

  return(out)
}
