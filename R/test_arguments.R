#' Test (multiple) arguments of a prediction algorithm
#'
#' Test the performance of a prediction algorithm over a range of argument
#' values. Multiple arguments can be tested simultaneously.
#'
#'
#' For each combination of the supplied argument levels, the value of
#' \code{pred_fun()} is combined with \code{df_test} using \code{cbind()},
#' which is then passed into \code{diagnostic_fun()} to compute the diagnostics.
#' Since the number of columns in the returned value of \code{pred_fun()} is arbitrary,
#' one can test both predictions and uncertainty quantification of the predictions
#' (e.g., by including prediction standard errors or predictive interval bounds)
#'
#' @param pred_fun The prediction algorithm to be tested.
#' It should be a function with formal arguments \code{df_train} and \code{df_test}, which are data used to train the model and test out-of-sample predictive
#' performance, respectively, as well as any arguments which are to be tested.
#' The value of \code{pred_fun} should be a matrix-like object
#' with named columns and the same number of rows as \code{df_test}
#' @param df_train training data
#' @param df_test testing data
#' @param arguments named list of arguments and their values to check
#' @param diagnostic_fun the criteria with which the predictive performance will be assessed
#' @export
#' @return an object of class \code{'testargs'} containing all information from the testing procedure
#' @seealso \code{\link{plot_diagnostics}}, \code{\link{optimal_arguments}}
#' @examples
#' library("testarguments")
#'
#' ## Simulate training and testing data
#' RNGversion("3.6.0"); set.seed(1)
#' n  <- 1000                                          # sample size
#' x  <- seq(-1, 1, length.out = n)                    # covariates
#' mu <- exp(3 + 2 * x * (x - 1) * (x + 1) * (x - 2))  # polynomial function in x
#' Z  <- rpois(n, mu)                                  # simulate data
# plot(x, Z) + lines(x, mu, col = "red")      # visualise the data and the true mean
#' df       <- data.frame(x = x, Z = Z, mu = mu)
#' train_id <- sample(1:n, n/2, replace = FALSE)
#' df_train <- df[train_id, ]
#' df_test  <- df[-train_id, ]
#'
#' ## Algorithm that uses df_train to predict over df_test. We use glm(), and
#' ## test the degree of the regression polynomial and the link function.
#' pred_fun <- function(df_train, df_test, degree, link) {
#'
#'   M <- glm(Z ~ poly(x, degree), data = df_train,
#'            family = poisson(link = as.character(link)))
#'
#'   ## Predict over df_test
#'   pred <- as.data.frame(predict(M, df_test, type = "link", se.fit = TRUE))
#'
#'   ## Compute response level predictions and 90% prediction interval
#'   inv_link <- family(M)$linkinv
#'   fit_Y <- pred$fit
#'   se_Y  <- pred$se.fit
#'   pred <- data.frame(fit_Z = inv_link(fit_Y),
#'                      upr_Z = inv_link(fit_Y + 1.645 * se_Y),
#'                      lwr_Z = inv_link(fit_Y - 1.645 * se_Y))
#'
#'   return(pred)
#' }
#'
#' ## Define diagnostic function. Should return a named vector
#' diagnostic_fun <- function(df) {
#'   with(df, c(
#'     RMSE = sqrt(mean((Z - fit_Z)^2)),
#'     MAE = mean(abs(Z - fit_Z)),
#'     coverage = mean(lwr_Z < mu & mu < upr_Z)
#'   ))
#' }
#'
#' ## Compute the user-defined diagnostics over a range of argument levels
#' testargs_object <- test_arguments(
#'   pred_fun, df_train, df_test, diagnostic_fun,
#'   arguments = list(degree = 1:6, link = c("log", "sqrt"))
#' )
#'
#' ## Visualise the performance across all combinations of the supplied arguments
#' plot_diagnostics(testargs_object)
# ggsave("./img/nres_link.png", device = "png", width = 6, height = 3)
#'
#' ## Focus on a subset of the tested arguments
#' plot_diagnostics(testargs_object, focused_args = "degree")
# ggsave("./img/nres.png", device = "png", width = 6, height = 3)
#'
#' ## Compute the optimal arguments for each diagnostic
#' optimal_arguments(
#'   testargs_object,
#'   optimality_criterion = list(coverage = function(x) which.min(abs(x - 0.90)))
#' )
test_arguments <- function(pred_fun, df_train, df_test, diagnostic_fun, arguments) {

  if(!all(names(arguments) %in% names(formals(pred_fun))))
    stop("names of arguments do not match the argument names of pred_fun")

  if(!all(c("df_train", "df_test") %in% names(formals(pred_fun))))
    stop("pred_fun should have formal arguments 'df_test' and 'df_train'")

  ## Every combination of the arguments
  ## NB: This assumes that arguments are atomic only
  arguments <- expand.grid(arguments)

  ## iterate over the rows of all combinations of arguments
  diagnostics <- ldply(seq_len(nrow(arguments)), function(i) {

    ## Convert current arguments to a list
    current_arguments <- as.list(arguments[i, ])

    ## Fit, predict, and record time
    ## Also need to pass in the training data and validation data

    current_arguments <- c(list(df_test = df_test, df_train = df_train),
                           current_arguments)

    time <- system.time({
      pred <- do.call(pred_fun, current_arguments)
    })["elapsed"]

    if (!is.matrix(pred) && !is.data.frame(pred))
      stop("pred_fun should return a matrix or data.frame")

    if (nrow(pred) != nrow(df_test))
      stop("pred_fun should return a matrix or data.frame with the *same number of rows* as df_test")

    if(is.null(names(pred))) {
      ## Some prediction algorithms return a matrix where the dimensions have
      ## names but names() is still null (e.g., lm()).
      if (!is.null(dimnames(pred)[[2]])) {
        names(pred) <- dimnames(pred)[[2]]
      } else {
        stop("pred_fun needs to return a matrix or data.frame with *named* columns")
      }
    }


    ## Incorporate prediction results to df_test
    df_test <- cbind(df_test, pred)

    ## Compute the diagnostics and add the time
    current_diagnostics <- c(diagnostic_fun(df_test), Time = unname(time))

    ## Convert to data.frame so we can add the current arugments (which may
    ## contain strings)
    current_diagnostics <- as.data.frame(as.list(current_diagnostics))

    ## record the current arguments
    current_diagnostics[names(arguments)] <- arguments[i, ]

    return(current_diagnostics)
  })

  return(new("testargs",
      diagnostics_df = diagnostics,
      arg_names = names(arguments),
      diagnostic_names = names(diagnostics)[which(!(names(diagnostics) %in% names(arguments)))]
      ))
}
