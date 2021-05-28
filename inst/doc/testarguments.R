## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = FALSE------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library("testarguments")
RNGversion("3.6.0"); set.seed(1)
n  <- 1000                                    # sample size
x  <- seq(-1, 1, length.out = n)              # covariates
Y  <- 3 + 2 * x * (x - 1) * (x + 1) * (x - 2) # linear predictor
mu <- exp(Y)                                  # mean of data
Z  <- rpois(n, mu)                            # simulate data
df       <- data.frame(x = x, Z = Z, mu = mu)
train_id <- sample(1:n, n/2, replace = FALSE) 
df_train <- df[train_id, ]                    # training set
df_test  <- df[-train_id, ]                   # testing set

## -----------------------------------------------------------------------------
pred_fun <- function(df_train, df_test, degree, link) {

  M <- glm(Z ~ poly(x, degree), data = df_train,
           family = poisson(link = as.character(link)))

  ## Predict over df_test
  pred <- as.data.frame(predict(M, df_test, type = "link", se.fit = TRUE))

  ## Compute response level predictions and 90% prediction interval
  inv_link <- family(M)$linkinv
  fit_Y <- pred$fit
  se_Y  <- pred$se.fit
  pred <- data.frame(fit_Z = inv_link(fit_Y),
                     upr_Z = inv_link(fit_Y + 1.645 * se_Y),
                     lwr_Z = inv_link(fit_Y - 1.645 * se_Y))

  return(pred)
}

## -----------------------------------------------------------------------------
diagnostic_fun <- function(df) {
  with(df, c(
    RMSE     = sqrt(mean((Z - fit_Z)^2)),
    MAE      = mean(abs(Z - fit_Z)),
    coverage = mean(lwr_Z < mu & mu < upr_Z)
  ))
}

## -----------------------------------------------------------------------------
testargs_object <- test_arguments(
  pred_fun, df_train, df_test, diagnostic_fun,
  arguments = list(degree = 1:6, link = c("log", "sqrt"))
)

## ---- fig.width=6, fig.height=3, fig.align='center'---------------------------
plot_diagnostics(testargs_object)

## ---- fig.width=6, fig.height=3, fig.align='center'---------------------------
plot_diagnostics(testargs_object, focused_args = "degree")

## -----------------------------------------------------------------------------
optimal_arguments(
  testargs_object,
  optimality_criterion = list(coverage = function(x) which.min(abs(x - 0.90)))
)

