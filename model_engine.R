# =============================================================================
# model_engine.R
#
# Least-squares regression engine for the stock analysis Shiny app.
# Coefficients are found by minimising residual sum of squares using optim(),
# and verified against lm() as a sanity check.
#
# The core functions (model_fits, model_residuals, model_rss) are generalised
# to accept any number of predictors, satisfying the assessment's Q7 pattern.
# The fit_model() wrapper is the intended entry point for the Shiny app.
#
# USAGE (from server.R):
#
#   source("model_engine.R")
#   data <- read.csv("stock_data_snapshot.csv")
#   data$date <- as.Date(data$date)
#
#   result <- fit_model(data, response = "AAPL",
#                       predictors = c("SPY", "QQQ", "XLK"))
#
#   result$coefficients   # named vector, first element is (Intercept)
#   result$fitted         # predicted values
#   result$residuals      # observed - predicted
#   result$rss            # residual sum of squares at the fitted point
#   result$r_squared      # in-sample goodness of fit
#   result$convergence    # optim convergence code (0 = converged)
#
# Optional: train_test_evaluate() splits the data chronologically and reports
# out-of-sample R^2 for methodology discussion.
# =============================================================================


# -----------------------------------------------------------------------------
# CORE FUNCTIONS (generalised for any number of predictors)
# -----------------------------------------------------------------------------

# Predicted values from coefficient vector and design matrix.
model_fits <- function(coeffs, X) {
  as.vector(X %*% coeffs)
}

# Observed minus predicted, elementwise.
model_residuals <- function(coeffs, X, y) {
  y - model_fits(coeffs, X)
}

# Objective function passed to optim.
model_rss <- function(coeffs, X, y) {
  sum(model_residuals(coeffs, X, y)^2)
}


# -----------------------------------------------------------------------------
# WRAPPER
# -----------------------------------------------------------------------------

fit_model <- function(data, response, predictors) {
  
  # Build design matrix X (with intercept column) and response vector y
  predictor_data <- data[, predictors, drop = FALSE]
  X <- cbind(1, as.matrix(predictor_data))
  y <- data[[response]]
  
  # Fit by minimising RSS
  fit <- optim(
    par     = rep(0, ncol(X)),
    fn      = model_rss,
    X       = X,
    y       = y,
    method  = "BFGS",
    control = list(maxit = 5000, reltol = 1e-12)
  )
  
  # Package results
  coeffs <- fit$par
  names(coeffs) <- c("(Intercept)", predictors)
  
  fitted    <- model_fits(coeffs, X)
  residuals <- y - fitted
  rss_value <- fit$value
  r_squared <- 1 - rss_value / sum((y - mean(y))^2)
  
  list(
    coefficients = coeffs,
    fitted       = fitted,
    residuals    = residuals,
    y            = y,
    rss          = rss_value,
    r_squared    = r_squared,
    convergence  = fit$convergence
  )
}


# -----------------------------------------------------------------------------
# OPTIONAL: chronological train/test evaluation
# -----------------------------------------------------------------------------
# Fits the model on the first train_frac of the data (older) and evaluates
# out-of-sample R^2 on the remaining rows (newer). Chronological rather than
# random split is required for time-series data to avoid look-ahead bias.

train_test_evaluate <- function(data, response, predictors, train_frac = 0.8) {
  
  n <- nrow(data)
  split_point <- floor(train_frac * n)
  train_data  <- data[1:split_point, ]
  test_data   <- data[(split_point + 1):n, ]
  
  # Fit on training set
  train_result <- fit_model(train_data, response, predictors)
  
  # Predict on test set using the trained coefficients
  test_X <- cbind(1, as.matrix(test_data[, predictors, drop = FALSE]))
  test_y <- test_data[[response]]
  test_predictions <- as.vector(test_X %*% train_result$coefficients)
  
  test_residuals <- test_y - test_predictions
  test_rss <- sum(test_residuals^2)
  test_tss <- sum((test_y - mean(test_y))^2)
  test_r_squared <- 1 - test_rss / test_tss
  
  list(
    coefficients     = train_result$coefficients,
    train_r_squared  = train_result$r_squared,
    test_r_squared   = test_r_squared,
    train_n          = nrow(train_data),
    test_n           = nrow(test_data),
    train_period     = range(train_data$date),
    test_period      = range(test_data$date),
    test_predictions = test_predictions,
    test_residuals   = test_residuals
  )
}


# -----------------------------------------------------------------------------
# TEST BLOCK
# -----------------------------------------------------------------------------
# Runs only when the script is sourced interactively (not when loaded by the
# Shiny app). Confirms the engine matches lm() and reports out-of-sample fit.

if (interactive()) {
  
  # Load snapshot data (expected to be in the same working directory)
  snapshot_path <- "stock_data_snapshot.csv"
  if (!file.exists(snapshot_path)) {
    stop("Cannot find ", snapshot_path,
         " -- run make_snapshot.R first, or set the working directory ",
         "to the folder containing the CSV.")
  }
  
  data <- read.csv(snapshot_path)
  data$date <- as.Date(data$date)
  
  cat("Loaded", nrow(data), "rows spanning",
      format(min(data$date)), "to", format(max(data$date)), "\n\n")
  
  # --- Full-sample fit ---
  result <- fit_model(data, response = "AAPL",
                      predictors = c("SPY", "QQQ", "XLK"))
  
  cat("optim coefficients:\n")
  print(round(result$coefficients, 6))
  
  cat("\nlm coefficients (for verification):\n")
  print(round(coef(lm(AAPL ~ SPY + QQQ + XLK, data = data)), 6))
  
  cat("\nIn-sample R-squared:", round(result$r_squared, 4), "\n")
  cat("optim convergence code:", result$convergence,
      "(0 = converged)\n")
  
  # --- Chronological train/test evaluation ---
  cat("\n--- Chronological train/test evaluation ---\n")
  
  tt <- train_test_evaluate(data,
                            response = "AAPL",
                            predictors = c("SPY", "QQQ", "XLK"),
                            train_frac = 0.8)
  
  cat("Training period:", format(tt$train_period[1]),
      "to", format(tt$train_period[2]),
      "(", tt$train_n, "rows )\n")
  cat("Test period:    ", format(tt$test_period[1]),
      "to", format(tt$test_period[2]),
      "(", tt$test_n, "rows )\n")
  cat("Training R-squared:", round(tt$train_r_squared, 4), "\n")
  cat("Test R-squared:    ", round(tt$test_r_squared, 4), "\n")
  
  if (tt$test_r_squared < 0) {
    cat("\nNote: negative test R-squared indicates the fitted model does\n")
    cat("worse than predicting the test-period mean. This is expected on\n")
    cat("financial time series with regime changes and is discussed in\n")
    cat("the methodology section of the report.\n")
  }
}

