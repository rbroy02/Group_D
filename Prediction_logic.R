prediction_inputs_ui <- function(predictors) {
  shiny::tagList(lapply(predictors, function(p) {
    shiny::numericInput(
      paste0("newval_", p),
      paste0(p, " return (as decimal, e.g. 0.02 = +2%)"),
      value = 0,
      step = 0.005,
      width = "260px"
    )
  }))
}


collect_prediction_values <- function(input, predictors) {
  sapply(predictors, function(p) {
    v <- input[[paste0("newval_", p)]]
    if (is.null(v) || is.na(v)) 0 else v
  })
}


calculate_target_prediction <- function(m, values) {
  sum(m$coefficients * c(1, as.numeric(values)))
}


format_prediction_percent <- function(prediction) {
  sprintf("%+.3f%%", 100 * prediction)
}


format_model_equation <- function(m, target_name) {
  terms <- paste0(
    sprintf("%+.4f", m$coefficients[-1]),
    " × ",
    m$predictors
  )

  paste0(
    target_name,
    " return = ",
    sprintf("%.5f", m$coefficients[1]),
    " ",
    paste(terms, collapse = " ")
  )
}
