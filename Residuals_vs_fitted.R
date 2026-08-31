make_residuals_vs_fitted_plot <- function(m) {
  residual_data <- data.frame(
    Fitted = as.numeric(m$fitted),
    Residual = as.numeric(m$residuals)
  )

  plotly::plot_ly(
    data = residual_data,
    x = ~Fitted,
    y = ~Residual,
    type = "scatter",
    mode = "markers",
    name = "Residuals",
    text = ~paste0(
      "Fitted: ", round(Fitted, 4),
      "<br>Residual: ", round(Residual, 4)
    ),
    hoverinfo = "text"
  ) |>
    plotly::add_lines(
      x = range(residual_data$Fitted, na.rm = TRUE),
      y = c(0, 0),
      name = "Zero residual",
      line = list(dash = "dash"),
      inherit = FALSE
    ) |>
    plotly::layout(
      title = "Residuals vs Fitted Returns",
      xaxis = list(title = "Fitted Return"),
      yaxis = list(title = "Residual")
    )
}
