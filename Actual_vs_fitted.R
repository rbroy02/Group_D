make_actual_vs_fitted_plot <- function(m) {
  plot_data <- data.frame(
    Fitted = as.numeric(m$fitted),
    Actual = as.numeric(m$y)
  )

  plot_range <- range(
    c(plot_data$Fitted, plot_data$Actual),
    na.rm = TRUE
  )

  p <- plotly::plot_ly(
    data = plot_data,
    x = ~Fitted,
    y = ~Actual,
    type = "scatter",
    mode = "markers",
    name = "Observations",
    text = ~paste0(
      "Fitted: ", round(Fitted, 4),
      "<br>Actual: ", round(Actual, 4)
    ),
    hoverinfo = "text"
  )

  p <- plotly::add_lines(
    p,
    x = plot_range,
    y = plot_range,
    name = "Perfect fit",
    line = list(dash = "dash"),
    inherit = FALSE
  )

  plotly::layout(
    p,
    title = "Actual vs Fitted Returns",
    xaxis = list(title = "Fitted Return"),
    yaxis = list(title = "Actual Return")
  )
}
