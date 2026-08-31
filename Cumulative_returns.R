make_cumulative_return_plot <- function(df) {
  stocks <- setdiff(names(df), "date")
  p <- plotly::plot_ly()

  for (stock in stocks) {
    cumulative_return <- 100 * cumsum(df[[stock]])

    p <- plotly::add_lines(
      p,
      x = df$date,
      y = cumulative_return,
      name = stock,
      text = paste0(
        "Stock: ", stock,
        "<br>Date: ", df$date,
        "<br>Cumulative return: ",
        round(cumulative_return, 2), "%"
      ),
      hoverinfo = "text"
    )
  }

  plotly::layout(
    p,
    title = "Cumulative Stock Returns",
    xaxis = list(title = "Date"),
    yaxis = list(title = "Cumulative Return (%)"),
    hovermode = "x unified",
    legend = list(
      orientation = "h",
      x = 0,
      y = 1.1
    )
  )
}
