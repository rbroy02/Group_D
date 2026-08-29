make_correlation_heatmap <- function(df) {
  numeric_df <- df[, setdiff(names(df), "date"), drop = FALSE]
  cor_matrix <- round(cor(numeric_df, use = "complete.obs"), 3)

  plotly::plot_ly(
    x = colnames(cor_matrix),
    y = rownames(cor_matrix),
    z = cor_matrix,
    type = "heatmap",
    text = cor_matrix,
    texttemplate = "%{text:.2f}"
  ) |>
    plotly::layout(
      xaxis = list(title = ""),
      yaxis = list(title = "")
    )
}
