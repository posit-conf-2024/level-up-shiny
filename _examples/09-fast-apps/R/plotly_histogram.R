plotly_expandable_histogram <- function(
  data, 
  x_var,
  x_title = "",
  bar_color = "white",
  opacity = 0.9,
  font_color = NULL
) {
  plot_ly(
    data,
    x = ~ get(x_var),
    type = "histogram",
    nbinsx = 10,
    color = I(bar_color),
    opacity = opacity
  ) |>
    layout(
      title = "",
      xaxis = list(visible = FALSE, showgrid = FALSE, title = x_title),
      yaxis = list(visible = FALSE, showgrid = FALSE, title = "Count"),
      hovermode = "x+y",
      margin = list(t = 0, r = 0, l = 0, b = 0),
      font = list(color = font_color),
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent"
    ) |>
    config(displayModeBar = FALSE) |>
    htmlwidgets::onRender(
      "function(el) {
          el.closest('.bslib-value-box')
            .addEventListener('bslib.card', function(ev) {
              Plotly.relayout(el, {
                'xaxis.visible': ev.detail.fullScreen,
                'yaxis.visible': ev.detail.fullScreen,
                'margin.l': ev.detail.fullScreen ? 40 : 0,
              });
            })
        }"
    )
}