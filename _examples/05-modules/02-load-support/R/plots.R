plot_cost_tuition <- function(scorecard, fill_color = "#007bc2") {
  scorecard |>
    mutate(academic_year = as.integer(substr(academic_year, 1, 4))) |>
    ggplot() +
    aes(x = academic_year, y = cost_tuition_in) +
    geom_col(fill = fill_color, na.rm = TRUE) +
    labs(
      x = "Academic Year",
      y = NULL
    ) +
    scale_y_continuous(labels = scales::label_dollar())
}
