card_dark <- function(title, ...) {
  card(
    card_header(title),
    class = "text-bg-dark",
    card_body(
      padding = 0,
      ...
    )
  )
}


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

filter_scorecard_by_school_name <- function(scorecard, school, name) {
  school_by_name <- 
    school |>
    filter(name == {{ name }})

  scorecard |>
    semi_join(school_by_name, by = "id")
}

map_school <- function(school, name) {
  the_school <- school |> filter(name == {{ name }})

  leaflet() |>
    addTiles() |>
    addMarkers(
      lng = the_school$longitude,
      lat = the_school$latitude,
      popup = the_school$name
    )
}
