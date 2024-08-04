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
