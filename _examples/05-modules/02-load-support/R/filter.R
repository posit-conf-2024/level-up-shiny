filter_scorecard_by_school_name <- function(scorecard, school, name) {
  school_by_name <- 
    school |>
    filter(name == {{ name }})

  scorecard |>
    semi_join(school_by_name, by = "id")
}
