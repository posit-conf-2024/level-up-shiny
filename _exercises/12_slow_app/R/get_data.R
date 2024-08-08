fetch_from_api <- function(x, factor = 2) {
  if (getOption("fetch_local", FALSE)) {
    return(x)
  }
  
  # Pretend like we're talking to the API
  Sys.sleep(runif(1, 0, 1) * factor)

  x
}

add_latest_scorecard <- function(school) {
  latest_scores <- 
    collegeScorecard::scorecard |>
    dplyr::filter(academic_year == "2020-21") |>
    dplyr::select(
      id,
      cost_avg,
      amnt_earnings_med_10y,
      score_sat_avg,
      dplyr::starts_with("cost_avg_income")
    )

  dplyr::left_join(school, latest_scores, by = "id")
}

get_schools_in_state <- function(state) {
  collegeScorecard::school |>
    dplyr::filter(state == !!state) |>
    fetch_from_api(factor = 1) |>
    add_latest_scorecard()
}

get_schools_in_state_with_sat <- function(state, sat_score) {
  sat_scores <- 
    collegeScorecard::scorecard |>
    dplyr::filter(score_sat_avg >= !!sat_score) |>
    fetch_from_api(factor = 3)

  collegeScorecard::school |>
    dplyr::filter(state == !!state) |> 
    dplyr::semi_join(sat_scores, by = "id") |>
    add_latest_scorecard()
}
