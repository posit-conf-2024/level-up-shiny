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
