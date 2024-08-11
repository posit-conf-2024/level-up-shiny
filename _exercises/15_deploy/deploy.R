library(rsconnect)

# If you haven't yet connected RStudio to pub.conf.posit.team
# you can run this interactively:
# connectUser(server = "pub.conf.posit.team")

app_to_deploy <- "15_01_app.R"

deployApp(
  appDir = here::here("_exercises/15_deploy"),
  appFiles = app_to_deploy,
  appPrimaryDoc = app_to_deploy,
  appName = "find-a-school",
  server = "pub.conf.posit.team",
  forceUpdate = TRUE,
  # You don't normally need appMode but rsconnect might get confused by the
  # workshop website that's in this repo.
  appMode = "shiny"
)
