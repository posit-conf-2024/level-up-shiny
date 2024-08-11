library(rsconnect)
connectUser(server = "pub.conf.posit.team")

app_files <- here::here(
  "_examples/15-deploy",
  c("01_error_app.R", "02_log_app.R")
)

for (app_file in app_files) {
  app_name <- sub(".R", "", fs::path_file(app_file), fixed = TRUE)

  deployApp(
    appDir = dirname(app_file),
    appFiles = basename(app_file),
    appPrimaryDoc = basename(app_file),
    appName = app_name,
    appMode = "shiny",
    server = "pub.conf.posit.team",
    forceUpdate = TRUE
  )
}

# 01_error_app
# https://pub.conf.posit.team/content/09813081-85ea-4e41-8e4c-d568c46bf2ad

# 02_log_app
# https://pub.conf.posit.team/content/c75d0219-4970-4791-b763-e54d50add928

