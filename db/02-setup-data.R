if (length(commandArgs(trailingOnly = TRUE)) != 1) {
  database_name <- "college_scorecard"
} else {
  database_name <- commandArgs(trailingOnly = TRUE)[1]
}

library(dplyr, warn.conflicts = FALSE)
library(dbplyr, warn.conflicts = FALSE)
library(DBI)
library(collegeScorecard)

dotenv::load_dot_env(here::here(".secrets/db-admin.env"))

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("DB_HOST"),
  port = Sys.getenv("DB_PORT"),
  dbname = database_name,
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)

cli::cli_progress_step("Creating table {.field school} in {.strong {database_name}}")
dbWriteTable(con, "school", school, overwrite = TRUE)

cli::cli_progress_step("Creating table {.field scorecard} in {.strong {database_name}}")
dbWriteTable(con, "scorecard", scorecard, overwrite = TRUE)

cli::cli_progress_done()
dbDisconnect(con)
