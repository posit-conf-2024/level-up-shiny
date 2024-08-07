library(dplyr)
library(dbplyr)
library(DBI)
library(collegeScorecard)

dotenv::load_dot_env(here::here(".secrets/db-user.env"))

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("DB_HOST"),
  port = Sys.getenv("DB_PORT"),
  dbname = Sys.getenv("DB_DATABASE"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)

tbl(con, "school")
tbl(con, "scorecard")
