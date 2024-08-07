library(dplyr)
library(dbplyr)
library(DBI)
library(collegeScorecard)

staging_env <- here::here("secrets/db-staging.env")
prod_env <- here::here("secrets/db-prod.env")

dotenv::load_dot_env(staging_env)

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
