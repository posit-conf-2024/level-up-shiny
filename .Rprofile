if (!exists(".__r_profile_loaded") || !isTRUE(.__r_profile_loaded)) {
  if (Sys.getenv("R_CONFIG_ACTIVE") != "rstudio_cloud") {
    source("renv/activate.R")
  } else if (requireNamespace("renv", quietly = TRUE)) {
    .libPaths(c(renv::paths$library(), .libPaths()))
  }
  
  if (Sys.getenv("R_CONFIG_ACTIVE") == "" && requireNamespace("usethis", quietly = TRUE)) {
    message("i Loaded usethis for local development.")
    library(usethis)
  }

  .update_packages <- function() {
    source("renv/activate.R")
    renv::restore()

    message("i Packages updated!")
    message("! Restart your R session to use the latest packages.")
  }

  .update_project <- function() {
    message("Stashing your changes...")
    system("git stash")

    message("Pulling the latest changes...")
    system("git pull")

    message("Applying your changes...")
    system("git stash pop")

    message("All set! You're up to date now :)")
  }

  .__r_profile_loaded <<- TRUE
}  
