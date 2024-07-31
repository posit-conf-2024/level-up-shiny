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

  message("i Loaded project root .Rprofile.")
  .__r_profile_loaded <<- TRUE
}  
