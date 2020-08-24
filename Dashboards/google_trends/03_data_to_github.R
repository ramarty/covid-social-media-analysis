# Transfer dashboard data from OneDrive to Github

# Move telecom data to github folder -------------------------------------------
IN_PATH <- file.path(dropbox_file_path, "Data", "google_trends", "DashboardData",
                     "precomputed_figures")

OUT_PATH <- file.path(github_file_path, "Dashboards", "google_trends", 
                      "precomputed_figures")

i <- 1
temp <- list.files(IN_PATH, pattern = "*.Rds") %>%
  lapply(function(file_i){
    if((i %% 100) %in% 0) print(i)
    i <<- i + 1
    
    file.copy(file.path(IN_PATH, file_i),
              paste0(OUT_PATH, "/"),
              overwrite=T)
  })

# Move telecom data to github folder -------------------------------------------
IN_PATH <- file.path(dropbox_file_path, "Data", "google_trends", "DashboardData",
                     "data")

OUT_PATH <- file.path(github_file_path, "Dashboards", "google_trends", 
                      "data")

i <- 1
temp <- list.files(IN_PATH, pattern = "*.Rds") %>%
  lapply(function(file_i){
    if((i %% 100) %in% 0) print(i)
    i <<- i + 1
    
    file.copy(file.path(IN_PATH, file_i),
              paste0(OUT_PATH, "/"),
              overwrite=T)
  })

