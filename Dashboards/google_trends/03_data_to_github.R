# Transfer dashboard data from OneDrive to Github

# Move data to github folder ---------------------------------------------------
IN_PATH <- file.path(dropbox_file_path, "Data", "google_trends", "DashboardData",
                     "data")

OUT_PATH <- file.path(github_file_path, "Dashboards", "google_trends", 
                      "data")

i <- 1
temp <- list.files(IN_PATH, pattern = "*.Rds|.*png") %>%
  lapply(function(file_i){
    if((i %% 100) %in% 0) print(i)
    i <<- i + 1
    
    file.copy(file.path(IN_PATH, file_i),
              paste0(OUT_PATH, "/"),
              overwrite=T)
  })

# Move correlation gif to github folder ----------------------------------------
file.copy(file.path(dropbox_file_path, "Data", "google_trends", "Outputs", 
                    "cor_gif", "cor.gif"),
          file.path(github_file_path, "Dashboards", "google_trends", "www/"),
          overwrite=T)

# Move keyword/langauge files to github folder ---------------------------------
keywords <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", 
                              "keywords", "FinalData", "covid_keywords_alllanguages_clean.Rds"))
saveRDS(keywords, file.path(github_file_path, "Dashboards", "google_trends", "data", "covid_keywords.Rds"))

languages <- read.csv(file.path(dropbox_file_path, "Data", "country_primary_language", "countries_lang.csv"), stringsAsFactors = F)
write.csv(languages, file.path(github_file_path, "Dashboards", "google_trends", "data", "countries_lang.csv"), row.names = F)


