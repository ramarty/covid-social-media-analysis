# Transfer dashboard data from OneDrive to Github



# Move telecom data to github folder -------------------------------------------
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

#### Keywords
keywords <- readRDS(file.path(dropbox_file_path, "Data", "google_trends", 
                              "keywords", "FinalData", "covid_keywords_alllanguages.Rds"))
write.csv(keywords, file.path(github_file_path, "Dashboards", "google_trends", "data", "covid_keywords.csv"))

languages <- read.csv(file.path(dropbox_file_path, "Data", "country_primary_language", "countries_lang.csv"), stringsAsFactors = F)
write.csv(languages, file.path(github_file_path, "Dashboards", "google_trends", "data", "countries_lang.csv"), row.names = F)


