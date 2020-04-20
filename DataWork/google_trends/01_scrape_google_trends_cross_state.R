# Scrape Data from Google Trends

# RESOURCES
# https://github.com/GeneralMills/pytrends
# https://support.google.com/trends/answer/4365533?hl=en

# Grab admin codes to scrape ---------------------------------------------------
data("countries")
brazil_search <- countries[grepl("BR-", countries$sub_code),] 
brazil_search$sub_code <- brazil_search$sub_code %>% as.character()
brazil_search$name <- brazil_search$name %>% as.character()
brazil_search$id <- 1:nrow(brazil_search)

brazil_search$search_group <- rep(1:550, each=5)[1:nrow(brazil_search)]

# Categories ----
data("categories")

# Scrape Data ------------------------------------------------------------------
## Loop through search terms

search_terms <- c(
  "tosse",
  "febre",
  "cansaço",
  "dificuldade ao respirar",
  "perda de olfato",
  "dor nos olhos",
  
  "ventiladores",
  "desinfetantes",
  "termômetros",
  "leito de hospital ou hospitalar",
  "hidroxicloroquina",
  "máscara facial",
  "papel higiênico",
  "médico",
  "médica",
  
  "fique em casa",
  "lavar as mãosv",
  "distância social",
  
  "Educação online",
  "desemprego",
  "teletrabalho",
  "vdívida",
  "terapia",
  "psicologia",
  "Violência baseada no gênero ou violência contra a mulher",
  
  "quais são os sintomas do coronavírus",
  "eu tenho coronavírus",
  "Perdi o olfato",
  "Estou com falta de ar",
  "meus olhos doem",
  "estou com febre",
  "como tratar o coronavírus",
  "teste de coronavírus",
  "número de emergência coronavírus",
  "ajuda do coronavírus",
  "Eu fico em casa",
  "Isolamento social",
  "profissionais de saúde",
  "medicos",
  "cloroquina",
  "isolamento vertical",
  "volta brasil"
)





results_df <- lapply(search_terms, 
                     function(term){
                       
                       print(term)
                       
                       out_all <- gtrends(term, 
                                          category = "0",
                                          geo = "BR",
                                          time = "today 3-m",
                                          onlyInterest=F,
                                          low_search_volume=T)
                       out_region <- out_all$interest_by_region
                       out_city <- out_all$interest_by_city
                       
                       out_region$unit_level <- "region"
                       out_city$unit_level <- "city"
                       
                       out <- bind_rows(out_region,
                                        out_city)
                       
                       out$time_extracted <- Sys.time()
                       
                       return(out)
                       
                     }) %>%
  bind_rows()





# Export -----------------------------------------------------------------------
saveRDS(results_df, file.path(dropbox_file_path, "Data/google_trends/RawData/brazil_extract_crosstate.Rds"))
write.csv(results_df, file.path(dropbox_file_path, "Data/google_trends/RawData/brazil_extract_crosstate.csv"), row.names=F)

