# Compare search interest across search terms in US

dir.create(file.path(gtrends_dir, "RawData", "search_interest_refivermectin_across_terms_us"))

#"2021-03-01_2021-05-31",
#"2021-03-01_2021-09-30"

for(time_span in c("2020-12-01_2021-09-30",
                   "2020-12-01_2021-05-31")){
  print(time_span)
  
  for(keyword_i in c("can i get the covid vaccine",
                     "covid vaccine appointment",
                     "vaccine appointment",
                     "covid vaccine center",
                     
                     "covid vaccine",
                     "covid vaccine priority",
                     "covid vaccine priority list",
                     "covid vaccine approved",
                     "is covid vaccine approved", ## COULD DELETE
                     "covid vaccine second dose",
                     "vaccine near me", ## COULD DELETE
                     "where to get covid vaccine",
                     "vaccine",
                     
                     "covid vaccine blood clots",
                     "covid vaccine safety",
                     "covid vaccine sick", ## COULD DELETE
                     "covid vaccine side effects",
                     "safety of covid vaccine",
                     "vaccine allergy",
                     "long term effects of covid vaccine",
                     "vaccine reaction",
                     "fear of needles",
                     "needle phobia",
                     "trypanophobia",
                     
                     "covid microchip",
                     "covid vaccine microchip",
                     "covid vaccine cause infertility",
                     "covid vaccine infertility",
                     "covid vaccine change dna",
                     "does covid vaccine change dna",
                     "covid vaccine dangerous",
                     "is the covid vaccine the mark of the beast",
                     "covid vaccine mercury")){
    
    PATH_OUT <- file.path(gtrends_dir, "RawData", "search_interest_refivermectin_across_terms_us", 
                          paste0("gtrends_missinfo_",keyword_i,"_", time_span, ".Rds"))
    
    if(!file.exists(PATH_OUT)){
      
      out <- gtrends(keyword = c(keyword_i,
                                 "ivermectin"),
                     geo = c("US"),
                     time = time_span %>% str_replace_all("_", " "),
                     onlyInterest = F)
      
      saveRDS(out, PATH_OUT)
    }
  }
}





