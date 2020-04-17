# Load data ---------------------------------------------------
data <- read.csv("https://raw.githubusercontent.com/datasciencecampus/google-mobility-reports-data/master/csvs/international_local_area_trends_G20_20200410.csv",
                 encoding="UTF-8", stringsAsFactors=FALSE)

# Clean and filter data ---------------------------------------
brazil <- data %>% 
    janitor::clean_names() %>% 
    filter(country == "BR") %>% 
    pivot_longer(
        cols = starts_with("x"),
        names_to = "date", 
        values_to = "value"
    ) %>% 
    mutate(
        date = gsub("x", "", date),
        date = gsub("\\_", "-", date),
        date = as.Date(date),
        location = gsub("State of", "", location),
        location = gsub("^\\s+|\\s+$", "", location),
        location = ifelse(location == "Federal District", "Distrito Federal", location)
    )

# Export ------------------------------------------------------
saveRDS(brazil,   file.path(dropbox_file_path, "Data/google_mobility/RawData/brazil_gmobility.Rds"))
write.csv(brazil, file.path(dropbox_file_path, "Data/google_mobility/RawData/brazil_gmobility.csv"), row.names=F)


# Plot --------------------------------------------------------
terms <- brazil$category %>% unique()

for(var in terms){
    
    temp <- brazil %>% 
        filter(category == var) 
    
    plot <- temp %>%
        ggplot(aes(x = date, y = value, fill = location)) + 
            geom_ribbon(aes(ymin = 0, ymax = value)) + 
            geom_line(size = 1) +
            facet_wrap(~location) + 
            labs(x = NULL,
                 y = NULL,
                 color = NULL, 
                 subtitle = "The baseline is the median value, for the day of the week, during the 5-week period Jan 3-Feb 6, 2020.",
                 title = paste0("Mobility changes in Brazil: ", var),
                 caption = "Data: Google Community Mobility Report") + 
            theme_ipsum_rc() + 
            theme(
                legend.position = "",
                plot.title = element_text(size = 20),
                plot.subtitle = element_text(size = 18),
                plot.caption = element_text(size = 14)
            )
    
    ggsave(plot, 
           filename = file.path(paste0(brazil_gmobility_figures_path, var, ".pdf")),
           device = cairo_pdf, 
           width = 22, height = 16, scale = 0.65)
}

