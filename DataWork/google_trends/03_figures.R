# Initial Figures of Trends

# Load Data --------------------------------------------------------------------
trends_df <- readRDS(file.path(dropbox_file_path, "Data/google_trends/FinalData/brazil_extract_clean.Rds"))
trends_df <- trends_df[trends_df$keyword %in% c("febre", "coronavirus", "tosse", "sintomas do coronavirus"),]

# Figure -----------------------------------------------------------------------
#### All
p <- ggplot() +
  geom_line(data=trends_df,
            aes(x=date,
                y=hits,
                group=name,
                color=name)) +
  labs(color = "Region") + 
  ggplot2::facet_wrap(~keyword,
             scales="free")

ggsave(p, filename = file.path(dropbox_file_path, "Data/google_trends/Outputs/brazil_trends.png"),
       height=10, width=15)

#### Past 3 months
p <- ggplot() +
  geom_line(data=trends_df[trends_df$date >= "2020-01-01",],
            aes(x=date,
                y=hits,
                group=name,
                color=name)) +
  labs(color = "Region") + 
  ggplot2::facet_wrap(~keyword,
                      scales="free")

ggsave(p, filename = file.path(dropbox_file_path, "Data/google_trends/Outputs/brazil_trends_2020.png"),
       height=10, width=15)