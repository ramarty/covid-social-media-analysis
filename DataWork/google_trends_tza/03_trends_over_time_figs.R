# Figures

df <- readRDS(file.path(dropbox_file_path, "Data", "google_trends_tza", "FinalData", "gtrends_tza.Rds"))

df <- df %>%
  filter(keyword_en %in% c("shortness of breath",
                           "tired",
                           "doctor",
                           "coronavirus",
                           "coronavirus symptoms",
                           "corona symptoms",
                           "covid symptoms",
                           "fever",
                           "cough",
                           "covid-19",
                           "fatigue",
                           "loss of smell",
                           "how to treat coronavirus",
                           "insomnia",
                           "thermometers",
                           "coronavirus test",
                           "fever treatment",
                           "ageusia",
                           "loss of taste"))

#### English
p <- df %>%
  filter(language %in% "en") %>%
  ggplot() +
  geom_line(aes(x=date,
                y=hits_ma7,
                color="Search\nInterest\n7-Day MA"),
            size = 1) +
  scale_color_manual(values = c("forestgreen")) +
  labs(color = "") +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  ggplot2::facet_wrap(~keyword,
                      scales="free")

ggsave(p, filename = file.path(dropbox_file_path, "Data", "google_trends_tza",
                               "Outputs", "figures", "trends_en.png"),
       height=10, width=15)


#### Swahili
p <- df %>%
  filter(language %in% "sw") %>%
  ggplot() +
  geom_line(aes(x=date,
                y=hits_ma7,
                color="Search\nInterest\n7-Day MA"),
            size = 1) +
  scale_color_manual(values = c("forestgreen")) +
  labs(color = "") +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  ggplot2::facet_wrap(~keyword,
                      scales="free")

ggsave(p, filename = file.path(dropbox_file_path, "Data", "google_trends_tza",
                               "Outputs", "figures", "trends_sw.png"),
       height=10, width=15)


