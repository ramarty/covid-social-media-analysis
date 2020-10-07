# Stylized Figures
library(dplyr)
library(ggplot2)
library(tidyverse)

color_cases <- "orange2"
color_hits <- "forestgreen"

dates <- seq(from = as.Date("2020-02-01"),
             to = as.Date("2020-04-01"),
             by = 1)

z_scores <- seq(-7, 15, length = length(dates))
values <- dchisq(z_scores, df=5)

df <- data.frame(date = dates,
                 values = values) %>%
  mutate(cases = round(values*10000)) %>%
  mutate(hits = cases) %>%
  mutate(hits = lead(hits, 14) %>% replace_na(0)) 

df_cor <- data.frame(NULL)

for(shift_i in -22:22){
  print(shift_i)
  
  if(shift_i < 0){
    df_shift <- df %>%
      mutate(hits_shift = lead(hits, abs(shift_i)))
  } else{
    df_shift <- df %>%
      mutate(hits_shift = lag(hits, abs(shift_i)))
  }
  
  df_shift_sub <- df_shift[!is.na(df_shift$hits_shift),]
  df_cor_i <- data.frame(cor = cor(df_shift_sub$cases, df_shift_sub$hits_shift),
                         shift = shift_i)
  df_cor <- bind_rows(df_cor,
                      df_cor_i)
  
  fig_line <- ggplot(df_shift) +
    geom_line(aes(x = date, y = cases, 
                  color = "Cases",
                  linetype = "Cases"),
              size = 0.5) +
    geom_line(aes(x = date, y = hits, 
                  color = "Hits",
                  linetype = "Hits"),
              size = 0.5) +
    geom_line(aes(x = date, y = hits_shift, 
                  color = "Hits - Shifted",
                  linetype = "Hits - Shifted")) +
    scale_color_manual(name = "", values = c(color_cases, color_hits, color_hits)) +
    scale_linetype_manual(name = "", values = c("solid", "solid", "dashed")) +
    labs(x = "") +
    theme_minimal() +
    theme(legend.position = "left") +
    scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Search\nInterest",
                                           breaks = c(0, 1500),
                                           labels = c("Low", "High"))) +
    labs(y = "COVID-19\nCases") +
    theme(axis.title.y.left = element_text(angle = 0, 
      vjust = 0.5, 
      color=color_cases,
      face = "bold",
      size=13),
      axis.title.y.right = element_text(angle = 0, 
        vjust = 0.5, 
        color=color_hits,
        face = "bold",
        size=13),
      axis.text.y.left = element_text(color = color_cases,
                                      size=13),
      axis.text.y.right = element_text(color = color_hits,
                                       size=13),
      axis.text = element_text(face = "bold", size=10),
      legend.text = element_text(face = "bold", size=10))
  
  
  fig_col <- ggplot() +
    geom_col(data = df_cor,
             aes(x = shift, y = cor), fill = "dodgerblue4") +
    geom_col(data = df_cor_i,
             aes(x = shift, y = cor), fill = "red") +
    scale_y_continuous(limits = c(-1,1)) +
    scale_x_continuous(limits = c(-22,22)) + 
    labs(x = "Days Shift", 
         y = "Correlation") +
    theme_minimal() +
    theme(axis.text = element_text(face = "bold", size = 10),
          axis.title = element_text(face = "bold", size = 13))
  
  best_cor <- max(df_cor$cor) %>% round(3)
  zscore <- round((max(df_cor$cor) - mean(df_cor$cor)) / sd(df_cor$cor), 3) %>%
    replace_na(0)
  best_lag <- df_cor$shift[which.max(df_cor$cor)]
  
  title <- paste0("The best correlation is ",
                  best_cor,
                  " with a ",best_lag, " day shift.\n",
                  "This correlation is ",
                  zscore, 
                  " standard deviations above the average correlation (z-score).")
  
  fig_all <- ggarrange(fig_line, fig_col, widths = c(0.6, 0.4)) %>%
    annotate_figure(fig_all, top = text_grob(title, color = "black", face = "bold", size = 14))

  ggsave(fig_all, filename = file.path(dropbox_file_path, "Data", "google_trends", "Outputs", 
                                       "cor_gif", "images", 
                                       paste0("image_", shift_i+100, ".png")),
         height = 4, width = 12)
  
}

