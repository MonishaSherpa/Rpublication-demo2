library(tidyverse)

# Clean and prepare data
df$Time <- trimws(as.character(df$Time))

# Separate numeric timepoints from M and F
df_num <- subset(df, Time != "M" & Time != "F")
df_num$Time <- as.numeric(df_num$Time)
df_num <- df_num[order(df_num$Time), ]

# Summary for numeric timepoints: mean and SD by time
group_summary_num <- df_num %>%
  group_by(Time) %>%
  summarise(mean_pH = mean(pH, na.rm = TRUE),
            sd_pH = sd(pH, na.rm = TRUE),
            .groups = "drop")

# Floating M data
df_M <- subset(df, Time == "M")
if(nrow(df_M) > 0) {
  group_summary_M <- df_M %>%
    summarise(mean_pH = mean(pH, na.rm = TRUE),
              sd_pH = sd(pH, na.rm = TRUE),
              .groups = "drop")
}

# Floating F data
df_F <- subset(df, Time == "F")
if(nrow(df_F) > 0) {
  group_summary_F <- df_F %>%
    summarise(mean_pH = mean(pH, na.rm = TRUE),
              sd_pH = sd(pH, na.rm = TRUE),
              .groups = "drop")
}

# Create the plot
ggplot() +
  # Individual data points for numeric times
  geom_jitter(data = df_num, aes(x = Time, y = pH),
              width = 0.2, alpha = 0.5, size = 2) +
  
  # Line connecting mean values for numeric times (BLUE)
  geom_line(data = group_summary_num, aes(x = Time, y = mean_pH),
            color = "blue", linewidth = 1.2) +
  
  # Mean points for numeric times
  geom_point(data = group_summary_num, aes(x = Time, y = mean_pH),
             size = 2, shape = 16) +
  
  # Error bars for numeric times
  geom_errorbar(data = group_summary_num,
                aes(x = Time, ymin = mean_pH - sd_pH, ymax = mean_pH + sd_pH),
                width = 0.2, linewidth = 0.8) +
  
  # Add F timepoint if it exists (floating at position -1)
  {if(nrow(df_F) > 0) list(
    geom_jitter(data = df_F, aes(x = -1, y = pH),
                width = 0.2, alpha = 0.5, size = 2),
    geom_point(data = group_summary_F, aes(x = -1, y = mean_pH),
               size = 2, shape = 16),
    geom_errorbar(data = group_summary_F,
                  aes(x = -1, ymin = mean_pH - sd_pH, ymax = mean_pH + sd_pH),
                  width = 0.2, linewidth = 0.8),
    geom_text(data = group_summary_F,
              aes(x = -1, y = mean_pH + sd_pH, label = "F"),
              vjust = -0.5, size = 3),
    # BLACK DASHED line from F to 0
    geom_segment(aes(x = -1, xend = 0,
                     y = group_summary_F$mean_pH,
                     yend = group_summary_num$mean_pH[group_summary_num$Time == 0]),
                 linetype = "dashed", color = "black", linewidth = 0.8)
  )} +
  
  # Add M timepoint if it exists (floating at position 25)
  {if(nrow(df_M) > 0) list(
    geom_jitter(data = df_M, aes(x = 25, y = pH),
                width = 0.2, alpha = 0.5, size = 2),
    geom_point(data = group_summary_M, aes(x = 25, y = mean_pH),
               size = 2, shape = 16),
    geom_errorbar(data = group_summary_M,
                  aes(x = 25, ymin = mean_pH - sd_pH, ymax = mean_pH + sd_pH),
                  width = 0.2, linewidth = 0.8),
    geom_text(data = group_summary_M,
              aes(x = 25, y = mean_pH, label = "M"),
              vjust = -2, hjust = 1.2, size = 3),
    # BLACK DASHED line from 24 to M
    geom_segment(aes(x = 24, xend = 25,
                     y = group_summary_num$mean_pH[group_summary_num$Time == 24],
                     yend = group_summary_M$mean_pH),
                 linetype = "dashed", color = "black", linewidth = 0.8)
  )} +
  
  coord_cartesian(ylim = c(3.5, 6.5)) +
  scale_x_continuous(breaks = c(-1, 0, 2, 4, 6, 8, 12, 24, 25),
                     labels = c("", "0", "2", "4", "6", "8", "12", "24", "")) +
  labs(
    title = "Culture pH over Culture time",
    x = "Culture time (hours)",
    y = "pH ± SD"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.title.position = "plot"
  )