library(tidyverse)

# Clean and prepare data
df$sample <- trimws(as.character(df$sample))

# Keep only numeric timepoints (remove M and F), convert and order
df_num <- subset(df, sample != "M" & sample != "F")
df_num$sample <- as.numeric(df_num$sample)
df_num <- df_num[!is.na(df_num$sample), ]
df_num <- df_num[order(df_num$sample), ]

# Summary for numeric timepoints
group_summary_num <- df_num %>%
  group_by(sample) %>%
  summarise(mean_density = mean(bacterial_cell_density, na.rm = TRUE),
            sd_density = sd(bacterial_cell_density, na.rm = TRUE),
            .groups = "drop")

# Create the plot
ggplot() +
  geom_jitter(data = df_num, aes(x = sample, y = bacterial_cell_density),
              width = 0.2, alpha = 0.5, size = 2, color = "black") +
  
  # Smooth curve with LOESS (confidence interval included)
  geom_smooth(data = df_num, aes(x = sample, y = bacterial_cell_density),
              method = "loess") +
  
  # Points for means
  geom_point(data = group_summary_num, aes(x = sample, y = mean_density),
             size = 2, shape = 16, color = "black") +
  
  # Error bars
  geom_errorbar(data = group_summary_num,
                aes(x = sample, ymin = mean_density - sd_density,
                    ymax = mean_density + sd_density),
                width = 0.2, linewidth = 0.8, color = "black") +
  
  scale_y_log10() +
  scale_x_continuous(breaks = unique(df_num$sample)) +
  labs(
    title = "Bacterial Cell Density vs Culture Time",
    x = "Culture Time (hours)",
    y = "Bacterial Cell Density ± SD (log scale)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.title.position = "plot"
  )