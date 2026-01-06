library(tidyverse)

# Combine Mitochondria and Chloroplast
df <- df %>%
  mutate(Mitochondria_Chloroplast = Mitochondria + Chloroplast)

# Separate numeric samples and F/M
df_num <- df %>%
  filter(!sample %in% c("F", "M")) %>%
  mutate(sample = as.numeric(sample))

df_fm <- df %>%
  filter(sample %in% c("F", "M")) %>%
  mutate(xpos = ifelse(sample == "F", -2, 26))

# Calculate means and SD for numeric samples
summary_mitochondria <- df_num %>%
  group_by(sample) %>%
  summarise(mean = mean(Mitochondria), sd = sd(Mitochondria))

summary_chloroplast <- df_num %>%
  group_by(sample) %>%
  summarise(mean = mean(Chloroplast), sd = sd(Chloroplast))

summary_combined <- df_num %>%
  group_by(sample) %>%
  summarise(mean = mean(Mitochondria_Chloroplast), sd = sd(Mitochondria_Chloroplast))

# Calculate means and SD for F/M
summary_fm_mitochondria <- df_fm %>%
  group_by(sample, xpos) %>%
  summarise(mean = mean(Mitochondria), sd = sd(Mitochondria), .groups = "drop")

summary_fm_chloroplast <- df_fm %>%
  group_by(sample, xpos) %>%
  summarise(mean = mean(Chloroplast), sd = sd(Chloroplast), .groups = "drop")

summary_fm_combined <- df_fm %>%
  group_by(sample, xpos) %>%
  summarise(mean = mean(Mitochondria_Chloroplast), sd = sd(Mitochondria_Chloroplast), .groups = "drop")

# Plot
ggplot() +
  # LOESS lines
  geom_smooth(data = df_num, aes(sample, Mitochondria), method = "loess", se = TRUE, alpha = 0.2) +
  geom_smooth(data = df_num, aes(sample, Chloroplast), method = "loess", se = TRUE, alpha = 0.2) +
  geom_smooth(data = df_num, aes(sample, Mitochondria_Chloroplast), method = "loess", se = TRUE, alpha = 0.2) +
  
  # Raw points for numeric samples
  geom_jitter(data = df_num, aes(sample, Mitochondria), width = 0.2, alpha = 0.4, size = 1.8, color = "black") +
  geom_jitter(data = df_num, aes(sample, Chloroplast), width = 0.2, alpha = 0.4, size = 1.8, color = "black") +
  geom_jitter(data = df_num, aes(sample, Mitochondria_Chloroplast), width = 0.2, alpha = 0.4, size = 1.8, color = "black") +
  
  # Mean points and error bars for numeric samples
  geom_point(data = summary_mitochondria, aes(sample, mean), size = 2, color = "black") +
  geom_errorbar(data = summary_mitochondria, aes(sample, ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "black") +
  
  geom_point(data = summary_chloroplast, aes(sample, mean), size = 2, color = "black") +
  geom_errorbar(data = summary_chloroplast, aes(sample, ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "black") +
  
  geom_point(data = summary_combined, aes(sample, mean), size = 2, color = "black") +
  geom_errorbar(data = summary_combined, aes(sample, ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "black") +
  
  # F and M raw points
  geom_jitter(data = df_fm, aes(xpos, Mitochondria), width = 0.2, alpha = 0.7, size = 2, color = "black") +
  geom_jitter(data = df_fm, aes(xpos, Chloroplast), width = 0.2, alpha = 0.7, size = 2, color = "black") +
  geom_jitter(data = df_fm, aes(xpos, Mitochondria_Chloroplast), width = 0.2, alpha = 0.7, size = 2, color = "black") +
  
  # F and M means and error bars
  geom_point(data = summary_fm_mitochondria, aes(xpos, mean), size = 2, color = "black") +
  geom_errorbar(data = summary_fm_mitochondria, aes(xpos, ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "black") +
  
  geom_point(data = summary_fm_chloroplast, aes(xpos, mean), size = 2, color = "black") +
  geom_errorbar(data = summary_fm_chloroplast, aes(xpos, ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "black") +
  
  geom_point(data = summary_fm_combined, aes(xpos, mean), size = 2, color = "black") +
  geom_errorbar(data = summary_fm_combined, aes(xpos, ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "black") +
  
  # Dotted connector lines for Mitochondria
  geom_segment(aes(x = -2, xend = 0,
                   y = summary_fm_mitochondria$mean[summary_fm_mitochondria$sample == "F"],
                   yend = summary_mitochondria$mean[summary_mitochondria$sample == 0]),
               linetype = "dotted", color = "black", linewidth = 0.8) +
  geom_segment(aes(x = 24, xend = 26,
                   y = summary_mitochondria$mean[summary_mitochondria$sample == 24],
                   yend = summary_fm_mitochondria$mean[summary_fm_mitochondria$sample == "M"]),
               linetype = "dotted", color = "black", linewidth = 0.8) +
  
  # Dotted connector lines for Chloroplast
  geom_segment(aes(x = -2, xend = 0,
                   y = summary_fm_chloroplast$mean[summary_fm_chloroplast$sample == "F"],
                   yend = summary_chloroplast$mean[summary_chloroplast$sample == 0]),
               linetype = "dotted", color = "black", linewidth = 0.8) +
  geom_segment(aes(x = 24, xend = 26,
                   y = summary_chloroplast$mean[summary_chloroplast$sample == 24],
                   yend = summary_fm_chloroplast$mean[summary_fm_chloroplast$sample == "M"]),
               linetype = "dotted", color = "black", linewidth = 0.8) +
  
  # Dotted connector lines for Combined
  geom_segment(aes(x = -2, xend = 0,
                   y = summary_fm_combined$mean[summary_fm_combined$sample == "F"],
                   yend = summary_combined$mean[summary_combined$sample == 0]),
               linetype = "dotted", color = "black", linewidth = 0.8) +
  geom_segment(aes(x = 24, xend = 26,
                   y = summary_combined$mean[summary_combined$sample == 24],
                   yend = summary_fm_combined$mean[summary_fm_combined$sample == "M"]),
               linetype = "dotted", color = "black", linewidth = 0.8) +
  
  # Axes
  scale_x_continuous(
    breaks = c(-2, 0, 2, 4, 6, 8, 10, 12, 24, 26),
    labels = c("F", "0", "2", "4", "6", "8", "10", "12", "24", "M"),
    limits = c(-3, 27)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1.0, 0.2),
    limits = c(-0.1, 1.1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  # Labels and theme
  labs(
    title = "Proportion of Plant Plastids vs Culture Time",
    x = "Culture Time (hours)",
    y = "Mitochondria + Chloroplast Proportion ± SD"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.title.position = "plot"
  )
