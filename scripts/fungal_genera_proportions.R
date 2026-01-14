library(tidyverse)

# Combine Pichia and Saccharomyces
df <- df %>%
  mutate(Pichia_Saccharomyces = Pichia + Saccharomyces)

# Separate numeric samples and F/M
df_num <- df %>%
  filter(!sample %in% c("F", "M")) %>%
  mutate(sample = as.numeric(sample))

df_fm <- df %>%
  filter(sample %in% c("F", "M")) %>%
  mutate(xpos = ifelse(sample == "F", -2, 26))

# Calculate means and SD for numeric samples
summary_pichia <- df_num %>%
  group_by(sample) %>%
  summarise(mean = mean(Pichia), sd = sd(Pichia))

summary_saccharomyces <- df_num %>%
  group_by(sample) %>%
  summarise(mean = mean(Saccharomyces), sd = sd(Saccharomyces))

summary_combined <- df_num %>%
  group_by(sample) %>%
  summarise(mean = mean(Pichia_Saccharomyces), sd = sd(Pichia_Saccharomyces))

# Calculate means and SD for F/M
summary_fm_pichia <- df_fm %>%
  group_by(sample, xpos) %>%
  summarise(mean = mean(Pichia), sd = sd(Pichia), .groups = "drop")

summary_fm_saccharomyces <- df_fm %>%
  group_by(sample, xpos) %>%
  summarise(mean = mean(Saccharomyces), sd = sd(Saccharomyces), .groups = "drop")

summary_fm_combined <- df_fm %>%
  group_by(sample, xpos) %>%
  summarise(mean = mean(Pichia_Saccharomyces), sd = sd(Pichia_Saccharomyces), .groups = "drop")

# Plot
ggplot() +
  # LOESS lines
  geom_smooth(data = df_num, aes(sample, Pichia), method = "loess", se = TRUE, alpha = 0.2) +
  geom_smooth(data = df_num, aes(sample, Saccharomyces), method = "loess", se = TRUE, alpha = 0.2) +
  geom_smooth(data = df_num, aes(sample, Pichia_Saccharomyces), method = "loess", se = TRUE, alpha = 0.2) +
  
  # Raw points for numeric samples
  geom_jitter(data = df_num, aes(sample, Pichia), width = 0.2, alpha = 0.4, size = 1.8, color = "black") +
  geom_jitter(data = df_num, aes(sample, Saccharomyces), width = 0.2, alpha = 0.4, size = 1.8, color = "black") +
  geom_jitter(data = df_num, aes(sample, Pichia_Saccharomyces), width = 0.2, alpha = 0.4, size = 1.8, color = "black") +
  
  # Mean points and error bars for numeric samples
  geom_point(data = summary_pichia, aes(sample, mean), size = 2, color = "black") +
  geom_errorbar(data = summary_pichia, aes(sample, ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "black") +
  
  geom_point(data = summary_saccharomyces, aes(sample, mean), size = 2, color = "black") +
  geom_errorbar(data = summary_saccharomyces, aes(sample, ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "black") +
  
  geom_point(data = summary_combined, aes(sample, mean), size = 2, color = "black") +
  geom_errorbar(data = summary_combined, aes(sample, ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "black") +
  
  # F and M raw points
  geom_jitter(data = df_fm, aes(xpos, Pichia), width = 0.2, alpha = 0.7, size = 2, color = "black") +
  geom_jitter(data = df_fm, aes(xpos, Saccharomyces), width = 0.2, alpha = 0.7, size = 2, color = "black") +
  geom_jitter(data = df_fm, aes(xpos, Pichia_Saccharomyces), width = 0.2, alpha = 0.7, size = 2, color = "black") +
  
  # F and M means and error bars
  geom_point(data = summary_fm_pichia, aes(xpos, mean), size = 2, color = "black") +
  geom_errorbar(data = summary_fm_pichia, aes(xpos, ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "black") +
  
  geom_point(data = summary_fm_saccharomyces, aes(xpos, mean), size = 2, color = "black") +
  geom_errorbar(data = summary_fm_saccharomyces, aes(xpos, ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "black") +
  
  geom_point(data = summary_fm_combined, aes(xpos, mean), size = 2, color = "black") +
  geom_errorbar(data = summary_fm_combined, aes(xpos, ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "black") +
  
  # Dotted connector lines for Pichia
  geom_segment(aes(x = -2, xend = 0,
                   y = summary_fm_pichia$mean[summary_fm_pichia$sample == "F"],
                   yend = summary_pichia$mean[summary_pichia$sample == 0]),
               linetype = "dotted", color = "black", linewidth = 0.8) +
  geom_segment(aes(x = 24, xend = 26,
                   y = summary_pichia$mean[summary_pichia$sample == 24],
                   yend = summary_fm_pichia$mean[summary_fm_pichia$sample == "M"]),
               linetype = "dotted", color = "black", linewidth = 0.8) +
  
  # Dotted connector lines for Saccharomyces
  geom_segment(aes(x = -2, xend = 0,
                   y = summary_fm_saccharomyces$mean[summary_fm_saccharomyces$sample == "F"],
                   yend = summary_saccharomyces$mean[summary_saccharomyces$sample == 0]),
               linetype = "dotted", color = "black", linewidth = 0.8) +
  geom_segment(aes(x = 24, xend = 26,
                   y = summary_saccharomyces$mean[summary_saccharomyces$sample == 24],
                   yend = summary_fm_saccharomyces$mean[summary_fm_saccharomyces$sample == "M"]),
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
    title = "Fungal Genera Proportions vs Culture Time",
    x = "Culture Time (hours)",
    y = "Fungal Genera Proportion ± SD"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.title.position = "plot"
  )
