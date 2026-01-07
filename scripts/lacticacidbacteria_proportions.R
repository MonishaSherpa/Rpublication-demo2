library(tidyverse)

# Combine Lactiplantibacillus, Lactobacillus, and Lacticaseibacillus
df <- df %>%
  mutate(Lactiplantibacillus_Lactobacillus_Lacticaseibacillus = 
           Lactiplantibacillus + Lactobacillus + Lacticaseibacillus)

# Separate numeric samples and F/M
df_num <- df %>%
  filter(!sample %in% c("F", "M")) %>%
  mutate(sample = as.numeric(sample))

df_fm <- df %>%
  filter(sample %in% c("F", "M")) %>%
  mutate(xpos = ifelse(sample == "F", -2, 26))

# Calculate means and SD for numeric samples
summary_lacto1 <- df_num %>%
  group_by(sample) %>%
  summarise(mean = mean(Lactiplantibacillus), sd = sd(Lactiplantibacillus))

summary_lacto2 <- df_num %>%
  group_by(sample) %>%
  summarise(mean = mean(Lactobacillus), sd = sd(Lactobacillus))

summary_lacto3 <- df_num %>%
  group_by(sample) %>%
  summarise(mean = mean(Lacticaseibacillus), sd = sd(Lacticaseibacillus))

summary_combined <- df_num %>%
  group_by(sample) %>%
  summarise(mean = mean(Lactiplantibacillus_Lactobacillus_Lacticaseibacillus), 
            sd = sd(Lactiplantibacillus_Lactobacillus_Lacticaseibacillus))

# Calculate means and SD for F/M
summary_fm_lacto1 <- df_fm %>%
  group_by(sample, xpos) %>%
  summarise(mean = mean(Lactiplantibacillus), sd = sd(Lactiplantibacillus), .groups = "drop")

summary_fm_lacto2 <- df_fm %>%
  group_by(sample, xpos) %>%
  summarise(mean = mean(Lactobacillus), sd = sd(Lactobacillus), .groups = "drop")

summary_fm_lacto3 <- df_fm %>%
  group_by(sample, xpos) %>%
  summarise(mean = mean(Lacticaseibacillus), sd = sd(Lacticaseibacillus), .groups = "drop")

summary_fm_combined <- df_fm %>%
  group_by(sample, xpos) %>%
  summarise(mean = mean(Lactiplantibacillus_Lactobacillus_Lacticaseibacillus), 
            sd = sd(Lactiplantibacillus_Lactobacillus_Lacticaseibacillus), .groups = "drop")

# Plot
ggplot() +
  # LOESS lines
  geom_smooth(data = df_num, aes(sample, Lactiplantibacillus), method = "loess", se = TRUE, alpha = 0.2) +
  geom_smooth(data = df_num, aes(sample, Lactobacillus), method = "loess", se = TRUE, alpha = 0.2) +
  geom_smooth(data = df_num, aes(sample, Lacticaseibacillus), method = "loess", se = TRUE, alpha = 0.2) +
  geom_smooth(data = df_num, aes(sample, Lactiplantibacillus_Lactobacillus_Lacticaseibacillus), method = "loess", se = TRUE, alpha = 0.2) +
  
  # Raw points for numeric samples
  geom_jitter(data = df_num, aes(sample, Lactiplantibacillus), width = 0.2, alpha = 0.4, size = 2, color = "black") +
  geom_jitter(data = df_num, aes(sample, Lactobacillus), width = 0.2, alpha = 0.4, size = 2, color = "black") +
  geom_jitter(data = df_num, aes(sample, Lacticaseibacillus), width = 0.2, alpha = 0.4, size = 2, color = "black") +
  geom_jitter(data = df_num, aes(sample, Lactiplantibacillus_Lactobacillus_Lacticaseibacillus), width = 0.2, alpha = 0.4, size = 2, color = "black") +
  
  # Mean points and error bars for numeric samples
  geom_point(data = summary_lacto1, aes(sample, mean), size = 2, color = "black") +
  geom_errorbar(data = summary_lacto1, aes(sample, ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "black") +
  
  geom_point(data = summary_lacto2, aes(sample, mean), size = 2, color = "black") +
  geom_errorbar(data = summary_lacto2, aes(sample, ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "black") +
  
  geom_point(data = summary_lacto3, aes(sample, mean), size = 2, color = "black") +
  geom_errorbar(data = summary_lacto3, aes(sample, ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "black") +
  
  geom_point(data = summary_combined, aes(sample, mean), size = 2, color = "black") +
  geom_errorbar(data = summary_combined, aes(sample, ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "black") +
  
  # F and M raw points
  geom_jitter(data = df_fm, aes(xpos, Lactiplantibacillus), width = 0.2, alpha = 0.7, size = 2, color = "black") +
  geom_jitter(data = df_fm, aes(xpos, Lactobacillus), width = 0.2, alpha = 0.7, size = 2, color = "black") +
  geom_jitter(data = df_fm, aes(xpos, Lacticaseibacillus), width = 0.2, alpha = 0.7, size = 2, color = "black") +
  geom_jitter(data = df_fm, aes(xpos, Lactiplantibacillus_Lactobacillus_Lacticaseibacillus), width = 0.2, alpha = 0.7, size = 2, color = "black") +
  
  # F and M means and error bars
  geom_point(data = summary_fm_lacto1, aes(xpos, mean), size = 2, color = "black") +
  geom_errorbar(data = summary_fm_lacto1, aes(xpos, ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "black") +
  
  geom_point(data = summary_fm_lacto2, aes(xpos, mean), size = 2, color = "black") +
  geom_errorbar(data = summary_fm_lacto2, aes(xpos, ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "black") +
  
  geom_point(data = summary_fm_lacto3, aes(xpos, mean), size = 2, color = "black") +
  geom_errorbar(data = summary_fm_lacto3, aes(xpos, ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "black") +
  
  geom_point(data = summary_fm_combined, aes(xpos, mean), size = 2, color = "black") +
  geom_errorbar(data = summary_fm_combined, aes(xpos, ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "black") +
  
  # Dotted connector lines for Lactiplantibacillus
  geom_segment(aes(x = -2, xend = 0,
                   y = summary_fm_lacto1$mean[summary_fm_lacto1$sample == "F"],
                   yend = summary_lacto1$mean[summary_lacto1$sample == 0]),
               linetype = "dotted", color = "black", linewidth = 0.8) +
  geom_segment(aes(x = 24, xend = 26,
                   y = summary_lacto1$mean[summary_lacto1$sample == 24],
                   yend = summary_fm_lacto1$mean[summary_fm_lacto1$sample == "M"]),
               linetype = "dotted", color = "black", linewidth = 0.8) +
  
  # Dotted connector lines for Lactobacillus
  geom_segment(aes(x = -2, xend = 0,
                   y = summary_fm_lacto2$mean[summary_fm_lacto2$sample == "F"],
                   yend = summary_lacto2$mean[summary_lacto2$sample == 0]),
               linetype = "dotted", color = "black", linewidth = 0.8) +
  geom_segment(aes(x = 24, xend = 26,
                   y = summary_lacto2$mean[summary_lacto2$sample == 24],
                   yend = summary_fm_lacto2$mean[summary_fm_lacto2$sample == "M"]),
               linetype = "dotted", color = "black", linewidth = 0.8) +
  
  # Dotted connector lines for Lacticaseibacillus
  geom_segment(aes(x = -2, xend = 0,
                   y = summary_fm_lacto3$mean[summary_fm_lacto3$sample == "F"],
                   yend = summary_lacto3$mean[summary_lacto3$sample == 0]),
               linetype = "dotted", color = "black", linewidth = 0.8) +
  geom_segment(aes(x = 24, xend = 26,
                   y = summary_lacto3$mean[summary_lacto3$sample == 24],
                   yend = summary_fm_lacto3$mean[summary_fm_lacto3$sample == "M"]),
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
    title = "Lactic Acid Bacteria Proportions vs Culture Time",
    x = "Culture Time (hours)",
    y = "Relative Abundance ± SD"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.title.position = "plot"
  )