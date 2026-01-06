library(tidyverse)
# Combine Lactiplantibacillus, Lactobacillus, and Lacticaseibacillus

df <- df %>%
  mutate(Lactiplantibacillus_Lactobacillus_Lacticaseibacillus = Lactiplantibacillus + Lactobacillus + Lacticaseibacillus)
# Separate numeric samples and F/M
df_num <- df %>%
  filter(!sample %in% c("F", "M")) %>%
  mutate(sample = as.numeric(sample))

df_fm <- df %>%
  filter(sample %in% c("F", "M")) %>%
  mutate(xpos = ifelse(sample == "F", -1.5, 25))

# Calculate means and SD for numeric samples
summary_mitochloro <- df_num %>%
  group_by(sample) %>%
  summarise(mean = mean(Mitochondria_Chloroplast), sd = sd(Mitochondria_Chloroplast))

summary_lacto_combo <- df_num %>%
  group_by(sample) %>%
  summarise(mean = mean(Lactiplantibacillus_Lactobacillus_Lacticaseibacillus), sd = sd(Lactiplantibacillus_Lactobacillus_Lacticaseibacillus))

# Calculate means and SD for F/M
summary_fm_mitochloro <- df_fm %>%
  group_by(sample, xpos) %>%
  summarise(mean = mean(Mitochondria_Chloroplast), sd = sd(Mitochondria_Chloroplast), .groups = "drop")

summary_fm_lacto_combo <- df_fm %>%
  group_by(sample, xpos) %>%
  summarise(mean = mean(Lactiplantibacillus_Lactobacillus_Lacticaseibacillus), sd = sd(Lactiplantibacillus_Lactobacillus_Lacticaseibacillus), .groups = "drop")

# Plot
ggplot() +
  # LOESS smooth lines
  geom_smooth(data = df_num, aes(sample, Mitochondria_Chloroplast),
              method = "loess", se = TRUE, alpha = 0.2) +
  geom_smooth(data = df_num, aes(sample, Lactiplantibacillus_Lactobacillus_Lacticaseibacillus),
              method = "loess", se = TRUE, alpha = 0.2) +
  
  # Raw points for numeric samples
  geom_jitter(data = df_num, aes(sample, Mitochondria_Chloroplast),
              width = 0.2, alpha = 0.4, size = 2, color = "black") +
  geom_jitter(data = df_num, aes(sample, Lactiplantibacillus_Lactobacillus_Lacticaseibacillus),
              width = 0.2, alpha = 0.4, size = 2, color = "black") +
  
  # Mean points and error bars for numeric samples
  geom_point(data = summary_mitochloro, aes(sample, mean), size = 2, color = "black") +
  geom_errorbar(data = summary_mitochloro,
                aes(sample, ymin = mean - sd, ymax = mean + sd),
                width = 0.2, color = "black") +
  
  geom_point(data = summary_lacto_combo, aes(sample, mean), size = 2, color = "black") +
  geom_errorbar(data = summary_lacto_combo,
                aes(sample, ymin = mean - sd, ymax = mean + sd),
                width = 0.2, color = "black") +
  
  # F/M raw points
  geom_jitter(data = df_fm, aes(xpos, Mitochondria_Chloroplast),
              width = 0.2, alpha = 0.7, size = 2, color = "black") +
  geom_jitter(data = df_fm, aes(xpos, Lactiplantibacillus_Lactobacillus_Lacticaseibacillus),
              width = 0.2, alpha = 0.7, size = 2, color = "black") +
  
  # F/M means and error bars
  geom_point(data = summary_fm_mitochloro, aes(xpos, mean), size = 2, color = "black") +
  geom_errorbar(data = summary_fm_mitochloro,
                aes(xpos, ymin = mean - sd, ymax = mean + sd),
                width = 0.2, color = "black") +
  
  geom_point(data = summary_fm_lacto_combo, aes(xpos, mean), size = 2, color = "black") +
  geom_errorbar(data = summary_fm_lacto_combo,
                aes(xpos, ymin = mean - sd, ymax = mean + sd),
                width = 0.2, color = "black") +
  
  # Dotted connector lines for Mitochondria_Chloroplast
  geom_segment(aes(x = -1.5, xend = 0,
                   y = summary_fm_mitochloro$mean[summary_fm_mitochloro$sample == "F"],
                   yend = summary_mitochloro$mean[summary_mitochloro$sample == 0]),
               linetype = "dotted", color = "black", linewidth = 0.8) +
  geom_segment(aes(x = 24, xend = 25,
                   y = summary_mitochloro$mean[summary_mitochloro$sample == 24],
                   yend = summary_fm_mitochloro$mean[summary_fm_mitochloro$sample == "M"]),
               linetype = "dotted", color = "black", linewidth = 0.8) +
  
  # Dotted connector lines for Lactiplantibacillus_Lactobacillus_Lacticaseibacillus
  geom_segment(aes(x = -1.5, xend = 0,
                   y = summary_fm_lacto_combo$mean[summary_fm_lacto_combo$sample == "F"],
                   yend = summary_lacto_combo$mean[summary_lacto_combo$sample == 0]),
               linetype = "dotted", color = "black", linewidth = 0.8) +
  geom_segment(aes(x = 24, xend = 25,
                   y = summary_lacto_combo$mean[summary_lacto_combo$sample == 24],
                   yend = summary_fm_lacto_combo$mean[summary_fm_lacto_combo$sample == "M"]),
               linetype = "dotted", color = "black", linewidth = 0.8) +
  
  # Axes
  scale_x_continuous(
    breaks = c(-1.5, 0, 2, 4, 6, 8, 10, 12, 24, 25),
    labels = c("F", "0", "2", "4", "6", "8", "10", "12", "24", "M")
  ) +
  scale_y_continuous(
    breaks = seq(0, 1.0, 0.2),
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  # Labels and theme
  labs(
    title = "16S Proportions vs Culture Time",
    x = "Culture Time (hours)",
    y = "Bacteria Genera Proportions ± SD"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 0.5)),
    plot.title.position = "plot",
    axis.title.x = element_text(margin = margin(t = 1)),
    axis.title.y = element_text(margin = margin(r = 1)),
    axis.text.x = element_text(margin = margin(t = 1)),
    axis.text.y = element_text(margin = margin(r = 1))
  )