library(tidyverse)

# Separate numeric and F/M samples
df_num <- df %>%
  filter(!sample %in% c("F", "M")) %>%
  mutate(sample = as.numeric(sample))

df_fm <- df %>%
  filter(sample %in% c("F", "M")) %>%
  mutate(xpos = ifelse(sample == "F", -0.4, 25))

# Calculate means and SD
summary_num <- df_num %>%
  group_by(sample) %>%
  summarise(mean = mean(Alternaria), sd = sd(Alternaria))

summary_fm <- df_fm %>%
  group_by(sample, xpos) %>%
  summarise(mean = mean(Alternaria), sd = sd(Alternaria), .groups = "drop")

# Plot
ggplot() +
  geom_smooth(data = df_num, aes(sample, Alternaria), method = "loess", se = TRUE, alpha = 0.2) +
  geom_jitter(data = df_num, aes(sample, Alternaria), width = 0.2, alpha = 0.5, color = "black") +
  geom_point(data = summary_num, aes(sample, mean), size = 2, color = "black") +
  geom_errorbar(data = summary_num, aes(sample, ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "black") +
  
  geom_jitter(data = df_fm, aes(xpos, Alternaria), width = 0.1, alpha = 0.7, color = "black") +
  geom_point(data = summary_fm, aes(xpos, mean), size = 2, color = "black") +
  geom_errorbar(data = summary_fm, aes(xpos, ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "black") +
  
  # Dotted connector lines
  geom_segment(aes(x = -0.4, xend = 0,
                   y = summary_fm$mean[summary_fm$sample == "F"],
                   yend = summary_num$mean[summary_num$sample == 0]),
               linetype = "dotted", color = "black", linewidth = 0.8) +
  geom_segment(aes(x = 24, xend = 25,
                   y = summary_num$mean[summary_num$sample == 24],
                   yend = summary_fm$mean[summary_fm$sample == "M"]),
               linetype = "dotted", color = "black", linewidth = 0.8) +
  
  scale_x_continuous(
    breaks = c(-0.4, 0, 2, 4, 6, 8, 10, 12, 24, 25),
    limits = c(-0.5, 25),
    labels = c("F", "0", "2", "4", "6", "8", "10", "12", "24", "M")
  ) +
  scale_y_continuous(
    breaks = seq(0, 1.0, 0.2),
    limits = c(-0.1, 1.0),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Proportion of Alternaria vs Culture Time",
    x = "Culture Time (hours)",
    y = "Proportion of Alternaria ± SD"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 0)),
    plot.title.position = "plot",
    axis.title.x = element_text(margin = margin(t = 1)),
    axis.title.y = element_text(margin = margin(r = 1)),
    axis.text.x = element_text(margin = margin(t = 1)),
    axis.text.y = element_text(margin = margin(r = 1)),
    plot.margin = margin(t = 1, r = 1, b = 1, l = 1)
  )