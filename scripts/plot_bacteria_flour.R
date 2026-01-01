library(readxl)
df <- read_excel("data/Microbial Composition Aug19-20 25 Time Course Bacterial with Flour, MT & Chlor Removed.xlsx")
View(df)
library(tidyverse)

#----------------------------------------------------------
# 1. Create sum column and split numeric vs F/M
#----------------------------------------------------------

pseudo <- 1e-4

df <- df %>%
  mutate(
    Four_Bacteria_Sum = Curtobacterium + Pantoea + Spingomaonas + Pseudomonas
  )

df_num <- df %>%
  filter(!sample %in% c("F", "M")) %>%
  mutate(sample = as.numeric(sample))

max_time <- max(df_num$sample, na.rm = TRUE)

df_fm <- df %>%
  filter(sample %in% c("F", "M")) %>%
  mutate(
    xpos = ifelse(sample == "F", -4, max_time + 1)
  )

#----------------------------------------------------------
# 2. Mean + SD helper
#----------------------------------------------------------

summarise_sd <- function(data, xvar, yvar) {
  data %>%
    group_by(.data[[xvar]]) %>%
    summarise(
      mean = mean(.data[[yvar]] + pseudo),
      sd   = sd(.data[[yvar]] + pseudo),
      .groups = "drop"
    ) %>%
    rename(x = 1)
}

summary_num <- bind_rows(
  summarise_sd(df_num, "sample", "Curtobacterium") %>% mutate(genus = "Curtobacterium"),
  summarise_sd(df_num, "sample", "Pantoea") %>% mutate(genus = "Pantoea"),
  summarise_sd(df_num, "sample", "Spingomaonas") %>% mutate(genus = "Spingomaonas"),
  summarise_sd(df_num, "sample", "Pseudomonas") %>% mutate(genus = "Pseudomonas"),
  summarise_sd(df_num, "sample", "Four_Bacteria_Sum") %>% mutate(genus = "Sum")
)

summary_fm <- bind_rows(
  summarise_sd(df_fm, "xpos", "Curtobacterium") %>% mutate(genus = "Curtobacterium"),
  summarise_sd(df_fm, "xpos", "Pantoea") %>% mutate(genus = "Pantoea"),
  summarise_sd(df_fm, "xpos", "Spingomaonas") %>% mutate(genus = "Spingomaonas"),
  summarise_sd(df_fm, "xpos", "Pseudomonas") %>% mutate(genus = "Pseudomonas"),
  summarise_sd(df_fm, "xpos", "Four_Bacteria_Sum") %>% mutate(genus = "Sum")
)

#----------------------------------------------------------
# 3. Smooth data (numeric only — blue curves)
#----------------------------------------------------------

make_smooth_num <- function(var, label) {
  df_num %>%
    transmute(x = sample, y = .data[[var]] + pseudo, genus = label)
}

smooth_num <- bind_rows(
  make_smooth_num("Curtobacterium", "Curtobacterium"),
  make_smooth_num("Pantoea", "Pantoea"),
  make_smooth_num("Spingomaonas", "Spingomaonas"),
  make_smooth_num("Pseudomonas", "Pseudomonas"),
  make_smooth_num("Four_Bacteria_Sum", "Sum")
)

#----------------------------------------------------------
# 4. Dashed connector datasets (TWO POINTS ONLY)
#----------------------------------------------------------

make_edge <- function(var, label, x_vals) {
  bind_rows(
    df_fm  %>% transmute(x = xpos,   y = .data[[var]] + pseudo),
    df_num %>% transmute(x = sample, y = .data[[var]] + pseudo)
  ) %>%
    filter(x %in% x_vals) %>%
    mutate(genus = label)
}

connect_F_0 <- bind_rows(
  make_edge("Curtobacterium", "Curtobacterium", c(-4, 0)),
  make_edge("Pantoea", "Pantoea", c(-4, 0)),
  make_edge("Spingomaonas", "Spingomaonas", c(-4, 0)),
  make_edge("Pseudomonas", "Pseudomonas", c(-4, 0)),
  make_edge("Four_Bacteria_Sum", "Sum", c(-4, 0))
)

connect_24_M <- bind_rows(
  make_edge("Curtobacterium", "Curtobacterium", c(24, max_time + 1)),
  make_edge("Pantoea", "Pantoea", c(24, max_time + 1)),
  make_edge("Spingomaonas", "Spingomaonas", c(24, max_time + 1)),
  make_edge("Pseudomonas", "Pseudomonas", c(24, max_time + 1)),
  make_edge("Four_Bacteria_Sum", "Sum", c(24, max_time + 1))
)

#----------------------------------------------------------
# 5. Raw points
#----------------------------------------------------------

df_long <- df %>%
  mutate(
    x = case_when(
      sample == "F" ~ -4,
      sample == "M" ~ max_time + 1,
      TRUE ~ as.numeric(sample)
    )
  ) %>%
  pivot_longer(
    cols = c(Curtobacterium, Pantoea, Spingomaonas,
             Pseudomonas, Four_Bacteria_Sum),
    names_to = "genus",
    values_to = "value"
  ) %>%
  mutate(value = value + pseudo)

#----------------------------------------------------------
# 6. X-axis
#----------------------------------------------------------

x_breaks <- c(-4, 0, 2, 4, 6, 8, 10, 12, 24, max_time + 1)
x_labels <- c("F", "0", "2", "4", "6", "8", "10", "12", "24", "M")

#----------------------------------------------------------
# 7. Plot
#----------------------------------------------------------

ggplot() +
  
  # Blue smooth curves WITH confidence band (numeric only)
  geom_smooth(
    data = smooth_num,
    aes(x, y, group = genus),
    method = "loess",
    se = TRUE,
    linewidth = 0.9,
    alpha = 0.25
  ) +
  
  # Dashed black connectors (ONLY two-point edges)
  geom_smooth(
    data = connect_F_0,
    aes(x, y, group = genus),
    method = "loess",
    se = FALSE,
    linetype = "dashed",
    color = "black",
    linewidth = 0.7
  ) +
  geom_smooth(
    data = connect_24_M,
    aes(x, y, group = genus),
    method = "loess",
    se = FALSE,
    linetype = "dashed",
    color = "black",
    linewidth = 0.7
  ) +
  
  # Raw points
  geom_jitter(
    data = df_long,
    aes(x, value),
    width = 0.15,
    alpha = 0.5,
    size = 1.8
  ) +
  
  # Mean ± SD
  geom_point(
    data = bind_rows(summary_num, summary_fm),
    aes(x, mean),
    size = 2.2
  ) +
  geom_errorbar(
    data = bind_rows(summary_num, summary_fm),
    aes(x, ymin = mean - sd, ymax = mean + sd),
    width = 0.18
  ) +
  
  scale_x_continuous(
    breaks = x_breaks,
    labels = x_labels,
    expand = expansion(mult = c(0.08, 0.05))
  ) +
  scale_y_log10(labels = function(x) format(x, digits = 2, nsmall = 1)) +
  coord_cartesian(ylim = c(min(df_long$value), 1.0), clip = "off") +
  
  labs(
    x = "Culture Time (hours)",
    y = "Relative Abundance (log scale)",
    title = "Proportion of Bacterial Genera in Flour vs Culture Time"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
