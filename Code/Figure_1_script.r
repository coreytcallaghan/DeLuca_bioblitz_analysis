###### Creating arbitrary values for figure 1
### and then plotting for Adaptive Management vs. Landscape Context

# load package
library(tidyverse)

# Define Factors
factors <- c(
  "Number of Species",
  "Number of Observations",
  "Number of Rare Species",
  "First Occurrence",
  "Introduced Species",
  "Endemic Species",
  "Sampling Intensity",
  "Observation Density"
)

# Define Variables
variables <- c("Adaptive Management", "Landscape Context")

# Create all combinations
likert_df <- expand.grid(
  Factor = factors,
  Variable = variables
) %>%
  arrange(Factor, Variable)

# Assign Likert values manually
adaptive_values <- c(2,1,3,4,5,6,8,7)
landscape_values <- c(2,1,5,8,6,7,4,3)

# Add Value column
likert_df <- likert_df %>%
  mutate(Value = case_when(
    Variable == "Adaptive Management" ~ adaptive_values[match(Factor, factors)],
    Variable == "Landscape Context"   ~ landscape_values[match(Factor, factors)]
  ))

# Reorder Factor levels by Landscape Context values
likert_ordered <- likert_df %>%
  filter(Variable == "Landscape Context") %>%
  arrange(Value) %>%
  pull(Factor)

likert_df <- likert_df %>%
  mutate(Factor = factor(Factor, levels = likert_ordered))

# Plot Likert-style bar chart
ggplot(likert_df, aes(x = Factor, y = Value, fill = Variable)) +
  geom_col(position = position_dodge(width = 0.9), color = "black") +
  scale_fill_manual(values = c("Adaptive Management" = "#1f78b4",
                               "Landscape Context" = "#d95f02")) +
  labs(
    x = NULL,
    y = "User Utility",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "bottom"
  )

ggsave("Figures/figure_1_theoretical.png", bg = "transparent")
