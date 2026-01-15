#' ==============================================================================
#' Example: Unified Tie-Breaking for Numerical Variables in ggpcp
#' ==============================================================================
# Load required packages
library(palmerpenguins)
library(ggpcp)
library(dplyr)
library(tidyr)
library(ggplot2)

# Source the unified tie-breaking functions
source(here::here("numerical_tie_attempts/unified_tie_breaking.R"))

# ==============================================================================
# Create Simple Dataset with Clear Ties
# ==============================================================================

# Use a small subset for clarity
set.seed(42)
simple_penguins <- penguins %>%
  filter(!is.na(sex), !is.na(body_mass_g)) %>%
  sample_n(40) %>%
  mutate(
    # Round to create obvious numerical ties
    bill_length = round(bill_length_mm, 0),
    bill_depth = round(bill_depth_mm, 0)
  ) %>%
  select(bill_length, bill_depth, sex, species)

# ==============================================================================
# Panel A: Standard ggpcp - Categorical Ties ARE Spread
# ==============================================================================

# Standard ggpcp spreads categorical ties beautifully
pcp_categorical <- simple_penguins %>%
  pcp_select(sex, species) %>%
  pcp_scale("uniminmax") %>%
  pcp_arrange("from-left")

p_categorical <- pcp_categorical %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes() +
  geom_pcp(aes(colour = species), alpha = 0.7, linewidth = 1) +
  geom_pcp_boxes(fill = "grey90") +
  geom_pcp_labels() +
  theme_pcp() +
  labs(
    title = "A) Categorical Variables",
    subtitle = "ggpcp spreads tied observations evenly"
  ) +
  scale_color_manual(
    values = c("Adelie" = "#540D6E", "Gentoo" = "#219B9D", "Chinstrap" = "#FF8000")
  ) +
  theme(legend.position = "none")

# ==============================================================================
# Panel B: Standard ggpcp - Numerical Ties NOT Spread
# ==============================================================================

# Standard ggpcp does NOT spread numerical ties - lines overlap
pcp_numerical_standard <- simple_penguins %>%
  pcp_select(bill_length, bill_depth) %>%
  pcp_scale("uniminmax") %>%
  pcp_arrange("from-left")  # Has no effect on numerical variables

p_numerical_standard <- pcp_numerical_standard %>%
  ggplot(aes_pcp()) +
  geom_pcp(aes(colour = species), alpha = 0.7, linewidth = 1,
           data = . %>% left_join(
             simple_penguins %>% mutate(pcp_id = row_number()) %>% select(pcp_id, species),
             by = "pcp_id"
           )) +
  geom_pcp_axes()  +
  geom_pcp_labels() +
  theme_pcp() +
  labs(
    title = "B) Numerical Variables (Standard)",
    subtitle = "Tied values overlap - lines indistinguishable"
  ) +
  scale_color_manual(
    values = c("Adelie" = "#540D6E", "Gentoo" = "#219B9D", "Chinstrap" = "#FF8000")
  ) +
  theme(legend.position = "none")

# ==============================================================================
# Panel C: Extended ggpcp - Numerical Ties NOW Spread
# ==============================================================================

# With unified tie-breaking, numerical ties are spread like categorical
pcp_numerical_unified <- simple_penguins %>%
  pcp_select(bill_length, bill_depth) %>%
  pcp_scale("uniminmax") %>%
  pcp_arrange_unified(
    method = "from-left",
    handle_numerical = TRUE,
    density_bandwidth = 0.08,
    min_spacing = 0.015
  )

p_numerical_unified <- pcp_numerical_unified %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes() +
  geom_pcp(pcp_numerical_unified, aes(colour = species), alpha = 0.7, linewidth = 1,
           data = . %>% left_join(
             simple_penguins %>% mutate(pcp_id = row_number()) %>% select(pcp_id, species),
             by = "pcp_id"
           )) +
  geom_pcp_labels() +
  theme_pcp() +
  labs(
    title = "C) Numerical Variables (Unified)",
    subtitle = "Tied values spread - each line traceable"
  ) +
  scale_color_manual(
    values = c("Adelie" = "#540D6E", "Gentoo" = "#219B9D", "Chinstrap" = "#FF8000")
  ) +
  theme(legend.position = "none")


# ==============================================================================
# Panel D: Mixed Variables - Full Unified Framework
# ==============================================================================

pcp_mixed <- simple_penguins %>%
  pcp_select(bill_length, sex, bill_depth, species) %>%
  pcp_scale("uniminmax") %>%
  pcp_arrange_unified(
    method = "from-left",
    handle_numerical = TRUE,
    density_bandwidth = 0.08,
    min_spacing = 0.012
  )

p_mixed <- pcp_mixed %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes() +
  geom_pcp(aes(colour = species), alpha = 0.6, linewidth = 0.8) +
  geom_pcp_boxes(fill = "grey90") +
  geom_pcp_labels() +
  theme_pcp() +
  labs(
    title = "D) Mixed Variables (Unified)",
    subtitle = "Same algorithm for both types"
  ) +
  scale_color_manual(
    values = c("Adelie" = "#540D6E", "Gentoo" = "#219B9D", "Chinstrap" = "#FF8000")
  ) +
  theme(legend.position = "bottom")

# ==============================================================================
# Combine Panels
# ==============================================================================

comparison <- (p_categorical | p_numerical_standard) /
  (p_numerical_unified | p_mixed) +
  plot_annotation(
    title = "Extending ggpcp's Tie-Breaking from Categorical to Numerical Variables",
    subtitle = expression(
      paste("Core principle: ", italic("spacing"), " = ",
            italic("available_space"), " / (", italic("n"), " - 1)")
    ),
    caption = "Unified framework maintains visual continuity across mixed-type data",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      plot.caption = element_text(size = 9, hjust = 0.5, face = "italic")
    )
  )

print(comparison)



