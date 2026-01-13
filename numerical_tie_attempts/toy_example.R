# =============================================================================
# Figure 8 Exact Replication: Vertical Spacing and Positioning in GPCPs
# =============================================================================
#
# This script recreates Figure 8 exactly as shown, demonstrating the difference
# between random rank tie-breaking and order-induced (GPCP) tie-breaking.
#
# Axes (Left Panel - Random Ranks):
#   1. Numeric/Ordinal (categorical: low, medium, high)
#   2. Average rank
#   3. Random rank
#   4. Numeric Variable
#
# Axes (Right Panel - Order-Induced):
#   1. Numeric/Ordinal (categorical: low, medium, high)
#   2. Average rank
#   3. GPCP (order-induced positioning)
#   4. Numeric Variable
#
# Marginal frequencies: low = 10, medium = 60, high = 30 (total = 100)
#
# =============================================================================

# -----------------------------------------------------------------------------
# Required Packages
# -----------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# Try to load ggpcp, but provide base R fallback
use_ggpcp <- requireNamespace("ggpcp", quietly = TRUE)
if (use_ggpcp) {
  library(ggpcp)
}

# -----------------------------------------------------------------------------
# Create Toy Dataset (100 observations)
# -----------------------------------------------------------------------------
set.seed(2024)
n <- 100

# Categorical variable with specified frequencies
numeric_ordinal <- factor(
  c(rep("low", 10), rep("medium", 60), rep("high", 30)),
  levels = c("low", "medium", "high"),
  ordered = TRUE
)

# Numeric variable (the final axis)
# Values correlate somewhat with category for realistic patterns
numeric_variable <- c(
  runif(10, 0.05, 0.35),    # low category -> lower values
  runif(60, 0.15, 0.85),    # medium category -> middle values
  runif(30, 0.55, 0.98)     # high category -> higher values
)

# Create the dataset
toy_data <- data.frame(
  id = 1:n,
  numeric_ordinal = numeric_ordinal,
  numeric_variable = numeric_variable
)

# Add color based on category
toy_data$color <- toy_data$numeric_ordinal


# =============================================================================
# GGPCP VERSION (if package is available)
# =============================================================================

if (use_ggpcp) {

  cat("\nCreating ggpcp version...\n")

  # Prepare data with different axis representations
  toy_data_pcp <- toy_data %>%
    mutate(
      # Average rank (normalized)
      average_rank = rank(numeric_variable) / n(),
      # Random rank within categories
      random_rank = ave(numeric_variable, numeric_ordinal,
                        FUN = function(x) sample(seq_along(x)) / length(x)),
      # GPCP position (will be handled by pcp_arrange)
      gpcp_position = numeric_variable
    )

  # Left panel: Random rank approach
  pcp_random <- toy_data_pcp %>%
    pcp_select(numeric_ordinal, average_rank, random_rank, numeric_variable) %>%
    pcp_scale(method = "uniminmax")

  p_random <- ggplot(pcp_random, aes_pcp()) +
    geom_pcp_axes() +
    geom_pcp_boxes(fill = "grey90", colour = "grey60", alpha = 0.5) +
    geom_pcp(aes(colour = color), alpha = 0.6, linewidth = 0.5) +
    geom_pcp_labels() +
    scale_colour_manual(
      values = c("low" = "#540D6E", "medium" = "#219B9D", "high" = "#FF8000"),
      guide = "none"
    ) +
    scale_x_discrete(labels = c("numeric_ordinal" = "Numeric/\nOrdinal",
                                "average_rank" = "Average\nrank",
                                "random_rank" = "Random\nrank",
                                "numeric_variable" = "Numeric\nVariable")) +
    labs(title = "Vertical positioning:\nbreaking ties with random ranks") +
    theme_pcp() +
    theme(
      plot.title = element_text(size = 11, hjust = 0.5),
      axis.text.x = element_text(size = 8)
    )

  # Right panel: GPCP approach with ordered tie-breaking
  pcp_gpcp <- toy_data_pcp %>%
    pcp_select(numeric_ordinal, average_rank, numeric_ordinal, numeric_variable) %>%
    pcp_scale(method = "uniminmax") %>%
    pcp_arrange(method = "from-left")

  # Rename the third axis to "GPCP"
  pcp_gpcp <- pcp_gpcp %>%
    mutate(pcp_x = factor(pcp_x,
                          levels = c("numeric_ordinal", "average_rank",
                                     "numeric_ordinal.1", "numeric_variable"),
                          labels = c("Numeric/\nOrdinal", "Average\nrank",
                                     "GPCP", "Numeric\nVariable")))

  p_gpcp <- ggplot(pcp_gpcp, aes_pcp()) +
    geom_pcp_axes() +
    geom_pcp_boxes(fill = "grey90", colour = "grey60", alpha = 0.5) +
    geom_pcp(aes(colour = color), alpha = 0.6, linewidth = 0.5) +
    geom_pcp_labels() +
    scale_colour_manual(
      values = c("low" = "#540D6E", "medium" = "#219B9D", "high" = "#FF8000"),
      guide = "none"
    ) +
    labs(title = "Vertical positioning:\nbreaking ties with order-induced method") +
    theme_pcp() +
    theme(
      plot.title = element_text(size = 11, hjust = 0.5),
      axis.text.x = element_text(size = 8)
    )

  # Combine panels
  combined_ggpcp <- p_random + p_gpcp +
    plot_annotation(
      title = "Figure 8: Vertical spacing and positioning of values in a categorical variable",
      subtitle = "Three categories with marginal frequencies of 10, 60, and 30",
      theme = theme(
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5)
      )
    )

  #return(combined_ggpcp)

  #ggsave("figure8_ggpcp_version.png", combined_ggpcp,
  #       width = 12, height = 6, dpi = 150)
}

# =============================================================================
# Summary Statistics
# =============================================================================

# cat("\n=== Dataset Summary ===\n")
# cat("Total observations:", n, "\n")
# cat("\nMarginal frequencies:\n")
# print(table(toy_data$numeric_ordinal))
# cat("\nProportions:\n")
# print(round(prop.table(table(toy_data$numeric_ordinal)), 2))

