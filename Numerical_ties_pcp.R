library(GGally)
library(palmerpenguins)
library(tidyverse)
library(gridExtra)
library(ggpcp)
library(datasets)
library(ggmagnify)
library(ggfx)

theme_set(theme_bw())

iris_scale_color <-   function(...) scale_color_manual(
  "Species", values = c("setosa" = "#540D6E", "versicolor" = "#219B9D", "virginica" = "#FF8000", ...))
iris_scale_fill <-   function(...) scale_fill_manual(
  "Species", values = c("setosa" = "#540D6E", "versicolor" = "#219B9D", "virginica" = "#FF8000", ...))

species_to_zoom <- "setosa"

iris <- iris %>%
  na.omit() %>%
  mutate(focus = Species == species_to_zoom)

intelligent_jitter <- function(values, epsilon = 0.025) {
  identify_tie_groups <- function(values) {
    split(seq_along(values), values)
  }

  tie_groups <- identify_tie_groups(values)
  golden_ratio <- 0.618033988749895
  two_pi <- 2 * pi

  for (group in tie_groups) {
    n_ties <- length(group)
    if (n_ties > 1) {
      for (i in seq_along(group)) {
        angle <- (i - 1) * two_pi * golden_ratio
        displacement <- epsilon * cos(angle) * ((i - 1) / n_ties)
        values[group[i]] <- values[group[i]] + displacement
      }
    }
  }
  return(values)
}

sunflower_jitter <- function(values, epsilon = 0.01) {
  # helper: group indices by exactly‐equal values
  identify_tie_groups <- function(v) split(seq_along(v), v)

  tie_groups   <- identify_tie_groups(values)
  golden_angle <- 137.508 * pi / 180  # convert degrees → radians

  for (grp in tie_groups) {
    n_ties <- length(grp)
    if (n_ties > 1) {
      for (j in seq_along(grp)) {
        idx    <- grp[j]
        angle  <- (j - 1) * golden_angle
        radius <- epsilon * sqrt((j - 1) / n_ties)
        values[idx] <- values[idx] + radius * cos(angle)
      }
    }
  }

  values
}

halton_sequence <- function(index, base) {
  result <- 0
  f <- 1 / base
  i <- index

  while (i > 0) {
    result <- result + f * (i %% base)
    i <- floor(i / base)
    f <- f / base
  }

  return(result)
}

halton_jitter <- function(values, epsilon = 0.01) {
  # Find which values are duplicated (i.e., are part of a tie group)
  tied_values <- unique(values[duplicated(values)])

  # Create a copy to modify
  jittered_values <- values

  for (tie in tied_values) {
    # Get the indices of the elements in this tie group
    group_indices <- which(values == tie)
    n_ties <- length(group_indices)

    # Apply Halton jitter to each member of the group
    for (i in 1:n_ties) {
      # Halton sequence is 0-indexed, so we use i-1
      h <- halton_sequence(i - 1, base = 2)

      # The raw Halton sequence is in [0, 1]. Center it on 0 by subtracting 0.5
      # This creates jitter in the range [-0.5 * epsilon, 0.5 * epsilon]
      jitter <- epsilon * (h - 0.5)

      # Apply the jitter to the original value
      jittered_values[group_indices[i]] <- jittered_values[group_indices[i]] + jitter
    }
  }

  return(jittered_values)
}

idx <- which(names(iris) == "Petal.Width")




p1 <- iris %>%
  pcp_select(c(1:5)) %>%
  pcp_scale(method="uniminmax") %>%
  pcp_arrange(method = "from-left") %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes() +
  geom_pcp(aes(colour = Species), alpha = 0.5) +
  ggpcp::geom_pcp_boxes(boxwidth = 0.2) +
  ggpcp::geom_pcp_labels() +
  theme_pcp() +
  #geom_point(aes(colour = Species), size = 0.75, position = position_dodge(0.01)) +
  theme(legend.position = "none") +
  iris_scale_color() +
  ggplot2::labs(title = "Iris Data no adjustment")

p1_zoom <- p1 +
  coord_cartesian(xlim = c(idx - 0.1, idx + 0.1), clip = "on") +
  labs(subtitle = "Zoomed on Petal.Width axis")

p1_magnify <- p1 +
  geom_magnify(
    from = aes(xmin = 2.8, xmax = 4.2, ymin = 0.00, ymax = 0.45),
    to   = aes(xmin = 5.0, xmax = 6.3, ymin = 0.35, ymax = 0.95),
    shadow       = TRUE,
    linewidth    = 0.4
  ) +
  # ensure nothing bleeds into the margins
  coord_cartesian(clip = "on")  +
  geom_pcp(
    data = ~ dplyr::filter(., Species == "setosa"),
    aes(colour = Species),
    linewidth = 1.05, alpha = 0.95
  )


p2 <- iris %>%
  mutate(Petal.Width = halton_jitter(Petal.Width),
         Petal.Length = halton_jitter(Petal.Length),
         Sepal.Width = halton_jitter(Sepal.Width),
         Sepal.Length = halton_jitter(Sepal.Length)
  ) %>%
  pcp_select(c(1:5)) %>%
  pcp_scale(method = "uniminmax") %>%
  pcp_arrange(method = "from-left") %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes() +
  geom_pcp(aes(colour = Species), alpha = 0.5) +
  ggpcp::geom_pcp_boxes(boxwidth = 0.2) +
  ggpcp::geom_pcp_labels()+
  theme_pcp() +
  #geom_point(aes(colour = Species), size = 0.75, position = position_dodge(0.01)) +
  theme(legend.position = "none") +
  iris_scale_color() +
  labs(title = "Iris Data with Halton Jitter for Ties")

p2_zoom <- p2 +
  coord_cartesian(xlim = c(idx - 0.1, idx + 0.1), clip = "on") +
  labs(subtitle = "Zoomed on Petal.Width axis")

#grid.arrange(p1, p2, ncol = 2)
#grid.arrange(p1_zoom, p2_zoom, ncol = 2)

p3 <- iris %>%
  mutate(Petal.Width = sunflower_jitter(Petal.Width),
         Petal.Length = sunflower_jitter(Petal.Length),
         Sepal.Width = sunflower_jitter(Sepal.Width),
         Sepal.Length = sunflower_jitter(Sepal.Length)
  ) %>%
  pcp_select(c(1:5)) %>%
  pcp_scale(method = "uniminmax") %>%
  pcp_arrange(method = "from-left") %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes() +
  geom_pcp(aes(colour = Species), alpha = 0.5) +
  ggpcp::geom_pcp_boxes(boxwidth = 0.2) +
  ggpcp::geom_pcp_labels()+
  theme_pcp() +
  #geom_point(aes(colour = Species), size = 0.75, position = position_dodge(0.01)) +
  theme(legend.position = "none") +
  iris_scale_color() +
  labs(title = "Iris Data with Sunflower Jitter Sequence for Ties")


p3_zoom <- p3 +
  coord_cartesian(xlim = c(idx - 0.1, idx + 0.1), clip = "on") +
  labs(subtitle = "Zoomed on Petal.Width axis")

#grid.arrange(p1, p3, ncol = 2)
#grid.arrange(p1_zoom, p3_zoom, ncol = 2)

p4 <- iris %>%
  mutate(Petal.Width = intelligent_jitter(Petal.Width),
         Petal.Length = intelligent_jitter(Petal.Length),
         Sepal.Width = intelligent_jitter(Sepal.Width),
         Sepal.Length = intelligent_jitter(Sepal.Length)
  ) %>%
  pcp_select(c(1:5)) %>%
  pcp_scale(method = "uniminmax") %>%
  pcp_arrange(method = "from-left") %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes() +
  geom_pcp(aes(colour = Species), alpha = 0.5) +
  ggpcp::geom_pcp_boxes(boxwidth = 0.2) +
  ggpcp::geom_pcp_labels()+
  theme_pcp() +
  #geom_point(aes(colour = Species), size = 0.75, position = position_dodge(0.01)) +
  theme(legend.position = "none") +
  iris_scale_color() +
  labs(title = "Iris Data with Intelligent Jitter Sequence for Ties")

p4_zoom <- p4 +
  coord_cartesian(xlim = c(idx - 0.1, idx + 0.1), clip = "on") +
  labs(subtitle = "Zoomed on Petal.Width axis")

#grid.arrange(p1, p4, ncol = 2)
#grid.arrange(p1_zoom, p4_zoom, ncol = 2)

