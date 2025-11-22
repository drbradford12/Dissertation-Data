library(GGally)
library(palmerpenguins)
library(tidyverse)
library(gridExtra)
library(ggpcp)
library(datasets)
library(ggmagnify)
library(ggfx)
library(janitor)
library(palmerpenguins)

asthma <- readr::read_csv(here::here("numerical_tie_attempts/asthma.csv"))

theme_set(theme_bw())

iris_scale_color <-   function(...) scale_color_manual(
  "Species", values = c("setosa" = "#540D6E", "versicolor" = "#219B9D", "virginica" = "#FF8000", ...))
iris_scale_fill <-   function(...) scale_fill_manual(
  "Species", values = c("setosa" = "#540D6E", "versicolor" = "#219B9D", "virginica" = "#FF8000", ...))

penguins_scale_color <-   function(...) scale_color_manual(
  "species", values = c("Adelie" = "#540D6E", "Gentoo" = "#219B9D", "Chinstrap" = "#FF8000", ...))
penguins_scale_fill <-   function(...) scale_fill_manual(
  "species", values = c("Adelie" = "#540D6E", "Gentoo" = "#219B9D", "Chinstrap" = "#FF8000", ...))

asthma_scale_color <-   function(...) scale_color_manual(
  "group_str", values = c("adult" = "#540D6E", "child" = "#219B9D", "adolescent" = "#FF8000", ...))
penguins_scale_fill <-   function(...) scale_fill_manual(
  "group_str", values = c("adult" = "#540D6E", "child" = "#219B9D", "adolescent" = "#FF8000", ...))

iris <- iris %>%
  na.omit()

penguins <- penguins %>%
  na.omit()

asthma <- asthma %>%
  na.omit()


position_even <- function(n) {
  (1:n) / (n + 1) - 0.5
}

position_even_edges <- function(n) {
  if (n == 1) {
    return(0)
  } else {
    # seq() generates n points between -0.5 and 0.5, inclusive
    return(seq(-0.5, 0.5, length.out = n))
  }
}

position_deltas <- function(n) {
  (1:n) / (n + 1) - 0.5
}


plot_original_iris <- iris  %>%
  clean_names() %>%
  pcp_select(c(1:5)) %>%
  pcp_scale(method="uniminmax") %>%
  ggpcp::pcp_arrange(method = "from-left") %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes() +
  geom_pcp(aes(colour = species), alpha = 0.5) +
  ggpcp::geom_pcp_boxes(boxwidth = 0.2) +
  ggpcp::geom_pcp_labels() +
  theme_pcp() +
  labs(title = "Original Data (with ties)",
       subtitle = "Overlapping lines create visual density issues") +
  iris_scale_color() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

p1_magnify_species <- plot_original_iris +
  geom_magnify(
    from = c(xmin = 4.8, xmax = 5.2, ymin = 0.45, ymax = 0.55),
    to   = c(xmin = 2.5, xmax = 4.5, ymin = 0.5, ymax = 1.0),
    shadow    = TRUE,
    linewidth = 0.5,
    colour    = "black"
  ) +
  # ensure nothing bleeds into the margins
  coord_cartesian(clip = "on")  +
  labs(subtitle = "Zoomed on Categorical Ties in ggpcp")

p1_magnify_ties <- plot_original_iris +
  geom_magnify(
    from = c(xmin = 3.8, xmax = 4.2, ymin = -0.05, ymax = 0.25),
    to   = c(xmin = 1.0, xmax = 3.0, ymin = 0.6, ymax = 1.0),
    shadow    = TRUE,
    linewidth = 0.5,
    colour    = "black"
  )+
  # ensure nothing bleeds into the margins
  coord_cartesian(clip = "on") +
  labs(subtitle = "Zoomed on Numerical Ties in ggpcp")


plot_original_penguins <- penguins  %>%
  clean_names() %>%
  pcp_select(c(3:8, 2)) %>%
  pcp_scale(method="uniminmax") %>%
  ggpcp::pcp_arrange(method = "from-left") %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes() +
  geom_pcp(aes(colour = species), alpha = 0.5) +
  ggpcp::geom_pcp_boxes(boxwidth = 0.2) +
  ggpcp::geom_pcp_labels() +
  theme_pcp() +
  labs(title = "Original Data (with ties)",
       subtitle = "Overlapping lines create visual density issues") +
  penguins_scale_color() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

p2_magnify_species <- plot_original_penguins +
  geom_magnify(
    from = c(xmin = 4.8, xmax = 5.2, ymin = 0.45, ymax = 0.55),
    to   = c(xmin = 2.5, xmax = 4.5, ymin = 0.5, ymax = 1.0),
    shadow    = TRUE,
    linewidth = 0.5,
    colour    = "black"
  ) +
  # ensure nothing bleeds into the margins
  coord_cartesian(clip = "on")  +
  labs(subtitle = "Zoomed on Categorical Ties in ggpcp")

p1_magnify_ties <- plot_original_penguins +
  geom_magnify(
    from = c(xmin = 3.8, xmax = 4.2, ymin = -0.05, ymax = 0.25),
    to   = c(xmin = 1.0, xmax = 3.0, ymin = 0.6, ymax = 1.0),
    shadow    = TRUE,
    linewidth = 0.5,
    colour    = "black"
  )+
  # ensure nothing bleeds into the margins
  coord_cartesian(clip = "on") +
  labs(subtitle = "Zoomed on Numerical Ties in ggpcp")


p2_original <- asthma %>%
  clean_names() %>%
  pcp_select(hospitalizations:comorbidities) %>%  # select everything
  pcp_scale("uniminmax") %>%
  pcp_arrange("from-right") %>%
         ggplot(method = position_deltas, aes_pcp()) +
         geom_pcp(aes(colour = group_str)) +
         geom_pcp_labels() +
         geom_pcp_boxes() +
         theme_pcp() +
  labs(title = "Original Plot of Asthma Data from Hammock Plot") +
  asthma_scale_color() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

even_tie_spread <- function(x, key = seq_along(x), width = NULL, frac = 0.02) {
  stopifnot(is.numeric(x))

  n <- length(x)
  if (n == 0L) return(x)

  out <- x
  is_na <- is.na(x)

  # Calculate default width
  if (is.null(width)) {
    rng <- range(x[!is_na], na.rm = TRUE)
    width <- if (is.finite(diff(rng))) frac * diff(rng) else 0
  }

  if (width <= 0 || !is.finite(width)) return(out)

  # Sort by value, then key
  ord <- order(x, key, na.last = TRUE)

  # Identify tie groups
  run_vals <- x[ord]
  run_id <- cumsum(c(TRUE, diff(run_vals) != 0 | is.na(diff(run_vals))))

  # Spread each tie group
  for (group in unique(run_id)) {
    idx <- which(run_id == group)

    if (length(idx) <= 1 || is.na(run_vals[idx[1]])) next

    # Create evenly spaced offsets
    k <- length(idx)
    offsets <- seq(-0.5, 0.5, length.out = k) * width

    # Apply offsets
    out[ord[idx]] <- run_vals[idx[1]] + offsets
  }

  out
}



p1_even_ties <- iris %>%
  dplyr::mutate(across(where(is.numeric),
                       ~ even_tie_spread(.x, width = 0.03, key = dplyr::row_number()))) %>%
  clean_names() %>%
  pcp_select(sepal_length:species) %>%
  pcp_scale("uniminmax") %>%
  pcp_arrange("from-right")

p2_even_ties <- asthma %>%
  dplyr::mutate(across(where(is.numeric),
                       ~ even_tie_spread(.x, width = 0.03, key = dplyr::row_number()))) %>%
  clean_names() %>%
  pcp_select(hospitalizations:comorbidities) %>%
  pcp_scale("uniminmax") %>%
  pcp_arrange("from-right")

p3_even_ties <- penguins %>%
  dplyr::mutate(across(where(is.numeric),
                       ~ even_tie_spread(.x, width = 0.03, key = dplyr::row_number()))) %>%
  clean_names() %>%
  pcp_select(c(3:8, 2)) %>%
  pcp_scale("uniminmax") %>%
  pcp_arrange("from-right")

p1_even_ties <- p1_even_ties %>%
  ggplot(method = position_deltas, aes_pcp()) +
  geom_pcp(aes(colour = species), alpha = 0.5) +
  geom_pcp_boxes() +
  geom_pcp_labels() +
  theme_pcp() +
  labs(title = "Uniform Spacing",
        subtitle = "Uniform distribution within adaptive intervals") +
  iris_scale_color() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

p2_even_ties <- p2_even_ties %>%
  ggplot(method = position_deltas, aes_pcp()) +
  geom_pcp(aes(colour = group_str), alpha = 0.5) +
  geom_pcp_boxes() +
  geom_pcp_labels() +
  theme_pcp() +
  labs(title = "Uniform Spacing",
       subtitle = "Uniform distribution within intervals") +
  asthma_scale_color() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

p3_even_ties <- p3_even_ties %>%
  ggplot(method = position_deltas, aes_pcp()) +
  geom_pcp(aes(colour = species), alpha = 0.5) +
  geom_pcp_boxes() +
  geom_pcp_labels() +
  theme_pcp() +
  labs(title = "Uniform Spacing",
       subtitle = "Uniform distribution within intervals") +
  penguins_scale_color() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

p1_even_magnify <- p1_even_ties +
  geom_magnify(
    from = c(xmin = 3.8, xmax = 4.2, ymin = -0.05, ymax = 0.25),
    to   = c(xmin = 1.0, xmax = 3.0, ymin = 0.6, ymax = 1.0),
    linewidth = 0.5,
    colour    = "black"
  ) +
  # ensure nothing bleeds into the margins
  coord_cartesian(clip = "on") +
  labs(subtitle = "Zoomed on Numerical Ties in ggpcp")

p2_even_magnify <- p2_even_ties +
  geom_magnify(
    from = c(xmin = 3.8, xmax = 4.2, ymin = -0.05, ymax = 0.25),
    to   = c(xmin = 1.0, xmax = 3.0, ymin = 0.6, ymax = 1.0),
    linewidth = 0.5,
    colour    = "black"
  ) +
  # ensure nothing bleeds into the margins
  coord_cartesian(clip = "on") +
  labs(subtitle = "Zoomed on Numerical Ties in ggpcp")

p3_even_magnify <- p3_even_ties +
  geom_magnify(
    from = c(xmin = 3.8, xmax = 4.2, ymin = -0.05, ymax = 0.25),
    to   = c(xmin = 1.0, xmax = 3.0, ymin = 0.6, ymax = 1.0),
    linewidth = 0.5,
    colour    = "black"
  ) +
  # ensure nothing bleeds into the margins
  coord_cartesian(clip = "on") +
  labs(subtitle = "Zoomed on Numerical Ties in ggpcp")


