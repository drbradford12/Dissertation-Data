library(GGally)
library(palmerpenguins)
library(tidyverse)
library(gridExtra)
library(ggpcp)
library(datasets)
library(ggmagnify)
library(ggfx)
library(janitor)

theme_set(theme_bw())

iris_scale_color <-   function(...) scale_color_manual(
  "Species", values = c("setosa" = "#540D6E", "versicolor" = "#219B9D", "virginica" = "#FF8000", ...))
iris_scale_fill <-   function(...) scale_fill_manual(
  "Species", values = c("setosa" = "#540D6E", "versicolor" = "#219B9D", "virginica" = "#FF8000", ...))


iris <- iris %>%
  na.omit()

# Uniform jittering function (ε = 1/n)
jitter_ties_uniform <- function(x, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  x_jittered <- numeric(length(x))
  unique_vals <- unique(x)

  for (v in unique_vals) {
    tie_indices <- which(x == v)
    n_ties <- length(tie_indices)
    epsilon <- 1 / n_ties
    lower_bound <- v - epsilon / 2
    upper_bound <- v + epsilon / 2
    x_jittered[tie_indices] <- runif(n_ties, min = lower_bound, max = upper_bound)
  }
  return(x_jittered)
}

# Halton sequence jittering
jitter_ties_halton <- function(x, base = 2) {
  halton_sequence <- function(n, base) {
    seq <- numeric(n)
    for (i in 1:n) {
      f <- 1 / base
      j <- i
      while (j > 0) {
        seq[i] <- seq[i] + f * (j %% base)
        j <- floor(j / base)
        f <- f / base
      }
    }
    return(seq)
  }

  x_jittered <- numeric(length(x))
  unique_vals <- unique(x)

  for (v in unique_vals) {
    tie_indices <- which(x == v)
    n_ties <- length(tie_indices)
    epsilon <- 1 / n_ties
    halton_vals <- halton_sequence(n_ties, base)
    x_jittered[tie_indices] <- v - epsilon/2 + halton_vals * epsilon
  }
  return(x_jittered)
}


# Apply jittering methods
data_uniform <- iris %>%
  clean_names() %>%
  mutate(across(sepal_length:petal_width, ~jitter_ties_uniform(., seed = 123), .names = "{.col}_uniform"))

data_halton <- iris %>%
  clean_names() %>%
  mutate(across(sepal_length:petal_width, ~jitter_ties_halton(., base = 2), .names = "{.col}_halton"))


plot_original <- iris  %>%
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

p1_magnify_species <- plot_original +
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

p1_magnify_ties <- plot_original +
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


# Uniform jittering plot
pcp_data_uniform <- data_uniform %>%
  select(species, ends_with("_uniform")) %>%
  rename_with(~gsub("_uniform", "", .), ends_with("_uniform"))

plot_uniform <- pcp_data_uniform %>%
  pcp_select(c(2:5, 1)) %>%
  pcp_scale(method="uniminmax") %>%
  ggpcp::pcp_arrange(method = "from-left") %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes() +
  geom_pcp(aes(colour = species), alpha = 0.5) +
  ggpcp::geom_pcp_boxes(boxwidth = 0.2) +
  ggpcp::geom_pcp_labels() +
  theme_pcp() +
  labs(title = "Uniform Jittering (epilson = 1/n)",
       subtitle = "Random distribution within adaptive intervals") +
  iris_scale_color() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

plot_uniform_magnify <- plot_uniform +
  geom_magnify(
    from = c(xmin = 3.8, xmax = 4.2, ymin = -0.05, ymax = 0.25),
    to   = c(xmin = 1.0, xmax = 3.0, ymin = 0.6, ymax = 1.0),
    linewidth = 0.5,
    colour    = "black"
  ) +
  # ensure nothing bleeds into the margins
  coord_cartesian(clip = "on") +
  labs(subtitle = "Zoomed on Numerical Ties in ggpcp")

# Halton sequence plot
pcp_data_halton <- data_halton %>%
  select(species, ends_with("_halton")) %>%
  rename_with(~gsub("_halton", "", .), ends_with("_halton"))


plot_halton <- pcp_data_halton %>%
  pcp_select(c(2:5, 1)) %>%
  pcp_scale(method="uniminmax") %>%
  ggpcp::pcp_arrange(method = "from-left") %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes() +
  geom_pcp(aes(colour = species), alpha = 0.5) +
  ggpcp::geom_pcp_boxes(boxwidth = 0.2) +
  ggpcp::geom_pcp_labels() +
  theme_pcp() +
  labs(title = "Halton Sequence Jittering",
       subtitle = "Quasi-random distribution for better visual uniformity") +
  iris_scale_color() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

plot_halton_magnify <- plot_halton +
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
