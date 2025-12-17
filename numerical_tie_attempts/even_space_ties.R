library(GGally)
library(palmerpenguins)
library(tidyverse)
library(gridExtra)
library(ggpcp)
library(datasets)
library(ggmagnify)
library(ggfx)
library(janitor)

# Load data (Assuming file exists in your path)
asthma <- readr::read_csv(here::here("numerical_tie_attempts/asthma.csv"))

theme_set(theme_bw())

# --- SCALES ---
iris_scale_color <- function(...) scale_color_manual(
  "Species", values = c("setosa" = "#540D6E", "versicolor" = "#219B9D", "virginica" = "#FF8000"), ...)
iris_scale_fill <- function(...) scale_fill_manual(
  "Species", values = c("setosa" = "#540D6E", "versicolor" = "#219B9D", "virginica" = "#FF8000"), ...)

penguins_scale_color <- function(...) scale_color_manual(
  "species", values = c("Adelie" = "#540D6E", "Gentoo" = "#219B9D", "Chinstrap" = "#FF8000"), ...)
penguins_scale_fill <- function(...) scale_fill_manual(
  "species", values = c("Adelie" = "#540D6E", "Gentoo" = "#219B9D", "Chinstrap" = "#FF8000"), ...)

asthma_scale_color <- function(...) scale_color_manual(
  "group_str", values = c("adult" = "#540D6E", "child" = "#219B9D", "adolescent" = "#FF8000"), ...)
asthma_scale_fill <- function(...) scale_fill_manual(
  "group_str", values = c("adult" = "#540D6E", "child" = "#219B9D", "adolescent" = "#FF8000"), ...)

# --- CLEANING ---
iris <- iris %>% na.omit()
penguins <- penguins %>% na.omit()
asthma <- asthma %>% na.omit()

# --- POSITION FUNCTIONS ---
position_even <- function(n) {
  (1:n) / (n + 1) - 0.5
}

position_even_edges <- function(n) {
  if (n == 1) {
    return(0)
  } else {
    return(seq(-0.5, 0.5, length.out = n))
  }
}

position_deltas <- function(n) {
  (1:n) / (n + 1) - 0.5
}

get_tie_box <- function(original_data, spread_data, col_name, x_axis_idx) {

  # 1. Identify rows that are ties in the ORIGINAL data
  # (Values that appear more than once)
  vals <- original_data[[col_name]]
  is_tie <- duplicated(vals) | duplicated(vals, fromLast = TRUE)

  # If no ties, return empty dataframe
  if (sum(is_tie) == 0) return(data.frame())

  # 2. Get the corresponding SPREAD values for those rows
  spread_vals <- spread_data[[col_name]][is_tie]

  # 3. Scale these values to 0-1 (matching pcp_scale("uniminmax"))
  # Note: We scale using the min/max of the SPREAD column to match the plot scaling
  col_min <- min(spread_data[[col_name]], na.rm = TRUE)
  col_max <- max(spread_data[[col_name]], na.rm = TRUE)
  scaled_vals <- (spread_vals - col_min) / (col_max - col_min)

  # 4. Group contiguous scaled values into boxes
  # We sort them and look for gaps larger than a small threshold to separate different tie groups
  scaled_vals <- sort(scaled_vals)
  gaps <- c(0, diff(scaled_vals))
  # Threshold: if gap is huge, it's a different tie group.
  # Since we spread them closely, real gaps are large.
  group_id <- cumsum(gaps > 0.05)

  # 5. Create a dataframe of bounding boxes for each group
  rects <- data.frame(val = scaled_vals, group = group_id) %>%
    group_by(group) %>%
    summarise(
      ymin = min(val) - 0.01, # Add small buffer
      ymax = max(val) + 0.01,
      xmin = x_axis_idx - 0.15, # Box width around axis
      xmax = x_axis_idx + 0.15
    )

  return(rects)
}

# --- PLOTTING ORIGINALS ---
plot_original_iris <- iris %>%
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

plot_original_penguins <- penguins %>%
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

# --- UPDATED SPREAD FUNCTION ---
# Adjusted to calculate width based on minimum resolution rather than fixed %
even_tie_spread <- function(x, key = seq_along(x), width = NULL) {
  stopifnot(is.numeric(x))

  n <- length(x)
  if (n == 0L) return(x)

  out <- x
  is_na <- is.na(x)

  # Calculate default width based on data resolution
  if (is.null(width)) {
    # Get unique sorted values to find the gaps
    unique_vals <- sort(unique(x[!is_na]))

    if (length(unique_vals) > 1) {
      # Find the smallest difference between any two unique values
      min_gap <- min(diff(unique_vals))

      # Set width to 90% of that gap to ensure clusters don't overlap
      width <- min_gap * 0.9
    } else {
      # Fallback if only one unique value exists
      width <- 0.05
    }
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

# --- APPLYING NEW LOGIC (Removed hardcoded width=0.03) ---

p1_even_ties <- iris %>%
  # Removed 'width = 0.03' so it calculates optimal even width automatically
  dplyr::mutate(across(where(is.numeric),
                       ~ even_tie_spread(.x, key = dplyr::row_number()))) %>%
  clean_names() %>%
  pcp_select(sepal_length:species) %>%
  pcp_scale("uniminmax") %>%
  pcp_arrange("from-right")

p2_even_ties <- asthma %>%
  dplyr::mutate(across(where(is.numeric),
                       ~ even_tie_spread(.x, key = dplyr::row_number()))) %>%
  clean_names() %>%
  pcp_select(hospitalizations:comorbidities) %>%
  pcp_scale("uniminmax") %>%
  pcp_arrange("from-right")

p3_even_ties <- penguins %>%
  dplyr::mutate(across(where(is.numeric),
                       ~ even_tie_spread(.x, key = dplyr::row_number()))) %>%
  clean_names() %>%
  pcp_select(c(3:8, 2)) %>%
  pcp_scale("uniminmax") %>%
  pcp_arrange("from-right")

# --- PLOTTING UPDATED VERSIONS ---

p1_even_ties <- p1_even_ties %>%
  ggplot(method = position_deltas, aes_pcp()) +
  geom_pcp(aes(colour = species), alpha = 0.5) +
  geom_pcp_boxes() +
  geom_pcp_labels() +
  theme_pcp() +
  labs(title = "Uniform Spacing",
       subtitle = "Spacing calculated based on minimum value resolution") +
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
       subtitle = "Spacing calculated based on minimum value resolution") +
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
       subtitle = "Spacing calculated based on minimum value resolution") +
  penguins_scale_color() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# --- MAGNIFY PLOTS ---

p1_even_magnify <- p1_even_ties +
  geom_magnify(
    from = c(xmin = 3.8, xmax = 4.2, ymin = -0.05, ymax = 0.25),
    to   = c(xmin = 1.0, xmax = 3.0, ymin = 0.6, ymax = 1.0),
    linewidth = 0.5,
    colour    = "black"
  ) +
  coord_cartesian(clip = "on") +
  labs(subtitle = "Zoomed on Numerical Ties in ggpcp")

p2_even_magnify <- p2_even_ties +
  geom_magnify(
    from = c(xmin = 3.8, xmax = 4.2, ymin = -0.05, ymax = 0.25),
    to   = c(xmin = 1.0, xmax = 3.0, ymin = 0.6, ymax = 1.0),
    linewidth = 0.5,
    colour    = "black"
  ) +
  coord_cartesian(clip = "on") +
  labs(subtitle = "Zoomed on Numerical Ties in ggpcp")

p3_even_magnify <- p3_even_ties +
  geom_magnify(
    from = c(xmin = 3.8, xmax = 4.2, ymin = -0.05, ymax = 0.25),
    to   = c(xmin = 1.0, xmax = 3.0, ymin = 0.6, ymax = 1.0),
    linewidth = 0.5,
    colour    = "black"
  ) +
  coord_cartesian(clip = "on") +
  labs(subtitle = "Zoomed on Numerical Ties in ggpcp")


# --- 1. DEFINE HELPER FUNCTIONS ---

# Function A: Spread ties
even_tie_spread <- function(x, key = seq_along(x), width = NULL) {
  stopifnot(is.numeric(x))
  n <- length(x)
  if (n == 0L) return(x)

  out <- x
  is_na <- is.na(x)

  if (is.null(width)) {
    unique_vals <- sort(unique(x[!is_na]))
    if (length(unique_vals) > 1) {
      width <- min(diff(unique_vals)) * 0.9
    } else {
      width <- 0.05
    }
  }

  if (width <= 0 || !is.finite(width)) return(out)

  ord <- order(x, key, na.last = TRUE)
  run_vals <- x[ord]
  run_id <- cumsum(c(TRUE, diff(run_vals) != 0 | is.na(diff(run_vals))))

  for (group in unique(run_id)) {
    idx <- which(run_id == group)
    if (length(idx) <= 1 || is.na(run_vals[idx[1]])) next
    k <- length(idx)
    offsets <- seq(-0.5, 0.5, length.out = k) * width
    out[ord[idx]] <- run_vals[idx[1]] + offsets
  }
  out
}

# Function B: Generate Annotation Box for one column
get_tie_box <- function(original_data, spread_data, col_name, x_axis_idx) {

  vals <- original_data[[col_name]]
  # Identify ties
  is_tie <- duplicated(vals) | duplicated(vals, fromLast = TRUE)

  if (sum(is_tie) == 0) return(data.frame())

  spread_vals <- spread_data[[col_name]][is_tie]
  original_vals_subset <- vals[is_tie]

  # Scale to 0-1
  col_min <- min(spread_data[[col_name]], na.rm = TRUE)
  col_max <- max(spread_data[[col_name]], na.rm = TRUE)

  if (col_max == col_min) {
    scaled_vals <- rep(0.5, length(spread_vals))
  } else {
    scaled_vals <- (spread_vals - col_min) / (col_max - col_min)
  }

  # Group and summarize
  df_ties <- data.frame(val = scaled_vals, original = original_vals_subset) %>%
    arrange(val) %>%
    mutate(
      gap = c(0, diff(val)),
      group = cumsum(gap > 0.05)
    )

  rects <- df_ties %>%
    group_by(group) %>%
    summarise(
      ymin = min(val) - 0.015,
      ymax = max(val) + 0.015,
      xmin = x_axis_idx - 0.1,
      xmax = x_axis_idx + 0.1,
      # Label logic: Round if numeric
      label = if(is.numeric(original)) as.character(round(first(original), 2)) else as.character(first(original)),
      .groups = "drop"
    )

  return(rects)
}

# Function C: Loop through ALL selected numeric columns
get_all_tie_boxes <- function(original_df, spread_df, selected_cols) {
  all_rects <- list()

  for (i in seq_along(selected_cols)) {
    col_name <- selected_cols[i]
    if (is.numeric(original_df[[col_name]])) {
      rects <- get_tie_box(original_df, spread_df, col_name, x_axis_idx = i)
      all_rects[[col_name]] <- rects
    }
  }
  bind_rows(all_rects)
}


# --- 2. PREPARE DATASETS ---

# A. ASTHMA
asthma <- readr::read_csv(here::here("numerical_tie_attempts/asthma.csv")) %>%
  na.omit() %>%
  janitor::clean_names()

asthma_spread <- asthma %>%
  mutate(across(where(is.numeric), ~ even_tie_spread(.x, key = row_number())))

# --- FIX: Dynamically get names from your range ---
# This ensures we use the exact column names present in your file
asthma_cols <- names(select(asthma, hospitalizations:comorbidities))

# Generate boxes
asthma_boxes <- get_all_tie_boxes(asthma, asthma_spread, asthma_cols)


# B. PENGUINS
penguins <- palmerpenguins::penguins %>%
  na.omit() %>%
  janitor::clean_names()

penguins_spread <- penguins %>%

  mutate(
    year = as.character(year),
    across(where(is.numeric), ~ even_tie_spread(.x, key = row_number())))

# Define selection explicitly
penguins_cols <- c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g", "island")
penguins_boxes <- get_all_tie_boxes(penguins, penguins_spread, penguins_cols)

# --- 3. PLOT ASTHMA ---
p_asthma <- asthma_spread %>%
  pcp_select(all_of(asthma_cols)) %>%
  pcp_scale("uniminmax") %>%
  pcp_arrange("from-right") %>%
  # Disable reordering so indices match (1,2,3...)
  ggplot(aes_pcp()) +

  # 1. Gray Boxes (Background)
  geom_rect(data = asthma_boxes,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey85", color = NA, inherit.aes = FALSE) +

  # 2. Main Lines
  geom_pcp(aes(colour = group_str), alpha = 0.5) +

  # 3. Labels
  geom_label(data = asthma_boxes,
             aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = label),
             fill = "grey40", color = "white", size = 2.5,
             label.padding = unit(0.1, "lines"), label.r = unit(0.1, "lines"),
             inherit.aes = FALSE) +

  geom_pcp_boxes() +
  geom_pcp_labels() +
  theme_pcp() +
  labs(title = "Asthma: Annotated Numerical Ties",
       subtitle = "Integer data (counts) creates frequent ties") +
  scale_color_manual(values = c("adult" = "#540D6E", "child" = "#219B9D", "adolescent" = "#FF8000"))


# --- 4. PLOT PENGUINS ---
p_penguins <- penguins_spread %>%
  pcp_select(all_of(penguins_cols)) %>%
  pcp_scale("uniminmax") %>%
  pcp_arrange("from-right") %>%
  ggplot(aes_pcp()) +

  geom_rect(data = penguins_boxes,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey85", color = NA, inherit.aes = FALSE) +

  geom_pcp(aes(colour = species), alpha = 0.5) +

  geom_label(data = penguins_boxes,
             aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = label),
             fill = "grey40", color = "white", size = 2.5,
             label.padding = unit(0.1, "lines"), label.r = unit(0.1, "lines"),
             inherit.aes = FALSE) +

  geom_pcp_boxes() +
  geom_pcp_labels() +
  theme_pcp() +
  labs(title = "Penguins: Annotated Numerical Ties",
       subtitle = "Labels show original values") +
  scale_color_manual(values = c("Adelie" = "#540D6E", "Gentoo" = "#219B9D", "Chinstrap" = "#FF8000"))

# 3. Create the spread data
iris_clean <- iris %>% na.omit()
iris_spread <- iris_clean %>%
  mutate(across(where(is.numeric), ~ even_tie_spread(.x, key = row_number())))

# 4. Generate gray boxes for specific columns
# petal_width is the 4th axis, sepal_width is the 2nd
box_petal <- get_tie_box(iris_clean, iris_spread, "petal_width", x_axis_idx = 4)
box_sepal <- get_tie_box(iris_clean, iris_spread, "sepal_width", x_axis_idx = 2)

# Combine them into one dataframe for plotting
all_boxes <- bind_rows(box_petal, box_sepal)

# 5. Plot
p_iris <- iris_spread %>%
  pcp_select(sepal_length:species) %>%
  pcp_scale("uniminmax") %>%
  pcp_arrange("from-right") %>%
  ggplot(method = position_deltas, aes_pcp()) +

  # --- ADD GRAY BOXES FIRST ---
  # fill = "grey80" creates the light gray box background
  # inherit.aes = FALSE prevents errors with pcp coordinates
  geom_rect(data = all_boxes,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey80",     # Gray fill (matches category style)
            color = NA,          # No border
            alpha = 0.8,         # Slight transparency
            inherit.aes = FALSE) +

  # --- THEN ADD LINES ---
  geom_pcp(aes(colour = species), alpha = 0.5) +
  geom_pcp_boxes() +
  geom_pcp_labels() +
  theme_pcp() +
  labs(title = "Numerical Ties with Gray Category Boxes",
       subtitle = "Gray boxes added behind numerical ties (Sepal Width & Petal Width)") +
  # Re-define color scale if needed (using your custom function if defined)
  scale_color_manual(values = c("setosa" = "#540D6E", "versicolor" = "#219B9D", "virginica" = "#FF8000"))

