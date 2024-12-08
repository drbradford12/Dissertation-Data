# Load required libraries
# Load necessary libraries
library(GGally)
library(palmerpenguins)
library(tidyverse)
library(gridExtra)
library(ggpcp)

# Function to create jitter amount based on data variability with categorical ties adjustment
calculate_jitter_amount <- function(data, factor = 0.1) {
  # Calculate jitter amount based on standard deviation of each numerical column
  num_cols <- sapply(data, is.numeric)
  jitter_amounts <- sapply(data[num_cols], sd, na.rm = TRUE) * factor
  return(jitter_amounts)
}

# Generic function for jittering numeric values
apply_jitter <- function(data, jitter_type = "random", amount = 0.5) {
  modified_data <- data

  # Identify numeric columns
  numeric_cols <- sapply(modified_data, is.numeric)

  # Apply the selected jittering method
  modified_data[numeric_cols] <- lapply(modified_data[numeric_cols], function(col) {
    if (jitter_type == "random") {
      jitter(col, amount = amount)
    } else if (jitter_type == "rank") {
      rank_col <- rank(col, na.last = "keep")
      col + rank_col * amount
    } else if (jitter_type == "deterministic") {
      ifelse(duplicated(col), col + amount, col)
    } else if (jitter_type == "mean_split") {
      col_mean <- mean(col, na.rm = TRUE)
      ifelse(duplicated(col), col + (col - col_mean) * amount, col)
    } else if (jitter_type == "kde") {
      density_values <- density(na.omit(col), na.rm = TRUE)$bw
      col + density_values[match(col, density_values)] * amount
    } else {
      stop("Invalid jitter_type. Choose from 'random', 'rank', 'deterministic', 'mean_split', or 'kde'.")
    }
  })

  return(modified_data)
}

# Function to plot parallel coordinates with jitter for ties
plot_parallel_with_jitter <- function(ogdata, data, factor = 0.1) {
  # Remove rows with missing values
  ogdata <- na.omit(ogdata)
  data <- na.omit(data)
  # Calculate jitter amounts for numerical columns
  jitter_amounts <- calculate_jitter_amount(data, factor)


  num_cols <- sapply(data, is.numeric)

  og_ggp <- ogdata %>%
    mutate(year = factor(year)) %>%
    pcp_select(3:6, year) %>%  # Adding year as a factor and selecting required variables
    pcp_scale(method="uniminmax") %>%
    pcp_arrange() %>%
    ggplot(aes_pcp()) +
    geom_pcp_axes() +
    geom_pcp_boxes(boxwidth = 0.1, fill="grey70") +
    geom_pcp(aes(colour = species), overplot = "none") +
    geom_pcp_labels() +
    theme_minimal() +
    scale_color_manual(
      values = c("Gentoo" = "#540D6E", "Adelie" = "#219B9D", "Chinstrap" = "#FF8000")
    ) +
    labs(title = "Parallel Coordinate Plot without Jitter",
         color = "Species") +
    theme(axis.text.x = element_text(angle = 45))

  # Apply horizontal jitter to numerical columns to separate ties
  data <- data %>%
  group_by(year, species)

  data[num_cols] <- mapply(function(col, jitter_amt) jitter(col, amount = jitter_amt),
                           data[num_cols], jitter_amounts)


  # Create the parallel coordinate plot
  ggp <- data %>%
    group_by(species) %>%
    arrange(desc(year)) %>%
    pcp_select(3:6, year) %>%  # Adding year as a factor and selecting required variables
    pcp_scale(method="uniminmax") %>%
    pcp_arrange(method="from-left", .by_group = TRUE) %>%
    ggplot(aes_pcp()) +
    geom_pcp_axes() +
    #geom_pcp_boxes(boxwidth = 0.1, fill="grey70") +
    geom_pcp(aes(colour = species), overplot = "none") +
    #geom_pcp_labels() +
    theme_minimal() +
    scale_color_manual(
      values = c("Gentoo" = "#540D6E", "Adelie" = "#219B9D", "Chinstrap" = "#FF8000")
    ) +
    labs(title = "Parallel Coordinate Plot with Jitter",
         color = "Species") +
    theme(axis.text.x = element_text(angle = 45))

  # Print the plot
  print(grid.arrange(og_ggp, ggp, nrow = 2))
}

# Function to determine delta distance for each point in numerical ties
determine_delta_distance <- function(data) {
  # Remove rows with missing values
  data <- na.omit(data)

  # Identify numeric columns
  num_cols <- sapply(data, is.numeric)

  # Calculate delta distance for each point in numerical ties
  delta_distances <- lapply(data[num_cols], function(col) {
    sort_col <- sort(col)
    delta <- diff(sort_col)
    return(delta)
  })

  # Combine results into a data frame
  delta_df <- do.call(cbind, delta_distances)
  colnames(delta_df) <- names(data)[num_cols]

  return(delta_df)
}

# Load the penguins dataset and call the function
data("penguins")
plot_parallel_with_jitter(penguins, apply_jitter(penguins, jitter_type = "random", amount = 2), factor = 0.07)

plot_parallel_with_jitter(penguins, apply_jitter(penguins, jitter_type = "rank", amount = 2), factor = 0.07)

plot_parallel_with_jitter(penguins, apply_jitter(penguins, jitter_type = "deterministic", amount = 2), factor = 0.07)

plot_parallel_with_jitter(penguins, apply_jitter(penguins, jitter_type = "mean_split", amount = 2), factor = 0.07)

#plot_parallel_with_jitter(penguins, apply_jitter(penguins, jitter_type = "kde", amount = 2), factor = 0.07)

#determine_delta_distance(penguins)


# Apply the function to the penguins dataset
penguins_jittered <- apply_jitter(penguins, jitter_type = "random", amount = 2)



# Function to identify numerical ties
detect_ties <- function(data) {
  # Identify numerical columns
  numeric_data <- data %>% select(where(is.numeric))

  # Calculate frequency of ties for each column
  tie_count <- numeric_data %>%
    summarise(across(everything(), ~ sum(duplicated(.))))

  tie_percentage <- numeric_data %>%
    summarise(across(everything(), ~ sum(duplicated(.)) / n() * 100))

  return(list(tie_count = tie_count, tie_percentage = tie_percentage))
}

data("iris")

# Detect numerical ties in the iris dataset
numerical_ties <- detect_ties(iris)

# Print the results
print("Tie Counts:")
print(numerical_ties$tie_count)

print("Tie Percentages:")
print(numerical_ties$tie_percentage)

# Function to handle numerical ties using jittering
handle_ties <- function(data, cols) {
  data %>% mutate(across(all_of(cols), ~ jitter(.)))
}

# Columns to handle ties (numerical columns only)
numerical_columns <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")

# Handle numerical ties by jittering these columns
iris_handled <- handle_ties(iris, numerical_columns)

# Add a group column to represent the species
iris_handled$Species <- as.factor(iris_handled$Species)


plot_data <- iris %>%
  mutate(Species = as.factor(Species))

# Adaptive Privacy-Preserving Visualization
# Adding noise to sensitive data
plot_data_privacy <- plot_data %>%
  mutate(across(starts_with("Sepal"), ~ . + rnorm(n(), mean = 0, sd = 0.1)),
         across(starts_with("Petal"), ~ . + rnorm(n(), mean = 0, sd = 0.1)))

plot_data_privacy <- penguins_clean %>%
  mutate(across(starts_with("_mm"), ~ . + rnorm(n(), mean = 0, sd = 0.1)),
         across(starts_with("_g"), ~ . + rnorm(n(), mean = 0, sd = 0.1)))


plot_data_privacy %>%
  pcp_select(species, 3:6, species) %>%
  pcp_scale(method = "uniminmax") %>%
  pcp_arrange() %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes() +
  geom_pcp(aes(color = species), overplot = "none", alpha = 0.5) +
  geom_pcp_labels() +
  theme_pcp() +
  theme(axis.text.x = element_text(angle = 45)) +
  penguin_scale_color() +
  guides(color = F) +
  ggtitle("Adaptive Privacy-Preserving Visualization")

GGally::ggparcoord(
  data = plot_data_privacy,
  columns = c(5,1:4,5),
  groupColumn = 5
) +
  ggtitle("Adaptive Privacy-Preserving Visualization")

# Plane with Parallel Coordinates
# Highlighting specific regions or planes
highlight_plane <- plot_data %>%
  mutate(Highlight = ifelse(Sepal.Width > 3.5, "Above Plane", "Below Plane"))

GGally::ggparcoord(
  data = highlight_plane,
  columns = 1:4,
  groupColumn = 7
) +
  ggtitle("Plane with Parallel Coordinates")

# Hierarchical Exploration
# Aggregating data by species
hierarchical_data <- plot_data %>%
  group_by(Species) %>%
  summarise(across(everything(), mean))

GGally::ggparcoord(
  data = hierarchical_data,
  columns = 1:4,
  groupColumn = 5
) +
  ggtitle("Hierarchical Exploration")

#### Promising Approaches to Numerical Ties ######

# Function to add space to numerical ties in data
add_space_to_ties <- function(data, tie_spacing = 0.01) {
  # Identify numeric columns
  num_cols <- sapply(data, is.numeric)

  # Adjust numerical ties by adding incremental spacing using
  # Group Averages Over Level Combinations of Factors

  data[num_cols] <- lapply(data[num_cols], function(col) {
    tie_indices <- ave(col, col, FUN = seq_along)
    col + (tie_indices - 1) * tie_spacing
  })

  return(data)
}

# Function to plot parallel coordinates with spacing for numerical ties
plot_parallel_with_spacing <- function(data, tie_spacing = 0.01) {
  # Remove rows with missing values
  data <- na.omit(data)

  # Add spacing to numerical ties
  data <- add_space_to_ties(data, tie_spacing)

  # Create the parallel coordinate plot with adjusted spacing
  ggp <- data %>%
    pcp_select(species, 3:6, species) %>%
    pcp_scale(method = "uniminmax") %>%
    pcp_arrange() %>%
    ggplot(aes_pcp()) +
    geom_pcp_axes() +
    geom_pcp(aes(colour = species)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45)) +
    scale_color_manual(
      values = c("Gentoo" = "#540D6E", "Adelie" = "#219B9D", "Chinstrap" = "#FF8000")
    )

  # Print the plot
  print(ggp)
}

plot_parallel_with_spacing(penguins, tie_spacing = 0.5)


# Function to add spacing to ties in numerical variables
adjust_ties <- function(df, cols_to_adjust, epsilon = 1e-5) {
  df_adjusted <- df

  for (col in cols_to_adjust) {
    if (is.numeric(df[[col]])) {
      ties <- duplicated(df[[col]]) | duplicated(df[[col]], fromLast = TRUE)
      unique_ties <- unique(df[[col]][ties])

      for (tie in unique_ties) {
        tie_indices <- which(df[[col]] == tie)
        adjustment <- seq(-epsilon, epsilon, length.out = length(tie_indices))
        df_adjusted[tie_indices, col] <- df[[col]][tie_indices] + adjustment
      }
    }
  }

  return(df_adjusted)
}

# Adjust ties in the numeric columns of iris
adjusted_data <- adjust_ties(iris, cols_to_adjust = names(iris)[1:4], epsilon = 0.1)

adjusted_data %>%
  pcp_select(Species, 1:4, Species) %>%
  pcp_scale(method = "uniminmax") %>%
  pcp_arrange() %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes() +
  geom_pcp(aes(color = Species)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_color_manual(
    values = c("setosa" = "#540D6E", "versicolor" = "#219B9D", "virginica" = "#FF8000")
  )

# Function to apply ggpcp tie-breaking to numerical data
apply_tie_breaking <- function(df, cols_to_adjust) {
  df_adjusted <- df

  for (col in cols_to_adjust) {
    if (is.numeric(df[[col]])) {
      # Rank the values with ties.method = "random"
      ranks <- rank(df[[col]], ties.method = "first")
      # Scale ranks back to the original range
      min_val <- min(df[[col]], na.rm = TRUE)
      max_val <- max(df[[col]], na.rm = TRUE)
      df_adjusted[[col]] <- scales::rescale(ranks, to = c(min_val, max_val))
    }
  }

  return(df_adjusted)
}

# Apply tie-breaking to the numeric columns of iris
adjusted_data <- apply_tie_breaking(iris, cols_to_adjust = names(iris)[1:4])

adjusted_data %>%
  mutate(Species = as.factor(Species)) %>%
  pcp_select(Species, 1:4, Species) %>%
  pcp_scale(method = "uniminmax") %>%
  pcp_arrange() %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes() +
  geom_pcp(aes(colour = Species), overplot = "none") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_colour_manual(
    values = c("setosa" = "#540D6E", "versicolor" = "#219B9D", "virginica" = "#FF8000")
  )



# Select numeric columns and outcome variable (species)
penguins_numeric <- penguins %>%
  dplyr::select(species, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)

# Tie-breaking function for numerical data with multiple rescaling options
tie_break_numeric <- function(df, group_var, num_var, method = "cumsum") {
  df <- df %>%
    arrange({{ group_var }}, {{ num_var }}) %>%
    group_by({{ group_var }}) %>%
    mutate(
      delta = c(0, diff({{ num_var }})),
      adjusted = case_when(
        method == "cumsum" ~ {{ num_var }} + cumsum(delta + 1e-4),
        method == "rank" ~ scales::rescale(rank({{ num_var }}, ties.method = "first"), to = range({{ num_var }}, na.rm = TRUE)),
        method == "zscore" ~ ({{ num_var }} - mean({{ num_var }}, na.rm = TRUE)) / sd({{ num_var }}, na.rm = TRUE),
        TRUE ~ {{ num_var }}
      )
    ) %>%
    ungroup()
  return(df)
}


# Apply tie-breaking for all numeric variables
penguins_tiebroken <- penguins_numeric  %>%
  mutate(flipper_length_mm = as.double(flipper_length_mm),
         body_mass_g = as.double(body_mass_g))

for (var in names(penguins_numeric)[-1]) {
  penguins_tiebroken <- tie_break_numeric(penguins_tiebroken, species, !!sym(var), method = "zscore")
}

penguins_tiebroken %>%
  pcp_select(species, 2:5, species) %>%
  pcp_scale(method = "uniminmax") %>%
  pcp_arrange() %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes() +
  geom_pcp(aes(color = species), overplot = "none", alpha = 0.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45)) +
  penguin_scale_color() +
  guides(color = F)


