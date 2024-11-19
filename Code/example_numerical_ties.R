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

# Function to create categorical jitter
# categorical_jitter <- function(column, factor = 0.1) {
#   unique_levels <- unique(column)
#   level_jitter <- seq(-factor, factor, length.out = length(unique_levels))
#   names(level_jitter) <- unique_levels
#   return(level_jitter[column])
# }

# Function to plot parallel coordinates with jitter for ties
plot_parallel_with_jitter <- function(data, factor = 0.1) {
  # Remove rows with missing values
  data <- na.omit(data)

  # Calculate jitter amounts for numerical columns
  jitter_amounts <- calculate_jitter_amount(data, factor)


  num_cols <- sapply(data, is.numeric)



  og_ggp <- data %>%
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
    labs(title = "Parallel Coordinate Plot without Jitter",
         color = "Species") +
    theme(axis.text.x = element_text(angle = 45))

  # Apply horizontal jitter to numerical columns to separate ties
  data <- data %>%
  group_by(species, year)

  data[num_cols] <- mapply(function(col, jitter_amt) jitter(col, amount = jitter_amt),
                           data[num_cols], jitter_amounts)


  # Create the parallel coordinate plot
  ggp <- data %>%
    pcp_select(as.vector(which(num_cols))) %>%  # Adding year as a factor and selecting required variables
    pcp_scale(method="uniminmax") %>%
    pcp_arrange(method="from-left") %>%
    ggplot(aes_pcp()) +
    geom_pcp_axes() +
    #geom_pcp_boxes(boxwidth = 0.1, fill="grey70") +
    geom_pcp(aes(colour = species), overplot = "none") +
    #geom_pcp_labels() +
    theme_minimal() +
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
plot_parallel_with_jitter(penguins, factor = 0.2)
determine_delta_distance(penguins)




