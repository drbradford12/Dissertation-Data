#' Unified Tie-Breaking Algorithm for Parallel Coordinate Plots
#'
#' This module extends ggpcp's categorical tie-breaking approach to numerical
#' variables, creating a single optimization framework that handles both
#' variable types consistently. The algorithm maintains visual continuity
#' across mixed-type data by applying the same spacing logic to numerical ties
#' that ggpcp uses for categorical levels.
#'
#' @references
#' VanderPlas et al. (2023). Penguins Go Parallel: A Grammar of Graphics
#' Framework for Generalized Parallel Coordinate Plots.

library(dplyr)
library(tidyr)
library(rlang)

#' Compute Available Space for Numerical Tie-Breaking
#'
#' Determines the available vertical space around a tied numerical value by
#' examining the local data density. This function identifies the nearest
#' distinct values above and below the tie point and computes the space
#' that can be allocated to spread tied observations.
#'
#' @param value The numerical value with tied observations.
#' @param all_values A numeric vector of all scaled values on the axis.
#' @param n_ties The number of observations tied at this value.
#' @param density_bandwidth Bandwidth parameter for local density estimation
#'   (default 0.05, representing 5% of the axis range).
#' @param min_spacing Minimum spacing to maintain between adjacent values
#'   (default 0.01).
#'
#' @return A list containing:
#'   \item{available_space}{The total vertical space available for spreading}
#'   \item{lower_bound}{The lower limit of the available region}
#'   \item{upper_bound}{The upper limit of the available region}
#'   \item{local_density}{Estimated local density at this value}
#'
#' @details
#' The algorithm proceeds as follows:
#' 1. Identify distinct values excluding the current tied value.
#' 2. Find the nearest neighbor above and below the tied value.
#' 3. Compute midpoints to establish boundaries that respect neighboring
#'    observations.
#' 4. Adjust boundaries based on local density to prevent excessive spread
#'    in sparse regions.
#'
#' @examples
#' values <- c(0.1, 0.3, 0.3, 0.3, 0.5, 0.7)
#' compute_available_space(0.3, values, n_ties = 3)
#'
compute_available_space <- function(value, all_values, n_ties,
                                     density_bandwidth = 0.05,
                                     min_spacing = 0.01) {
  
  # Extract distinct values excluding the tied value
  distinct_vals <- sort(unique(all_values[all_values != value]))
  
  # Find nearest neighbors
  lower_neighbors <- distinct_vals[distinct_vals < value]
  upper_neighbors <- distinct_vals[distinct_vals > value]
  
  # Determine boundaries: use midpoints to nearest neighbors, or axis limits
  lower_bound <- if (length(lower_neighbors) > 0) {
    nearest_lower <- max(lower_neighbors)
    (value + nearest_lower) / 2
  } else {
    max(0, value - density_bandwidth)
  }
  
  upper_bound <- if (length(upper_neighbors) > 0) {
    nearest_upper <- min(upper_neighbors)
    (value + nearest_upper) / 2
  } else {
    min(1, value + density_bandwidth)
  }
  
  # Compute local density using a simple kernel estimate
  # This helps scale the available space based on data concentration
  local_density <- sum(abs(all_values - value) < density_bandwidth) / 
                   length(all_values)
  
  # Adjust available space based on density: denser regions get less spread
  # to maintain visual clarity in crowded areas
  density_factor <- 1 - (local_density * 0.5)  # Scale factor between 0.5 and 1
  
  available_space <- (upper_bound - lower_bound) * density_factor
  
  # Ensure minimum viable spacing
  available_space <- max(available_space, min_spacing * (n_ties - 1))
  
  # Recompute symmetric bounds around the original value
  half_space <- available_space / 2
  adjusted_lower <- max(0, value - half_space)
  adjusted_upper <- min(1, value + half_space)
  
  list(
    available_space = adjusted_upper - adjusted_lower,
    lower_bound = adjusted_lower,
    upper_bound = adjusted_upper,
    local_density = local_density
  )
}


#' Compute Optimal Spacing for Tied Observations
#'
#' Given the available space and number of tied observations, computes the
#' optimal spacing between observations following the ggpcp paradigm:
#' spacing = available_space / (n - 1).
#'
#' @param available_space The total vertical space available.
#' @param n_ties The number of tied observations.
#'
#' @return The spacing value between consecutive tied observations.
#'
compute_optimal_spacing <- function(available_space, n_ties) {
  if (n_ties <= 1) return(0)
  available_space / (n_ties - 1)
}


#' Hierarchical Ordering for Numerical Ties
#'
#' Orders tied observations hierarchically based on adjacent variables to
#' minimize line crossings. This mirrors ggpcp's categorical approach where
#' observations are arranged based on their values in neighboring axes.
#'
#' @param data A data frame containing observations to order.
#' @param tie_indices Integer vector of row indices for tied observations.
#' @param current_var Name of the current variable (column).
#' @param adjacent_vars Character vector of adjacent variable names,
#'   ordered by proximity (nearest first).
#' @param method Direction of hierarchical sorting: "from-left" or
#'   "from-right" (default "from-left").
#'
#' @return An integer vector of the tie_indices reordered to minimize
#'   line crossings with adjacent axes.
#'
#' @details
#' The hierarchical sorting proceeds by:
#' 1. Extracting values from the nearest adjacent variable.
#' 2. Sorting tied observations by these adjacent values.
#' 3. Breaking remaining ties using successively more distant variables.
#' This approach contributes to the Gestalt principle of "common fate,"
#' where observations with similar values move together through the plot.
#'
hierarchical_order_ties <- function(data, tie_indices, current_var,
                                    adjacent_vars, method = "from-left") {
  
  if (length(tie_indices) <= 1) return(tie_indices)
  if (length(adjacent_vars) == 0) return(tie_indices)
  
  # Extract the subset of tied observations

  tied_data <- data[tie_indices, , drop = FALSE]
  
  # Build a sorting key from adjacent variables
  # Priority is given to nearer variables
  sort_cols <- intersect(adjacent_vars, names(data))
  
  if (length(sort_cols) == 0) return(tie_indices)
  
  # Create a composite sorting key
  # For "from-left", variables to the left have priority
  # For "from-right", reverse the priority
  if (method == "from-right") {
    sort_cols <- rev(sort_cols)
  }
  
  # Order by adjacent values hierarchically
  order_expr <- lapply(sort_cols, function(col) {
    if (is.numeric(tied_data[[col]])) {
      tied_data[[col]]
    } else if (is.factor(tied_data[[col]])) {
      as.numeric(tied_data[[col]])
    } else {
      as.numeric(as.factor(tied_data[[col]]))
    }
  })
  
  # Combine into ordering
  sorted_local_idx <- do.call(order, order_expr)
  
  tie_indices[sorted_local_idx]
}


#' Assign Spread Positions to Tied Observations
#'
#' Distributes tied observations evenly within the available space,
#' maintaining perpendicular spread to the axis direction. Observations
#' are ordered hierarchically before position assignment.
#'
#' @param ordered_indices Integer vector of observation indices in their
#'   hierarchically determined order.
#' @param original_value The original (tied) value on the scaled axis.
#' @param available_space The total space available for spreading.
#' @param lower_bound The lower limit of the spread region.
#' @param upper_bound The upper limit of the spread region.
#'
#' @return A named numeric vector where names are observation indices and
#'   values are the adjusted y-coordinates.
#'
assign_spread_positions <- function(ordered_indices, original_value,
                                    available_space, lower_bound, upper_bound) {
  
  n <- length(ordered_indices)
  
  if (n == 1) {
    result <- original_value
    names(result) <- as.character(ordered_indices)
    return(result)
  }
  
  # Compute evenly spaced positions within bounds
  spacing <- compute_optimal_spacing(available_space, n)
  
  # Center the spread around the original value
  center <- (lower_bound + upper_bound) / 2
  total_span <- spacing * (n - 1)
  start_pos <- center - total_span / 2
  
  # Assign positions
  positions <- start_pos + (seq_len(n) - 1) * spacing
  
  # Ensure positions stay within valid range [0, 1]
  positions <- pmax(0, pmin(1, positions))
  
  names(positions) <- as.character(ordered_indices)
  positions
}


#' Unified Tie-Breaking for Numerical Variables
#'
#' Main function that applies the complete tie-breaking algorithm to a
#' numerical variable in a parallel coordinate plot dataset. This function
#' integrates all components: space computation, hierarchical ordering,
#' and position assignment.
#'
#' @param data A data frame in long format with columns for observation ID,
#'   variable name, and scaled value (pcp_id, pcp_x, pcp_y as in ggpcp).
#' @param var_name The name of the numerical variable being processed.
#' @param adjacent_vars Character vector of adjacent variable names for
#'   hierarchical ordering.
#' @param method Sorting method: "from-left" or "from-right".
#' @param tolerance Numerical tolerance for identifying ties (default 1e-10).
#' @param density_bandwidth Bandwidth for local density estimation.
#' @param min_spacing Minimum spacing between observations.
#'
#' @return A data frame with adjusted pcp_y values for tied observations.
#'
#' @details
#' The algorithm follows these steps:
#' 1. Identify all unique values with multiple observations (ties).
#' 2. For each tied value, compute available space based on neighbors
#'    and local density.
#' 3. Order tied observations hierarchically using adjacent variables.
#' 4. Distribute observations evenly within the available space.
#' 5. Return the modified dataset with adjusted y-coordinates.
#'
#' @examples
#' # Assuming pcp_data is output from ggpcp's pcp_select and pcp_scale
#' adjusted_data <- break_numerical_ties(
#'   data = pcp_data,
#'   var_name = "bill_length_mm",
#'   adjacent_vars = c("bill_depth_mm", "flipper_length_mm"),
#'   method = "from-left"
#' )
#'
break_numerical_ties <- function(data, var_name, adjacent_vars,
                                  method = "from-left",
                                  tolerance = 1e-10,
                                  density_bandwidth = 0.05,
                                  min_spacing = 0.01) {
  
  # Filter to the specific variable
  var_data <- data %>%
    filter(pcp_x == var_name)
  
  if (nrow(var_data) == 0) {
    warning(paste("No data found for variable:", var_name))
    return(data)
  }
  
  # Identify ties using tolerance-based grouping
  var_data <- var_data %>%
    mutate(rounded_y = round(pcp_y / tolerance) * tolerance)
  
  tie_groups <- var_data %>%
    group_by(rounded_y) %>%
    filter(n() > 1) %>%
    summarise(
      n_ties = n(),
      indices = list(pcp_id),
      .groups = "drop"
    )
  
  if (nrow(tie_groups) == 0) {
    # No ties to break
    return(data)
  }
  
  # Get all scaled values for this variable (for space computation)
  all_y_values <- var_data$pcp_y
  
  # Need original wide-format data for hierarchical ordering
  # Reconstruct from long format if necessary
  wide_data <- data %>%
    select(pcp_id, pcp_x, pcp_y) %>%
    pivot_wider(names_from = pcp_x, values_from = pcp_y)
  
  # Process each tie group
  adjustments <- list()
  
  for (i in seq_len(nrow(tie_groups))) {
    tied_value <- tie_groups$rounded_y[i]
    n_ties <- tie_groups$n_ties[i]
    tied_ids <- unlist(tie_groups$indices[i])
    
    # Step 1: Compute available space
    space_info <- compute_available_space(
      value = tied_value,
      all_values = all_y_values,
      n_ties = n_ties,
      density_bandwidth = density_bandwidth,
      min_spacing = min_spacing
    )
    
    # Step 2: Hierarchical ordering
    tie_row_indices <- which(wide_data$pcp_id %in% tied_ids)
    ordered_ids <- hierarchical_order_ties(
      data = wide_data,
      tie_indices = tied_ids,  # Use IDs directly
      current_var = var_name,
      adjacent_vars = adjacent_vars,
      method = method
    )
    
    # Match ordered IDs to row indices in wide_data
    ordered_row_indices <- match(ordered_ids, wide_data$pcp_id)
    ordered_row_indices <- ordered_row_indices[!is.na(ordered_row_indices)]
    
    # Use the IDs directly for position assignment
    ordered_ids_clean <- wide_data$pcp_id[ordered_row_indices]
    
    # Step 3: Assign spread positions
    new_positions <- assign_spread_positions(
      ordered_indices = ordered_ids_clean,
      original_value = tied_value,
      available_space = space_info$available_space,
      lower_bound = space_info$lower_bound,
      upper_bound = space_info$upper_bound
    )
    
    # Store adjustments
    for (id_char in names(new_positions)) {
      adjustments[[id_char]] <- new_positions[id_char]
    }
  }
  
  # Apply adjustments to the data
  if (length(adjustments) > 0) {
    adjustment_df <- tibble(
      pcp_id = as.integer(names(adjustments)),
      new_pcp_y = unlist(adjustments)
    )
    
    data <- data %>%
      left_join(adjustment_df, by = "pcp_id", suffix = c("", "_adj")) %>%
      mutate(
        pcp_y = if_else(
          pcp_x == var_name & !is.na(new_pcp_y),
          new_pcp_y,
          pcp_y
        )
      ) %>%
      select(-new_pcp_y)
  }
  
  data
}


#' Unified Arrangement for Mixed-Type Parallel Coordinate Data
#'
#' A comprehensive wrapper function that applies tie-breaking to both
#' categorical and numerical variables using a unified framework. This
#' function extends ggpcp's pcp_arrange to handle numerical ties with
#' the same optimization principles.
#'
#' @param data A data frame output from pcp_select and pcp_scale.
#' @param method Hierarchical sorting direction: "from-left" or "from-right".
#' @param space Proportion of axis for spacing between categorical levels
#'   (default 0.05, matching ggpcp's default).
#' @param handle_numerical Logical indicating whether to apply tie-breaking
#'   to numerical variables (default TRUE).
#' @param tolerance Numerical tolerance for identifying ties.
#' @param density_bandwidth Bandwidth for local density estimation in
#'   numerical tie-breaking.
#' @param min_spacing Minimum spacing to maintain between spread observations.
#'
#' @return A data frame with adjusted pcp_y values for both categorical
#'   and numerical ties.
#'
#' @details
#' The unified framework applies consistent logic to both variable types:
#' - Categorical variables: Standard ggpcp approach with equi-spaced
#'   observations within category boxes.
#' - Numerical variables: Extended approach using local density to
#'   determine available space and hierarchical ordering to minimize
#'   line crossings.
#'
#' The key insight is that numerical ties are analogous to categorical
#' levels: both represent multiple observations at the same position
#' that benefit from systematic separation. By applying the same
#' optimization framework (spacing = available_space / (n-1)) to both
#' types, we achieve visual consistency across mixed-type data.
#'
#' @examples
#' library(palmerpenguins)
#' library(ggpcp)
#'
#' pcp_data <- penguins %>%
#'   pcp_select(bill_length_mm, bill_depth_mm, species, sex) %>%
#'   pcp_scale(method = "uniminmax") %>%
#'   pcp_arrange_unified(method = "from-left", handle_numerical = TRUE)
#'
#' ggplot(pcp_data, aes_pcp()) +
#'   geom_pcp_axes() +
#'   geom_pcp(aes(colour = species)) +
#'   geom_pcp_labels()
#'
pcp_arrange_unified <- function(data, method = "from-left", space = 0.05,
                                 handle_numerical = TRUE,
                                 tolerance = 1e-10,
                                 density_bandwidth = 0.05,
                                 min_spacing = 0.01) {
  
  # Validate method argument
  if (!method %in% c("from-left", "from-right")) {
    stop("method must be 'from-left' or 'from-right'")
  }
  
  # Extract variable information
  var_info <- data %>%
    group_by(pcp_x) %>%
    summarise(
      var_class = first(pcp_class),
      .groups = "drop"
    )
  
  # Get variable order for adjacency determination
  var_order <- unique(data$pcp_x)
  
  # Process each variable
  for (i in seq_along(var_order)) {
    var_name <- var_order[i]
    var_class <- var_info$var_class[var_info$pcp_x == var_name]
    
    # Determine adjacent variables based on method
    if (method == "from-left") {
      adjacent_vars <- if (i > 1) var_order[1:(i-1)] else character(0)
      adjacent_vars <- rev(adjacent_vars)  # Nearest first
    } else {
      adjacent_vars <- if (i < length(var_order)) {
        var_order[(i+1):length(var_order)]
      } else {
        character(0)
      }
    }
    
    # Apply appropriate tie-breaking based on variable type
    if (var_class %in% c("factor", "character")) {
      # For categorical variables, the standard ggpcp approach applies
      # This would call the existing pcp_arrange logic
      # (Integration point with ggpcp internals)
      data <- break_categorical_ties_internal(
        data = data,
        var_name = var_name,
        adjacent_vars = adjacent_vars,
        method = method,
        space = space
      )
    } else if (handle_numerical && var_class %in% c("numeric", "integer")) {
      # For numerical variables, apply the extended algorithm
      data <- break_numerical_ties(
        data = data,
        var_name = var_name,
        adjacent_vars = adjacent_vars,
        method = method,
        tolerance = tolerance,
        density_bandwidth = density_bandwidth,
        min_spacing = min_spacing
      )
    }
  }
  
  data
}


#' Internal Categorical Tie-Breaking (ggpcp-compatible)
#'
#' Handles categorical tie-breaking using ggpcp's established approach.
#' This function serves as a placeholder that would integrate with ggpcp's
#' internal pcp_arrange functionality.
#'
#' @param data Data frame with pcp structure.
#' @param var_name Variable name being processed.
#' @param adjacent_vars Adjacent variables for hierarchical ordering.
#' @param method Sorting direction.
#' @param space Spacing proportion.
#'
#' @return Data frame with adjusted positions.
#'
#' @keywords internal
#'
break_categorical_ties_internal <- function(data, var_name, adjacent_vars,
                                             method, space) {
  
  # Filter to the specific variable
  var_data <- data %>%
    filter(pcp_x == var_name)
  
  # For categorical variables, pcp_level contains the category
  if (!"pcp_level" %in% names(var_data)) {
    return(data)  # Not a categorical variable in expected format
  }
  
  # Get unique levels
  levels <- unique(var_data$pcp_level)
  n_levels <- length(levels)
  
  if (n_levels == 0) return(data)
  
  # Compute box boundaries for each level
  # Following ggpcp: allocate proportional space with gaps
  level_counts <- var_data %>%
    group_by(pcp_level) %>%
    summarise(n = n(), .groups = "drop")
  
  total_obs <- sum(level_counts$n)
  usable_space <- 1 - (space * (n_levels - 1))  # Space minus gaps
  
  # Assign box boundaries
  current_pos <- 0
  box_bounds <- list()
  
  for (lvl in levels) {
    n_in_level <- level_counts$n[level_counts$pcp_level == lvl]
    box_height <- (n_in_level / total_obs) * usable_space
    box_bounds[[lvl]] <- c(lower = current_pos, upper = current_pos + box_height)
    current_pos <- current_pos + box_height + space
  }
  
  # Need wide data for hierarchical ordering
  wide_data <- data %>%
    select(pcp_id, pcp_x, pcp_y) %>%
    pivot_wider(names_from = pcp_x, values_from = pcp_y)
  
  # Process each level
  adjustments <- list()
  
  for (lvl in levels) {
    level_ids <- var_data$pcp_id[var_data$pcp_level == lvl]
    n_in_level <- length(level_ids)
    
    if (n_in_level <= 1) {
      # Single observation: place in center of box
      center <- mean(box_bounds[[lvl]])
      adjustments[[as.character(level_ids)]] <- center
    } else {
      # Multiple observations: order hierarchically and spread
      ordered_ids <- hierarchical_order_ties(
        data = wide_data,
        tie_indices = level_ids,
        current_var = var_name,
        adjacent_vars = adjacent_vars,
        method = method
      )
      
      # Spread evenly within box
      bounds <- box_bounds[[lvl]]
      available <- bounds["upper"] - bounds["lower"]
      spacing <- available / (n_in_level - 1)
      
      positions <- bounds["lower"] + (seq_len(n_in_level) - 1) * spacing
      
      for (j in seq_along(ordered_ids)) {
        adjustments[[as.character(ordered_ids[j])]] <- positions[j]
      }
    }
  }
  
  # Apply adjustments
  if (length(adjustments) > 0) {
    adjustment_df <- tibble(
      pcp_id = as.integer(names(adjustments)),
      new_pcp_y = unlist(adjustments)
    )
    
    data <- data %>%
      left_join(adjustment_df, by = "pcp_id", suffix = c("", "_adj")) %>%
      mutate(
        pcp_y = if_else(
          pcp_x == var_name & !is.na(new_pcp_y),
          new_pcp_y,
          pcp_y
        )
      ) %>%
      select(-new_pcp_y)
  }
  
  data
}


#' Demonstrate Unified Tie-Breaking
#'
#' A demonstration function showing the unified tie-breaking algorithm
#' on synthetic data with both categorical and numerical variables.
#'
#' @param n_obs Number of observations to generate.
#' @param seed Random seed for reproducibility.
#'
#' @return A list containing the original data, processed data, and
#'   diagnostic information.
#'
#' @examples
#' demo <- demonstrate_unified_tiebreaking(n_obs = 50, seed = 42)
#' print(demo$summary)
#'
demonstrate_unified_tiebreaking <- function(n_obs = 50, seed = 42) {
  set.seed(seed)
  
  # Create synthetic data with intentional ties
  demo_data <- tibble(
    id = 1:n_obs,
    # Categorical variable with clear groups
    category = sample(c("A", "B", "C"), n_obs, replace = TRUE,
                      prob = c(0.3, 0.4, 0.3)),
    # Numerical variable with some ties (rounded values)
    numeric_1 = round(runif(n_obs, 0, 10), 1),
    # Another numerical variable
    numeric_2 = round(rnorm(n_obs, 5, 2), 1),
    # Binary categorical
    binary = sample(c("Yes", "No"), n_obs, replace = TRUE)
  )
  
  # Count ties in numerical variables
  ties_num1 <- demo_data %>%
    count(numeric_1) %>%
    filter(n > 1) %>%
    summarise(
      n_tied_values = n(),
      total_tied_obs = sum(n),
      max_tie_size = max(n)
    )
  
  ties_num2 <- demo_data %>%
    count(numeric_2) %>%
    filter(n > 1) %>%
    summarise(
      n_tied_values = n(),
      total_tied_obs = sum(n),
      max_tie_size = max(n)
    )
  
  # Simulate pcp-style long format
  pcp_data <- demo_data %>%
    mutate(pcp_id = id) %>%
    pivot_longer(
      cols = c(category, numeric_1, numeric_2, binary),
      names_to = "pcp_x",
      values_to = "pcp_value"
    ) %>%
    mutate(
      pcp_class = case_when(
        pcp_x %in% c("category", "binary") ~ "factor",
        TRUE ~ "numeric"
      ),
      pcp_level = if_else(pcp_class == "factor", as.character(pcp_value), NA_character_),
      # Simple min-max scaling for demonstration
      pcp_y = as.numeric(pcp_value)
    ) %>%
    group_by(pcp_x) %>%
    mutate(
      pcp_y = if (pcp_class[1] == "numeric") {
        (pcp_y - min(pcp_y, na.rm = TRUE)) / 
          (max(pcp_y, na.rm = TRUE) - min(pcp_y, na.rm = TRUE) + 1e-10)
      } else {
        as.numeric(as.factor(pcp_value)) / length(unique(pcp_value))
      }
    ) %>%
    ungroup()
  
  list(
    original_data = demo_data,
    pcp_data = pcp_data,
    summary = list(
      n_observations = n_obs,
      ties_in_numeric_1 = ties_num1,
      ties_in_numeric_2 = ties_num2
    ),
    message = paste(
      "Created demonstration dataset with", n_obs, "observations.",
      "\nNumeric_1 has", ties_num1$n_tied_values, "values with ties",
      "(", ties_num1$total_tied_obs, "observations affected).",
      "\nNumeric_2 has", ties_num2$n_tied_values, "values with ties",
      "(", ties_num2$total_tied_obs, "observations affected)."
    )
  )
}


# Print package information when sourced
cat("
================================================================================
Unified Tie-Breaking Algorithm for Parallel Coordinate Plots
================================================================================

This module extends ggpcp's categorical tie-breaking approach to numerical
variables, creating a single optimization framework that handles both variable
types using consistent principles:

  1. Space Computation: Determines available space based on local density
     and neighboring values.
  
  2. Optimal Spacing: spacing = available_space / (n - 1)
  

  3. Hierarchical Ordering: Organizes tied observations to minimize line
     crossings with adjacent axes.
  
  4. Visual Continuity: Maintains smooth line flow across mixed-type data.

Main Functions:
  - pcp_arrange_unified()      : Complete pipeline for mixed-type data

  - break_numerical_ties()     : Core algorithm for numerical variables
  - compute_available_space()  : Local density-based space allocation
  - hierarchical_order_ties()  : Crossing-minimizing observation ordering

Usage Example:
  library(palmerpenguins)
  source('unified_tie_breaking.R')
  
  # Apply unified tie-breaking
  processed_data <- pcp_data %>%
    pcp_arrange_unified(method = 'from-left', handle_numerical = TRUE)

================================================================================
")
