library(tidyverse)
library(tidyselect)

# This function was used to get the automatic values of each of the numerical variables. This may no longer needed for this work.
auto_fraction <- function(values, scale_factor = 0.01) {
  unique_vals <- unique(sort(values))
  diffs <- diff(unique_vals)
  positive_diffs <- diffs[diffs > 0]
  if (length(positive_diffs) == 0) return(scale_factor)
  typical_gap <- median(positive_diffs)
  scale_factor * typical_gap
}

numerical_tie_breaker <- function(values, alpha = 1, default_delta = 1e-8) {
  # Compute delta as the minimal positive difference among sorted unique values
  unique_vals <- unique(sort(values))
  diffs <- diff(unique_vals)
  positive_diffs <- diffs[diffs > 0]
  delta <- if (length(positive_diffs) > 0) min(positive_diffs) else default_delta

  # Use a secondary key (original order) to ensure stability
  sorted_indices <- order(values, seq_along(values))
  sorted_values <- values[sorted_indices]
  adjusted_sorted <- sorted_values

  # Identify consecutive runs of tied values
  rle_vals <- rle(sorted_values)
  cum_lengths <- cumsum(rle_vals$lengths)
  start_indices <- c(1, head(cum_lengths, -1) + 1)

  # For each tied group, assign symmetric offsets with max offset = α*(delta/2)
  for (j in seq_along(rle_vals$lengths)) {
    len <- rle_vals$lengths[j]
    if (len > 1) {
      spacing <- (alpha * delta) / (len - 1)
      offsets <- spacing * (seq_len(len) - (len + 1) / 2)
      indices <- start_indices[j]:cum_lengths[j]
      adjusted_sorted[indices] <- sorted_values[indices] + offsets
    }
  }

  result <- numeric(length(values))
  result[sorted_indices] <- adjusted_sorted
  result
}

adjusted_numerical_tie_breaker <- function(values, alpha = 1) {
  # Only apply offsets if there are ties
  if (!any(duplicated(values))) return(values)
  idx <- order(values, seq_along(values))
  adjusted_sorted <- numerical_tie_breaker(values[idx], alpha = alpha)
  result <- numeric(length(values))
  result[idx] <- adjusted_sorted
  result
}

pcp_arrange_with_ties <- function(data, method = "from-right", space = 0.05, .by_group = TRUE, alpha = 1) {
  pcp_id <- pcp_x <- pcp_y <- pcp_yend <- pcp_class <- NULL
  pcp_right <- pcp_left <- replace_y <- replace_yend <- NULL

  if (.by_group == FALSE) data <- data %>% ungroup()
  groups <- names(attr(data, "groups"))
  if (length(groups) > 0) groups <- setdiff(groups, ".rows")

  add_space <- function(subdata, space, replace_var, y_var) {
    subdata %>% mutate(
      "{{ replace_var }}" := (1 - space) * {{ replace_var }} + {{ y_var }} * space
    )
  }

  from_right <- function(subdata, index_start, index_last, var_y, replace_var) {
    select1 <- setdiff(unique(c(groups, "pcp_id", paste0(".pcp.", vars$pcp_x[index_start:index_last]))), "pcp_x")
    select2 <- setdiff(unique(c(groups, "pcp_id", paste0(".pcp.", vars$pcp_x[index_start]))), "pcp_x")
    var_y <- rlang::enquo(var_y)

    subdata <- subdata %>%
      tidyr::pivot_wider(names_from = .data$pcp_x, values_from = {{ var_y }},
                         names_prefix = ".pcp.", names_repair = "unique") %>%
      select(select1) %>%
      arrange(across(-.data$pcp_id)) %>%
      select(select2) %>%
      tidyr::pivot_longer(cols = paste0(".pcp.", vars$pcp_x[index_start]),
                          names_to = "pcp_x", values_to = as_label(var_y), names_prefix = ".pcp.")

    subdata %>% mutate(
      "{{ replace_var }}" := (1:n() - 0.5) / n() * max({{ var_y }}, na.rm = TRUE)
    )
  }

  vars <- data %>% group_by(pcp_x, .add = FALSE) %>%
    summarize(pcp_class = pcp_class[1]) %>%
    mutate(pcp_x = as.character(pcp_x))
  factor_targets <- which(vars$pcp_class == "factor")
  numvars <- nrow(vars)
  selects <- unique(c(groups, "pcp_id", "pcp_x", "pcp_y"))

  if (method == "from-right") {
    sapply(rev(factor_targets), function(index_start) {
      index_end <- if (index_start < numvars) numvars else 1
      block <- data %>% filter(pcp_x %in% vars$pcp_x[index_start:index_end]) %>% select(selects)
      b1 <- from_right(block, index_start, index_end, pcp_y, replace)
      b1 <- add_space(b1, space = space, replace, pcp_y)
      bys <- unique(c(groups, "pcp_id", "pcp_x"))
      selects2 <- unique(c(groups, "pcp_id", "pcp_x", "replace"))
      data <<- data %>% left_join(b1 %>% select(selects2), by = bys)
      data <<- data %>% mutate(
        pcp_y = ifelse(!is.na(replace) & !is.na(pcp_y), replace, pcp_y),
        pcp_x = factor(pcp_x, levels = vars$pcp_x)
      ) %>% select(-replace)
    })
  }

  if (method == "from-left") {
    sapply(factor_targets, function(index_start) {
      index_end <- if (index_start == 1) numvars else 1
      block <- data %>% filter(pcp_x %in% vars$pcp_x[index_start:index_end]) %>% select(selects)
      b1 <- from_right(block, index_start, index_end, pcp_y, replace)
      b1 <- add_space(b1, space = space, replace, pcp_y)
      bys <- unique(c(groups, "pcp_id", "pcp_x"))
      selects2 <- unique(c(groups, "pcp_id", "pcp_x", "replace"))
      data <<- data %>% left_join(b1 %>% select(selects2), by = bys)
      data <<- data %>% mutate(
        pcp_y = ifelse(!is.na(replace), replace, pcp_y),
        pcp_x = factor(pcp_x, levels = vars$pcp_x)
      ) %>% select(-replace)
    })
  }

  if (method == "from-both") {
    right <- data %>% select(selects, "pcp_yend") %>% select(-pcp_y) %>%
      tidyr::pivot_wider(names_from = "pcp_x", values_from = "pcp_yend")
    left <- data %>% select(selects, "pcp_yend") %>% select(-pcp_yend) %>%
      tidyr::pivot_wider(names_from = "pcp_x", values_from = "pcp_y")
    sapply(factor_targets, function(index_start) {
      index_right <- if (index_start < numvars) index_start + 1 else index_start
      block_right <- data.frame(
        pcp_id = left$pcp_id,
        pcp_yend = right[[vars$pcp_x[index_start]]],
        left %>% select(vars$pcp_x[index_right:numvars]),
        pcp_y = left %>% select(vars$pcp_x[index_start])
      )
      block_right <- block_right %>% arrange(across(-pcp_id)) %>%
        mutate(replace_yend = (1:n() - 0.5) / n() * max(pcp_yend, na.rm = TRUE)) %>%
        arrange(pcp_id)
      block_right <- add_space(block_right, space = space, replace_yend, pcp_yend)
      right[[vars$pcp_x[index_start]]] <<- block_right$replace_yend
      if (index_start == 1) {
        left[[vars$pcp_x[index_start]]] <<- block_right$replace_yend
      } else {
        block_left <- data.frame(
          pcp_id = right$pcp_id,
          pcp_y = left[[vars$pcp_x[index_start]]],
          right %>% select(vars$pcp_x[(index_start-1):1]) %>% select(-pcp_id)
        )
        block_left <- block_left %>% arrange(across(-pcp_id)) %>%
          mutate(replace_y = (1:n() - 0.5) / n() * max(pcp_y, na.rm = TRUE)) %>%
          arrange(pcp_id)
        block_left <- add_space(block_left, space = space, replace_y, pcp_y)
        left[[vars$pcp_x[index_start]]] <<- block_left$replace_y
      }
    })
    replace_yend <- right %>% tidyr::pivot_longer(-pcp_id, names_to = "pcp_x", values_to = "replace_yend")
    replace_y <- left %>% tidyr::pivot_longer(-pcp_id, names_to = "pcp_x", values_to = "replace_y")
    data <- data %>% left_join(replace_yend %>% select("pcp_id", "pcp_x", starts_with("replace_")),
                               by = c("pcp_x", "pcp_id"))
    data <- data %>% left_join(replace_y %>% select("pcp_id", "pcp_x", starts_with("replace_")),
                               by = c("pcp_x", "pcp_id"))
    if (!is.null(data$replace_y))
      data <- data %>% mutate(pcp_y = ifelse(!is.na(replace_y), replace_y, pcp_y))
    if (!is.null(data$replace_yend))
      data <- data %>% mutate(pcp_yend = ifelse(!is.na(replace_yend), replace_yend, pcp_yend))
    data <- data %>% select(-starts_with("replace_")) %>%
      mutate(pcp_x = factor(pcp_x, levels = vars$pcp_x))
  }

  if (method == "from-both-keep") {
    sapply(factor_targets, function(index_start) {
      index_left <- if (index_start == 1) index_start else index_start - 1
      index_right <- if (index_start == numvars) index_start else index_start + 1
      block <- data %>% filter(pcp_x %in% vars$pcp_x[index_start])
      right <- data %>% filter(pcp_x %in% vars$pcp_x[index_right]) %>% ungroup()
      left <- data %>% filter(pcp_x %in% vars$pcp_x[index_left]) %>% ungroup()
      block <- block %>% left_join(
        right %>% select(pcp_y, pcp_id) %>% rename(pcp_right = pcp_y),
        by = "pcp_id"
      )
      block <- block %>% arrange(pcp_yend, pcp_right) %>%
        mutate(replace_yend = (1:n() - 0.5) / n() * max(pcp_yend, na.rm = TRUE))
      if (index_start == 1) {
        block$replace_y <- block$replace_yend
      } else {
        block <- block %>% left_join(
          left %>% select(pcp_yend, pcp_id) %>% rename(pcp_left = pcp_yend),
          by = "pcp_id"
        ) %>% arrange(pcp_y, pcp_left) %>%
          mutate(replace_y = (1:n() - 0.5) / n() * max(pcp_y, na.rm = TRUE))
      }
      block <- add_space(block, space = space, replace_y, pcp_y)
      block <- add_space(block, space = space, replace_yend, pcp_y)
      data <- data %>% left_join(block %>% select("pcp_id", "pcp_x", starts_with("replace_")),
                                 by = c("pcp_x", "pcp_id"))
      if (!is.null(data$replace_y))
        data <- data %>% mutate(pcp_y = ifelse(!is.na(replace_y), replace_y, pcp_y))
      if (!is.null(data$replace_yend))
        data <- data %>% mutate(pcp_yend = ifelse(!is.na(replace_yend), replace_yend, pcp_yend))
      data <<- data %>% select(-starts_with("replace_"))
    })
  }

  # If not "from-both", copy pcp_y into pcp_yend so lines remain consistent
  if (method != "from-both")
    data$pcp_yend <- data$pcp_y

  # --- Tie adjustment logic only: if numeric vars have ties, offset them. ---
  numeric_vars <- vars %>% filter(pcp_class == "numeric") %>% pull(pcp_x)
  if (length(numeric_vars) > 0) {
    data <- data %>%
      group_by(pcp_x) %>%
      mutate(
        # Mark ties for each numeric variable
        tied_flag = duplicated(pcp_y) | duplicated(pcp_y, fromLast = TRUE),
        tie_group = ifelse(tied_flag, as.integer(factor(pcp_y)), NA_integer_)
      ) %>%
      ungroup() %>%
      group_by(pcp_x) %>%
      arrange(pcp_y, pcp_id) %>%
      mutate(
        # Only apply offset if the variable is numeric and has ties
        pcp_y = if (unique(pcp_x) %in% numeric_vars)
          adjusted_numerical_tie_breaker(pcp_y, alpha = alpha)
        else pcp_y,
        pcp_yend = if (unique(pcp_x) %in% numeric_vars)
          adjusted_numerical_tie_breaker(pcp_yend, alpha = alpha)
        else pcp_yend
      ) %>%
      ungroup() %>%
      mutate(
        # Mark these as 'factor' for box drawing if they have tie groups
        class = ifelse(!is.na(tie_group), "factor", "numeric"),
        level = tie_group,
        tie_box_label = ifelse(!is.na(tie_group),
                               paste0("Box_", pcp_x, "_", tie_group),
                               NA_character_)
      )
  }

  # --- New section: Ensure adjusted coordinates are within (0,1) ---
  # Rescale vertical coordinates (pcp_y and pcp_yend) within each axis
  data <- data %>% group_by(pcp_x) %>%
    mutate(
      pcp_y = (pcp_y - min(pcp_y, na.rm = TRUE)) / (max(pcp_y, na.rm = TRUE) - min(pcp_y, na.rm = TRUE)),
      pcp_yend = (pcp_yend - min(pcp_yend, na.rm = TRUE)) / (max(pcp_yend, na.rm = TRUE) - min(pcp_yend, na.rm = TRUE))
    ) %>% ungroup()

  # Convert pcp_x from a factor to numeric positions scaled between 0 and 1
  # axes <- unique(data$pcp_x)
  # axes <- sort(axes)
  # data <- data %>% mutate(
  #   pcp_x = as.numeric(factor(pcp_x, levels = axes)),
  #   pcp_x = (pcp_x - 1) / (length(axes) - 1)
  # )
  # --- End new section ---

  data
}
