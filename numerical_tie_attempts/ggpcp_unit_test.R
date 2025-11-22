# tests/testthat/test-even_tie_spread.R

testthat::test_that("even_tie_spread returns numeric of same length and is deterministic", {
  x <- c(1, 1, 1, 2, 3, 3, NA, 4)
  out1 <- even_tie_spread(x, width = 0.1)
  out2 <- even_tie_spread(x, width = 0.1)
  expect_type(out1, "double")
  expect_length(out1, length(x))
  expect_equal(out1, out2)  # RNG-free determinism
})

testthat::test_that("unique values are unchanged; width=0 leaves input unchanged", {
  x <- c(1, 2, 3, 4)
  expect_identical(even_tie_spread(x, width = 0.1), x)
  expect_identical(even_tie_spread(x, width = 0), x)

  y <- c(1, 1, 2, 3)
  expect_identical(even_tie_spread(y, width = 0), y)
})

testthat::test_that("ties are evenly spaced and centered with explicit width", {
  x <- c(1, 1, 1)
  out <- even_tie_spread(x, width = 0.1, cap_to_neighbor = FALSE)
  # Expected offsets: -0.05, 0, +0.05 around 1
  expect_equal(sort(out), sort(1 + c(-0.05, 0, 0.05)), tolerance = 1e-12)

  x2 <- c(2, 2)
  out2 <- even_tie_spread(x2, width = 0.2, cap_to_neighbor = FALSE)
  expect_equal(sort(out2), sort(c(1.9, 2.1)), tolerance = 1e-12)
})

testthat::test_that("cap_to_neighbor prevents crossing the midpoint to the nearest neighbor", {
  # For the tie at 0 with a neighbor at 1, the midpoint is 0.5.
  x <- c(0, 0, 0, 1)
  out <- even_tie_spread(x, width = Inf, cap_to_neighbor = TRUE)
  tie_vals <- out[which(x == 0)]
  expect_true(max(tie_vals) < 0.5 + 1e-12)

  # Symmetric case around an interior value:
  # neighbors at 1 and 3, ties at 2 may not cross 1.5 or 2.5.
  x2 <- c(1, 2, 2, 2, 3)
  out2 <- even_tie_spread(x2, width = Inf, cap_to_neighbor = TRUE)
  tie2 <- out2[which(x2 == 2)]
  expect_true(max(tie2) < 2.5 + 1e-12)
  expect_true(min(tie2) > 1.5 - 1e-12)
})

testthat::test_that("NA values are preserved and not spread", {
  x <- c(1, 1, NA, 1, NA)
  out <- even_tie_spread(x, width = 0.1)
  expect_true(all(is.na(out[is.na(x)])))
  expect_false(any(is.na(out[!is.na(x)])))
})

testthat::test_that("within-tie order is stable and controlled by key", {
  # Three equal values with a key that reverses the natural order
  x   <- c(5, 5, 5)
  key <- c(3, 2, 1)  # desired order within-tie
  out <- even_tie_spread(x, key = key, width = 0.09, cap_to_neighbor = FALSE)

  # Offsets should be assigned in key order: -0.045, 0, +0.045
  # Map back to original indices by ordering 'key'
  ord <- order(key)  # indices in increasing key: (3rd, 2nd, 1st)
  expect_equal(out[ord],
               5 + c(-0.045, 0, 0.045),
               tolerance = 1e-12)
})

testthat::test_that("width = Inf uses local capping; width < 0 is treated as no-op (defensive)", {
  x <- c(0, 0, 1)
  out_inf <- even_tie_spread(x, width = Inf, cap_to_neighbor = TRUE)
  # Should be spread but still below 0.5 for the 0-tie group
  expect_true(diff(range(out_inf[x == 0])) > 0)
  expect_true(max(out_inf[x == 0]) < 0.5 + 1e-12)

  # Negative width: expect identical to input (or at least unchanged ties)
  out_neg <- even_tie_spread(x, width = -1, cap_to_neighbor = TRUE)
  expect_identical(out_neg, x)
})

test_that("empty input is returned unchanged", {
  expect_identical(even_tie_spread(numeric()), numeric())
})

testthat::test_that("works sensibly after uniminmax scaling for ggpcp pipelines", {
  skip_if_not_installed("ggpcp")
  df <- iris
  df$Petal.Width <- round(df$Petal.Width, 1)  # induce ties

  scaled <- df |>
    ggpcp::pcp_select(1:5) |>
    ggpcp::pcp_scale(method = "uniminmax")

  # Apply in [0,1] space; width is directly interpretable
  spread <- dplyr::mutate(
    scaled,
    dplyr::across(where(is.numeric),
                  ~ even_tie_spread(.x, width = 0.03, cap_to_neighbor = TRUE))
  )

  # Check: values stay within [0,1] and ties have non-zero spread
  nums <- dplyr::select(spread, where(is.numeric))
  expect_true(all(nums >= 0 - 1e-12 & nums <= 1 + 1e-12))

  # Find at least one column with ties in the original scaled data
  scaled_nums <- dplyr::select(scaled, where(is.numeric))
  tie_cols <- vapply(scaled_nums, function(v) any(duplicated(v)), logical(1))
  if (any(tie_cols)) {
    colname <- names(which(tie_cols))[1]
    v0 <- scaled_nums[[colname]]
    v1 <- dplyr::pull(nums, colname)
    # The range should increase or stay same, and variance should not decrease to zero
    expect_true(diff(range(v1, na.rm = TRUE)) >= diff(range(v0, na.rm = TRUE)) - 1e-12)
    expect_true(var(v1, na.rm = TRUE) >= var(v0, na.rm = TRUE) - 1e-12)
  } else {
    succeed()  # no ties found (unlikely with rounding), test is vacuously true
  }
})
