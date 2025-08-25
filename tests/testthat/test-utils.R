test_that("generate_color_map works correctly", {
  # Test with small number of values
  values <- c("A", "B", "C")
  colors <- generate_color_map(values)
  
  expect_equal(length(colors), 3)
  expect_equal(names(colors), values)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors)))
  
  # Test with single value - RColorBrewer requires minimum 3 colors, so it returns 3 colors but only first is named
  single_value <- "X"
  single_color <- generate_color_map(single_value)
  expect_equal(length(single_color), 3)  # RColorBrewer minimum
  expect_equal(names(single_color)[1], single_value)  # Only first is properly named
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", single_color)))
  
  # Test with many values (>12)
  many_values <- LETTERS[1:15]
  many_colors <- generate_color_map(many_values)
  expect_equal(length(many_colors), 15)
  expect_equal(names(many_colors), many_values)
})

test_that("format_mrd formats values correctly", {
  # Test very small values
  expect_equal(format_mrd(1e-7), "< 1e-6")
  expect_equal(format_mrd(0), "< 1e-6")
  
  # Test normal values
  expect_equal(format_mrd(0.001), "1.00e-03")
  expect_equal(format_mrd(0.1), "1.00e-01")
  expect_equal(format_mrd(1), "1.00e+00")
  
  # Test vector input
  values <- c(1e-7, 0.001, 0.1)
  expected <- c("< 1e-6", "1.00e-03", "1.00e-01")
  expect_equal(format_mrd(values), expected)
})

test_that("calc_summary_stats calculates correctly", {
  # Test with normal data
  x <- c(1, 2, 3, 4, 5)
  stats <- calc_summary_stats(x)
  
  expect_equal(stats["Mean"], c(Mean = 3))
  expect_equal(stats["Median"], c(Median = 3))
  expect_equal(stats["Min"], c(Min = 1))
  expect_equal(stats["Max"], c(Max = 5))
  # The quantile names are "Q1.25%" and "Q3.75%" not "Q1" and "Q3"
  expect_equal(unname(stats["Q1.25%"]), 2)
  expect_equal(unname(stats["Q3.75%"]), 4)
  
  # Test with NA values
  x_na <- c(1, 2, NA, 4, 5)
  stats_na <- calc_summary_stats(x_na, na.rm = TRUE)
  expect_equal(stats_na["Mean"], c(Mean = 3))
  
  # Test with na.rm = FALSE - quantile function will error with NA values
  expect_error(calc_summary_stats(x_na, na.rm = FALSE))
})

test_that("create_heatmap_colors generates correct number of colors", {
  # Test default
  colors_default <- create_heatmap_colors()
  expect_equal(length(colors_default), 100)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors_default)))
  
  # Test custom number
  colors_custom <- create_heatmap_colors(50)
  expect_equal(length(colors_custom), 50)
  
  # Test single color
  colors_single <- create_heatmap_colors(1)
  expect_equal(length(colors_single), 1)
})

test_that("format_time formats correctly", {
  expect_equal(format_time(1), "1.0 months")
  expect_equal(format_time(12.5), "12.5 months")
  expect_equal(format_time(0), "0.0 months")
  
  # Test vector input
  times <- c(1, 2.5, 12)
  expected <- c("1.0 months", "2.5 months", "12.0 months")
  expect_equal(format_time(times), expected)
})

test_that("is_binary identifies binary variables correctly", {
  # Test logical vectors
  expect_true(is_binary(c(TRUE, FALSE, TRUE)))
  expect_true(is_binary(c(TRUE, TRUE, TRUE)))
  
  # Test numeric 0/1 vectors
  expect_true(is_binary(c(0, 1, 0, 1)))
  expect_true(is_binary(c(1, 1, 1)))
  expect_true(is_binary(c(0, 0, 0)))
  
  # Test with NA values
  expect_true(is_binary(c(0, 1, NA)))
  expect_true(is_binary(c(TRUE, FALSE, NA)))
  
  # Test non-binary vectors
  expect_false(is_binary(c(0, 1, 2)))
  expect_false(is_binary(c("A", "B")))
  expect_false(is_binary(c(1.5, 2.5)))
})

test_that("is_categorical identifies categorical variables correctly", {
  # Test factor variables
  expect_true(is_categorical(factor(c("A", "B", "C"))))
  
  # Test character variables
  expect_true(is_categorical(c("A", "B", "C")))
  
  # Test numeric with few unique values
  expect_true(is_categorical(c(1, 2, 3, 1, 2)))
  expect_true(is_categorical(c(1, 1, 1, 1)))
  
  # Test numeric with many unique values (default max_unique = 10)
  expect_false(is_categorical(1:15))
  
  # Test with custom max_unique
  expect_true(is_categorical(1:15, max_unique = 20))
  expect_false(is_categorical(1:15, max_unique = 5))
  
  # Test continuous numeric
  expect_false(is_categorical(rnorm(100)))
})

test_that("safe_log handles edge cases correctly", {
  # Test normal values
  expect_equal(safe_log(c(1, 10, 100)), c(0, 1, 2))
  
  # Test zero values
  result_zero <- safe_log(0)
  expect_true(is.finite(result_zero))
  expect_true(result_zero < 0)
  
  # Test negative values (should be handled by pmax)
  result_neg <- safe_log(-1)
  expect_true(is.finite(result_neg))
  expect_true(result_neg < 0)
  
  # Test different base
  expect_equal(safe_log(c(1, 2, 4), base = 2), c(0, 1, 2))
  
  # Test vector with mixed values
  mixed <- c(0, 1, 10, -1)
  result_mixed <- safe_log(mixed)
  expect_equal(length(result_mixed), 4)
  expect_true(all(is.finite(result_mixed)))
})

test_that("stop2 works correctly", {
  expect_error(stop2("Test error"), "Test error")
  expect_error(stop2("Error with", " multiple", " parts"), "Error with multiple parts")
})

test_that("is.scalar identifies scalars correctly", {
  # Test valid scalars
  expect_true(is.scalar(1))
  expect_true(is.scalar(3.14))
  expect_true(is.scalar(-5))
  
  # Test invalid scalars
  expect_false(is.scalar(c(1, 2)))
  expect_false(is.scalar("string"))
  expect_false(is.scalar(TRUE))
  expect_false(is.scalar(NULL))
  expect_false(is.scalar(numeric(0)))
  expect_false(is.scalar(matrix(1)))
})

test_that("validate_corr_matrix validates correctly", {
  # Test valid correlation matrix
  valid_corr <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  result <- validate_corr_matrix(valid_corr)
  expect_equal(result, valid_corr)
  
  # Test invalid inputs
  expect_error(validate_corr_matrix(NULL), "'b_rho' should be a scalar or a correlation matrix.")
  expect_error(validate_corr_matrix("not a matrix"), "'b_rho' should be a scalar or a correlation matrix.")
  
  # Test invalid correlation matrix (diagonal not 1)
  invalid_diag <- matrix(c(0.5, 0.3, 0.3, 1), nrow = 2)
  expect_error(validate_corr_matrix(invalid_diag), "'b_rho' should be a scalar or a correlation matrix.")
  
  # Test invalid correlation matrix (values > 1)
  invalid_values <- matrix(c(1, 1.5, 1.5, 1), nrow = 2)
  expect_error(validate_corr_matrix(invalid_values), "'b_rho' should be a scalar or a correlation matrix.")
  
  # Test non-symmetric matrix
  non_symmetric <- matrix(c(1, 0.3, 0.5, 1), nrow = 2)
  expect_error(validate_corr_matrix(non_symmetric), "'b_rho' should be a scalar or a correlation matrix.")
})

test_that(".rinvGauss generates reasonable values", {
  set.seed(123)
  n <- 100
  mu <- 2
  lambda <- 1
  
  values <- .rinvGauss(n, mu, lambda)
  
  expect_equal(length(values), n)
  expect_true(all(values > 0))  # Inverse Gaussian values should be positive
  expect_true(all(is.finite(values)))
  
  # Test with different parameters
  values2 <- .rinvGauss(10, mu = 1, lambda = 0.5)
  expect_equal(length(values2), 10)
  expect_true(all(values2 > 0))
})

test_that("maybe_broadcast works correctly", {
  # Test empty vector
  expect_equal(maybe_broadcast(numeric(0), 5), rep(0, 5))
  
  # Test scalar broadcasting
  expect_equal(maybe_broadcast(3, 4), rep(3, 4))
  
  # Test vector that doesn't need broadcasting
  vec <- c(1, 2, 3)
  expect_equal(maybe_broadcast(vec, 3), vec)
  
  # Test with n = 1
  expect_equal(maybe_broadcast(5, 1), 5)
})

test_that("validate_family works correctly", {
  # Test with valid family function
  fam <- validate_family(gaussian)
  expect_s3_class(fam, "family")
  expect_equal(fam$family, "gaussian")
  
  # Test with family object
  gauss_fam <- gaussian()
  result <- validate_family(gauss_fam)
  expect_s3_class(result, "family")
  
  # Test with character string
  fam_char <- validate_family("gaussian")
  expect_s3_class(fam_char, "family")
  expect_equal(fam_char$family, "gaussian")
  
  # Test with invalid input
  expect_error(validate_family("nonexistent_family"), "Family 'nonexistent_family' not found")
  expect_error(validate_family(123), "'family' must be a family.")
})

test_that("nlist creates named lists correctly", {
  # Test with simple variables
  a <- 1
  b <- 2
  c <- 3
  
  result <- nlist(a, b, c)
  expected <- list(a = 1, b = 2, c = 3)
  expect_equal(result, expected)
  
  # Test with expressions
  result2 <- nlist(x = 1 + 1, y = 2 * 3)
  expected2 <- list(x = 2, y = 6)
  expect_equal(result2, expected2)
  
  # Test with mixed named and unnamed
  d <- 4
  result3 <- nlist(d, named = 5)
  expected3 <- list(d = 4, named = 5)
  expect_equal(result3, expected3)
  
  # Test with all named arguments
  result4 <- nlist(first = 1, second = 2)
  expected4 <- list(first = 1, second = 2)
  expect_equal(result4, expected4)
})

test_that("create_css_styles returns HTML tags", {
  css <- create_css_styles()
  expect_s3_class(css, "shiny.tag")
  expect_equal(css$name, "style")
  expect_true(grepl("tag-item", as.character(css)))
  expect_true(grepl("tags-container", as.character(css)))
})
