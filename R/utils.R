#' Create CSS Style Tags
#'
#' Creates a CSS style string for use in the Shiny app
#' @keywords internal
create_css_styles <- function() {
  shiny::tags$style(HTML("
    .tag-item {
      display: inline-block;
      padding: 8px 16px;
      margin: 0px;
      border-radius: 15px;
      color: white;
      cursor: pointer;
    }
    .tag-item.selected {
      border: 4px solid black;
      font-weight: bold;
    }
    .tags-container {
      display: flex;
      flex-wrap: wrap;
      gap: 1px;
      align-items: center;
      width: 90%;
      margin-bottom: 10px;
    }
    .flex-container {
      display: flex;
      width: 100%;
      align-items: stretch;
      margin-top: 10px;
    }
    #randomSelectCount {
      flex: 1;
      margin-right: 10px;
      height: 38px;
      padding: 6px 12px;
    }
    #randomSelectButton {
      flex: 2;
      height: 38px;
      padding: 6px 12px;
    }
    .large-button {
      height: 50px;
      font-size: 18px;
    }
    .sidebar .shiny-input-container {
      width: 100%;
    }
    .sidebar-menu {
      margin-bottom: 10px;
    }
  "))
}

#' Generate Color Map
#'
#' Generates a color mapping for categorical variables
#' @param values Vector of unique values to map to colors
#' @return Named vector of colors
#' @keywords internal
generate_color_map <- function(values) {
  n_colors <- length(values)
  colors <- RColorBrewer::brewer.pal(
    n = min(max(3, n_colors), 12),
    name = "Set3"
  )
  if (n_colors > 12) {
    colors <- colorRampPalette(colors)(n_colors)
  }
  stats::setNames(colors, values)
}

#' Format MRD Value
#'
#' Formats MRD values for display
#' @param x Numeric MRD value
#' @return Character string with formatted MRD value
#' @keywords internal
format_mrd <- function(x) {
  ifelse(x < 1e-6,
         sprintf("< 1e-6"),
         sprintf("%.2e", x))
}

#' Calculate Summary Statistics
#'
#' Calculates summary statistics for numeric vectors
#' @param x Numeric vector
#' @param na.rm Logical indicating whether to remove NA values
#' @return Named vector of summary statistics
#' @keywords internal
calc_summary_stats <- function(x, na.rm = TRUE) {
  c(
    Mean = mean(x, na.rm = na.rm),
    SD = stats::sd(x, na.rm = na.rm),
    Median = stats::median(x, na.rm = na.rm),
    Q1 = stats::quantile(x, 0.25, na.rm = na.rm),
    Q3 = stats::quantile(x, 0.75, na.rm = na.rm),
    Min = min(x, na.rm = na.rm),
    Max = max(x, na.rm = na.rm)
  )
}

#' Create Heatmap Colors
#'
#' Generates a color palette for heatmap visualization
#' @param n Number of colors to generate
#' @return Character vector of colors
#' @keywords internal
create_heatmap_colors <- function(n = 100) {
  colorRampPalette(c("#313695", "#FFFFE5"))(n)
}

#' Format Time
#'
#' Formats time values for display
#' @param x Numeric time value
#' @return Character string with formatted time
#' @keywords internal
format_time <- function(x) {
  sprintf("%.1f months", x)
}

#' Check Binary Variable
#'
#' Checks if a variable is binary (0/1 or TRUE/FALSE)
#' @param x Vector to check
#' @return Logical indicating whether x is binary
#' @keywords internal
is_binary <- function(x) {
  if (is.logical(x)) return(TRUE)
  if (!is.numeric(x)) return(FALSE)
  all(x %in% c(0, 1, NA))
}

#' Check Categorical Variable
#'
#' Checks if a variable should be treated as categorical
#' @param x Vector to check
#' @param max_unique Maximum number of unique values for categorical variables
#' @return Logical indicating whether x should be treated as categorical
#' @keywords internal
is_categorical <- function(x, max_unique = 10) {
  if (is.factor(x)) return(TRUE)
  if (is.character(x)) return(TRUE)
  if (is.numeric(x)) {
    n_unique <- length(unique(x))
    return(n_unique <= max_unique && n_unique > 0)
  }
  FALSE
}

#' Safe Log Transform
#'
#' Safely transforms values to log scale, handling zeros and negative values
#' @param x Numeric vector to transform
#' @param base Base for logarithm (default 10)
#' @return Log-transformed values
#' @keywords internal
safe_log <- function(x, base = 10) {
  # Add small constant to handle zeros
  eps <- .Machine$double.eps
  log(pmax(x, eps), base = base)
}

#' Stop with No Call
#'
#' Error message with call. set to FALSE
#' @param ... Arguments passed to stop()
#' @keywords internal
stop2 <- function(...) {
  stop(..., call. = FALSE)
}

#' Check Scalar
#'
#' Check if x is a numeric scalar
#' @param x Object to check
#' @return Logical indicating if x is a numeric scalar
#' @keywords internal
is.scalar <- function(x) {
  length(x) == 1L && is.vector(x) && is.numeric(x)
}

#' Validate Correlation Matrix
#'
#' Check x is a valid correlation matrix
#' @param x Matrix to validate
#' @return Validated correlation matrix
#' @keywords internal
validate_corr_matrix <- function(x) {
  if (is.null(x) || !is.matrix(x))
    stop2("'b_rho' should be a scalar or a correlation matrix.")
  if (!all(diag(x) == 1) ||
      !all(abs(x)  <= 1) ||
      !all(x[lower.tri(x)] == t(x)[lower.tri(x)]))
    stop2("'b_rho' should be a scalar or a correlation matrix.")
  as.matrix(x)
}

#' Draw from Inverse Gaussian Distribution
#'
#' Generate random numbers from inverse Gaussian distribution
#' @param n Number of observations
#' @param mu Mean parameter
#' @param lambda Shape parameter
#' @return Vector of random numbers
#' @keywords internal
.rinvGauss <- function(n, mu, lambda) {
  mu2 <- mu^2
  y <- stats::rnorm(n)^2
  z <- stats::runif(n)
  tmp <- (mu2 * y - mu * sqrt(4 * mu * lambda * y + mu2 * y^2))
  x <- mu + tmp / (2 * lambda)
  ifelse(z <= (mu / (mu + x)), x, mu2 / x)
}

#' Maybe Broadcast
#'
#' Broadcast a vector or scalar to specified length
#' @param x A vector or scalar
#' @param n Number of replications to possibly make
#' @return Broadcasted vector
#' @keywords internal
maybe_broadcast <- function(x, n) {
  if (!length(x)) {
    rep(0, times = n)
  } else if (length(x) == 1L) {
    rep(x, times = n)
  } else {
    x
  }
}

#' Validate Family
#'
#' Check and validate family argument
#' @param f The family argument specified by user
#' @return Validated family object
#' @keywords internal
validate_family <- function(f) {
  if (is.character(f)) {
    # Try to get the function from the parent environment
    tryCatch({
      f <- get(f, mode = "function", envir = parent.frame())
    }, error = function(e) {
      stop(sprintf("Family '%s' not found", f), call. = FALSE)
    })
  }
  if (is.function(f))
    f <- f()
  if (!is(f, "family"))
    stop("'family' must be a family.", call. = FALSE)
  
  return(f)
}

#' Create Named List
#'
#' Create a named list using specified names or object names
#' @param ... Objects to include in the list
#' @return A named list
#' @keywords internal
nlist <- function(...) {
  m <- match.call()
  out <- list(...)
  no_names <- is.null(names(out))
  has_name <- if (no_names) FALSE else nzchar(names(out))
  if (all(has_name))
    return(out)
  nms <- as.character(m)[-1L]
  if (no_names) {
    names(out) <- nms
  } else {
    names(out)[!has_name] <- nms[!has_name]
  }
  
  return(out)
}
