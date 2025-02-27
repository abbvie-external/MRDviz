#' @title Convert Data Files to MRDviz JSON Format
#' @description Convert longitudinal measurements and event data from CSV or Excel files
#' to the MRDviz JSON format required for visualization.
#' @importFrom purrr map pmap
#'
#' @param longitudinal_file Path to CSV or Excel file containing longitudinal data with columns:
#'   \itemize{
#'     \item subject_id: Subject identifier
#'     \item time: Time point of measurement
#'     \item measurement: Value of measurement
#'     \item Additional columns will be treated as time-variant covariates
#'   }
#' @param event_file Path to CSV or Excel file containing event data with columns:
#'   \itemize{
#'     \item subject_id: Subject identifier (must match longitudinal_data)
#'     \item Additional columns will be treated as static covariates
#'   }
#' @param output_file Output JSON file path. Default: "mrdviz_data.json"
#' @return Invisibly returns the JSON data structure
#' @export
#'
#' @examples
#' \dontrun{
#' # Convert from CSV files
#' convert_to_json("longitudinal.csv", "events.csv", "visualization.json")
#'
#' # Convert from Excel files
#' convert_to_json("longitudinal.xlsx", "events.xlsx", "visualization.json")
#' }
convert_to_json <- function(longitudinal_file, event_file, output_file = "mrdviz_data.json") {
  # Check file existence
  if (!file.exists(longitudinal_file)) {
    stop("Longitudinal data file not found: ", longitudinal_file)
  }
  if (!file.exists(event_file)) {
    stop("Event data file not found: ", event_file)
  }
  
  # Determine file type and read accordingly
  read_data <- function(file) {
    ext <- tolower(tools::file_ext(file))
    
    # First read a few rows to determine column types
    if (ext == "csv") {
      sample_data <- utils::read.csv(file, nrows = 5, stringsAsFactors = FALSE)
    } else if (ext %in% c("xls", "xlsx")) {
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        stop("Package 'openxlsx' needed to read Excel files. Please install it.")
      }
      sample_data <- openxlsx::read.xlsx(file, rows = 1:5)
    } else {
      stop("Unsupported file format: ", ext, ". Please use CSV or Excel files.")
    }
    
    # Determine which columns should be numeric
    col_classes <- sapply(names(sample_data), function(col) {
      # Always keep eventtime and time columns as numeric
      if (col == "time" || startsWith(col, "eventtime_")) {
        return("numeric")
      }
      x <- sample_data[[col]]
      # Skip non-numeric columns
      if (!all(grepl("^-?[0-9]+\\.?[0-9]*$|^-?\\.[0-9]+$|^\\s*$|^NA$", as.character(x[!is.na(x)])))) {
        return("character")
      }
      # Convert to numeric for checking
      x_num <- as.numeric(x[!is.na(x)])
      # Keep as numeric if:
      # 1. More than 10 unique values, or
      # 2. Contains non-integer values
      if (length(unique(x_num)) > 10 || any(abs(x_num - round(x_num)) > 1e-10)) {
        return("numeric")
      }
      "character"
    })
    
    # Read the full file with correct column types
    if (ext == "csv") {
      utils::read.csv(file, stringsAsFactors = FALSE, colClasses = col_classes)
    } else {
      data <- openxlsx::read.xlsx(file)
      # Convert numeric columns
      for (col in names(col_classes)[col_classes == "numeric"]) {
        data[[col]] <- as.numeric(data[[col]])
      }
      data
    }
  }
  
  # Read the data files
  longitudinal_data <- read_data(longitudinal_file)
  event_data <- read_data(event_file)
  
  # Ensure data is in data.frame format and numeric columns are properly typed
  longitudinal_data <- as.data.frame(longitudinal_data)
  event_data <- as.data.frame(event_data)
  
  # Ensure time is numeric
  if (!is.numeric(longitudinal_data$time)) {
    longitudinal_data$time <- as.numeric(longitudinal_data$time)
  }
  
  # Convert eventtime columns to numeric
  eventtime_cols <- grep("^eventtime_", names(event_data), value = TRUE)
  for (col in eventtime_cols) {
    if (!is.numeric(event_data[[col]])) {
      event_data[[col]] <- as.numeric(event_data[[col]])
    }
  }
  
  # Export to JSON using export_json
  export_json(longitudinal_data, event_data, output_file)
}

#' Generate Internal JSON Structure
#'
#' Internal function to generate the JSON structure for MRDviz visualization
#' @noRd
generate_json <- function(longitudinal_data, event_data) {
  # Ensure subject_id is character type in both datasets
  longitudinal_data$subject_id <- as.character(longitudinal_data$subject_id)
  event_data$subject_id <- as.character(event_data$subject_id)

  # Check for required columns
  required_cols <- c("subject_id", "time", "measurement")
  if (!all(required_cols %in% names(longitudinal_data))) {
    stop("Longitudinal data must contain 'subject_id', 'time', and 'measurement' columns")
  }

  # Identify additional columns (time-variant covariates)
  time_variant_cols <- setdiff(names(longitudinal_data), required_cols)

  # Function to check if column should be excluded (starts with underscore)
  is_excluded <- function(col_name) {
    startsWith(as.character(col_name), "_")
  }

  # Function to determine if a column is categorical
  is_categorical <- function(x) {
    if (is.factor(x) || is.character(x)) {
      return(TRUE)
    }
    if (is.numeric(x)) {
      # Check if it's a continuous numeric value
      unique_vals <- unique(x[!is.na(x)])
      if (length(unique_vals) <= 10 && all(abs(unique_vals - round(unique_vals)) < 1e-10)) {
        return(TRUE)
      }
    }
    FALSE
  }

  # Function to check if a column should be treated as categorical
  should_be_categorical <- function(x, name) {
    # Never convert these to categorical
    if (name == "time" || startsWith(name, "eventtime_") || startsWith(name, "_")) {
      return(FALSE)
    }
    # For numeric values, only convert to categorical if they are integers with ≤10 unique values
    if (is.numeric(x)) {
      unique_vals <- unique(x[!is.na(x)])
      return(length(unique_vals) <= 10 && all(abs(unique_vals - round(unique_vals)) < 1e-10))
    }
    # Character or factor values are always categorical
    is.factor(x) || is.character(x)
  }

  # Helper function to convert values based on type
  convert_value <- function(x, name = NULL) {
    if (should_be_categorical(x, name)) {
      x <- as.character(x)
      x[is.na(x) | x == ""] <- "NA"
    }
    x
  }

  # Filter out columns that start with underscore from time-variant covariates
  time_variant_cols <- time_variant_cols[!sapply(time_variant_cols, function(x) is_excluded(as.character(x)))]

  # Get categorical time-variant columns for color coding
  time_variant_data <- longitudinal_data[, time_variant_cols, drop = FALSE]
  categorical_cols <- names(time_variant_data)[sapply(names(time_variant_data), function(col) {
    if (col == "time" || startsWith(col, "eventtime_") || startsWith(col, "_")) {
      return(FALSE)
    }
    x <- time_variant_data[[col]]
    if (is.numeric(x)) {
      unique_vals <- unique(x[!is.na(x)])
      return(length(unique_vals) <= 10 && all(abs(unique_vals - round(unique_vals)) < 1e-10))
    }
    is.factor(x) || is.character(x)
  })]
  categorical_time_variant <- time_variant_data[, categorical_cols, drop = FALSE]
  
  # Process time-variant covariates
  time_variant_covariates <- lapply(names(categorical_time_variant), function(name) {
    x <- categorical_time_variant[[name]]
    vals <- convert_value(x, name)
    unique_vals <- unique(vals)
    setNames(
      lapply(unique_vals, function(x) list()),
      unique_vals
    )
  })
  names(time_variant_covariates) <- names(categorical_time_variant)

  # Get all non-underscore columns for subject data
  valid_cols <- names(event_data)[!sapply(names(event_data), is_excluded)]
  valid_cols <- setdiff(valid_cols, "subject_id")  # Remove subject_id
  
  # Get all static columns for subject data
  all_static <- event_data[, valid_cols, drop = FALSE]
  
  # Get categorical static columns for color coding
  static_categorical_cols <- names(all_static)[sapply(names(all_static), function(col) {
    if (col == "time" || startsWith(col, "eventtime_") || startsWith(col, "_")) {
      return(FALSE)
    }
    x <- all_static[[col]]
    if (is.numeric(x)) {
      unique_vals <- unique(x[!is.na(x)])
      return(length(unique_vals) <= 10 && all(abs(unique_vals - round(unique_vals)) < 1e-10))
    }
    is.factor(x) || is.character(x)
  })]
  categorical_static <- all_static[, static_categorical_cols, drop = FALSE]
  
  # Process static covariates
  static_covariates <- lapply(names(categorical_static), function(name) {
    x <- categorical_static[[name]]
    vals <- convert_value(x, name)
    unique_vals <- unique(vals)
    setNames(
      lapply(unique_vals, function(x) list()),
      unique_vals
    )
  })
  names(static_covariates) <- names(categorical_static)

  # Get categorical time-variant columns for heatmap
  categorical_time_variant_cols <- names(categorical_time_variant)
  
    # Split data by subject_id and prepare subjects data
  subject_groups <- split(longitudinal_data, longitudinal_data$subject_id)
  subjects_data <- lapply(names(subject_groups), function(sid) {
    subject_df <- subject_groups[[sid]]
    subject_events <- event_data[event_data$subject_id == sid, , drop = FALSE]
    
    # Prepare measurements data
    measurements <- lapply(seq_len(nrow(subject_df)), function(i) {
      result <- list(
        time = subject_df$time[i],
        value = subject_df$measurement[i]
      )
      
      # Add time-variant data
      if (length(time_variant_cols) > 0) {
        for (col in time_variant_cols) {
          val <- subject_df[[col]][i]
          if (col %in% categorical_cols) {
            result[[col]] <- convert_value(val, col)
          } else {
            result[[col]] <- val
          }
        }
      }
      
      result
    })
    
    # Prepare covariates
    covariates <- if (length(valid_cols) > 0) {
      covariate_data <- subject_events[, valid_cols, drop = FALSE]
      converted <- lapply(names(covariate_data), function(name) {
        val <- unlist(covariate_data[1, name])  # Use unlist to get atomic value
        if (name %in% static_categorical_cols) {
          convert_value(val, name)
        } else {
          val
        }
      })
      names(converted) <- names(covariate_data)
      converted
    } else {
      list()
    }
    
    list(
      id = sid,
      covariates = covariates,
      data = measurements
    )
  })

  # Return the final JSON structure
  list(
    title = "MRD trajectories",
    xlab = "Time",
    ylab = "Measurement",
    covariates = static_covariates,
    time_variant_covariates = time_variant_covariates,
    subjects = subjects_data
  )
}

#' Export Data to MRDviz JSON Format
#'
#' Convert longitudinal measurements and event data to MRDviz JSON format.
#' This function takes data frames containing longitudinal measurements
#' and event data, and exports them to a JSON file compatible with MRDviz visualization.
#'
#' @param longitudinal_data A data frame in long format containing:
#'   \itemize{
#'     \item subject_id: Subject identifier
#'     \item time: Time point of measurement
#'     \item measurement: Value of measurement
#'     \item Additional columns will be treated as time-variant covariates
#'   }
#' @param event_data A data frame containing subject-level data with:
#'   \itemize{
#'     \item subject_id: Subject identifier (must match longitudinal_data)
#'     \item Additional columns will be treated as static covariates
#'   }
#' @param output_file Output JSON file path. Default: "mrdviz_data.json"
#' @return Invisibly returns the JSON data structure
#' @export
#'
#' @examples
#' \dontrun{
#' # Create sample longitudinal data
#' long_data <- data.frame(
#'   subject_id = rep(1:2, each = 3),
#'   time = rep(0:2, 2),
#'   measurement = rnorm(6),
#'   Response = rep(c("PR", "CR"), each = 3),
#'   MRD_negativity = rep(c("MRD+", "MRD-"), each = 3)
#' )
#'
#' # Create sample event data
#' event_data <- data.frame(
#'   subject_id = 1:2,
#'   OS = c("Event", "Censor"),
#'   PFS = c("Event", "Censor"),
#'   eventtime_OS = c(1.5, 6.0),
#'   eventtime_PFS = c(1.2, 5.8)
#' )
#'
#' # Export to JSON
#' export_json(long_data, event_data, "visualization.json")
#' }
export_json <- function(longitudinal_data, event_data, output_file = "mrdviz_data.json") {
  # Input validation
  if (!is.data.frame(longitudinal_data) || !is.data.frame(event_data)) {
    stop("Both longitudinal_data and event_data must be data frames")
  }
  
  if (!"subject_id" %in% names(longitudinal_data) || !"subject_id" %in% names(event_data)) {
    stop("Both longitudinal_data and event_data must contain a 'subject_id' column")
  }

  # Generate JSON structure
  json_data <- generate_json(longitudinal_data, event_data)
  
  # Write to file
  jsonlite::write_json(json_data, output_file, auto_unbox = TRUE, pretty = TRUE)
  
  # Return invisibly
  invisible(json_data)
}