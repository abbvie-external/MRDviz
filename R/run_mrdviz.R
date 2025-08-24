#' Launch the MRDviz Application
#'
#' This function launches the MRDviz Shiny application, which provides interactive
#' visualization and simulation capabilities for Minimal Residual Disease (MRD) data
#' in clinical trials.
#'
#' @param json_file Character. Path to a JSON file to load automatically when the app starts.
#'   If NULL (default), the app starts with no data loaded. Use \code{system.file("testdata", "filename.json", package = "MRDviz")}
#'   to load example datasets included with the package.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # Launch app with no data
#'   run_mrdviz()
#'   
#'   # Launch app with example data pre-loaded
#'   example_file <- system.file("testdata", "simulated_mrd.short.json", package = "MRDviz")
#'   run_mrdviz(example_file)
#'   
#'   # Launch app with your own JSON file
#'   run_mrdviz("/path/to/your/data.json")
#' }
run_mrdviz <- function(json_file = NULL) {
  # Load required packages
  requireNamespace("shiny", quietly = TRUE)
  requireNamespace("shinyjs", quietly = TRUE)
  requireNamespace("highcharter", quietly = TRUE)
  requireNamespace("data.table", quietly = TRUE)
  requireNamespace("memoise", quietly = TRUE)
  requireNamespace("survival", quietly = TRUE)
  requireNamespace("jsonlite", quietly = TRUE)
  requireNamespace("DT", quietly = TRUE)
  requireNamespace("RColorBrewer", quietly = TRUE)
  requireNamespace("shinydashboard", quietly = TRUE)
  requireNamespace("ggplot2", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("simsurv", quietly = TRUE)

  # Validate json_file if provided
  if (!is.null(json_file)) {
    if (!file.exists(json_file)) {
      stop("JSON file not found: ", json_file)
    }
    
    if (!grepl("\\.json$", json_file, ignore.case = TRUE)) {
      warning("File does not have .json extension: ", json_file)
    }
  }

  # Store json_file path in global options for the app to access
  options(mrdviz_preload_file = json_file)

  # Launch the Shiny app using the UI and server from app.R
  shiny::shinyApp(ui = ui, server = server)
}

#' Get Available Example Datasets
#'
#' Returns information about available example datasets included with the MRDviz package.
#' Use \code{system.file("testdata", filename, package = "MRDviz")} to get the full path
#' to any of these files for use with \code{run_mrdviz()}.
#'
#' @return A data.frame with information about available example files
#' @export
#'
#' @examples
#' # View available example datasets
#' get_example_datasets()
#' 
#' # Get path to a specific example file
#' example_path <- system.file("testdata", "simulated_mrd.short.json", package = "MRDviz")
#' 
#' # Launch MRDviz with the example file
#' if (interactive() && file.exists(example_path)) {
#'   run_mrdviz(example_path)
#' }
get_example_datasets <- function() {
  testdata_path <- system.file("testdata", package = "MRDviz")
  
  if (!dir.exists(testdata_path)) {
    return(data.frame(
      filename = character(0),
      description = character(0),
      size_kb = numeric(0),
      full_path = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  json_files <- list.files(testdata_path, pattern = "\\.json$", full.names = TRUE)
  
  if (length(json_files) == 0) {
    return(data.frame(
      filename = character(0),
      description = character(0),
      size_kb = numeric(0),
      full_path = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Create descriptions for known files
  descriptions <- c(
    "simulated_mrd.short.json" = "Small example dataset with 6 subjects - ideal for quick testing",
    "simulated_mrd.json" = "Full simulated dataset with multiple subjects and covariates",
    "MRDsim_joint_2025-02-23.json" = "Joint model simulation from February 2025",
    "MRDsim_joint_2025-04-07.json" = "Joint model simulation from April 2025"
  )
  
  file_info <- data.frame(
    filename = basename(json_files),
    description = descriptions[basename(json_files)],
    size_kb = round(file.size(json_files) / 1024, 1),
    full_path = json_files,
    stringsAsFactors = FALSE
  )
  
  # Add default description for any files not explicitly described
  file_info$description[is.na(file_info$description)] <- "Example MRD dataset"
  
  return(file_info)
}

#' Load Example Dataset
#'
#' Loads an example dataset from the MRDviz package for use outside the Shiny app.
#' This is useful for exploring the data structure or for programmatic analysis.
#'
#' @param filename Character. Name of the example file to load from inst/testdata.
#'   Use \code{get_example_datasets()} to see available options.
#'
#' @return A list containing the parsed JSON data
#' @export
#'
#' @examples
#' # View available datasets
#' get_example_datasets()
#' 
#' # Load the small example dataset
#' example_data <- load_example_data("simulated_mrd.short.json")
#' 
#' # View the structure
#' str(example_data, max.level = 2)
#' 
#' # Access specific components
#' length(example_data$subjects)  # Number of subjects
#' names(example_data$covariates)  # Available covariates
load_example_data <- function(filename = "simulated_mrd.short.json") {
  example_path <- system.file("testdata", filename, package = "MRDviz")
  
  if (!file.exists(example_path)) {
    available_files <- get_example_datasets()$filename
    stop("Example file not found: ", filename, 
         "\nAvailable files: ", paste(available_files, collapse = ", "),
         "\nUse get_example_datasets() to see all available options.")
  }
  
  jsonlite::fromJSON(example_path, simplifyVector = FALSE)
}
