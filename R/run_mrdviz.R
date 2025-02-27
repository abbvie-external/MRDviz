#' Launch the MRDviz Application
#'
#' This function launches the MRDviz Shiny application, which provides interactive
#' visualization and simulation capabilities for Minimal Residual Disease (MRD) data
#' in clinical trials.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   run_mrdviz()
#' }
run_mrdviz <- function() {
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

  # Launch the Shiny app using the UI and server from app.combined.R
  shiny::shinyApp(ui = ui, server = server)
}