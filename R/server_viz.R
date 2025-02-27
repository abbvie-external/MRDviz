#' MRDviz Visualization Server Logic
#'
#' Server logic for the MRD visualization tab
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param shared_storage Reactive values object for sharing data between tabs
#' @export
viz_server <- function(input, output, session, shared_storage) {
  # Reactive values for visualization state
  viz_state <- shiny::reactiveValues(
    selected_subjects = NULL,
    color_map = NULL,
    binary_color_pairs = list(),
    time_variant_covariates = NULL,
    clearing_filters = FALSE
  )

  # File upload handler
  shiny::observeEvent(input$datafile, {
    req(input$datafile)
    tryCatch({
      data <- jsonlite::fromJSON(input$datafile$datapath)
      shared_storage$data <- data
      
      # Process data into data.tables
      shared_storage$subject_dt <- data.table::as.data.table(data$subject_data)
      shared_storage$longitudinal_dt <- data.table::as.data.table(data$longitudinal_data)
      
      # Update UI elements
      updateSelectInput(session, "colorCovariateSelect",
                       choices = names(shared_storage$subject_dt))
      updateSelectInput(session, "heatmapCovariate",
                       choices = names(shared_storage$subject_dt))
      updateSelectizeInput(session, "subjectIdInput",
                          choices = unique(shared_storage$subject_dt$subject_id))
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message),
                      type = "error", duration = NULL)
    })
  })

  # Longitudinal chart
  output$longitudinalChart <- highcharter::renderHighchart({
    req(shared_storage$longitudinal_dt)
    # Create longitudinal plot using highcharter
    # Implementation details would go here
  })

  # Heatmap
  output$heatmapContainer <- shiny::renderUI({
    req(shared_storage$subject_dt, input$heatmapCovariate)
    # Create heatmap visualization
    # Implementation details would go here
  })

  # Survival chart
  output$survivalChart <- highcharter::renderHighchart({
    req(shared_storage$subject_dt)
    # Create survival plot using highcharter
    # Implementation details would go here
  })

  # Subject data table
  output$subjectDataTable <- DT::renderDT({
    req(shared_storage$subject_dt)
    DT::datatable(
      shared_storage$subject_dt,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip'
      )
    )
  })

  # Random subject selection
  shiny::observeEvent(input$randomSelectButton, {
    req(shared_storage$subject_dt, input$randomSelectCount)
    all_subjects <- unique(shared_storage$subject_dt$subject_id)
    n_select <- min(input$randomSelectCount, length(all_subjects))
    selected <- sample(all_subjects, n_select)
    updateSelectizeInput(session, "subjectIdInput",
                        selected = selected)
  })

  # Clear filters
  shiny::observeEvent(input$clearButton, {
    viz_state$clearing_filters <- TRUE
    updateSelectizeInput(session, "subjectIdInput",
                        selected = character(0))
    viz_state$clearing_filters <- FALSE
  })
}