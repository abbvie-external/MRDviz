#' MRDviz Server Functions
#'
#' Server-side logic for the MRDviz application
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param shared_storage Reactive values object for storing shared data
#' @export
mrdviz_server <- function(input, output, session, shared_storage) {
  # Function to generate unique color pairs for binary covariates
  generateBinaryColorPairs <- function(n) {
  paired_palette <- brewer.pal(12, "Paired")
  if (n > 6) {
    paired_palette <- rep(paired_palette, ceiling(n/6))
  }
  color_pairs <- lapply(seq(1, by = 2, length.out = n), function(i) {
    paired_palette[c(i, i+1)]
  })
  return(color_pairs)
}

# Function to generate colors based on index, covariate type, and subgroup names
generateColor <- function(index, covariate_type, subgroup_names, binary_color_pairs) {
  if (length(subgroup_names) == 2) {
    return(binary_color_pairs[[covariate_type]][[index]])
  } else {
    colors <- colorRampPalette(c("#4daf4a", "#377eb8", "#ff7f00", "#e41a1c", "#984ea3"))(length(subgroup_names))
    return(colors[index])
  }
}

# Define process_colors function with conditional NA handling
process_colors <- function(covariate, covariate_type, binary_pairs) {
  covariate_values <- names(covariate)
  
  # Check if we need to add NA handling
  has_na <- any(sapply(shared_storage$subject_dt[[covariate_type]], is.na))
  if (has_na) {
    covariate_values <- c(covariate_values, "NA")
  }
  
  is_binary <- length(names(covariate)) == 2
  
  colors <- sapply(seq_along(covariate_values), function(i) {
    value <- covariate_values[i]
    
    if (value == "NA") {
      return("#D3D3D3")  # lightgray for NA values
    }
    
    color_spec <- covariate[[value]]
    
    if (length(color_spec) > 0 && is.character(color_spec[[1]])) {
      return(color_spec[[1]])
    } else {
      if (is_binary && i <= 2) {
        return(binary_pairs[[covariate_type]][[i]])
      } else {
        palette_colors <- colorRampPalette(c("#4daf4a", "#377eb8", "#ff7f00", "#e41a1c", "#984ea3"))(length(names(covariate)))
        return(if(i > length(names(covariate))) "#D3D3D3" else palette_colors[i])
      }
    }
  })
  
  return(setNames(colors, covariate_values))
}

# Observe file upload and parse JSON
observeEvent(input$datafile, {
  req(input$datafile)
  tryCatch({
    datafile <- fromJSON(input$datafile$datapath, simplifyVector = FALSE)
    
    shared_storage$selected_subgroups <- list()
    shared_storage$clearing_filters <- FALSE
    
    subjects_data <- lapply(datafile$subjects, function(subject) {
      c(list(id = subject$id), subject$covariates)
    })
    shared_storage$subject_dt <- setDT(rbindlist(subjects_data, fill = TRUE))
    
    longitudinal_data <- lapply(datafile$subjects, function(subject) {
      if (is.data.frame(subject$data)) {
        data <- as.data.table(subject$data)
      } else if (is.list(subject$data) && length(subject$data) > 0) {
        data <- rbindlist(lapply(subject$data, as.list), fill = TRUE)
      } else {
        data <- data.table()
      }
      if (nrow(data) > 0) {
        data[, id := subject$id]
      }
      return(data)
    })
    
    longitudinal_dt_unsorted <- rbindlist(longitudinal_data, fill = TRUE)
    longitudinal_dt_unsorted[, time := as.numeric(time)]
    shared_storage$longitudinal_dt <- longitudinal_dt_unsorted[order(id, time)]
    
    # Filter out covariates that start with underscore from both baseline and time-variant covariates
    # This convention (e.g., '_true_value') allows covariates with many categories or those not meant
    # for visualization to be present in the data but excluded from the visualization
    covariates <- datafile$covariates
    visible_covariates <- names(covariates)[!grepl("^_", names(covariates))]
    covariates <- covariates[visible_covariates]
    
    binary_covariates <- names(covariates)[sapply(covariates, function(x) length(names(x)) == 2)]
    binary_pairs <- generateBinaryColorPairs(length(binary_covariates))
    names(binary_pairs) <- binary_covariates
    shared_storage$binary_color_pairs <- binary_pairs
    
    covariate_colors <- lapply(names(covariates), function(covariate_type) {
      process_colors(covariates[[covariate_type]], covariate_type, binary_pairs)
    })
    names(covariate_colors) <- names(covariates)
    shared_storage$color_map <- covariate_colors
    
    time_variant_covariates_data <- datafile$time_variant_covariates
    if (!is.null(time_variant_covariates_data)) {
      time_variant_covariates_data <- lapply(time_variant_covariates_data, function(covariate) {
        covariate_values <- names(covariate)
        colors <- sapply(covariate_values, function(value) {
          color_spec <- covariate[[value]]
          if (length(color_spec) > 0 && is.character(color_spec[[1]])) {
            return(color_spec[[1]])
          } else {
            NULL
          }
        })
        
        null_colors <- sapply(colors, is.null)
        if (any(null_colors)) {
          n_colors_needed <- sum(null_colors)
          default_colors <- colorRampPalette(brewer.pal(9, "Set1"))(n_colors_needed)
          colors[null_colors] <- default_colors
        }
        
        return(colors)
      })
      shared_storage$time_variant_covariates <- time_variant_covariates_data
    }
    
    # Update color covariate selection
    updateSelectInput(session, "colorCovariateSelect", 
                      choices = names(covariates))
    
  # Function to determine if a column is categorical
  is_categorical <- function(x) {
    if (is.factor(x) || is.character(x)) return(TRUE)
    if (!is.numeric(x)) return(FALSE)
    # For numeric columns, check total number of unique values
    unique_vals <- unique(x[!is.na(x)])
    length(unique_vals) <= 10 && all(abs(unique_vals - round(unique_vals)) < 1e-10)
  }
    
    # Update heatmap covariate selection
    # First, get all time-variant columns except standard ones
    tv_covariates <- setdiff(names(shared_storage$longitudinal_dt), c("id", "time", "value"))
    
    # Then filter to only those defined in time_variant_covariates section of JSON
    if (!is.null(shared_storage$time_variant_covariates)) {
      tv_covariates <- intersect(tv_covariates, names(shared_storage$time_variant_covariates))
    } else {
      tv_covariates <- character(0)
    }
    
    # Finally, exclude any that start with underscore
    tv_covariates <- tv_covariates[!grepl("^_", tv_covariates)]
    
    updateSelectInput(session, "heatmapCovariate", choices = tv_covariates)
    
    # Update survival endpoint selection
    # Find all covariates that have corresponding eventtime_ fields
    first_subject_covariates <- names(datafile$subjects[[1]]$covariates)
    time_fields <- grep("^eventtime_", first_subject_covariates, value = TRUE)
    endpoints <- sub("^eventtime_", "", time_fields)
    updateSelectInput(session, "survivalEndpoint", choices = endpoints)
    
    shared_storage$data <- datafile
    
  }, error = function(e) {
    showNotification(paste("Error loading data:", e$message), type = "error")
    shared_storage$data <- NULL
    shared_storage$subject_dt <- NULL
    shared_storage$longitudinal_dt <- NULL
    shared_storage$selected_subgroups <- list()
    shared_storage$color_map <- NULL
    shared_storage$binary_color_pairs <- NULL
    shared_storage$time_variant_covariates <- NULL
  })
})

# Filtered data reactive
filtered_data <- reactive({
  req(shared_storage$subject_dt, shared_storage$longitudinal_dt)
  
  selected_subgroups_data <- shared_storage$selected_subgroups
  selected_subjects <- input$subjectIdInput
  
  # Check if any filters are actually applied
  has_active_filters <- length(selected_subjects) > 0 || length(selected_subgroups_data) > 0
  
  filtered_subjects <- shared_storage$subject_dt
  
  if (length(selected_subjects) > 0) {
    filtered_subjects <- filtered_subjects[id %in% selected_subjects]
  }
  
  for (covariate in names(selected_subgroups_data)) {
    if (length(selected_subgroups_data[[covariate]]) > 0) {
      filtered_subjects <- filtered_subjects[get(covariate) %in% selected_subgroups_data[[covariate]]]
    }
  }
  
  filtered_subjects[, selected := TRUE]
  
  all_subjects <- shared_storage$subject_dt
  if (has_active_filters) {
    # If filters are applied, mark only matching subjects as selected
    all_subjects[, selected := id %in% filtered_subjects$id]
  } else {
    # If no filters are applied, mark all subjects as selected
    all_subjects[, selected := TRUE]
  }
  
  filtered_longitudinal <- shared_storage$longitudinal_dt[id %in% filtered_subjects$id]
  
  list(
    subjects = all_subjects, 
    filtered_subjects = filtered_subjects, 
    longitudinal = filtered_longitudinal,
    has_active_filters = has_active_filters
  )
})

  # Create chart function with NA handling
  createChart <- function(filtered_data) {
  req(filtered_data)
  
  subjects_to_plot <- filtered_data$filtered_subjects
  longitudinal_data <- filtered_data$longitudinal
  has_active_filters <- isTRUE(filtered_data$has_active_filters)
  
  # Base chart configuration
  hc <- highchart() %>%
    hc_chart(type = "line",
             borderWidth = 0) %>%
    hc_title(text = shared_storage$data[["title"]], align = "left", style = list(fontSize = "20px")) %>%
    hc_xAxis(title = list(text = shared_storage$data[["xlab"]])) %>%
    hc_yAxis(title = list(text = shared_storage$data[["ylab"]])) %>%
    hc_plotOptions(line = list(marker = list(enabled = TRUE)))
    
  # Return empty chart with message if no data and filters are active
  if (has_active_filters && (nrow(subjects_to_plot) == 0 || nrow(longitudinal_data) == 0)) {
    return(hc %>% 
           # hc_title(text = "No subjects match the selected criteria") %>%
           hc_subtitle(text = "No subjects match the selected criteria"))
  }
  
  for (subject_id in subjects_to_plot$id) {
    subject_data <- longitudinal_data[id == subject_id]
    subject_covariates <- subjects_to_plot[id == subject_id]
    
    if (nrow(subject_data) == 0) next
    
    subject_data <- subject_data[order(time)]
    
    selected_covariate <- input$colorCovariateSelect
    covariate_value <- subject_covariates[[selected_covariate]]
    color <- shared_storage$color_map[[selected_covariate]][[as.character(covariate_value)]]
    
    series_data <- lapply(seq_len(nrow(subject_data)), function(i) {
      list(x = subject_data$time[i], y = subject_data$value[i])
    })
    
    time_variant_covariates <- setdiff(names(subject_data), c("id", "time", "value"))
    
    hc <- hc %>%
      hc_add_series(
        data = series_data,
        type = "line",
        name = subject_id,
        color = color,
        tooltip = list(
          useHTML = TRUE,
          headerFormat = "",
          pointFormatter = JS(sprintf("
          function() {
            var point = this;
            var tooltipContent = '<span style=\"color: {point.color}\">●</span> <span style=\"font-size: 14px;\"><b>Subject ID: ' + this.series.name + '</b></span><br>';
            tooltipContent += '<br><b>Baseline covariates:</b><br>';
            %s
            tooltipContent += '<br><b>Time-variant covariates:</b><br>';
            tooltipContent += 'Time: ' + this.x + '<br>';
            tooltipContent += 'Value: ' + this.y + '<br>';
            %s
            return tooltipContent;
          }
        ", 
                                    paste(sprintf("tooltipContent += '%s: <span style=\"color: %s;\">●</span> %s<br>';", 
                                                  names(subject_covariates), 
                                                  sapply(names(subject_covariates), function(cov) shared_storage$color_map[[cov]][[subject_covariates[[cov]]]]),
                                                  sapply(subject_covariates, as.character)), 
                                          collapse = "\n"),
                                    paste(sprintf("if (point.%s !== undefined) { tooltipContent += '%s: <span style=\"color: ' + %s[point.%s] + ';\">●</span> ' + point.%s + '<br>'; }", 
                                                  time_variant_covariates, 
                                                  time_variant_covariates,
                                                  jsonlite::toJSON(shared_storage$time_variant_covariates),
                                                  time_variant_covariates,
                                                  time_variant_covariates), 
                                          collapse = "\n")
          ))
        )
      )
    
    for (covariate in time_variant_covariates) {
      for (i in seq_along(series_data)) {
        series_data[[i]][[covariate]] <- subject_data[[covariate]][i]
      }
    }
    
    hc$x$hc_opts$series[[length(hc$x$hc_opts$series)]]$data <- series_data
  }
  
  return(hc)
}

  # Update chart outputs
  output$longitudinalChart <- renderHighchart({
  req(filtered_data())
  createChart(filtered_data())
})

# Heatmap outputs
output$heatmapTitle <- renderUI({
  req(input$heatmapCovariate)
  h3(paste("Heatmap of", input$heatmapCovariate), style = "text-align: left;")
})

output$heatmapContainer <- renderUI({
  req(filtered_data(), input$heatmapCovariate)
  tagList(
    highchartOutput("heatmapChart")
  )
})

output$heatmapLegend <- renderUI({
  req(filtered_data(), input$heatmapCovariate)
  uiOutput("heatmapLegendContent")
})

createHeatmap <- function(filtered_data, covariate) {
  # Note: This function only processes time-variant covariates that don't start with underscore.
  # Covariates starting with underscore are filtered out during data loading and selection.
  # This convention allows certain time-variant covariates to be excluded from the heatmap
  # while still being present in the underlying data.
  
  # Check for empty data first
  if (nrow(filtered_data$longitudinal) == 0) {
    return(list(
      chart = highchart() %>%
        hc_chart(type = "heatmap") %>%
        #hc_title(text = "No data available") %>%
        hc_subtitle(text = "No subjects match the selected criteria"),
      levels = character(0),
      colors = character(0)
    ))
  }
  
  heatmap_data <- filtered_data$longitudinal[, c("id", "time", covariate), with = FALSE]
  setnames(heatmap_data, covariate, "covariate_value")
  
  if (covariate %in% names(shared_storage$time_variant_covariates)) {
    covariate_data <- shared_storage$time_variant_covariates[[covariate]]
    unique_values <- names(covariate_data)
    if (all(sapply(covariate_data, is.character))) {
      colors <- unname(covariate_data)
    } else {
      n_colors <- length(unique_values)
      colors <- colorRampPalette(brewer.pal(9, "Spectral"))(n_colors)
    }
  } else {
    unique_values <- unique(heatmap_data$covariate_value)
    is_binary <- length(unique_values) == 2
    if (is_binary) {
      colors <- c("#E41A1C", "#377EB8")
    } else {
      n_colors <- length(unique_values)
      colors <- colorRampPalette(brewer.pal(9, "Spectral"))(n_colors)
    }
  }
  
  heatmap_data[, covariate_value := factor(covariate_value, levels = unique_values)]
  heatmap_data[, covariate_numeric := as.numeric(covariate_value) - 1]
  
  unique_ids <- unique(heatmap_data$id)  # Natural order to fill from top
  n_subjects <- length(unique_ids)
  
  # Always fill from top with sequential positions
  positions <- seq(0, n_subjects - 1)
  
  # Create the mapping
  id_map <- setNames(positions, unique_ids)
  heatmap_data[, y := id_map[id]]
  
  # Create categories array with empty strings
  n_categories <- max(12, n_subjects)
  full_categories <- character(n_categories)
  
  # Fill in subject IDs at correct positions
  for (id in unique_ids) {
    pos <- id_map[id] + 1  # +1 for R's 1-based indexing
    full_categories[pos] <- id
  }
  
  hc <- highchart() %>%
    hc_chart(
      type = "heatmap",
      marginTop = 40,
      marginBottom = 40,
      marginRight = 60,
      plotBorderWidth = 0,
      events = list(
        load = JS("function() {
          var chart = this;
          chart.container.addEventListener('wheel', function(e) {
            e.preventDefault();
            e.stopPropagation();
            
            var yAxis = chart.yAxis[0];
            var delta = e.deltaY > 0 ? 1 : -1;  // Natural scrolling
            var newMin = yAxis.min + delta;
            var newMax = yAxis.max + delta;
            var maxPossible = chart.yAxis[0].categories.length - 1;
            
            // Keep 12-row window and ensure we don't scroll beyond data bounds
            if (newMin >= 0 && newMax <= maxPossible && (newMax - newMin) === 11) {
              yAxis.setExtremes(newMin, newMax);
              chart.redraw(false);
            }
          }, { passive: false });
        }")
      ),
      animation = FALSE
    ) %>%
    hc_plotOptions(series = list(
      dataLabels = list(
        enabled = FALSE
      ),
      states = list(
        hover = list(
          enabled = TRUE,
          brightness = 0.1
        )
      ),
      stickyTracking = FALSE,
      enableMouseTracking = TRUE,
      point = list(
        events = list(
          mouseOver = JS("function() { 
            this.series.chart.container.style.cursor = 'pointer';
            this.graphic.attr({
              'stroke-width': 2,
              'stroke': '#808080'
            });
          }"),
          mouseOut = JS("function() { 
            this.series.chart.container.style.cursor = 'default';
            this.graphic.attr({
              'stroke-width': 0,
              'stroke': 'none'
            });
          }")
        )
      )
    )) %>%
    hc_xAxis(
      title = list(text = "Time"),
      gridLineWidth = 1
    ) %>%
    hc_yAxis(
      title = list(text = "Subject ID"),
      categories = full_categories,
      min = 0,
      max = 11,
      scrollbar = list(enabled = TRUE),
      staticScale = 20,
      endOnTick = FALSE,
      startOnTick = FALSE
    ) %>%
    hc_colorAxis(
      min = 0,
      max = length(unique_values) - 1,
      stops = color_stops(n = length(colors), colors = colors),
      showInLegend = FALSE
    ) %>%
    hc_legend(enabled = FALSE) %>%
    hc_credits(enabled = FALSE) %>%
    hc_exporting(enabled = FALSE) %>%
    hc_tooltip(formatter = JS(sprintf(
      "function() { return '<b>' + this.series.yAxis.categories[this.point.y] + '</b><br>' +
 'Time: ' + this.point.x + '<br>' +
 '%s: ' + %s[this.point.value]; }",
      covariate, 
      jsonlite::toJSON(unique_values)
    ))) %>%
    hc_add_series(
      data = heatmap_data,
      type = "heatmap",
      borderWidth = 0,
      hcaes(x = time, y = y, value = covariate_numeric)
    )
  
  return(list(chart = hc, levels = unique_values, colors = colors))
}

output$heatmapChart <- renderHighchart({
  req(filtered_data(), input$heatmapCovariate)
  createHeatmap(filtered_data(), input$heatmapCovariate)$chart
})

output$heatmapLegendContent <- renderUI({
  req(filtered_data(), input$heatmapCovariate)
  
  heatmap_data <- createHeatmap(filtered_data(), input$heatmapCovariate)
  unique_levels <- heatmap_data$levels
  colors <- heatmap_data$colors
  
  legend_items <- lapply(seq_along(unique_levels), function(i) {
    tags$div(
      style = "display: inline-block; margin-right: 10px; margin-bottom: 5px;",
      tags$span(
        style = sprintf("display: inline-block; width: 20px; height: 20px; background-color: %s; margin-right: 5px;", colors[i])
      ),
      tags$span(unique_levels[i])
    )
  })
  
  tagList(
    div(style = "text-align: right;", legend_items)
  )
})

# Create survival plot
createSurvivalPlot <- function(data, endpoint) {
  req(data, endpoint)
  
  # Check if we have active filters but no matches
  if (data$has_active_filters && all(!data$subjects$selected)) {
    return(highchart() %>%
           hc_chart(borderWidth = 0) %>%
           hc_title(text = paste(endpoint, "Survival Plot"), align = "left", style = list(fontSize = "20px")) %>%
           hc_subtitle(text = "No subjects match the selected criteria") %>%
           hc_xAxis(title = list(text = "Time")) %>%
           hc_yAxis(title = list(text = "Survival Probability")))
  }
  
  time_col <- paste0("eventtime_", endpoint)
  status_col <- endpoint
  
  # Ensure subjects data has a 'selected' column, defaulting to TRUE if not present
  subjects_data <- data$subjects
  if (!"selected" %in% names(subjects_data)) {
    subjects_data[, selected := TRUE]
  }
  
  surv_fit <- survfit(as.formula(paste0("Surv(", time_col, ", ", status_col, " == 'Event') ~ selected")), 
                      data = subjects_data)
  
  hc <- hchart(surv_fit) %>%
    hc_chart(borderWidth = 0) %>%
    hc_title(text = paste(endpoint, "Survival Plot"), align = "left", style = list(fontSize = "20px")) %>%
    hc_xAxis(title = list(text = "Time")) %>%
    hc_yAxis(title = list(text = "Survival Probability")) %>%
    hc_tooltip(shared = TRUE) %>%
    hc_plotOptions(line = list(marker = list(enabled = FALSE))) %>%
    hc_colors(c("#FF0000", "#0000FF"))
  
  return(hc)
}

output$survivalChart <- renderHighchart({
  req(filtered_data(), input$survivalEndpoint)
  createSurvivalPlot(filtered_data(), input$survivalEndpoint)
})

# Render covariate filters
output$covariateFilters <- renderUI({
  req(shared_storage$data)
  # Use the filtered covariates (excluding those starting with underscore)
  # to maintain consistency with color selection and heatmap visualization
  covariate_data <- shared_storage$data[["covariates"]]
  if (!is.list(covariate_data)) stop("Covariates should be a list")
  
  covariate_types <- names(covariate_data)[!grepl("^_", names(covariate_data))]
  current_selections <- shared_storage$selected_subgroups
  
  tagList(
    lapply(covariate_types, function(covariate_type) {
      subgroups <- names(covariate_data[[covariate_type]])
      tags <- lapply(subgroups, function(subgroup) {
        color <- shared_storage$color_map[[covariate_type]][[subgroup]]
        is_selected <- subgroup %in% current_selections[[covariate_type]]
        actionButton(
          inputId = paste0("tag_", covariate_type, "_", subgroup),
          label = subgroup,
          class = paste("tag-item", if(is_selected) "selected" else ""),
          style = paste("background-color:", color, ";")
        )
      })
      tagList(
        h5(covariate_type),
        div(class = "tags-container", tags)
      )
    })
  )
})

# Update subject choices
observe({
  req(shared_storage$subject_dt)
  updateSelectizeInput(
    session, "subjectIdInput",
    server = TRUE,
    choices = shared_storage$subject_dt[, unique(id)]
  )
})

# Observe tag clicks
observe({
  req(shared_storage$data)
  covariate_data <- shared_storage$data[["covariates"]]
  for (covariate_type in names(covariate_data)) {
    for (subgroup in names(covariate_data[[covariate_type]])) {
      local({
        local_covariate_type <- covariate_type
        local_subgroup <- subgroup
        input_id <- paste0("tag_", local_covariate_type, "_", local_subgroup)
        observeEvent(input[[input_id]], {
          current_selections <- shared_storage$selected_subgroups
          
          if (is.null(current_selections[[local_covariate_type]])) {
            current_selections[[local_covariate_type]] <- character(0)
          }
          
          if (local_subgroup %in% current_selections[[local_covariate_type]]) {
            current_selections[[local_covariate_type]] <- setdiff(current_selections[[local_covariate_type]], local_subgroup)
            shinyjs::removeClass(input_id, "selected")
          } else {
            current_selections[[local_covariate_type]] <- c(current_selections[[local_covariate_type]], local_subgroup)
            shinyjs::addClass(input_id, "selected")
          }
          
          if (length(current_selections[[local_covariate_type]]) == 0) {
            current_selections[[local_covariate_type]] <- NULL
          }
          
          shared_storage$selected_subgroups <- current_selections
        }, ignoreInit = TRUE)
      })
    }
  }
})

  observeEvent(input$colorCovariateSelect, {
    output$longitudinalChart <- renderHighchart({
      req(filtered_data())
      createChart(filtered_data())
    })
  })

  # Random selection
  observeEvent(input$randomSelectButton, {
    req(shared_storage$subject_dt)
    subject_ids <- shared_storage$subject_dt[, unique(id)]
    if (length(subject_ids) > 0) {
      num_to_select <- min(as.numeric(input$randomSelectCount), length(subject_ids))
      random_ids <- sample(subject_ids, size = num_to_select)
      updateSelectizeInput(session, "subjectIdInput", selected = random_ids)
    } else {
      showNotification("No subjects available to select", type = "warning")
    }
  })

  # Clear filters
  observeEvent(input$clearButton, {
    shared_storage$clearing_filters <- TRUE
    
    isolate({
      updateSelectizeInput(session, "subjectIdInput", selected = character(0))
      shared_storage$selected_subgroups <- list()
      
      req(shared_storage$data)
      lapply(names(shared_storage$data[["covariates"]]), function(covariate_type) {
        lapply(names(shared_storage$data[["covariates"]][[covariate_type]]), function(subgroup) {
          session$sendCustomMessage(
            type = 'updateButtonClass',
            message = list(
              id = paste0("tag_", covariate_type, "_", subgroup),
              class = ''
            )
          )
        })
      })
    })
    
    shinyjs::delay(100, {
      all_subjects <- isolate(shared_storage$subject_dt)
      all_subjects[, selected := TRUE]  # Ensure all subjects are selected when filters are cleared
      all_longitudinal <- isolate(shared_storage$longitudinal_dt)
      createChart(list(
        subjects = all_subjects, 
        filtered_subjects = all_subjects,
        longitudinal = all_longitudinal,
        has_active_filters = FALSE  # Explicitly set to FALSE when clearing filters
      ))
      
      shared_storage$clearing_filters <- FALSE
    })
  })

  # Subject table title
  output$subjectTableTitle <- renderUI({
    req(filtered_data())
    filtered_subjects <- filtered_data()$filtered_subjects
    
    if (nrow(filtered_subjects) > 0) {
      h3(paste("Selected Subjects (", nrow(filtered_subjects), " subjects )"), 
         style = "text-align: left;")
    }
  })

  # Data table
  output$subjectDataTable <- renderDT({
    req(filtered_data())
    filtered_subjects <- filtered_data()$filtered_subjects
    
    if (nrow(filtered_subjects) > 0) {
      if ("selected" %in% names(filtered_subjects)) {
        filtered_subjects[, selected := NULL]
      }
      
      datatable(filtered_subjects, 
                options = list(pageLength = 10, 
                               scrollX = TRUE, 
                               scrollY = "400px",
                               dom = 'Bfrtip',
                               buttons = c('csv', 'excel')),
                extensions = 'Buttons',
                rownames = FALSE)
    } else {
      datatable(data.frame(Message = "No subjects selected. Please use the filters to select subjects."),
                options = list(dom = 't'),
                rownames = FALSE)
    }
  })
} # Close mrdviz_server function