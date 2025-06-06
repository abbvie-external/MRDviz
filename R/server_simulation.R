#' MRDviz Simulation Server Functions
#'
#' Server-side logic for the MRDviz simulation functionality
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param shared_storage Reactive values object for storing shared data
#' @export
simulation_server <- function(input, output, session, shared_storage) {
  # Reactive values for simulation
  groupParams <- reactiveVal(data.frame())
  simulations <- reactiveValues(long = list(), event = list())
  
  # Function to generate consistent colors for groups and maintain order
  group_colors <- reactive({
    # Get groups in the order they were added
    groups <- if (length(simulations$long) > 0) {
      group_names <- names(simulations$long)
      if (is.null(group_names) || all(group_names == "")) {
        # If names not available, fall back to unique groups from data
        unique(do.call(rbind, simulations$long)$group)
      } else {
        # Use names if available (preserves order of addition)
        group_names
      }
    } else {
      character(0)
    }
    
    # Create a named vector of colors for each group
    colors <- setNames(
      scales::hue_pal()(length(groups)),
      groups
    )
    
    colors
  })

# Cache for MRD trajectories
mrdCache <- reactiveValues(
  Long1 = NULL,
  seed = NULL,
  params = list()
)

# Function to check if MRD-related parameters have changed
mrdParamsChanged <- function(current_params, cached_params) {
  if (length(cached_params) == 0) return(TRUE)
  
  # List of parameters that affect MRD trajectories
  mrd_params <- c(
    "n", "missing_data_rate", "betaLong_intercept", "betaLong_binary", 
    "betaLong_continuous", "betaLong_mrd_eot", "betaLong_mrd_relapse_prob", 
    "betaLong_mrd_relapse_time_mean", "betaLong_mrd_relapse_time_sd", 
    "betaLong_mrd_relapse_rate", "betaLong_baseline_mean", "betaLong_baseline_sd", 
    "betaLong_plateau_mean", "betaLong_plateau_sd", "betaLong_plateau_binary", 
    "betaLong_plateau_continuous", "betaLong_aux", "b_sd1", "b_sd2", 
    "prob_Z1", "mean_Z2", "sd_Z2", "max_fuptime", "max_yobs"
  )
  
  # Check if any MRD-related parameter has changed
  for (param in mrd_params) {
    if (!identical(current_params[[param]], cached_params[[param]])) {
      return(TRUE)
    }
  }
  
  return(FALSE)
}

# Reactive simulation using simjm
simData <- reactive({
  req(input$missing_data_rate)  # Ensure missing_data_rate is available
  validate(
    need(is.numeric(input$missing_data_rate) && 
         input$missing_data_rate >= 0 && 
         input$missing_data_rate <= 1,
         "Missing data rate must be between 0 and 1")
  )
  
  # Collect current parameters
  current_params <- list(
    n = input$n,
    missing_data_rate = input$missing_data_rate,
    betaLong_intercept = input$betaLong_intercept,
    betaLong_binary = input$betaLong_binary,
    betaLong_continuous = input$betaLong_continuous,
    betaLong_mrd_eot = input$betaLong_mrd_eot,
    betaLong_mrd_relapse_prob = input$betaLong_mrd_relapse_prob,
    betaLong_mrd_relapse_time_mean = input$betaLong_mrd_relapse_time_mean,
    betaLong_mrd_relapse_time_sd = input$betaLong_mrd_relapse_time_sd,
    betaLong_mrd_relapse_rate = input$betaLong_mrd_relapse_rate,
    betaLong_baseline_mean = input$betaLong_baseline_mean,
    betaLong_baseline_sd = input$betaLong_baseline_sd,
    betaLong_plateau_mean = input$betaLong_plateau_mean,
    betaLong_plateau_sd = input$betaLong_plateau_sd,
    betaLong_plateau_binary = input$betaLong_plateau_binary,
    betaLong_plateau_continuous = input$betaLong_plateau_continuous,
    betaLong_aux = input$betaLong_aux,
    betaEvent_intercept = input$betaEvent_intercept,
    betaEvent_binary = input$betaEvent_binary,
    betaEvent_continuous = input$betaEvent_continuous,
    betaEvent_assoc_value = input$betaEvent_assoc_value,
    betaEvent_assoc_slope = input$betaEvent_assoc_slope,
    betaEvent_assoc_auc = input$betaEvent_assoc_auc,
    betaEvent_aux = input$betaEvent_aux,
    b_sd1 = input$b_sd1,
    b_sd2 = input$b_sd2,
    prob_Z1 = input$prob_Z1,
    mean_Z2 = input$mean_Z2,
    sd_Z2 = input$sd_Z2,
    max_fuptime = input$max_fuptime,
    max_yobs = input$max_yobs
  )
  
  # Check if MRD trajectories need to be regenerated
  regenerate_mrd <- mrdParamsChanged(current_params, mrdCache$params)
  
  # Use the same seed if only regenerating survival curves
  seed <- if (regenerate_mrd) as.integer(Sys.time()) else mrdCache$seed
  b_sd <- c(input$b_sd1, input$b_sd2)
  
  if (regenerate_mrd) {
    # Store the seed and parameters for future reference
    mrdCache$seed <- seed
    mrdCache$params <- current_params
    
    # Generate full simulation
    result <- simjm(
      n = input$n,
      M = 1,
      fixed_trajectory = "mrd_piecewise",
      missing_data_rate = input$missing_data_rate,
      betaLong_intercept = input$betaLong_intercept,
      betaLong_binary = input$betaLong_binary,
      betaLong_continuous = input$betaLong_continuous,
      
      betaLong_mrd_eot = input$betaLong_mrd_eot,
      betaLong_mrd_relapse_prob = input$betaLong_mrd_relapse_prob,
      betaLong_mrd_relapse_time_mean = input$betaLong_mrd_relapse_time_mean,
      betaLong_mrd_relapse_time_sd = input$betaLong_mrd_relapse_time_sd,
      betaLong_mrd_relapse_rate = input$betaLong_mrd_relapse_rate,
      
      betaLong_baseline_mean = input$betaLong_baseline_mean,
      betaLong_baseline_sd = input$betaLong_baseline_sd,
      betaLong_plateau_mean = input$betaLong_plateau_mean,
      betaLong_plateau_sd = input$betaLong_plateau_sd,
      betaLong_plateau_binary = input$betaLong_plateau_binary,
      betaLong_plateau_continuous = input$betaLong_plateau_continuous,
      
      betaLong_aux = input$betaLong_aux,
      
      betaEvent_intercept = input$betaEvent_intercept,
      betaEvent_binary = input$betaEvent_binary,
      betaEvent_continuous = input$betaEvent_continuous,
      betaEvent_assoc_value = input$betaEvent_assoc_value,
      betaEvent_assoc_slope = input$betaEvent_assoc_slope,
      betaEvent_assoc_auc = input$betaEvent_assoc_auc,
      betaEvent_aux = input$betaEvent_aux,
      
      b_sd = b_sd,
      prob_Z1 = input$prob_Z1,
      mean_Z2 = input$mean_Z2,
      sd_Z2 = input$sd_Z2,
      
      max_fuptime = input$max_fuptime,
      max_yobs = input$max_yobs,
      
      balanced = FALSE,
      family = "lognormal",
      clust_control = list(),
      return_eta = TRUE,
      grp_assoc = NULL,
      seed = seed,
      interval = c(1E-8, 200)
    )
    
    # Cache the MRD trajectories
    mrdCache$Long1 <- result$Long1
    
    return(result)
  } else {
    # Only regenerate survival curves using cached MRD trajectories
    # We need to use the same covariates and subject IDs as in the cached MRD data
    
    # Extract subject IDs and covariates from cached MRD data
    subject_ids <- unique(mrdCache$Long1$id)
    n_subjects <- length(subject_ids)
    
    # Get unique subject data (one row per subject)
    subject_data <- mrdCache$Long1[!duplicated(mrdCache$Long1$id), ]
    
    # Generate new survival data with the same subjects but updated event parameters
    result <- simjm(
      n = n_subjects,
      M = 1,
      fixed_trajectory = "mrd_piecewise",
      missing_data_rate = input$missing_data_rate,
      betaLong_intercept = input$betaLong_intercept,
      betaLong_binary = input$betaLong_binary,
      betaLong_continuous = input$betaLong_continuous,
      
      betaLong_mrd_eot = input$betaLong_mrd_eot,
      betaLong_mrd_relapse_prob = input$betaLong_mrd_relapse_prob,
      betaLong_mrd_relapse_time_mean = input$betaLong_mrd_relapse_time_mean,
      betaLong_mrd_relapse_time_sd = input$betaLong_mrd_relapse_time_sd,
      betaLong_mrd_relapse_rate = input$betaLong_mrd_relapse_rate,
      
      betaLong_baseline_mean = input$betaLong_baseline_mean,
      betaLong_baseline_sd = input$betaLong_baseline_sd,
      betaLong_plateau_mean = input$betaLong_plateau_mean,
      betaLong_plateau_sd = input$betaLong_plateau_sd,
      betaLong_plateau_binary = input$betaLong_plateau_binary,
      betaLong_plateau_continuous = input$betaLong_plateau_continuous,
      
      betaLong_aux = input$betaLong_aux,
      
      betaEvent_intercept = input$betaEvent_intercept,
      betaEvent_binary = input$betaEvent_binary,
      betaEvent_continuous = input$betaEvent_continuous,
      betaEvent_assoc_value = input$betaEvent_assoc_value,
      betaEvent_assoc_slope = input$betaEvent_assoc_slope,
      betaEvent_assoc_auc = input$betaEvent_assoc_auc,
      betaEvent_aux = input$betaEvent_aux,
      
      b_sd = b_sd,
      prob_Z1 = input$prob_Z1,
      mean_Z2 = input$mean_Z2,
      sd_Z2 = input$sd_Z2,
      
      max_fuptime = input$max_fuptime,
      max_yobs = input$max_yobs,
      
      balanced = FALSE,
      family = "lognormal",
      clust_control = list(),
      return_eta = TRUE,
      grp_assoc = NULL,
      seed = seed,
      interval = c(1E-8, 200)
    )
    
    # Use the cached MRD trajectories instead of the newly generated ones
    result$Long1 <- mrdCache$Long1
    
    return(result)
  }
})

# Store combined simulation data
combinedLong <- reactive({
  if (length(simulations$long) > 0) {
    do.call(rbind, simulations$long)
  } else {
    NULL
  }
})

combinedEvent <- reactive({
  if (length(simulations$event) > 0) {
    do.call(rbind, simulations$event)
  } else {
    NULL
  }
})

# Add simulation for group
observeEvent(input$addSim, {
  currentLong <- simData()$Long1 %>%
    rename(
      subject_id = id,
      time = tij,
      true_value = Xij_1,
      measurement = Yij_1
    )
  currentEvent <- simData()$Event
  
  currentLong$subject_id <- sprintf("%s_%03d", input$groupName, as.numeric(currentLong$subject_id))
  currentEvent$id <- sprintf("%s_%03d", input$groupName, as.numeric(currentEvent$id))
  
  currentLong$group <- input$groupName
  currentEvent$group <- input$groupName
  
  # Name the list element to preserve group order
  next_index <- length(simulations$long) + 1
  simulations$long[[next_index]] <- currentLong
  simulations$event[[next_index]] <- currentEvent
  
  # Store the group name as the list element name for ordering
  names(simulations$long)[next_index] <- input$groupName
  names(simulations$event)[next_index] <- input$groupName
  
  new_row <- data.frame(
    group = input$groupName,
    n = input$n,
    max_fuptime = input$max_fuptime,
    max_yobs = input$max_yobs,
    missing_data_rate = input$missing_data_rate,
    betaLong_intercept = input$betaLong_intercept,
    betaLong_binary = input$betaLong_binary,
    betaLong_continuous = input$betaLong_continuous,
    betaLong_mrd_eot = input$betaLong_mrd_eot,
    betaLong_mrd_relapse_prob = input$betaLong_mrd_relapse_prob,
    betaLong_mrd_relapse_time_mean = input$betaLong_mrd_relapse_time_mean,
    betaLong_mrd_relapse_time_sd = input$betaLong_mrd_relapse_time_sd,
    betaLong_mrd_relapse_rate = input$betaLong_mrd_relapse_rate,
    betaLong_baseline_mean = input$betaLong_baseline_mean,
    betaLong_baseline_sd = input$betaLong_baseline_sd,
    betaLong_plateau_mean = input$betaLong_plateau_mean,
    betaLong_plateau_sd = input$betaLong_plateau_sd,
    betaLong_plateau_binary = input$betaLong_plateau_binary,
    betaLong_plateau_continuous = input$betaLong_plateau_continuous,
    betaLong_aux = input$betaLong_aux,
    betaEvent_intercept = input$betaEvent_intercept,
    betaEvent_aux = input$betaEvent_aux,
    betaEvent_binary = input$betaEvent_binary,
    betaEvent_continuous = input$betaEvent_continuous,
    betaEvent_assoc_value = input$betaEvent_assoc_value,
    betaEvent_assoc_slope = input$betaEvent_assoc_slope,
    betaEvent_assoc_auc = input$betaEvent_assoc_auc,
    prob_Z1 = input$prob_Z1,
    mean_Z2 = input$mean_Z2,
    sd_Z2 = input$sd_Z2,
    timestamp = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )
  groupParams(rbind(groupParams(), new_row))
  
  showNotification(paste("Simulation for group", input$groupName, "added."), type = "message")
  updateTextInput(session, "groupName", value = "")
})

# Trajectory Plot
output$trajPlot <- renderPlot({
  data_to_plot <- if (input$show_all_groups) {
    tmp <- combinedLong()
    if (is.null(tmp)) {
      simData()$Long1 %>%
        rename(
          subject_id = id,
          time = tij,
          true_value = Xij_1,
          measurement = Yij_1
        )
    } else tmp
  } else {
    simData()$Long1 %>%
      rename(
        subject_id = id,
        time = tij,
        true_value = Xij_1,
        measurement = Yij_1
      )
  }
  y_col <- if(input$show_true) "true_value" else "measurement"
  y_label <- if(input$show_true) "True MRD Value" else "Observed MRD Value"
  
  p <- ggplot(data_to_plot, aes(x = time, y = .data[[y_col]]))
  if ("group" %in% names(data_to_plot)) {
    # Get group order based on the order they were added
    ordered_groups <- names(simulations$long)
    
    # Convert group to factor with levels in order of addition
    if (length(ordered_groups) > 0) {
      data_to_plot$group <- factor(data_to_plot$group, levels = ordered_groups)
    }
    
    p <- p + aes(color = group, group = interaction(subject_id, group))
    
    # Apply consistent color scale if there are multiple groups
    if (length(unique(data_to_plot$group)) > 1) {
      p <- p + scale_color_manual(values = group_colors())
    }
  } else {
    p <- p + aes(group = subject_id)
  }
  
  # Filter data for non-missing values
  non_missing_data <- data_to_plot[!is.na(data_to_plot[[y_col]]), ]
  
  p + geom_line(data = non_missing_data, alpha = 0.3) +
    geom_point(data = non_missing_data, alpha = 0.3) +
    geom_vline(xintercept = input$betaLong_mrd_eot, linetype = "dashed", color = "red") +
    labs(x = "Time", y = y_label, color = NULL) +
    theme_bw() +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14)
    ) +
    scale_y_continuous(trans = "log10") +
    {if(input$show_all_groups) theme(legend.position = "top") else NULL}
})

# Trajectory Summary
output$trajSummary <- renderPrint({
  long_data <- simData()$Long1 %>%
    rename(
      subject_id = id,
      time = tij,
      true_value = Xij_1,
      measurement = Yij_1
    )
  
  # Calculate per-subject statistics
  by_subject <- long_data %>%
    group_by(subject_id) %>%
    summarise(
      n_obs = sum(!is.na(measurement)),
      mean_value = mean(measurement, na.rm = TRUE),
      min_value = min(measurement, na.rm = TRUE),
      max_value = max(measurement, na.rm = TRUE),
      missing_rate = mean(is.na(measurement))
    )
  
  # Calculate overall statistics
  overall_stats <- by_subject %>%
    summarise(
      mean_obs_per_subject = mean(n_obs),
      mean_value = mean(mean_value, na.rm = TRUE),
      overall_min = min(min_value, na.rm = TRUE),
      overall_max = max(max_value, na.rm = TRUE),
      avg_missing_rate = mean(missing_rate, na.rm = TRUE)
    )
  
  # Print in a formatted way like Event Summary
  cat("Number of subjects:", nrow(by_subject), "\n")
  cat("Mean observations per subject:", round(overall_stats$mean_obs_per_subject, 2), "\n")
  cat("Mean MRD value:", format(overall_stats$mean_value, scientific = TRUE, digits = 3), "\n")
  cat("Minimum MRD value:", format(overall_stats$overall_min, scientific = TRUE, digits = 3), "\n")
  cat("Maximum MRD value:", format(overall_stats$overall_max, scientific = TRUE, digits = 3), "\n")
  cat("Missing data rate:", round(overall_stats$avg_missing_rate, 3))
})

# Event Plot
output$eventPlot <- renderPlot({
  if (isTRUE(input$show_all_groups_surv)) {
    event_data <- combinedEvent()
    if (is.null(event_data) || nrow(event_data) == 0) {
      event_data <- simData()$Event
    }
    if (!("group" %in% names(event_data)) || length(event_data$group) == 0) {
      event_data$group <- rep("Current", nrow(event_data))
    }
    # Ensure consistent column naming for both single and all-groups scenarios
    event_data <- event_data %>%
      rename(eventtime_OS = eventtime, OS = status)
      
    # Get group order based on the order they were added
    ordered_groups <- names(simulations$long)
    
    # Create a factor with levels in the order of addition
    if (length(ordered_groups) > 0) {
      event_data$group <- factor(event_data$group, levels = ordered_groups)
    } else {
      event_data$group <- as.factor(event_data$group)
    }
    
    surv_obj <- survival::survfit(survival::Surv(eventtime_OS, OS) ~ group, data = event_data)
    
    # Get the ordered groups just like in the trajectory plot
    ordered_groups <- names(simulations$long)
    
    # Make sure event_data$group has the same factor levels as the trajectory plot
    if (length(ordered_groups) > 0) {
      event_data$group <- factor(event_data$group, levels = ordered_groups)
    }
    
    # Get group levels after factoring
    group_levels <- levels(event_data$group)
    
    # Get the exact same colors as used in the trajectory plot
    colors <- group_colors()
    
    # Use ggsurvplot with proper error handling
    tryCatch({
      # Create survival object with explicitly factored groups
      surv_obj <- survival::survfit(survival::Surv(eventtime_OS, OS) ~ group, data = event_data)
      
      # Use ggsurvplot with the same direct reference to group_colors()
      # that's used in the trajectory plot
      p <- survminer::ggsurvplot(
        surv_obj,
        data = event_data,
        conf.int = input$show_ci,
        # Use the same direct reference to colors
        palette = unname(colors),
        legend.title = "",
        legend.labs = names(colors),
        xlab = "Time", 
        ylab = "Survival Probability",
        legend = "bottom",
        ggtheme = theme_bw(),
        risk.table = FALSE,
        mark.time = TRUE
      )
      
      # Apply additional theme elements for consistency with the MRD trajectory plot
      p$plot <- p$plot + 
        theme(
          legend.position = "top",
          legend.background = element_rect(fill = "white", color = NA),
          legend.key = element_rect(fill = "white", color = NA),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14)
        )
    }, error = function(e) {
      # In case of error, fall back to a simpler plot
      message("Error in survival plot: ", e$message)
      
      # Create a simple plot as fallback
      simple_fit <- survival::survfit(survival::Surv(eventtime_OS, OS) ~ 1, data = event_data)
      p <- survminer::ggsurvplot(
        simple_fit,
        data = event_data,
        conf.int = FALSE,
        palette = "blue",
        xlab = "Time", 
        ylab = "Survival Probability",
        legend = "none",
        ggtheme = theme_bw()
      )
    })
    
    print(p)
    
  } else {
    event_data <- simData()$Event %>%
      rename(eventtime_OS = eventtime, OS = status)
    surv_obj <- survival::survfit(survival::Surv(eventtime_OS, OS) ~ 1, data = event_data)
    
    # Use ggsurvplot instead of base R plot
    p <- survminer::ggsurvplot(
      surv_obj,
      data = event_data,
      conf.int = input$show_ci,
      palette = "blue",
      xlab = "Time", 
      ylab = "Survival Probability",
      legend = "none",
      ggtheme = theme_bw() + theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)
      ),
      risk.table = FALSE,
      mark.time = TRUE
    )
    
    print(p)
  }
})

# Event Summary
output$eventSummary <- renderPrint({
  event_data <- simData()$Event %>%
    rename(eventtime_OS = eventtime, OS = status)
  n_events <- sum(event_data$OS)
  n_censored <- sum(!event_data$OS)
  n_relapse <- sum(event_data$has_relapse_1, na.rm = TRUE)
  median_time <- median(event_data$eventtime_OS)
  median_relapse_time <- median(event_data$relapse_time_1[event_data$has_relapse_1 == 1], na.rm = TRUE)
  
  cat("Number of events:", n_events, "\n")
  cat("Number censored:", n_censored, "\n")
  cat("Number of relapses:", n_relapse, "\n")
  cat("Median event time:", round(median_time, 2), "\n")
  cat("Median relapse time:", round(median_relapse_time, 2), "\n")
  cat("Event rate:", round(n_events / nrow(event_data), 3), "\n")
  cat("Relapse rate:", round(n_relapse / nrow(event_data), 3))
})

# Data Preview Tables
output$longData <- DT::renderDT(server = FALSE, {
  data_to_show <- combinedLong()
  if (is.null(data_to_show)) {
    data_to_show <- simData()$Long1
  }
  
  # Handle both old and new column names
  if ("id" %in% names(data_to_show)) {
    data_to_show <- data_to_show %>% 
      select(id, tij, Xij_1, Yij_1) %>%
      rename(
        subject_id = id,
        time = tij,
        true_value = Xij_1,
        measurement = Yij_1
      )
  } else {
    data_to_show <- data_to_show %>% 
      select(subject_id, time, true_value, measurement)
  }
  
  DT::datatable(
    data_to_show, 
    extensions = 'Buttons',
    options = list(
      dom = 'Bfrtip', 
      buttons = list(
        list(extend = 'csv', filename = paste0("MRDsim_traj_", Sys.Date()),
             exportOptions = list(modifier = list(page = 'all'))),
        list(extend = 'excel', filename = paste0("MRDsim_traj_", Sys.Date()),
             exportOptions = list(modifier = list(page = 'all')))
      ),
      scrollX = TRUE, 
      pageLength = 5
    ),
    rownames = FALSE
  )
})

output$eventData <- DT::renderDT(server = FALSE, {
  data_to_show <- combinedEvent()
  if (is.null(data_to_show)) {
    event_data <- simData()$Event
    event_data <- event_data %>%
      mutate(
        relapse_status = ifelse(has_relapse_1 == 1, "Relapse", "No Relapse"),
        relapse_time = round(relapse_time_1, 2),
        eventtime = round(eventtime, 2)
      ) %>%
      rename(
        subject_id = id,
        eventtime_OS = eventtime,
        OS = status
      ) %>%
      select(subject_id, Z1, Z2, eventtime_OS, OS, relapse_status, relapse_time)
    data_to_show <- event_data
  } else {
    data_to_show <- data_to_show %>%
      mutate(
        relapse_status = ifelse(has_relapse_1 == 1, "Relapse", "No Relapse"),
        relapse_time = round(relapse_time_1, 2),
        eventtime = round(eventtime, 2)
      ) %>%
      rename(
        subject_id = id,
        eventtime_OS = eventtime,
        OS = status
      ) %>%
      select(subject_id, Z1, Z2, eventtime_OS, OS, relapse_status, relapse_time, group)
  }
  DT::datatable(
    data_to_show,
    extensions = 'Buttons',
    options = list(
      dom = 'Bfrtip', 
      buttons = list(
        list(extend = 'csv', filename = paste0("MRDsim_event_", Sys.Date()),
             exportOptions = list(modifier = list(page = 'all'))),
        list(extend = 'excel', filename = paste0("MRDsim_event_", Sys.Date()),
             exportOptions = list(modifier = list(page = 'all')))
      ),
      scrollX = TRUE, 
      pageLength = 5
    ),
    rownames = FALSE
  )
})

  output$paramTable <- DT::renderDT(server = FALSE, {
    DT::datatable(
      groupParams(), 
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip', 
        buttons = list(
          list(extend = 'csv', filename = paste0("MRDsim_params_", Sys.Date()),
               exportOptions = list(modifier = list(page = 'all'))),
          list(extend = 'excel', filename = paste0("MRDsim_params_", Sys.Date()),
               exportOptions = list(modifier = list(page = 'all')))
        ),
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
} # Close simulation_server function
