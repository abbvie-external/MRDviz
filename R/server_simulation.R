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

# Reactive simulation using simjm
simData <- reactive({
  req(input$missing_data_rate)  # Ensure missing_data_rate is available
  validate(
    need(is.numeric(input$missing_data_rate) && 
         input$missing_data_rate >= 0 && 
         input$missing_data_rate <= 1,
         "Missing data rate must be between 0 and 1")
  )
  
  seed <- as.integer(Sys.time())
  b_sd <- c(input$b_sd1, input$b_sd2)
  
  simjm(
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
  
  simulations$long[[length(simulations$long) + 1]] <- currentLong
  simulations$event[[length(simulations$event) + 1]] <- currentEvent
  
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
    p <- p + aes(color = group, group = interaction(subject_id, group))
  } else {
    p <- p + aes(group = subject_id)
  }
  
  # Filter data for non-missing values
  non_missing_data <- data_to_plot[!is.na(data_to_plot[[y_col]]), ]
  
  p + geom_line(data = non_missing_data, alpha = 0.3) +
    geom_point(data = non_missing_data, alpha = 0.3) +
    geom_vline(xintercept = input$betaLong_mrd_eot, linetype = "dashed", color = "red") +
    labs(x = "Time", y = y_label,
         title = "Individual MRD Trajectories",
         subtitle = paste("EoT time =", input$betaLong_mrd_eot)) +
    theme_bw() +
    scale_y_continuous(trans = "log10")
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
    event_data$group <- as.factor(event_data$group)
    surv_obj <- survival::survfit(survival::Surv(eventtime_OS, OS) ~ group, data = event_data)
    
    group_levels <- levels(event_data$group)
    plot(surv_obj,
         xlab = "Time",
         ylab = "Survival Probability",
         main = "Kaplan-Meier Plot (By Group)",
         conf.int = input$show_ci,
         lwd = 2,
         mark.time = TRUE,
         col = 1:length(group_levels))
    
    if (input$show_ci) {
      for(i in 1:length(group_levels)) {
        lines(surv_obj$time[surv_obj$strata == i],
              surv_obj$upper[surv_obj$strata == i],
              col = adjustcolor(i, alpha.f = 0.2),
              lty = 2)
        lines(surv_obj$time[surv_obj$strata == i],
              surv_obj$lower[surv_obj$strata == i],
              col = adjustcolor(i, alpha.f = 0.2),
              lty = 2)
      }
    }
    
    legend("topright",
           legend = group_levels,
           lty = 1,
           col = 1:length(group_levels),
           lwd = 2,
           bty = "n")
  } else {
    event_data <- simData()$Event %>%
      rename(eventtime_OS = eventtime, OS = status)
    surv_obj <- survival::survfit(survival::Surv(eventtime_OS, OS) ~ 1, data = event_data)
    plot(surv_obj,
         xlab = "Time",
         ylab = "Survival Probability",
         main = "Kaplan-Meier Plot",
         conf.int = input$show_ci,
         lwd = 2,
         mark.time = TRUE,
         col = "blue")
    
    if (input$show_ci) {
      lines(surv_obj$time,
            surv_obj$upper,
            col = adjustcolor("blue", alpha.f = 0.2),
            lty = 2)
      lines(surv_obj$time,
            surv_obj$lower,
            col = adjustcolor("blue", alpha.f = 0.2),
            lty = 2)
    }
    
    if (input$show_ci) {
      legend("topright",
             legend = c("Survival estimate", "95% CI"),
             lty = c(1, 2),
             col = c("blue", adjustcolor("blue", alpha.f = 0.2)),
             lwd = c(2, 1),
             bty = "n")
    } else {
      legend("topright",
             legend = "Survival estimate",
             lty = 1,
             col = "blue",
             lwd = 2,
             bty = "n")
    }
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