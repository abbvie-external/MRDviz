#' Lognormal Family
#'
#' Creates a family object for lognormal distribution, used in the simulation of MRD data.
#' This is a custom family object that implements the lognormal distribution for use
#' in modeling longitudinal MRD measurements.
#'
#' @return A family object with the following components:
#'   \item{family}{Character string "lognormal"}
#'   \item{link}{Link function name "log"}
#'   \item{linkinv}{Inverse link function (exponential)}
#'   \item{variance}{Variance function}
#'   \item{dev.resids}{Deviance residuals function}
#'   \item{aic}{AIC function}
#'   \item{initialize}{Initialization expression}
#' @export
lognormal <- function() {
  structure(
    list(
      family     = "lognormal",
      link       = "log",
      linkinv    = exp,
      variance   = function(mu) rep(1, length(mu)),
      dev.resids = function(y, mu, wt) 2 * wt * (log(y) - mu)^2,
      aic        = function(y, n, mu, wt, dev) NA,
      initialize = expression({
        if (any(y <= 0))
          stop("Non-positive values not allowed for lognormal family", call. = FALSE)
        n <- rep(1, nobs)
        mustart <- log(y)
      })
    ),
    class = "family"
  )
}

#' Simulate Joint Model for MRD Data
#'
#' Simulates longitudinal and survival data for MRD trajectories using a joint modeling approach
#' @param n Number of subjects
#' @param M Number of markers
#' @param fixed_trajectory Type of fixed trajectory ("mrd_piecewise", "linear", "quadratic", "cubic")
#' @param random_trajectory Type of random trajectory
#' @param assoc Association structure
#' @param basehaz Baseline hazard type
#' @param betaLong_intercept Intercept for longitudinal component
#' @param betaLong_binary Binary covariate effect
#' @param betaLong_continuous Continuous covariate effect
#' @param betaLong_linear Linear time effect
#' @param betaLong_quadratic Quadratic time effect
#' @param betaLong_cubic Cubic time effect
#' @param betaLong_mrd_eot End of treatment time
#' @param betaLong_mrd_relapse_prob Relapse probability
#' @param betaLong_mrd_relapse_time_mean Mean relapse time
#' @param betaLong_mrd_relapse_time_sd SD of relapse time
#' @param betaLong_mrd_relapse_rate Relapse rate
#' @param betaLong_baseline_mean Baseline MRD mean
#' @param betaLong_baseline_sd Baseline MRD SD
#' @param betaLong_plateau_mean Plateau MRD mean
#' @param betaLong_plateau_sd Plateau MRD SD
#' @param betaLong_plateau_binary Binary covariate effect on plateau
#' @param betaLong_plateau_continuous Continuous covariate effect on plateau
#' @param betaLong_aux Auxiliary parameter for longitudinal component
#' @param betaEvent_intercept Event model intercept
#' @param betaEvent_binary Binary covariate effect on event
#' @param betaEvent_continuous Continuous covariate effect on event
#' @param betaEvent_assoc_value Association parameter for current value
#' @param betaEvent_assoc_slope Association parameter for slope
#' @param betaEvent_assoc_auc Association parameter for AUC
#' @param betaEvent_aux Auxiliary parameter for event model
#' @param b_sd Random effects SD
#' @param b_rho Random effects correlation
#' @param prob_Z1 Probability for binary covariate
#' @param mean_Z2 Mean for continuous covariate
#' @param sd_Z2 SD for continuous covariate
#' @param max_yobs Maximum number of observations per subject
#' @param max_fuptime Maximum follow-up time
#' @param balanced Whether to use balanced design
#' @param family Distribution family
#' @param clust_control Clustering control parameters
#' @param return_eta Whether to return linear predictor
#' @param grp_assoc Group association structure
#' @param missing_data_rate Proportion of missing data points (0-1)
#' @param seed Random seed
#' @param interval Interval for hazard calculation
#' @return A list containing simulated longitudinal and event data
#' @export
simjm <- function(n = 200, M = 1,
                  fixed_trajectory = "mrd_piecewise",
                  random_trajectory = "linear",
                  assoc = "etavalue",
                  basehaz = c("weibull"),
                  missing_data_rate = 0,
                  betaLong_intercept = 10,
                  betaLong_binary = -1,
                  betaLong_continuous = 1,
                  betaLong_linear = -0.25,
                  betaLong_quadratic = 0.03,
                  betaLong_cubic = -0.0015,
                  betaLong_mrd_eot = 6,
                  betaLong_mrd_relapse_prob = 0.3, 
                  betaLong_mrd_relapse_time_mean = 12, 
                  betaLong_mrd_relapse_time_sd = 2,   
                  betaLong_mrd_relapse_rate = 0.2,
                  betaLong_baseline_mean = 4,
                  betaLong_baseline_sd = 0.3,
                  betaLong_plateau_mean = 1,
                  betaLong_plateau_sd = 0.2,
                  betaLong_plateau_binary = -0.1,
                  betaLong_plateau_continuous = -0.05,
                  betaLong_aux = 0.5,
                  betaEvent_intercept = -7.5,
                  betaEvent_binary = -0.5,
                  betaEvent_continuous = 0.5,
                  betaEvent_assoc_value = 0.5,
                  betaEvent_assoc_slope = 0.5,
                  betaEvent_assoc_auc = 0.5,
                  betaEvent_aux = 1.2,
                  b_sd = c(1.5, 0.07), 
                  b_rho = -0.2,
                  prob_Z1 = 0.5,
                  mean_Z2 = 0, 
                  sd_Z2 = 1,
                  max_yobs = 10,
                  max_fuptime = 20,
                  balanced = FALSE,
                  family = "lognormal",
                  clust_control = list(),
                  return_eta = FALSE,
                  grp_assoc = NULL,
                  seed = sample.int(.Machine$integer.max, 1),
                  interval = c(1E-8, 200)) {
  
  #----- Preliminaries
  set.seed(seed)
  basehaz <- match.arg(basehaz)
  
  if (max_yobs < 1)
    stop("'max_yobs' must be at least 1.")
  
  # Handle family parameter: broadcast to a list of length M.
  if (!is.list(family)) {
    if (identical(family, "lognormal")) {
      # Create lognormal family directly
      family_obj <- structure(
        list(
          family = "lognormal",
          link = "log",
          linkinv = exp,
          variance = function(mu) rep(1, length(mu)),
          dev.resids = function(y, mu, wt) 2 * wt * (log(y) - mu)^2,
          aic = function(y, n, mu, wt, dev) NA,
          initialize = expression({
            if (any(y <= 0))
              stop("Non-positive values not allowed for lognormal family", call. = FALSE)
            n <- rep(1, nobs)
            mustart <- log(y)
          })
        ),
        class = "family"
      )
      family <- replicate(M, family_obj, simplify = FALSE)
    } else {
      family <- replicate(M, validate_family(family), simplify = FALSE)
    }
  } else if (length(family) != M) {
    stop("If 'family' is a list, it must have length equal to M")
  } else {
    family <- lapply(family, validate_family)
  }
  
  # Valid trajectories and associations (for checking)
  ok_trajs  <- c("none", "linear", "quadratic", "cubic", "mrd_piecewise")
  ok_assocs <- c("etavalue", "etaslope", "etaauc", "muvalue",
                 "null", "shared_b(1)", "shared_coef(1)",
                 "shared_b(2)", "shared_coef(2)")
  
  assoc <- maybe_broadcast(assoc, M)
  fixed_trajectory  <- maybe_broadcast(fixed_trajectory, M)
  random_trajectory <- maybe_broadcast(random_trajectory, M)
  
  # Broadcast standard longitudinal parameters
  betaLong_intercept  <- maybe_broadcast(betaLong_intercept, M)
  betaLong_binary     <- maybe_broadcast(betaLong_binary, M)
  betaLong_continuous <- maybe_broadcast(betaLong_continuous, M)
  
  # Broadcast MRD timing & relapse parameters
  betaLong_mrd_eot <- maybe_broadcast(betaLong_mrd_eot, M)
  betaLong_mrd_relapse_prob <- maybe_broadcast(betaLong_mrd_relapse_prob, M)
  betaLong_mrd_relapse_time_mean <- maybe_broadcast(betaLong_mrd_relapse_time_mean, M)
  betaLong_mrd_relapse_time_sd <- maybe_broadcast(betaLong_mrd_relapse_time_sd, M)
  betaLong_mrd_relapse_rate <- maybe_broadcast(betaLong_mrd_relapse_rate, M)
  
  # Broadcast new baseline and plateau parameters
  betaLong_baseline_mean <- maybe_broadcast(betaLong_baseline_mean, M)
  betaLong_baseline_sd   <- maybe_broadcast(betaLong_baseline_sd, M)
  betaLong_plateau_mean  <- maybe_broadcast(betaLong_plateau_mean, M)
  betaLong_plateau_sd    <- maybe_broadcast(betaLong_plateau_sd, M)
  betaLong_plateau_binary <- maybe_broadcast(betaLong_plateau_binary, M)
  betaLong_plateau_continuous <- maybe_broadcast(betaLong_plateau_continuous, M)
  
  # Broadcast event association parameters
  betaEvent_assoc_value <- maybe_broadcast(betaEvent_assoc_value, M)
  betaEvent_assoc_slope <- maybe_broadcast(betaEvent_assoc_slope, M)
  betaEvent_assoc_auc   <- maybe_broadcast(betaEvent_assoc_auc, M)
  
  # Generate baseline covariates
  Z1 <- stats::rbinom(n, 1, prob_Z1)
  Z2 <- stats::rnorm(n, mean_Z2, sd_Z2)
  covs <- data.frame(id = 1:n, Z1, Z2)
  
  # Initialize betas list
  betas <- list()
  
  # Construct data frames of parameters for each marker
  for (m in 1:M) {
    nm <- paste0("Long", m)
    betas[[nm]] <- data.frame(id = 1:n)
    
    # Common parameters for all trajectory types
    betas[[nm]][[paste0("betaLong_intercept", m)]] <- rep(betaLong_intercept[m], n)
    betas[[nm]][[paste0("betaLong_binary", m)]] <- rep(betaLong_binary[m], n)
    betas[[nm]][[paste0("betaLong_continuous", m)]] <- rep(betaLong_continuous[m], n)
    
    if (fixed_trajectory[m] == "mrd_piecewise") {
      # Store MRD timing & relapse parameters per subject
      betas[[nm]][[paste0("betaLong_mrd_eot", m)]] <- rep(betaLong_mrd_eot[m], n)
      betas[[nm]][[paste0("betaLong_mrd_relapse_rate", m)]] <- rep(betaLong_mrd_relapse_rate[m], n)
      
      # Generate relapse times (subject-level) from a probability and normal distribution.
      relapse_indicators <- rbinom(n, 1, betaLong_mrd_relapse_prob[m])
      relapse_times <- rep(NA_real_, n)
      relapse_times[relapse_indicators == 1] <- 
        rnorm(sum(relapse_indicators), betaLong_mrd_relapse_time_mean[m], betaLong_mrd_relapse_time_sd[m])
      # Here the column is called "relapse_time_<m>"
      betas[[nm]][[paste0("relapse_time_", m)]] <- relapse_times
      
      # Store the new distribution parameters (these are subject-level parameters, but here stored per observation)
      betas[[nm]][[paste0("betaLong_baseline_mean", m)]] <- rep(betaLong_baseline_mean[m], n)
      betas[[nm]][[paste0("betaLong_baseline_sd", m)]] <- rep(betaLong_baseline_sd[m], n)
      betas[[nm]][[paste0("betaLong_plateau_mean", m)]] <- rep(betaLong_plateau_mean[m], n)
      betas[[nm]][[paste0("betaLong_plateau_sd", m)]] <- rep(betaLong_plateau_sd[m], n)
      betas[[nm]][[paste0("betaLong_plateau_binary", m)]] <- rep(betaLong_plateau_binary[m], n)
      betas[[nm]][[paste0("betaLong_plateau_continuous", m)]] <- rep(betaLong_plateau_continuous[m], n)
    }
  }
  
  # Event submodel parameters
  betas[["Event"]] <- data.frame(
    id = 1:n,
    betaEvent_intercept = rep(betaEvent_intercept, n),
    betaEvent_binary = rep(betaEvent_binary, n),
    betaEvent_continuous = rep(betaEvent_continuous, n),
    betaEvent_aux = rep(betaEvent_aux, n)
  )
  for (m in 1:M) {
    betas[["Event"]][[paste0("betaEvent_assoc_value", m)]] <- rep(betaEvent_assoc_value[m], n)
    betas[["Event"]][[paste0("betaEvent_assoc_slope", m)]] <- rep(betaEvent_assoc_slope[m], n)
    betas[["Event"]][[paste0("betaEvent_assoc_auc", m)]]   <- rep(betaEvent_assoc_auc[m], n)
  }
  
  # Generate survival times using simsurv
  ss <- simsurv::simsurv(
    hazard = jm_hazard,
    x = covs,
    betas = betas,
    idvar = "id",
    ids = covs$id,
    maxt = max_fuptime,
    interval = interval,
    basehaz = basehaz,
    M = M,
    trajectory = fixed_trajectory,
    family = family,
    grp_assoc = grp_assoc
  )
  
  # Generate longitudinal measurements
  dat <- list(Event = data.frame(betas[["Event"]], covs, ss))
  
  for (m in 1:M) {
    nm <- paste0("Long", m)
    dat[[nm]] <- merge(betas[[nm]], dat[["Event"]])
    dat[[nm]] <- merge(dat[[nm]], covs)
    
    # Store original data before replication
    orig_data <- dat[[nm]]
    
    # Create time points data frame
    time_data <- data.frame(
      id = rep(orig_data$id, each = max_yobs)
    )
    
    # Generate unique time points for each subject
    eot_time <- betaLong_mrd_eot[m]
    time_data$tij <- NA_real_
    
    # Generate time points subject by subject
    for (i in unique(time_data$id)) {
      # Create intermediate points between 0 and max_fuptime
      n_intermediate <- max_yobs - 2  # Space for 0 and eot_time
      intermediates <- sort(runif(n_intermediate, 0.2, max_fuptime - 0.2))
      
      # Combine with fixed points and sort
      subject_times <- sort(c(0, intermediates, eot_time))
      
      # Assign to the correct rows
      time_data$tij[time_data$id == i] <- subject_times
    }
    
    # Now replicate the original data to match the time points
    dat[[nm]] <- orig_data[rep(seq_len(nrow(orig_data)), each = max_yobs), ]
    rownames(dat[[nm]]) <- NULL
    
    # Add the time points
    dat[[nm]]$tij <- time_data$tij
    
    # Sort by id and time
    dat[[nm]] <- dat[[nm]][order(dat[[nm]]$id, dat[[nm]]$tij), ]
    
    if (fixed_trajectory[m] == "mrd_piecewise") {
      # Generate subject-level parameters once per subject.
      subj_params <- data.frame(id = covs$id, Z1 = covs$Z1, Z2 = covs$Z2)
      subj_params$baseline <- stats::rlnorm(n, 
                                            meanlog = betaLong_baseline_mean[m],
                                            sdlog = betaLong_baseline_sd[m])
      subj_params$plateau <- stats::rlnorm(n,
                                           meanlog = betaLong_plateau_mean[m] +
                                             betaLong_plateau_binary[m] * subj_params$Z1 +
                                             betaLong_plateau_continuous[m] * subj_params$Z2,
                                           sdlog = betaLong_plateau_sd[m])
      subj_params$plateau <- pmin(subj_params$plateau, subj_params$baseline)
      subj_params$decay_rate <- -log(subj_params$plateau / subj_params$baseline) / betaLong_mrd_eot[m]
      
      # Generate relapse times (subject-level) from a probability and normal distribution.
      relapse_indicators <- rbinom(n, 1, betaLong_mrd_relapse_prob[m])
      relapse_times <- rep(NA_real_, n)
      relapse_times[relapse_indicators == 1] <- 
        rnorm(sum(relapse_indicators), betaLong_mrd_relapse_time_mean[m], betaLong_mrd_relapse_time_sd[m])
      
      # Add relapse information to subject parameters
      subj_params$has_relapse <- relapse_indicators
      subj_params$subj_relapse_time <- relapse_times
      
      # Store original relapse-related parameters
      subj_params$relapse_time <- relapse_times
      
      # Merge subject-level parameters into the longitudinal data.
      dat[[nm]] <- merge(dat[[nm]], subj_params, by = "id", all.x = TRUE)
      
      # Define effective relapse time
      dat[[nm]]$effective_relapse_time <- with(dat[[nm]],
                                               ifelse(is.na(subj_relapse_time) | 
                                                        subj_relapse_time <= get(paste0("betaLong_mrd_eot", m)),
                                                      Inf, subj_relapse_time))
      
      # Compute the noiseless trajectory Xij using subject-level parameters.
      dat[[nm]][[paste0("Xij_", m)]] <- with(dat[[nm]], {
        ifelse(tij <= get(paste0("betaLong_mrd_eot", m)),
               baseline * exp(-decay_rate * tij),
               ifelse(tij <= effective_relapse_time,
                      plateau,
                      plateau * exp(betaLong_mrd_relapse_rate[m] * 
                                      (tij - effective_relapse_time))
               ))
      })
      
      # Store subject parameters for later use in event data
      assign(paste0("subj_params_", m), subj_params)
      
    } else {
      # Handle other trajectory types (linear, quadratic, cubic)
      dat[[nm]][[paste0("Xij_", m)]] <- 
        dat[[nm]][[paste0("betaLong_intercept", m)]] +
        dat[[nm]][[paste0("betaLong_binary", m)]] * dat[[nm]]$Z1 +
        dat[[nm]][[paste0("betaLong_continuous", m)]] * dat[[nm]]$Z2
      
      if (fixed_trajectory[m] %in% c("linear", "quadratic", "cubic")) {
        dat[[nm]][[paste0("Xij_", m)]] <- dat[[nm]][[paste0("Xij_", m)]] +
          dat[[nm]][[paste0("betaLong_linear", m)]] * dat[[nm]]$tij
      }
      if (fixed_trajectory[m] %in% c("quadratic", "cubic")) {
        dat[[nm]][[paste0("Xij_", m)]] <- dat[[nm]][[paste0("Xij_", m)]] +
          dat[[nm]][[paste0("betaLong_quadratic", m)]] * dat[[nm]]$tij^2
      }
      if (fixed_trajectory[m] %in% c("cubic")) {
        dat[[nm]][[paste0("Xij_", m)]] <- dat[[nm]][[paste0("Xij_", m)]] +
          dat[[nm]][[paste0("betaLong_cubic", m)]] * dat[[nm]]$tij^3
      }
    }  
    
    # Generate observed values Yij from the noiseless Xij using the specified family.
    fam <- family[[m]]$family
    invlink <- family[[m]]$linkinv
    mu <- invlink(dat[[nm]][[paste0("Xij_", m)]])
    
    if (fam == "gaussian") {
      sigma <- betaLong_aux[m]
      dat[[nm]][[paste0("Yij_", m)]] <- stats::rnorm(length(mu), mu, sigma)
    } else if (fam == "binomial") {
      trials <- betaLong_aux[m]
      dat[[nm]][[paste0("Yij_", m)]] <- stats::rbinom(length(mu), trials, mu)
    } else if (fam == "lognormal") {
      mu <- dat[[nm]][[paste0("Xij_", m)]]
      sigma <- betaLong_aux[m]
      dat[[nm]][[paste0("Yij_", m)]] <- stats::rlnorm(length(mu),
                                                      meanlog = log(mu),
                                                      sdlog = sigma)
    }
    
    # Introduce missing data after generating Yij values
    if (missing_data_rate > 0) {
      n_obs <- nrow(dat[[nm]])
      missing_indicators <- stats::rbinom(n_obs, 1, missing_data_rate)
      dat[[nm]][[paste0("Yij_", m)]][missing_indicators == 1] <- NA
    }
  }
  
  ret <- lapply(dat, function(x) {
    if ("tij" %in% colnames(x))
      x <- x[x$tij <= x$eventtime, ]
    
    # For Event data, add relapse information
    if (all(c("eventtime", "status") %in% colnames(x))) {
      # This is the Event data frame
      for (m in 1:M) {
        subj_params <- get(paste0("subj_params_", m))
        x <- merge(x, subj_params[, c("id", "has_relapse", "subj_relapse_time")], by = "id")
        # Rename columns to include marker number
        names(x)[names(x) == "has_relapse"] <- paste0("has_relapse_", m)
        names(x)[names(x) == "subj_relapse_time"] <- paste0("relapse_time_", m)
      }
    }
    
    sel <- grep("^id$|^clust|^Z|^tij|^Yij|^Xij|eventtime|status|has_relapse|relapse_time", colnames(x))
    x <- x[, sel, drop = FALSE]
    rownames(x) <- NULL
    return(x)
  })
  
  commonids <- ret[[1L]]$id
  for (i in 1:length(ret))
    commonids <- intersect(commonids, ret[[i]]$id)
  ret <- lapply(ret, function(x) x[x$id %in% commonids, , drop = FALSE])
  
  structure(ret,
            params = list(
              betaLong_intercept = betaLong_intercept,
              betaLong_binary = betaLong_binary,
              betaLong_continuous = betaLong_continuous,
              betaLong_aux = betaLong_aux,
              betaEvent_intercept = betaEvent_intercept,
              betaEvent_binary = betaEvent_binary,
              betaEvent_continuous = betaEvent_continuous,
              betaEvent_assoc_value = betaEvent_assoc_value,
              betaEvent_assoc_slope = betaEvent_assoc_slope,
              betaEvent_assoc_auc = betaEvent_assoc_auc,
              betaEvent_aux = betaEvent_aux
            ),
            n = length(unique(ret$Event$id)),
            M = M,
            max_yobs = max_yobs,
            max_fuptime = max_fuptime,
            balanced = balanced,
            family = family,
            fixed_trajectory = fixed_trajectory,
            random_trajectory = random_trajectory,
            return_eta = return_eta,
            clust_control = clust_control,
            seed = seed,
            class = c("simjm", class(ret)))
}

#' Plot All Trajectories
#'
#' Creates a plot of MRD trajectories for all groups
#' @param sim_data_list List of simulated data for each group
#' @param show_true Logical; whether to show true trajectories
#' @return A ggplot2 object
#' @export
plot_all_trajectories <- function(sim_data_list, show_true = FALSE) {
  # Combine data from all groups
  all_data <- data.frame()
  for (group in names(sim_data_list)) {
    group_data <- sim_data_list[[group]]$longitudinal
    group_data$group <- group
    all_data <- rbind(all_data, group_data)
  }
  
  # Create base plot
  p <- ggplot2::ggplot(all_data, 
                       ggplot2::aes(x = time, y = log10(mrd), 
                                   group = interaction(subject_id, group),
                                   color = group))
  
  # Add lines connecting non-missing points
  p <- p + ggplot2::geom_line(data = subset(all_data, !is.na(mrd)), alpha = 0.5)
  
  # Add points for observed values
  p <- p + ggplot2::geom_point(data = subset(all_data, !is.na(mrd)), size = 1)
  
  # Add theme and labels
  p <- p + ggplot2::theme_bw() +
    ggplot2::labs(x = "Time", y = "log10(MRD)")
  
  if (show_true) {
    p <- p + ggplot2::geom_smooth(ggplot2::aes(group = group), 
                                 method = "loess", se = FALSE)
  }
  
  p
}

#' Plot Trajectories
#'
#' Creates a plot of MRD trajectories
#' @param sim_data Simulated data list
#' @param show_true Logical; whether to show true trajectories
#' @return A ggplot2 object
#' @export
plot_trajectories <- function(sim_data, show_true = FALSE) {
  long_data <- sim_data$longitudinal
  
  # Create base plot
  p <- ggplot2::ggplot(long_data, ggplot2::aes(x = time, y = log10(mrd), group = subject_id))
  
  # Add lines connecting non-missing points
  p <- p + ggplot2::geom_line(data = subset(long_data, !is.na(mrd)), alpha = 0.5)
  
  # Add points for observed values
  p <- p + ggplot2::geom_point(data = subset(long_data, !is.na(mrd)), size = 1)
  
  # Add theme and labels
  p <- p + ggplot2::theme_bw() +
    ggplot2::labs(x = "Time", y = "log10(MRD)")
  
  if (show_true) {
    p <- p + ggplot2::geom_smooth(method = "loess", se = FALSE, color = "red")
  }
  
  p
}

#' Plot All Survival
#'
#' Creates survival plots for all groups
#' @param sim_data_list List of simulated data for each group
#' @param show_ci Logical; whether to show confidence intervals
#' @return A ggplot2 object
#' @export
plot_all_survival <- function(sim_data_list, show_ci = TRUE) {
  # Combine event data from all groups
  all_data <- data.frame()
  for (group in names(sim_data_list)) {
    group_data <- sim_data_list[[group]]$event
    group_data$group <- group
    all_data <- rbind(all_data, group_data)
  }
  
  # Fit Kaplan-Meier by group
  surv_obj <- survival::Surv(all_data$eventtime, all_data$status)
  km_fit <- survival::survfit(surv_obj ~ group, data = all_data)
  
  # Create plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_step(
      data = data.frame(
        time = km_fit$time,
        surv = km_fit$surv,
        group = rep(names(sim_data_list), km_fit$strata)
      ),
      ggplot2::aes(x = time, y = surv, color = group)
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Time", y = "Survival Probability")
  
  if (show_ci) {
    ci_data <- data.frame(
      time = km_fit$time,
      lower = km_fit$lower,
      upper = km_fit$upper,
      group = rep(names(sim_data_list), km_fit$strata)
    )
    p <- p + ggplot2::geom_ribbon(
      data = ci_data,
      ggplot2::aes(x = time, ymin = lower, ymax = upper, fill = group),
      alpha = 0.2
    )
  }
  
  p
}

#' Plot Survival
#'
#' Creates a survival plot
#' @param sim_data Simulated data list
#' @param show_ci Logical; whether to show confidence intervals
#' @return A ggplot2 object
#' @export
plot_survival <- function(sim_data, show_ci = TRUE) {
  event_data <- sim_data$event
  
  # Create survival object
  surv_obj <- survival::Surv(event_data$eventtime, event_data$status)
  
  # Fit Kaplan-Meier
  km_fit <- survival::survfit(surv_obj ~ 1)
  
  # Create plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_step(
      data = data.frame(
        time = km_fit$time,
        surv = km_fit$surv
      ),
      ggplot2::aes(x = time, y = surv)
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Time", y = "Survival Probability")
  
  if (show_ci) {
    ci_data <- data.frame(
      time = km_fit$time,
      lower = km_fit$lower,
      upper = km_fit$upper
    )
    p <- p + ggplot2::geom_ribbon(
      data = ci_data,
      ggplot2::aes(x = time, ymin = lower, ymax = upper),
      alpha = 0.2
    )
  }
  
  p
}

#' Summarize Trajectories
#'
#' Creates a summary of MRD trajectories
#' @param sim_data Simulated data list
#' @return A summary object
#' @export
summarize_trajectories <- function(sim_data) {
  long_data <- sim_data$longitudinal
  
  # Calculate summary statistics
  summary_stats <- list(
    n_subjects = length(unique(long_data$subject_id)),
    n_observations = nrow(long_data),
    mean_mrd = mean(log10(long_data$mrd)),
    sd_mrd = sd(log10(long_data$mrd)),
    median_mrd = median(log10(long_data$mrd)),
    time_range = range(long_data$time)
  )
  
  summary_stats
}

#' Summarize Events
#'
#' Creates a summary of survival events
#' @param sim_data Simulated data list
#' @return A summary object
#' @export
summarize_events <- function(sim_data) {
  event_data <- sim_data$event
  
  # Calculate summary statistics
  summary_stats <- list(
    n_subjects = nrow(event_data),
    n_events = sum(event_data$status),
    median_time = median(event_data$eventtime),
    event_rate = mean(event_data$status),
    time_range = range(event_data$eventtime)
  )
  
  summary_stats
}

#' Calculate Hazard for Joint Model
#'
#' Calculates the hazard function for the joint model
#' @param t Time point
#' @param x Covariates
#' @param betas Model parameters
#' @param basehaz Baseline hazard type
#' @param M Number of markers
#' @param trajectory Trajectory type
#' @param family Distribution family
#' @param grp_assoc Group association structure
#' @return Calculated hazard value
#' @keywords internal
jm_hazard <- function(t, x, betas, basehaz = "weibull", M = 1,
                      trajectory = "mrd_piecewise",
                      family = list("lognormal"),
                      grp_assoc = NULL) {
  if (t == 0) return(0)
  
  # Compute the baseline hazard
  if (basehaz == "weibull") {
    # Using betaEvent_aux as the shape parameter
    shape <- betas[["Event"]][["betaEvent_aux"]]
    h0 <- shape * (t^(shape - 1))
  }
  
  # Build the event linear predictor from event covariates
  etaevent <- betas[["Event"]][["betaEvent_intercept"]] +
    betas[["Event"]][["betaEvent_binary"]] * x[["Z1"]] +
    betas[["Event"]][["betaEvent_continuous"]] * x[["Z2"]]
  
  # Loop over each longitudinal marker
  for (m in 1:M) {
    nm <- paste0("Long", m)
    
    if (trajectory[m] == "mrd_piecewise") {
      # Compute the baseline MRD value from the lognormal parameters (no covariate effects)
      baseline_m <- exp(betas[[nm]][[paste0("betaLong_baseline_mean", m)]])
      
      # Compute the plateau MRD value from the lognormal parameters with covariate adjustments
      plateau_m <- exp(betas[[nm]][[paste0("betaLong_plateau_mean", m)]] +
                         betas[[nm]][[paste0("betaLong_plateau_binary", m)]] * x[["Z1"]] +
                         betas[[nm]][[paste0("betaLong_plateau_continuous", m)]] * x[["Z2"]])
      
      # End-of-treatment time (EoT)
      eot_time <- betas[[nm]][[paste0("betaLong_mrd_eot", m)]]
      
      # Compute the decay rate bridging baseline to plateau
      decay_rate <- -log(plateau_m / baseline_m) / eot_time
      
      # Retrieve relapse parameters (if available)
      relapse_time <- betas[[nm]][[paste0("relapse_time_", m)]]
      relapse_rate <- betas[[nm]][[paste0("betaLong_mrd_relapse_rate", m)]]
      
      # Compute the longitudinal summary for marker m at time t
      if (t <= eot_time) {
        # Decay phase
        etavalue_m <- baseline_m * exp(-decay_rate * t)
        etaslope_m <- -decay_rate * baseline_m * exp(-decay_rate * t)
        etaauc_m   <- baseline_m * (1 - exp(-decay_rate * t)) / decay_rate
      } else if (is.na(relapse_time) || t <= relapse_time) {
        # Plateau phase
        etavalue_m <- plateau_m
        etaslope_m <- 0
        decay_integral <- baseline_m * (1 - exp(-decay_rate * eot_time)) / decay_rate
        etaauc_m <- decay_integral + plateau_m * (t - eot_time)
      } else {
        # Relapse phase
        etavalue_m <- plateau_m * exp(relapse_rate * (t - relapse_time))
        etaslope_m <- relapse_rate * plateau_m * exp(relapse_rate * (t - relapse_time))
        decay_integral <- baseline_m * (1 - exp(-decay_rate * eot_time)) / decay_rate
        plateau_integral <- plateau_m * (relapse_time - eot_time)
        relapse_integral <- plateau_m * (exp(relapse_rate * (t - relapse_time)) - 1) / relapse_rate
        etaauc_m <- decay_integral + plateau_integral + relapse_integral
      }
      
      # Add contributions from the current value, slope, and AUC
      res_value_m <- collapse_across_clusters(etavalue_m, grp_assoc)
      res_slope_m <- collapse_across_clusters(etaslope_m, grp_assoc)
      res_auc_m   <- collapse_across_clusters(etaauc_m, grp_assoc)
      
      etaevent <- etaevent +
        betas[["Event"]][[paste0("betaEvent_assoc_value", m)]] * res_value_m +
        betas[["Event"]][[paste0("betaEvent_assoc_slope", m)]] * res_slope_m +
        betas[["Event"]][[paste0("betaEvent_assoc_auc", m)]]   * res_auc_m
      
    } else {
      # For other trajectories (linear, quadratic, cubic)
      etabaseline_m <- betas[[nm]][[paste0("betaLong_intercept", m)]] +
        betas[[nm]][[paste0("betaLong_binary", m)]] * x[["Z1"]] +
        betas[[nm]][[paste0("betaLong_continuous", m)]] * x[["Z2"]]
      
      if (trajectory[m] %in% c("linear", "quadratic", "cubic")) {
        etavalue_m <- etabaseline_m + betas[[nm]][[paste0("betaLong_linear", m)]] * t
      }
      if (trajectory[m] %in% c("quadratic", "cubic")) {
        etavalue_m <- etavalue_m + betas[[nm]][[paste0("betaLong_quadratic", m)]] * t^2
      }
      if (trajectory[m] %in% c("cubic")) {
        etavalue_m <- etavalue_m + betas[[nm]][[paste0("betaLong_cubic", m)]] * t^3
      }
      
      res_value_m <- collapse_across_clusters(etavalue_m, grp_assoc)
      etaevent <- etaevent +
        betas[["Event"]][[paste0("betaEvent_assoc", m)]] * res_value_m
    }
  }
  
  # Final hazard: multiply the baseline hazard by the exponential of the event linear predictor
  h <- h0 * exp(etaevent)
  
  if (!length(h) == 1) {
    stop("Bug found: returned hazard should be a scalar.")
  }
  return(h)
}

#' Collapse Across Clusters
#'
#' Helper function to collapse values across clusters
#' @param x Values to collapse
#' @param collapse_fun Function to use for collapsing
#' @return Collapsed values
#' @keywords internal
collapse_across_clusters <- function(x, collapse_fun = NULL) {
  if (is.null(collapse_fun)) {
    return(x)
  } else {
    fn <- match.fun(collapse_fun)
    return(fn(x))
  }
}

#' Verify Distribution of Time Points
#'
#' Verifies that time points are properly distributed
#' @param tij Time points
#' @param ids Subject IDs
#' @param eot_time End of treatment time
#' @return TRUE if distribution is valid
#' @keywords internal
verify_distribution <- function(tij, ids, eot_time) {
  for (id in unique(ids)) {
    subj_times <- tij[ids == id]
    if (length(unique(round(subj_times, 8))) != length(subj_times)) {
      stop2(sprintf("Subject %d has duplicate time points", id))
    }
    if (!0 %in% subj_times) {
      stop2(sprintf("Subject %d missing baseline measurement", id))
    }
    if (!eot_time %in% subj_times) {
      stop2(sprintf("Subject %d missing EoT measurement", id))
    }
  }
  return(TRUE)
}