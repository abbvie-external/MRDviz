test_that("export_json creates correct JSON structure", {
  # Create sample longitudinal data
  long_data <- data.frame(
    subject_id = c(1, 1, 2, 2),
    time = c(0, 1, 0, 1),
    measurement = c(-0.5, -0.3, 0.2, 0.4),
    Response = c("PD", "SD", "PR", "CR"),
    MRD_negativity = c("MRD+", "MRD+", "MRD-", "MRD-"),
    stringsAsFactors = FALSE
  )
  
  # Create sample event data
  event_data <- data.frame(
    subject_id = c(1, 2),
    OS = c("Event", "Censor"),
    PFS = c("Event", "Censor"),
    sex = c("M", "F"),
    eventtime_OS = c(1.5, 6.0),
    eventtime_PFS = c(1.2, 5.8),
    stringsAsFactors = FALSE
  )
  
  # Create temporary file for testing
  temp_file <- tempfile(fileext = ".json")
  
  # Export data
  result <- export_json(long_data, event_data, temp_file)
  
  # Read the created JSON file
  json_data <- jsonlite::read_json(temp_file)
  
  # Test structure
  expect_equal(json_data$xlab, "Time")
  expect_equal(json_data$ylab, "Measurement")
  
  # Test covariates structure
  expect_true("OS" %in% names(json_data$covariates))
  expect_true("PFS" %in% names(json_data$covariates))
  expect_true("sex" %in% names(json_data$covariates))
  
  # Test time_variant_covariates structure
  expect_true("Response" %in% names(json_data$time_variant_covariates))
  expect_true("MRD_negativity" %in% names(json_data$time_variant_covariates))
  
  # Test subjects data
  expect_equal(length(json_data$subjects), 2)
  expect_equal(json_data$subjects[[1]]$id, "1")
  expect_equal(json_data$subjects[[2]]$id, "2")
  
  # Clean up
  unlink(temp_file)
})

test_that("export_json validates input data frames", {
  # Test with non-data frame inputs
  expect_error(
    export_json(list(), data.frame()),
    "Both longitudinal_data and event_data must be data frames"
  )
  
  # Test with missing subject_id column
  invalid_long <- data.frame(time = 1, measurement = 0.5)
  invalid_event <- data.frame(OS = "Event")
  
  expect_error(
    export_json(invalid_long, invalid_event),
    "Both longitudinal_data and event_data must contain a 'subject_id' column"
  )
  
  # Test with missing required columns in longitudinal data
  invalid_long <- data.frame(
    subject_id = 1,
    measurement = 0.5
  )
  valid_event <- data.frame(
    subject_id = 1,
    OS = "Event"
  )
  
  expect_error(
    export_json(invalid_long, valid_event),
    "Longitudinal data must contain 'subject_id', 'time', and 'measurement' columns"
  )
})