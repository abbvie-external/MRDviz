# MRDviz JSON Format Guide

## Introduction

The MRDviz JSON format is designed to store longitudinal measurement data along with subject-level covariates for visualization in the MRDviz application. This guide explains the JSON structure, data requirements, and how to generate JSON files from Excel or CSV data using the `export_json` functions.

## ⚠️ Important: Data Privacy and De-identification

MRDviz is designed for use with research data. Before converting your data, you are responsible for ensuring that it has been properly **de-identified** to protect patient privacy and comply with all relevant data protection regulations (such as HIPAA).

- **Do not use real patient identifiers.** The `subject_id` field must contain only **pseudo-anonymous study identifiers** (e.g., "PT_001", "Subject_A", "Case_123") that cannot be used to trace back to the individual.
- **Remove all PII.** Ensure your dataset does not contain any personally identifiable information, such as names, full dates of birth, medical record numbers, or addresses.

The responsibility for ensuring data is appropriately de-identified rests solely with the user.

## Getting Started

MRDviz provides two main functions for creating JSON files:

1. **`convert_to_json()`** - Converts data directly from CSV or Excel files
2. **`export_json()`** - Converts data from R data frames

Both functions generate the same JSON structure compatible with MRDviz visualization.

## JSON Structure Overview

The MRDviz JSON format consists of five main components:

```json
{
  "title": "MRD trajectories",
  "xlab": "Time", 
  "ylab": "Measurement",
  "covariates": { ... },
  "time_variant_covariates": { ... },
  "subjects": [ ... ]
}
```

### Core Components

- **`title`**: Display title for the visualization
- **`xlab`**: X-axis label (typically "Time")
- **`ylab`**: Y-axis label (typically "Measurement")
- **`covariates`**: Static subject-level covariates for grouping and coloring
- **`time_variant_covariates`**: Time-varying covariates that change over time
- **`subjects`**: Array of individual subject data

## JSON Schema Diagram

This diagram outlines the required structure, data types, and nesting for a valid `MRDviz` JSON file.

```
MRDviz_schema.json (Root Object)
├── title: String
├── xlab: String
├── ylab: String
├── covariates: Object
│   └── [covariate_name]: Object
│       └── [category_name]: Array
├── time_variant_covariates: Object
│   └── [tv_covariate_name]: Object
│       └── [category_name]: Array
└── subjects: [ ... ] (Array of Subject Objects)
    └── Subject Object
        ├── id: String (Unique subject identifier)
        ├── covariates: Object  // Static baseline data for this subject
        │   └── [covariate_name]: (String | Number)
        └── data: [ ... ] (Array of Measurement Objects)
            └── Measurement Object
                ├── time: Number
                ├── value: Number // The primary longitudinal measurement
                └── [tv_covariate_name]: (String | Number)
```

## Data Requirements

### Longitudinal Data (Trajectory Data)

Your longitudinal data must contain these **required columns**:

| Column | Type | Description |
|--------|------|-------------|
| `subject_id` | Character/Numeric | Unique identifier for each subject |
| `time` | Numeric | Time point of measurement |
| `measurement` | Numeric | The measured value (e.g., MRD level) |

**Optional columns** will be treated as time-variant covariates:
- Any additional columns in the longitudinal data
- Categorical variables (≤10 unique values) will be used for color coding
- Continuous variables will be included but not used for visualization grouping

### Event Data (Subject-Level Data)

Your event data must contain:

| Column | Type | Description |
|--------|------|-------------|
| `subject_id` | Character/Numeric | Must match longitudinal data subject IDs |

**Optional columns** will be treated as static covariates:
- Survival outcomes (e.g., `OS`, `PFS`)
- Event times (e.g., `eventtime_OS`, `eventtime_PFS`)
- Baseline characteristics (e.g., `age`, `sex`, `treatment_group`)
- Any other subject-level variables

### Example Data Structure

#### Longitudinal Data (Excel/CSV)
```
subject_id | time | measurement | Response | MRD_negativity
-----------|------|-------------|----------|---------------
CR_001     | 0    | 18.36       | CR       | MRD+
CR_001     | 2    | 0.17        | CR       | MRD-
CR_001     | 4    | 0.09        | CR       | MRD-
CR_002     | 0    | 7.10        | CR       | MRD+
CR_002     | 2    | 0.08        | CR       | MRD-
```

#### Event Data (Excel/CSV)
```
subject_id | OS     | eventtime_OS | PFS    | eventtime_PFS | sex | age
-----------|--------|--------------|--------|---------------|-----|----
CR_001     | Censor | 10           | Censor | 10            | M   | 65
CR_002     | Censor | 10           | Event  | 8.5           | F   | 58
```

## Using the Export Functions

### Method 1: From Files (`convert_to_json`)

Convert directly from CSV or Excel files:

```r
library(MRDviz)

# From CSV files
convert_to_json(
  longitudinal_file = "trajectory_data.csv",
  event_file = "subject_data.csv", 
  output_file = "mrdviz_data.json"
)

# From Excel files  
convert_to_json(
  longitudinal_file = "trajectory_data.xlsx",
  event_file = "subject_data.xlsx",
  output_file = "mrdviz_data.json"
)
```

### Method 2: From Data Frames (`export_json`)

Convert from R data frames:

```r
library(MRDviz)

# Create or load your data frames
longitudinal_data <- data.frame(
  subject_id = rep(1:3, each = 4),
  time = rep(c(0, 1, 2, 3), 3),
  measurement = rnorm(12),
  Response = rep(c("CR", "PR", "SD"), each = 4),
  MRD_negativity = sample(c("MRD+", "MRD-"), 12, replace = TRUE)
)

event_data <- data.frame(
  subject_id = 1:3,
  OS = c("Event", "Censor", "Event"),
  eventtime_OS = c(2.5, 5.0, 1.8),
  sex = c("M", "F", "M")
)

# Export to JSON
export_json(longitudinal_data, event_data, "mrdviz_data.json")
```

## Data Processing Rules

### Categorical vs. Continuous Variables

The export functions automatically determine variable types:

**Categorical Variables** (used for color coding):
- Character or factor variables
- Numeric variables with ≤10 unique integer values
- Missing values converted to "NA"

**Continuous Variables** (included but not used for grouping):
- Numeric variables with >10 unique values
- Numeric variables with non-integer values

### Special Column Handling

- **Time columns**: `time` and `eventtime_*` columns are always treated as numeric
- **Excluded columns**: Columns starting with underscore (`_`) are ignored
- **Missing data**: Missing values in categorical variables become "NA"

### File Format Support

- **CSV files**: Standard comma-separated values
- **Excel files**: `.xlsx` and `.xls` formats (requires `openxlsx` package)
- **Automatic detection**: File format determined by extension

## JSON Output Structure Details

### Covariates Section

Static covariates create grouping categories:

```json
"covariates": {
  "OS": {
    "Event": [],
    "Censor": []
  },
  "sex": {
    "M": [],
    "F": []
  }
}
```

### Time-Variant Covariates Section

Variables that change over time:

```json
"time_variant_covariates": {
  "Response": {
    "CR": [],
    "PR": [],
    "SD": [],
    "PD": []
  },
  "MRD_negativity": {
    "MRD+": [],
    "MRD-": []
  }
}
```

### Subjects Section

Individual subject data with measurements and covariates:

```json
"subjects": [
  {
    "id": "CR_001",
    "covariates": {
      "eventtime_OS": 10,
      "OS": "Censor",
      "sex": "M"
    },
    "data": [
      {
        "time": 0,
        "value": 18.36,
        "Response": "CR",
        "MRD_negativity": "MRD+"
      },
      {
        "time": 2,
        "value": 0.17,
        "Response": "CR", 
        "MRD_negativity": "MRD-"
      }
    ]
  }
]
```

## Example Workflow

Here's a complete example using the test data included with MRDviz:

```r
library(MRDviz)

# Using the included test data
trajectory_file <- system.file("testdata", "MRDsim_traj_2025-02-23.xlsx", 
                              package = "MRDviz")
event_file <- system.file("testdata", "MRDsim_event_2025-02-23.xlsx", 
                         package = "MRDviz")

# Convert to JSON
convert_to_json(
  longitudinal_file = trajectory_file,
  event_file = event_file,
  output_file = "example_mrd_data.json"
)

# Load in MRDviz
run_mrdviz()
# Then upload the JSON file in the application
```

## Common Use Cases

### Clinical Trial Data

For clinical trial visualization:

```r
# Longitudinal measurements
clinical_data <- data.frame(
  subject_id = rep(paste0("PT_", 1:50), each = 6),
  time = rep(c(0, 1, 2, 3, 6, 12), 50),
  measurement = log10(abs(rnorm(300, mean = 1, sd = 2))),
  treatment_response = sample(c("CR", "PR", "SD", "PD"), 300, replace = TRUE),
  mrd_status = sample(c("Positive", "Negative"), 300, replace = TRUE)
)

# Subject characteristics  
subject_data <- data.frame(
  subject_id = paste0("PT_", 1:50),
  treatment_arm = sample(c("Control", "Experimental"), 50, replace = TRUE),
  age_group = sample(c("Young", "Old"), 50, replace = TRUE),
  overall_survival = sample(c("Event", "Censor"), 50, replace = TRUE),
  os_time = runif(50, 0.5, 24)
)

export_json(clinical_data, subject_data, "clinical_trial.json")
```

### Biomarker Studies

For biomarker trajectory analysis:

```r
# Biomarker measurements over time
biomarker_data <- data.frame(
  patient_id = rep(1:30, each = 8),
  visit_time = rep(c(0, 0.5, 1, 2, 3, 6, 9, 12), 30),
  biomarker_level = exp(rnorm(240, mean = 2, sd = 1)),
  response_category = sample(c("Responder", "Non-responder"), 240, replace = TRUE)
)

# Patient baseline data
baseline_data <- data.frame(
  patient_id = 1:30,
  progression_free_survival = sample(c("Event", "Censored"), 30, replace = TRUE),
  pfs_months = runif(30, 1, 18),
  histology = sample(c("Type A", "Type B", "Type C"), 30, replace = TRUE)
)

export_json(biomarker_data, baseline_data, "biomarker_study.json")
```

## Troubleshooting

### Common Issues

**File not found errors**:
```r
# Check file paths
file.exists("your_file.xlsx")
```

**Column name mismatches**:
```r
# Verify required columns exist
names(your_longitudinal_data)
# Must include: subject_id, time, measurement

names(your_event_data) 
# Must include: subject_id
```

**Package dependencies**:
```r
# For Excel files, install openxlsx
install.packages("openxlsx")
```

**Data type issues**:
```r
# Ensure numeric columns are properly formatted
your_data$time <- as.numeric(your_data$time)
your_data$measurement <- as.numeric(your_data$measurement)
```

### Validation

Check your JSON output:

```r
# Read and inspect the generated JSON
json_data <- jsonlite::fromJSON("your_output.json")
str(json_data)

# Verify subject count
length(json_data$subjects)

# Check covariate categories
names(json_data$covariates)
names(json_data$time_variant_covariates)
```

## Best Practices

1. **Consistent subject IDs**: Ensure subject IDs match exactly between longitudinal and event data
2. **Meaningful variable names**: Use descriptive column names that will appear in the visualization
3. **Appropriate data types**: Let the function auto-detect types, or manually convert if needed
4. **Missing data handling**: The function handles missing values automatically
5. **File organization**: Keep longitudinal and event data in separate files for clarity
6. **JSON validation**: Always check the generated JSON structure before using in MRDviz

## Advanced Usage

### Custom Processing

For more control over the conversion process:

```r
# Read data manually first
long_data <- read.csv("trajectories.csv")
event_data <- read.csv("events.csv")

# Custom preprocessing
long_data$log_measurement <- log10(long_data$measurement + 1)
event_data$age_group <- cut(event_data$age, breaks = c(0, 50, 70, 100), 
                           labels = c("Young", "Middle", "Old"))

# Then export
export_json(long_data, event_data, "custom_processed.json")
```

### Batch Processing

Process multiple datasets:

```r
datasets <- list(
  study1 = list(traj = "study1_traj.csv", events = "study1_events.csv"),
  study2 = list(traj = "study2_traj.csv", events = "study2_events.csv")
)

for (study_name in names(datasets)) {
  convert_to_json(
    longitudinal_file = datasets[[study_name]]$traj,
    event_file = datasets[[study_name]]$events,
    output_file = paste0(study_name, "_mrdviz.json")
  )
}
```

This comprehensive guide should help you understand and use the MRDviz JSON format effectively for your longitudinal data visualization needs.
