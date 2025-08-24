# MRDviz Visualization Tutorial

## Introduction

MRDviz is an interactive web-based application designed specifically for medical doctors and clinical researchers to visualize and analyze Minimal Residual Disease (MRD) data from clinical trials. This tutorial provides comprehensive, step-by-step instructions for using all visualization features, with minimal technical jargon and clear explanations tailored for healthcare professionals.

## What is MRDviz?

MRDviz allows you to:
- **Visualize patient trajectories**: See how MRD levels change over time for individual patients
- **Compare patient groups**: Analyze differences between treatment arms, risk groups, or other clinical categories
- **Perform survival analysis**: Examine relationships between MRD patterns and clinical outcomes
- **Create heatmaps**: Visualize patterns across multiple patients and time points
- **Filter and explore data**: Focus on specific patient subsets or characteristics

## Getting Started

### System Requirements

MRDviz runs in your web browser and requires:
- A modern web browser (Chrome, Firefox, Safari, or Edge)
- Internet connection (for initial setup)
- Your MRD data in the proper JSON format (see [Data Preparation](#data-preparation))

### Launching MRDviz

If MRDviz is installed on your system:

1. Open R or RStudio
2. Type the following commands:
   ```R
   library(MRDviz)
   run_mrdviz()
   ```
3. The application will open automatically in your web browser

If you're using a shared server installation, your IT administrator will provide you with a web link to access MRDviz directly.

## Data Preparation

### Understanding the Data Format

MRDviz uses a special JSON format that combines:
- **Longitudinal measurements**: MRD values over time for each patient
- **Patient characteristics**: Baseline information like age, treatment group, etc.
- **Clinical outcomes**: Survival data, response status, etc.

### Converting Your Data

Most clinical data starts in Excel or CSV format. MRDviz provides tools to convert your data:

#### Required Data Structure

**Longitudinal Data (one row per measurement):**
```
Patient_ID | Time | MRD_Level | Response | MRD_Status
-----------|------|-----------|----------|------------
PT_001     | 0    | 1000      | CR       | Positive
PT_001     | 3    | 0.01      | CR       | Negative
PT_001     | 6    | 0.005     | CR       | Negative
```

**Patient Data (one row per patient):**
```
Patient_ID | Treatment | Age | Sex | Overall_Survival | OS_Time
-----------|-----------|-----|-----|------------------|--------
PT_001     | Drug_A    | 65  | M   | Alive           | 24
PT_002     | Drug_B    | 58  | F   | Deceased        | 18
```

#### Converting to JSON Format

```R
library(MRDviz)

# Convert from Excel files
convert_to_json(
  longitudinal_file = "patient_trajectories.xlsx",
  event_file = "patient_characteristics.xlsx",
  output_file = "my_study_data.json"
)

# Convert from CSV files
convert_to_json(
  longitudinal_file = "patient_trajectories.csv",
  event_file = "patient_characteristics.csv",
  output_file = "my_study_data.json"
)
```

For detailed data preparation instructions, see the [JSON Format Guide](JSON_Format.md).

## Main Interface Overview

The MRDviz interface consists of:

### Left Sidebar (Controls)
- **File Upload**: Load your data
- **Covariate Filters**: Select patient groups to analyze
- **Subject Filters**: Choose specific patients
- **Visualization Options**: Customize charts and plots

### Main Display Area
- **Trajectory Plot**: Interactive line chart showing MRD over time
- **Heatmap**: Pattern visualization across patients and time
- **Survival Plot**: Kaplan-Meier survival curves
- **Data Table**: Detailed patient information

## Step-by-Step Tutorial

### Step 1: Loading Your Data

1. **Upload Data File**
   - Click "Choose File" next to "Upload JSON Data File"
   - Select your converted JSON file
   - Wait for the data to load (you'll see the interface populate with your data)

2. **Verify Data Loading**
   - Check that the trajectory plot shows patient lines
   - Confirm that filter options appear in the sidebar
   - Ensure the data table at the bottom shows your patients

### Step 2: Understanding the Trajectory Plot

The trajectory plot is the main visualization showing MRD levels over time.

#### Key Features:
- **Each line represents one patient's MRD journey**
- **Colors indicate patient groups** (treatment arms, response status, etc.)
- **Interactive tooltips** show detailed information when you hover over data points
- **Logarithmic scale** accommodates the wide range of MRD values

#### Interpreting the Plot:
- **Declining lines**: Patients with decreasing MRD (good response)
- **Flat lines**: Stable MRD levels
- **Rising lines**: Increasing MRD (possible relapse)
- **Missing segments**: Periods without measurements

### Step 3: Using Covariate Filters

Covariate filters help you focus on specific patient groups.

#### How to Use:
1. **Select Color Coding**: Choose which patient characteristic to use for coloring lines
   - Treatment groups, response status, risk categories, etc.
   - Lines will automatically recolor based on your selection

2. **Filter by Groups**: Click colored buttons to show/hide patient groups
   - **Clicked (highlighted)**: Group is selected and visible
   - **Unclicked**: Group is hidden from view
   - **Multiple selections**: Show multiple groups simultaneously

#### Clinical Applications:
- **Compare treatment arms**: Select different treatment groups
- **Analyze responders vs. non-responders**: Filter by response status
- **Study high-risk patients**: Focus on specific risk categories

### Step 4: Subject-Level Filtering

#### Individual Patient Selection:
1. **Search Box**: Type patient IDs to find specific patients
2. **Multiple Selection**: Hold Ctrl (Windows) or Cmd (Mac) to select multiple patients
3. **Remove Patients**: Click the X next to patient IDs to deselect

#### Random Sampling:
1. **Set Sample Size**: Enter number of patients to randomly select
2. **Click "Random Selection"**: Automatically selects random patients
3. **Useful for**: Getting representative samples from large datasets

#### Clear All Filters:
- **"Clear Filters" button**: Removes all selections and shows all patients

### Step 5: Interpreting the Heatmap

The heatmap provides a different view of your data, showing patterns across all patients simultaneously.

#### Understanding the Heatmap:
- **Rows**: Individual patients (scrollable if many patients)
- **Columns**: Time points
- **Colors**: Values of selected time-varying characteristic (e.g., response status, MRD negativity)
- **Patterns**: Horizontal bands show consistent responses; vertical bands show time-specific changes

#### Using the Heatmap:
1. **Select Variable**: Choose which time-varying characteristic to display
2. **Scroll Through Patients**: Use mouse wheel to scroll through patient rows
3. **Identify Patterns**: Look for clusters of similar colors

#### Clinical Insights:
- **Response patterns**: See when patients achieve/lose response
- **Treatment effects**: Identify time periods of maximum benefit
- **Patient stratification**: Group patients with similar patterns

### Step 6: Survival Analysis

The survival plot shows clinical outcomes related to your MRD data.

#### Understanding Survival Plots:
- **X-axis**: Time (months, years, etc.)
- **Y-axis**: Probability of survival (0-100%)
- **Lines**: Different patient groups (filtered vs. unfiltered)
- **Drops**: Events (deaths, progressions, etc.)
- **Tick marks**: Censored observations (patients lost to follow-up)

#### Interpreting Results:
- **Higher curves**: Better survival
- **Steeper drops**: More events occurring
- **Separated curves**: Difference between groups
- **Confidence intervals**: Uncertainty around estimates

### Step 7: Data Table Analysis

The data table at the bottom provides detailed patient information.

#### Features:
- **Sortable columns**: Click headers to sort by any variable
- **Searchable**: Use search box to find specific values
- **Exportable**: Download filtered data as CSV or Excel
- **Scrollable**: Navigate through large datasets

#### Clinical Use:
- **Patient lookup**: Find specific patient details
- **Data verification**: Check individual measurements
- **Export for analysis**: Download subsets for further study

## Advanced Features

### Interactive Tooltips

Hover over any data point to see:
- **Patient ID**
- **Exact measurement values**
- **Time point**
- **All patient characteristics**
- **Time-varying covariates**

### Dynamic Filtering

All visualizations update automatically when you:
- Change covariate selections
- Filter patient groups
- Select individual patients
- Modify display options

### Multi-Group Comparisons

Compare multiple patient groups simultaneously:
1. Select multiple covariate groups
2. Use different colors to distinguish groups
3. Analyze patterns across groups in all visualizations

## Clinical Applications and Use Cases

### Treatment Efficacy Studies

**Scenario**: Comparing two treatment arms in a clinical trial

**Steps**:
1. Load trial data with treatment arm information
2. Set color coding to "Treatment_Arm"
3. Compare trajectory patterns between arms
4. Use survival plot to assess clinical outcomes
5. Export filtered data for statistical analysis

**Insights**:
- Which treatment achieves deeper MRD responses?
- How quickly do patients respond to each treatment?
- Are there differences in durability of response?

### Biomarker Analysis

**Scenario**: Identifying patients who benefit most from treatment

**Steps**:
1. Use covariate filters to select high-risk patients
2. Analyze MRD trajectory patterns
3. Correlate with survival outcomes
4. Use heatmap to identify response patterns

**Insights**:
- Which baseline characteristics predict response?
- When do patients typically achieve MRD negativity?
- How does MRD status relate to survival?

### Response Monitoring

**Scenario**: Understanding patterns of treatment response

**Steps**:
1. Filter by response categories (CR, PR, SD, PD)
2. Examine trajectory shapes for each group
3. Use heatmap to see response evolution over time
4. Identify optimal monitoring timepoints

**Insights**:
- When should MRD be measured for maximum information?
- What MRD patterns predict durable remission?
- How early can treatment failure be detected?

### Relapse Prediction

**Scenario**: Identifying patients at risk for relapse

**Steps**:
1. Focus on patients who initially responded
2. Look for early signs of MRD increase
3. Correlate with eventual clinical relapse
4. Use survival analysis to quantify risk

**Insights**:
- What MRD threshold predicts relapse?
- How much lead time does MRD provide?
- Which patients need more intensive monitoring?

## Troubleshooting Common Issues

### Data Loading Problems

**Issue**: "Error loading data" message
**Solutions**:
- Check that your JSON file is properly formatted
- Ensure file size is reasonable (<100MB)
- Verify patient IDs match between longitudinal and patient data
- Contact your data analyst if conversion failed

**Issue**: Missing visualizations
**Solutions**:
- Confirm data loaded successfully (check data table)
- Verify you have both longitudinal and patient data
- Check that required columns are present
- Try clearing filters and reloading

### Visualization Issues

**Issue**: No lines visible in trajectory plot
**Solutions**:
- Check if filters are too restrictive
- Click "Clear Filters" to reset
- Verify patients have longitudinal measurements
- Try selecting different covariate for coloring

**Issue**: Heatmap appears empty
**Solutions**:
- Ensure selected variable has time-varying data
- Check that patients have measurements at multiple timepoints
- Try different heatmap variables
- Verify data contains the selected covariate

### Performance Issues

**Issue**: Slow response or freezing
**Solutions**:
- Reduce number of displayed patients using filters
- Close other browser tabs to free memory
- Try refreshing the browser
- Contact IT support for server-based installations

### Filter Problems

**Issue**: Filters not working as expected
**Solutions**:
- Click "Clear Filters" and start over
- Check that covariate values are correctly formatted
- Verify patient groups contain expected patients
- Try different filter combinations

## Best Practices for Clinical Research

### Data Quality

1. **Consistent Patient IDs**: Ensure IDs match exactly between datasets
2. **Complete Time Information**: Include all measurement timepoints
3. **Standardized Categories**: Use consistent naming for groups and responses
4. **Missing Data**: Clearly indicate missing values in source data

### Analysis Workflow

1. **Start Broad**: Begin with all patients to understand overall patterns
2. **Progressive Filtering**: Gradually narrow focus to specific groups
3. **Multiple Views**: Use all visualization types for comprehensive analysis
4. **Document Findings**: Export filtered data and screenshots for reports

### Interpretation Guidelines

1. **Clinical Context**: Always interpret MRD patterns in clinical context
2. **Statistical Significance**: Use appropriate statistical tests for comparisons
3. **Sample Size**: Consider patient numbers when drawing conclusions
4. **Temporal Patterns**: Pay attention to timing of changes
5. **Individual Variation**: Account for patient-to-patient differences

### Collaboration

1. **Share Insights**: Use screenshots and exported data for team discussions
2. **Reproducible Analysis**: Document filter settings and analysis steps
3. **Version Control**: Keep track of different data versions and analyses
4. **Cross-Validation**: Verify findings with independent datasets when possible

## Exporting and Reporting

### Screenshots

Capture visualizations for presentations:
1. Use browser's screenshot tools
2. Ensure high resolution for publications
3. Include legends and axis labels
4. Document filter settings used

### Data Export

Export filtered datasets:
1. Use "CSV" or "Excel" buttons in data table
2. Include only relevant patients and variables
3. Document selection criteria
4. Maintain patient confidentiality

### Report Generation

Create comprehensive reports:
1. Combine multiple visualization types
2. Include methodology and filter settings
3. Provide clinical interpretation
4. Document limitations and assumptions

## Getting Help

### Technical Support

- **Data Format Issues**: Consult the [JSON Format Guide](JSON_Format.md)
- **Installation Problems**: Contact your IT administrator
- **Software Bugs**: Report issues through your institution's support channels

### Clinical Interpretation

- **Statistical Analysis**: Consult with biostatisticians
- **Clinical Significance**: Discuss findings with clinical team
- **Regulatory Questions**: Involve regulatory affairs specialists

### Training Resources

- **Hands-on Training**: Request training sessions from your institution
- **Example Datasets**: Practice with simulated data before using real studies
- **User Groups**: Connect with other MRDviz users in your organization

## Conclusion

MRDviz provides powerful tools for visualizing and analyzing MRD data in clinical research. By following this tutorial, medical doctors and clinical researchers can:

- Effectively load and visualize their clinical trial data
- Identify important patterns in patient responses
- Compare treatment effects across different patient groups
- Generate insights for clinical decision-making
- Create publication-ready visualizations

The interactive nature of MRDviz allows for exploratory analysis that can reveal unexpected patterns and generate new hypotheses for clinical research. Regular use of these visualization tools can enhance understanding of MRD dynamics and improve patient care.

Remember that MRDviz is a visualization tool that complements, but does not replace, rigorous statistical analysis and clinical judgment. Always interpret results in the context of your specific clinical setting and consult with appropriate specialists when making treatment decisions based on MRD data.

## Additional Resources

- **[JSON Format Guide](JSON_Format.md)**: Detailed instructions for data preparation
- **[MRD Simulation Tutorial](MRDsim.md)**: Learn to simulate MRD data for planning studies
- **[Docker Installation Guide](README.docker.md)**: Technical installation instructions
- **Main Documentation**: See [README.md](README.md) for technical details

For the most current information and updates, consult the official MRDviz documentation and your institution's guidelines for clinical data analysis.
