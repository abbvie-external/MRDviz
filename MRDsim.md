# MRD Simulation Tutorial

## Introduction

The MRD Simulation feature in MRDviz allows you to generate synthetic Minimal Residual Disease (MRD) trajectories and associated survival outcomes using a joint modeling approach. This tutorial will guide you through the process of creating, visualizing, and analyzing simulated MRD data.

## Getting Started

1. Launch MRDviz by running `library(MRDviz)` followed by `run_mrdviz()` in R
2. Navigate to the "Simulation" tab in the main interface
3. You'll see a panel of parameters on the left and visualization areas on the right

## Basic Workflow

The simulation workflow consists of these key steps:

1. **Configure Parameters**: Adjust the simulation parameters to model specific clinical scenarios
2. **Generate Simulation**: Click "Add Simulation" to create a simulation with current parameters
3. **Visualize Results**: Examine the generated MRD trajectories and survival curves
4. **Compare Groups**: Create multiple simulations with different parameters to compare outcomes
5. **Export Data**: Download the simulated data for further analysis

## Creating Your First Simulation

1. Start with the default parameters or adjust them based on your research question
2. Enter a descriptive name in the "Group Name" field (e.g., "Standard Treatment")
3. Click "Add Simulation" to generate data
4. Review the trajectory plot and survival curve that appear on the right
5. Examine the summary statistics below each plot

## Comparing Multiple Scenarios

To compare different clinical scenarios:

1. After creating your first simulation, modify parameters to represent an alternative scenario
2. Enter a new group name (e.g., "Experimental Treatment")
3. Click "Add Simulation" to add this scenario
4. Enable "Show All Groups" to visualize both scenarios together
5. Compare the trajectories and survival outcomes between groups

## Interpreting Simulation Results

### MRD Trajectory Plot
- Each line represents one simulated patient's MRD measurements over time
- The vertical dashed red line indicates the End-of-Treatment (EoT) time
- The y-axis uses a log10 scale to better visualize the wide range of MRD values
- Toggle "Show True Values" to see the underlying trajectory without measurement error

### Survival Plot
- Shows the Kaplan-Meier survival curve for simulated patients
- Enable "Show Confidence Intervals" to display uncertainty
- Enable "Show All Groups" for survival plots to compare groups that have been added
- When comparing groups, different colors represent different scenarios
- Tick marks indicate censored observations

## Exporting Simulation Data

You can export the simulated data for further analysis:

1. Scroll down to the "Data Preview" section
2. Use the "CSV" or "Excel" buttons to download:
   - Trajectory data (longitudinal MRD measurements)
   - Event data (survival outcomes)
   - Parameter settings for each simulation group

## Parameter Reference

The simulation model includes parameters for both the longitudinal MRD component and the survival component. Below is a reference guide to help you understand how each parameter affects the simulation.

### **Longitudinal MRD Model Parameters**

- **Number of subjects** (`n`):  
  _Higher values simulate more patients, increasing statistical power._

- **End-of-Treatment Time (EoT)** (`betaLong_mrd_eot`):  
  _A later EoT delays when MRD response is evaluated._

- **Maximum follow-up time** (`max_fuptime`):  
  _A longer follow-up allows more time for relapse events to occur._

- **Maximum observations per subject** (`max_yobs`):  
  _Affects data sparsity and frequency of MRD measurements._

- **Intercept** (`betaLong_intercept`):  
  _Controls the baseline MRD level; higher values lead to higher initial MRD._

- **Binary covariate effect (log scale)** (`betaLong_binary`):  
  _Higher values modify MRD levels based on a binary covariate._

- **Continuous covariate effect (log scale)** (`betaLong_continuous`):  
  _Higher values alter MRD trajectories based on continuous covariates._

- **Longitudinal error SD (aux)** (`betaLong_aux`):  
  _Controls random noise in MRD observations._

- **Random Intercept SD** (`b_sd1`):  
  _Higher values indicate greater variability between individuals._

- **Random Slope SD** (`b_sd2`):  
  _Increases variability in individual MRD trajectory slopes._

- **Baseline MRD mean (log scale)** (`betaLong_baseline_mean`):  
  _Affects initial MRD levels across patients._

- **Baseline MRD SD (log scale)** (`betaLong_baseline_sd`):  
  _Higher values create more variability in baseline MRD._

- **Plateau MRD mean (log scale)** (`betaLong_plateau_mean`):  
  _Defines MRD levels at long-term follow-up._

- **Plateau MRD SD (log scale)** (`betaLong_plateau_sd`):  
  _Higher values lead to greater variability in long-term MRD levels._

- **Plateau effect of Binary Covariate** (`betaLong_plateau_binary`):  
  _Affects whether MRD levels remain suppressed or rebound for different subgroups._

- **Plateau effect of Continuous Covariate** (`betaLong_plateau_continuous`):  
  _Determines long-term MRD response based on continuous predictors._

- **Base Relapse Proportion** (`betaLong_mrd_relapse_prob`):  
  _Higher values increase the likelihood of MRD relapse._

- **Mean Relapse Time** (`betaLong_mrd_relapse_time_mean`):  
  _Higher values delay the time to relapse._

- **SD of Relapse Time** (`betaLong_mrd_relapse_time_sd`):  
  _Larger values increase the variability in relapse times._

- **Relapse Growth Rate** (`betaLong_mrd_relapse_rate`):  
  _A higher rate leads to faster MRD resurgence after relapse._

### **Survival Model Parameters**

- **Binary Covariate Effect** (`betaEvent_binary`):  
  _Affects survival probability for different patient groups._

- **Continuous Covariate Effect** (`betaEvent_continuous`):  
  _Higher values modify survival probability based on continuous covariates._

- **Weibull Intercept (scale, log scale)** (`betaEvent_intercept`):  
  _A higher value represents a worse baseline survival probability._

- **Weibull Shape Parameter** (`betaEvent_aux`):  
  _Controls the shape of the survival distribution._

- **Association (Value)** (`betaEvent_assoc_value`):  
  _Higher values indicate a stronger impact of MRD level on survival._

- **Association (Slope)** (`betaEvent_assoc_slope`):  
  _Rapidly increasing MRD levels correlate with worse survival._

- **Association (AUC)** (`betaEvent_assoc_auc`):  
  _Higher cumulative MRD burden over time reduces survival probability._

### **Covariate and Study Design Parameters**

- **Probability for Binary Covariate (Z1)** (`prob_Z1`):  
  _Adjusts prevalence of specific patient subgroups._

- **Mean of Continuous Covariate (Z2)** (`mean_Z2`):  
  _Influences patient distribution and MRD-survival relationships._

- **SD of Continuous Covariate (Z2)** (`sd_Z2`):  
  _Higher values introduce more variability in patient risk factors._

- **Missing Data Rate** (`missing_data_rate`):  
  _Controls the proportion of missing MRD measurements (0-1)._

## Example Scenarios

Here are some example scenarios you might want to simulate:

### Scenario 1: Standard vs. Experimental Treatment
- **Standard Treatment**: Use default parameters
- **Experimental Treatment**: Decrease `betaLong_plateau_mean` and `betaEvent_assoc_value`

### Scenario 2: High vs. Low Risk Patients
- **Low Risk**: Decrease `betaLong_baseline_mean` and `betaEvent_intercept`
- **High Risk**: Increase `betaLong_baseline_mean` and `betaEvent_intercept`

### Scenario 3: Different Relapse Patterns
- **Early Relapse**: Decrease `betaLong_mrd_relapse_time_mean`
- **Late Relapse**: Increase `betaLong_mrd_relapse_time_mean`
- **Aggressive Relapse**: Increase `betaLong_mrd_relapse_rate`

## Advanced Usage

### Understanding the MRD Trajectory Model

The MRD trajectory model simulates three phases:

1. **Initial Decay Phase** (0 to EoT): MRD decreases from baseline to plateau
2. **Plateau Phase** (EoT to Relapse): MRD remains stable at the plateau level
3. **Relapse Phase** (after Relapse): MRD increases exponentially if relapse occurs

### Relationship Between MRD and Survival

The joint model links MRD trajectories to survival through three association parameters:

- **Value Association**: Current MRD level affects hazard
- **Slope Association**: Rate of change in MRD affects hazard
- **AUC Association**: Cumulative MRD burden affects hazard

Adjusting these parameters allows you to model different hypotheses about how MRD relates to survival outcomes.

## Troubleshooting

- **No visible trajectories**: Check if the missing data rate is too high
- **All patients relapse**: Reduce the relapse probability parameter
- **Too few events**: Increase follow-up time or adjust survival parameters
- **Error messages**: Ensure all parameters are within valid ranges
