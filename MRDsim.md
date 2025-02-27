# MRD-Survival Joint Simulator

## Overview
This R Shiny app simulates longitudinal minimal residual disease (MRD) trajectories and associated survival outcomes using a joint modeling approach. The app allows users to adjust various parameters to study their effects on MRD progression and patient survival.

---

## Parameters and Their Effects

### **Longitudinal MRD Model Parameters**
These parameters influence the MRD trajectories observed over time.

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

---

### **Survival Model Parameters**
These parameters control the survival outcome based on MRD progression.

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

---

### **Covariate and Study Design Parameters**
- **Probability for Binary Covariate (Z1)** (`prob_Z1`):  
  _Adjusts prevalence of specific patient subgroups._

- **Mean of Continuous Covariate (Z2)** (`mean_Z2`):  
  _Influences patient distribution and MRD-survival relationships._

- **SD of Continuous Covariate (Z2)** (`sd_Z2`):  
  _Higher values introduce more variability in patient risk factors._
