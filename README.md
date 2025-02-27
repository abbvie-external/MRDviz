# MRDviz

MRDviz is an R package that provides a powerful Shiny application for visualizing and simulating Minimal Residual Disease (MRD) data in clinical trials. This interactive tool enables researchers and clinicians to analyze longitudinal MRD measurements, perform survival analysis, and simulate MRD trajectories.

## Features

- **Interactive MRD Visualization**: Explore longitudinal MRD measurements through dynamic, interactive plots
- **Survival Analysis**: Integrate survival data with MRD measurements for comprehensive analysis
- **Simulation Capabilities**: Generate simulated MRD trajectories and associated survival outcome ([Manual](MRDsim.md))
- **Covariate-based Analysis**: Filter and group data based on various covariates
- **Heatmap Visualization**: Visualize MRD patterns across different patient groups and time points

## Installation

You can install MRDviz from GitHub using the `devtools` package:

```R
# Install devtools if you haven't already
install.packages("devtools")

# Install MRDviz
devtools::install_github("username/MRDviz")
```

### Dependencies

MRDviz relies on several R packages that will be automatically installed:

- shiny
- shinyjs
- highcharter
- data.table
- survival
- ggplot2
- And more...

## Usage

To launch the MRDviz application, simply run:

```R
library(MRDviz)
run_mrdviz()
```

This will start the Shiny application in your default web browser, providing access to all visualization and simulation features.

## License

This package is licensed under the MIT License - see the LICENSE file for details.

## Author

**KB Choi, Ph.D**  
AbbVie

## Version

Current version: 0.2.0

## Contributing

We welcome contributions! Please feel free to submit a Pull Request.

## Citation

If you use MRDviz in your research, please cite:

```
Choi, K. (2025). MRDviz: Visualization and Simulation of Minimal Residual Disease (MRD) Data. R package version 0.3.1.