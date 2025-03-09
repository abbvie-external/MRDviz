# MRDviz

MRDviz is an R package that provides a powerful Shiny application for visualizing and simulating Minimal Residual Disease (MRD) data in clinical trials. This interactive tool enables researchers and clinicians to analyze longitudinal MRD measurements, perform survival analysis, and simulate MRD trajectories.

## Features

- **Interactive MRD Visualization**: Explore longitudinal MRD measurements through dynamic, interactive plots
- **Survival Analysis**: Integrate survival data with MRD measurements for comprehensive analysis
- **Simulation Capabilities**: Generate simulated MRD trajectories and associated survival outcome ([Manual](MRDsim.md))
- **Covariate-based Analysis**: Filter and group data based on various covariates
- **Heatmap Visualization**: Visualize MRD patterns across different patient groups and time points

## Installation

You can install MRDviz from GitHub using the `remotes` package:

```R
# Install devtools if you haven't already
install.packages("remotes")

# Clone the code repository
git clone https://pig.abbvienet.com/choikx3/MRDviz.git

# Install MRDviz
remotes::install_local(path = "MRDviz", dependencies = TRUE)
```

### Dependencies

MRDviz relies on several R packages that will be automatically installed:

- shiny
- shinyjs
- shinydashboard
- memoise
- highcharter
- data.table
- DT
- survival
- survminer
- tidyverse (especially ggplot2, dplyr, purrr)

## Usage

To launch the MRDviz application, simply run:

```R
library(MRDviz)
run_mrdviz()
```

This will start the Shiny application in your default web browser, providing access to all visualization and simulation features. If you want to run the app off of a dockerized shiny-server container, see [this](README.docker.md) tutorial.


## License

This package is licensed under the MIT License - see the LICENSE file for details.

## Author

**KB Choi, Ph.D**  
Bioinformatics, GRC

## Version

Current version: 0.3.1

## Contributing

We welcome contributions! Please feel free to submit a Pull Request.

## Citation

If you use MRDviz in your research, please cite:

```
Choi, K. (2025). MRDviz: An Integrated Platform for Interactive Visualization and simulation of Measurable/Minimal Residual Disease Trajectories and Associated Survival Outcomes.