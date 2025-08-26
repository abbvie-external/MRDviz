# MRDviz

[![R-CMD-check](https://github.com/abbvie-external/MRDviz/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/abbvie-external/MRDviz/actions/workflows/R-CMD-check.yml)

MRDviz is an R package that provides a powerful Shiny application for visualizing and simulating Minimal Residual Disease (MRD) data in clinical trials. This interactive tool enables researchers and clinicians to analyze longitudinal MRD measurements, perform survival analysis, and simulate MRD trajectories.

## Features

- **Interactive MRD Visualization**: Explore longitudinal MRD measurements through dynamic, interactive plots
- **Survival Analysis**: Integrate survival data with MRD measurements for comprehensive analysis
- **Simulation Capabilities**: Generate simulated MRD trajectories and associated survival outcome ([Manual](MRDsim.md))
- **JSON Data Format**: Import and export data using the MRDviz JSON format ([Guide](JSON_Format.md))
- **Covariate-based Analysis**: Filter and group data based on various covariates
- **Heatmap Visualization**: Visualize MRD patterns across different patient groups and time points

## Installation

You can install MRDviz from GitHub using the `remotes` package:

```R
# Install devtools if you haven't already
install.packages("remotes")

# Clone the code repository
git clone https://github.com/kbchoi/MRDviz

# Install MRDviz
remotes::install_local(path = "MRDviz", dependencies = TRUE)
remotes::install_github("kbchoi/MRDviz")
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

### Basic Usage

To launch the MRDviz application, simply run:

```R
library(MRDviz)
run_mrdviz()
```

This will start the Shiny application in your default web browser, providing access to all visualization and simulation features.

### Quick Start with Example Data

To get started immediately with example data, you can launch the app with a pre-loaded dataset:

```R
library(MRDviz)

# Launch with small example dataset (recommended for first-time users)
example_file <- system.file("testdata", "simulated_mrd.short.json", package = "MRDviz")
run_mrdviz(example_file)

# Or launch with the full example dataset
example_file <- system.file("testdata", "simulated_mrd.json", package = "MRDviz")
run_mrdviz(example_file)
```

### Exploring Available Example Datasets

To see what example datasets are available:

```R
# View all available example datasets
get_example_datasets()

# Load example data for programmatic analysis
example_data <- load_example_data("simulated_mrd.short.json")
str(example_data, max.level = 2)
```

### Using Your Own Data

To launch the app with your own JSON data file:

```R
run_mrdviz("/path/to/your/data.json")
```

### Docker Usage

If you want to run the app off of a dockerized shiny-server container, see [this](README.docker.md) tutorial.


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
