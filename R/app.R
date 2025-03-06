library(shiny)
library(shinyjs)
library(highcharter)
library(data.table)
library(memoise)
library(survival)
library(survminer)
library(jsonlite)
library(DT)
library(RColorBrewer)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(simsurv)

# UI Components from MRDviz
mrdviz_sidebar <- sidebarMenu(
  h4("Input File"),
  fileInput("datafile", "Upload JSON Data File", accept = ".json"),
  h4("Covariate Filter"),
  selectInput("colorCovariateSelect", "Select covariate for color-coding", choices = NULL),
  uiOutput("covariateFilters"),
  br(),
  h4("Subject Filter"),
  selectizeInput(
    "subjectIdInput", "Select Subjects",
    choices = NULL, multiple = TRUE,
    options = list(
      maxItems = 100,
      placeholder = 'Type to search subjects',
      plugins = list("remove_button")
    )
  ),
  div(
    class = "flex-container",
    numericInput("randomSelectCount", label = NULL, value = 3, min = 1),
    actionButton("randomSelectButton", "Random Selection",
                 style = "width: 80%; margin-top: 12px;")
  ),
  actionButton("clearButton", "Clear Filters",
               style = "width: 89%; margin-top: 10px;",
               class = "large-button"),
  br(), br(),
  h4("Heatmap Options"),
  selectInput("heatmapCovariate", "Select Covariate for Heatmap", choices = NULL),
  br(), br(),
  h4("Survival Analysis Options"),
  selectInput("survivalEndpoint", "Select Survival Endpoint", choices = NULL)
)

# UI Components from Simulation App
simulation_inputs <- div(
  style = "background-color: #f5f5f5; padding: 20px; margin-bottom: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
  fluidRow(
    style = "overflow-x: auto; white-space: nowrap;",
    column(12,
           div(style = "display: flex; gap: 20px;",
               # First group of parameters
               div(
                 style = "min-width: 300px; background-color: white; padding: 15px; border-radius: 5px;",
                 h4("General Parameters"),
                 numericInput("n", "Number of subjects:", value = 10, min = 10, max = 500),
                 numericInput("betaLong_mrd_eot", "End-of-Treatment Time (EoT):", value = 2, min = 0.5, step = 0.5),
                 numericInput("max_fuptime", "Maximum follow-up time:", value = 10, min = 1, step = 1),
                 numericInput("max_yobs", "Maximum observations per subject:", value = 15, min = 2, step = 1),
                 numericInput("missing_data_rate", "Missing Data Rate:", min = 0, max = 0.3, value = 0, step = 0.01)
               ),
               # Second group of parameters
               div(
                 style = "min-width: 300px; background-color: white; padding: 15px; border-radius: 5px;",
                 h4("Basic Longitudinal Parameters"),
                 numericInput("betaLong_intercept", "Intercept:", value = 10, step = 0.1),
                 numericInput("betaLong_binary", "Binary covariate effect (log scale):", value = -1, step = 0.1),
                 numericInput("betaLong_continuous", "Continuous covariate effect (log scale):", value = 1, step = 0.1),
                 h4("Noise Parameters"),
                 numericInput("betaLong_aux", "Longitudinal error SD (aux):", value = 0.5, step = 0.1),
                 numericInput("b_sd1", "Random Intercept SD:", value = 1.5, min = 0, step = 0.1),
                 numericInput("b_sd2", "Random Slope SD:", value = 0.07, min = 0, step = 0.01)
               ),
               # Third group of parameters
               div(
                 style = "min-width: 300px; background-color: white; padding: 15px; border-radius: 5px;",
                 h4("MRD Distribution Parameters"),
                 numericInput("betaLong_baseline_mean", "Baseline MRD mean (log scale):", value = log10(1000), step = 0.5),
                 numericInput("betaLong_baseline_sd", "Baseline MRD SD (log scale):", value = 0.2, step = 0.1),
                 numericInput("betaLong_plateau_mean", "Plateau MRD mean (log scale):", value = log10(0.01), step = 0.1),
                 numericInput("betaLong_plateau_sd", "Plateau MRD SD (log scale):", value = 0.2, step = 0.1),
                 numericInput("betaLong_plateau_binary", "Plateau effect of Binary Covariate:", value = -0.1, step = 0.1),
                 numericInput("betaLong_plateau_continuous", "Plateau effect of Continuous Covariate:", value = -0.05, step = 0.1)
               ),
               # Fourth group of parameters
               div(
                 style = "min-width: 300px; background-color: white; padding: 15px; border-radius: 5px;",
                 h4("Relapse Parameters"),
                 numericInput("betaLong_mrd_relapse_prob", "Base Relapse Proportion:", value = 0.3, min = 0, max = 1, step = 0.1),
                 numericInput("betaLong_mrd_relapse_time_mean", "Mean Relapse Time:", value = 6, min = 0, step = 0.5),
                 numericInput("betaLong_mrd_relapse_time_sd", "SD of Relapse Time:", value = 0.2, min = 0, step = 0.1),
                 numericInput("betaLong_mrd_relapse_rate", "Relapse Growth Rate:", value = 1.0, min = 0, step = 0.1)
               ),
               # Fifth group of parameters
               div(
                 style = "min-width: 300px; background-color: white; padding: 15px; border-radius: 5px;",
                 h4("Covariate Parameters"),
                 numericInput("prob_Z1", "Probability for Binary Covariate (Z1):", value = 0.5, min = 0, max = 1, step = 0.1),
                 numericInput("mean_Z2", "Mean of Continuous Covariate (Z2):", value = 0, step = 0.1),
                 numericInput("sd_Z2", "SD of Continuous Covariate (Z2):", value = 1, min = 0, step = 0.1),
                 h4("Covariate Effects on Event"),
                 numericInput("betaEvent_binary", "Binary Covariate Effect:", value = -0.5, step = 0.1),
                 numericInput("betaEvent_continuous", "Continuous Covariate Effect:", value = 0.5, step = 0.1)
               ),
               # Sixth group of parameters
               div(
                 style = "min-width: 300px; background-color: white; padding: 15px; border-radius: 5px;",
                 h4("Baseline Hazard Parameters"),
                 numericInput("betaEvent_intercept", "Weibull Intercept (scale, log scale):", value = -8.0, step = 0.5),
                 numericInput("betaEvent_aux", "Weibull Shape Parameter:", value = 2.0, min = 0.1, step = 0.1)
               ),
               # Seventh group of parameters
               div(
                 style = "min-width: 300px; background-color: white; padding: 15px; border-radius: 5px;",
                 h4("Association Parameters"),
                 numericInput("betaEvent_assoc_value", "Association (Value):", value = 0.5, step = 0.1),
                 numericInput("betaEvent_assoc_slope", "Association (Slope):", value = 0.2, step = 0.1),
                 numericInput("betaEvent_assoc_auc", "Association (AUC):", value = 0.0, step = 0.01)
               )
           )
    )
  )
)

# Define the UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = paste0("MRDviz v", packageVersion("MRDviz"))),
  dashboardSidebar(
    tags$head(
      tags$style(HTML("
        /* Increase sidebar width by 25% */
        .main-sidebar {
          width: 288px !important;
        }
        .content-wrapper, .main-header .navbar {
          margin-left: 288px !important;
        }
        .main-header .logo {
          width: 288px !important;
        }
        /* Existing styles */
        .tag-item {
          display: inline-block;
          padding: 8px 16px;
          margin: 0px;
          border-radius: 15px;
          color: white;
          cursor: pointer;
        }
        .tag-item.selected {
          border: 4px solid black;
          font-weight: bold;
        }
        .tags-container {
          display: flex;
          flex-wrap: wrap;
          gap: 1px;
          align-items: center;
          width: 90%;
          margin-bottom: 10px;
        }
        .flex-container {
          display: flex;
          width: 100%;
          align-items: stretch;
          margin-top: 10px;
        }
        #randomSelectCount {
          flex: 1;
          height: 38px;
          padding: 6px 12px;
          text-align: left;
        }
        #randomSelectButton {
          flex: 2;
          height: 38px;
          padding: 6px 12px;
          margin-left: -6px;
          margin-right: 16px;
        }
        .large-button {
          height: 50px;
          font-size: 18px;
        }
        .sidebar .shiny-input-container {
          width: 100%;
        }
        .sidebar-menu {
          margin-bottom: 20px;
          position: relative;
          height: auto;
          overflow: visible;
        }
        /* Make the main menu buttons larger */
        .sidebar-menu > li > a {
          padding: 15px 15px 15px 15px;
          font-size: 24px;
          height: 60px;
          box-sizing: border-box;
        }
        /* Prevent size changes on hover */
        .sidebar-menu > li > a:hover {
          height: 60px;
          box-sizing: border-box;
        }
        /* Add some extra emphasis to the icons */
        .sidebar-menu > li > a > i.fa {
          font-size: 18px;
          margin-right: 10px;
        }
        /* Fix positioning of conditional panel content */
        .sidebar-menu + .shiny-bound-output,
        .sidebar-menu ~ div {
          position: relative;
          z-index: 1;
        }
      "))
    ),
    useShinyjs(),
    sidebarMenu(
      id = "tabs",
      menuItem("MRD Visualization", tabName = "viz", icon = icon("chart-line")),
      menuItem("MRD Simulation", tabName = "sim", icon = icon("random"))
    ),
    conditionalPanel(
      condition = "input.tabs == 'viz'",
      mrdviz_sidebar
    )
  ),
  dashboardBody(
    tabItems(
      # MRD Visualization Tab
      tabItem(
        tabName = "viz",
        fluidRow(
          column(12, 
            div(
              style = "margin-bottom: 20px;",
              highchartOutput("longitudinalChart", height = "600px")
            )
          )
        ),
        fluidRow(
          column(12,
            div(
              style = "margin-bottom: 20px;",
              uiOutput("heatmapTitle"),
              uiOutput("heatmapLegend"),
              div(
                style = "margin-top: 10px;",
                uiOutput("heatmapContainer")
              )
            )
          )
        ),
        fluidRow(
          column(12, highchartOutput("survivalChart", height = "400px"))
        ),
        fluidRow(
          column(12,
            div(
              style = "margin-top: 20px;",
              uiOutput("subjectTableTitle"),
              DTOutput("subjectDataTable")
            )
          )
        )
      ),

      # MRD Simulation Tab
      tabItem(
        tabName = "sim",
        simulation_inputs,
        # Group Simulation Controls Panel
        div(
          style = "background-color: #e0e0e0; padding: 10px; margin-bottom: 20px; border-radius: 8px;",
          div(
            style = "display: inline-flex; align-items: center;",
            div(
              style = "margin-right: 10px;",
              tags$label("Enter Group Name:", `for` = "groupName", style = "display: block; margin-bottom: 5px;"),
              tags$input(
                type = "text",
                id = "groupName",
                class = "form-control",
                style = "width: 200px; height: 38px;"
              )
            ),
            actionButton("addSim", "Add Simulation for Group",
                         style = "height: 38px; margin-top: 25px;")
          )
        ),
        # Plots row
        fluidRow(
          column(6,
                 div(style = "display: flex; justify-content: space-between; align-items: center;",
                     h4("Individual MRD Trajectories"),
                     checkboxInput("show_true", "Show True Trajectories", value = FALSE),
                     checkboxInput("show_all_groups", "Show All Group Trajectories", value = FALSE)
                 ),
                 plotOutput("trajPlot", height = "400px"),
                 h4("Trajectory Summary"),
                 verbatimTextOutput("trajSummary")
          ),
          column(6,
                 div(style = "display: flex; justify-content: space-between; align-items: center;",
                     h4("Survival Outcome"),
                     checkboxInput("show_ci", "Show Confidence Intervals", value = TRUE),
                     checkboxInput("show_all_groups_surv", "Show All Groups", value = FALSE)
                 ),
                 plotOutput("eventPlot", height = "400px"),
                 h4("Event Summary"),
                 verbatimTextOutput("eventSummary")
          )
        ),
        # Data Preview Tables
        fluidRow(
          column(12,
                 h4("Data Preview (Combined Longitudinal Data)"),
                 DT::dataTableOutput("longData")
          )
        ),
        fluidRow(
          column(12,
                 h4("Data Preview (Combined Event Data)"),
                 DT::dataTableOutput("eventData")
          )
        ),
        fluidRow(
          column(12,
                 h4("Parameters for Added Groups"),
                 DT::dataTableOutput("paramTable")
          )
        )
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Create shared reactive values that persist across tab switches
  shared_storage <- reactiveValues(
    data = NULL,
    subject_dt = NULL,
    longitudinal_dt = NULL,
    selected_subgroups = list(),
    color_map = NULL,
    binary_color_pairs = list(),
    time_variant_covariates = NULL,
    clearing_filters = FALSE
  )
  
  # Initialize server logic using package functions
  mrdviz_server(input, output, session, shared_storage)
  simulation_server(input, output, session, shared_storage)
}

# Run the application
shinyApp(ui = ui, server = server)