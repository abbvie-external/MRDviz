#' MRDviz: Visualization and Simulation of Minimal Residual Disease Data
#'
#' MRDviz is a Shiny application package for visualizing and simulating Minimal
#' Residual Disease (MRD) data in clinical trials. It provides interactive
#' visualization of longitudinal MRD measurements, survival analysis, and
#' simulation capabilities for MRD trajectories.
#'
#'
#' @section Main Features:
#' \itemize{
#'   \item Interactive visualization of MRD trajectories
#'   \item Survival analysis integration
#'   \item MRD simulation capabilities
#'   \item Covariate-based filtering and grouping
#'   \item Heatmap visualization
#' }
#'
#' @section Usage:
#' To launch the application:
#' \code{
#' library(MRDviz)
#' run_mrdviz()
#' }
#'
#' @name MRDviz
#' @aliases MRDviz-package
#' @keywords internal
"_PACKAGE"
#' @import highcharter
#' @import memoise
#' @import survival
#' @import RColorBrewer
#' @import shinydashboard
#' @import ggplot2
#' @import simsurv
#' @importFrom stats setNames
#' @importFrom utils head tail
#' @importFrom shiny reactiveVal reactiveValues renderPlot renderUI selectInput sliderInput tabPanel tableOutput textInput uiOutput validate downloadHandler downloadButton plotOutput reactive observeEvent isolate req
#' @importFrom shiny renderTable dataTableOutput renderDataTable
#' @importFrom shinyjs useShinyjs runExample enable disable toggle hide show
#' @importFrom data.table data.table as.data.table setDT :=
#' @importFrom dplyr filter mutate select arrange group_by summarise ungroup first last between
#' @importFrom DT datatable formatStyle styleInterval renderDT DTOutput
#' @importFrom jsonlite fromJSON toJSON
#' @export ui
#' @export server
#' @export run_mrdviz
NULL