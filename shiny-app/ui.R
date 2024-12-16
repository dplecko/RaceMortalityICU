library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  theme = shinytheme("cerulean"),
  tags$head(
    tags$script(src = "https://code.jquery.com/jquery-3.6.0.min.js"),
    tags$script(src = "https://code.jquery.com/ui/1.12.1/jquery-ui.min.js"),
    tags$script(src = "https://stackpath.bootstrapcdn.com/bootstrap/3.4.1/js/bootstrap.min.js"),
    tags$script(src = "www/script.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css")
  ),
  fluidRow(
    column(5,
           h3("Dataset Upload"),
           fileInput("file1", "Choose CSV File",
                     accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
           uiOutput("draggable_blocks")
    ),
    column(6,  # Top right: Graph
           div(id = "sfm-container",
               h3("Standard Fairness Model", style = "text-align: center;"),
               div(class = "sfm-content", source("sfm_ui.R", local = TRUE)$value)
           )
    )
  ),
  fluidRow(
    column(5, id = "sfm-clustering-container",
           h3("SFM Clustering"),
           verbatimTextOutput("output"),
           h3("Arguments"),
           fluidRow(
             column(6, selectInput("method", "Method", choices = c("debiasing", "causal_forest", "medDML"))),
             column(6, selectInput("model", "Model", choices = c("ranger", "linear")))
           ),
           fluidRow(
             column(6, textInput("x0", "x0", "")),
             column(6, textInput("x1", "x1", ""))
           ),
           actionButton("compute", "Compute")
    ),
    column(6, id = "tv-decomposition",
           div(style = "margin: 0 auto; width: 90%; border: 0.5px solid #ccc; padding: 10px;",
               plotOutput("result_plot", height = "350px", width = "100%")
           )
    )
  )
))
