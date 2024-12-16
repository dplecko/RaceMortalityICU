library(shiny)
library(faircause)
library(ggplot2)
library(shinyjs)

shinyServer(function(input, output, session) {

  # Add a resource path for the www directory
  addResourcePath("www", "www")

  sfm_blocks <- reactiveVal(
    list(area1 = character(), area2 = character(), area3 = character(),
         area4 = character())
  )

  observe({
    session$sendCustomMessage(type = "initDraggable", message = list())
    session$sendCustomMessage(type = "initDroppable", message = list())
  })

  output$output <- renderText({
    drawn <- sfm_blocks()
    paste(
      paste0("Z (Confounders):", paste(drawn$area1, collapse = ", ")),
      paste0("X (Protected Attribute):", paste(drawn$area2, collapse = ", ")),
      paste0("W (Mediators):", paste(drawn$area3, collapse = ", ")),
      paste0("Y (Outcome):", paste(drawn$area4, collapse = ", ")),
      sep = "\n"
    )
  })

  observeEvent(input$blocksDrawn, {
    sfm_blocks(input$blocksDrawn)
  })

  # Observe CSV file input and dynamically create draggable blocks
  observe({
    req(input$file1)

    df <- read.csv(input$file1$datapath)
    col_names <- colnames(df)

    output$draggable_blocks <- renderUI({
      num_cols <- 3
      col_splits <- split(col_names, cut(seq_along(col_names), num_cols, labels = FALSE))

      fluidRow(
        lapply(seq_len(num_cols), function(i) {
          column(4,
                 lapply(col_splits[[i]], function(name) {
                   div(id = name, class = "draggable-block", name)
                 })
          )
        })
      )
    })

    # Ensure draggable elements are initialized after the UI updates
    session$sendCustomMessage(type = 'reinitializeDraggables', message = list())

    # Optional: Use onFlushed without `once = TRUE` to ensure it's called every time
    session$onFlushed(function() {
      session$sendCustomMessage(type = "reinitializeDraggables", message = list())
    })
  })

  observeEvent(input$compute, {
    sfm <- sfm_blocks()
    method <- input$method
    model <- input$model
    x0 <- input$x0
    x1 <- input$x1

    data <- read.csv(input$file1$datapath)
    
    set.seed(2024)
    fcb <- fairness_cookbook(
      data = data, Z = unlist(sfm$area1), X = unlist(sfm$area2),
      W = unlist(sfm$area3), Y = unlist(sfm$area4), x0 = x0, x1 = x1,
      method = method, model = model
    )
    output$result_plot <- renderPlot({
      autoplot(fcb) +
        geom_col(fill = "black")
    })
  })
})
