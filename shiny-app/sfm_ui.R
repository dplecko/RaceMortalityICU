div(id = "main-container",
    div(id = "label-1", class = "label", "Z (Confounders)"),
    div(id = "label-2", class = "label", "X (Prot. Attr.)"),
    div(id = "label-3", class = "label", "W (Mediators)"),
    div(id = "label-4", class = "label", "Y (Outcome)"),
    div(id = "area-1", class = "drag-area"),
    div(id = "area-2", class = "drag-area"),
    div(id = "area-3", class = "drag-area"),
    div(id = "area-4", class = "drag-area"),
    tags$svg(width = "100%", height = "100%", style = "position: absolute; top: 0; left: 0;",
             tags$defs(
               tags$marker(id = "arrowhead", markerWidth = "10", markerHeight = "7", refX = "10", refY = "3.5", orient = "auto",
                           tags$polygon(points = "0 0, 10 3.5, 0 7", fill = "black")
               ),
               tags$marker(id = "arrowhead-start", markerWidth = "10", markerHeight = "7", refX = "0", refY = "3.5", orient = "auto",
                           tags$polygon(points = "10 0, 0 3.5, 10 7", fill = "black") # Flipped arrowhead
               )
             ),
             tags$line(id = "arrow-z-x", class = "arrow", stroke = "black", `stroke-width` = "2", `marker-end` = "url(#arrowhead)", `marker-start` = "url(#arrowhead-start)")
    ),
    tags$svg(width = "100%", height = "100%", style = "position: absolute; top: 0; left: 0;",
             tags$line(id = "arrow-z-y", class = "arrow", stroke = "black", `stroke-width` = "2", `marker-end` = "url(#arrowhead)")
    ),
    tags$svg(width = "100%", height = "100%", style = "position: absolute; top: 0; left: 0;",
             tags$line(id = "arrow-z-w", class = "arrow", stroke = "black", `stroke-width` = "2", `marker-end` = "url(#arrowhead)")
    ),
    tags$svg(width = "100%", height = "100%", style = "position: absolute; top: 0; left: 0;",
             tags$line(id = "arrow-x-w", class = "arrow", stroke = "black", `stroke-width` = "2", `marker-end` = "url(#arrowhead)")
    ),
    tags$svg(width = "100%", height = "100%", style = "position: absolute; top: 0; left: 0;",
             tags$line(id = "arrow-x-y", class = "arrow", stroke = "black", `stroke-width` = "2", `marker-end` = "url(#arrowhead)")
    ),
    tags$svg(width = "100%", height = "100%", style = "position: absolute; top: 0; left: 0;",
             tags$line(id = "arrow-w-y", class = "arrow", stroke = "black", `stroke-width` = "2", `marker-end` = "url(#arrowhead)")
    )
)
