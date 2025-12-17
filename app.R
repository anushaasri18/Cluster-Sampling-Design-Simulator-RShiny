library(shiny)

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  
  titlePanel("Sampling 2: Cluster Sampling for Population Mean"),
  
  sidebarLayout(
    sidebarPanel(
      
      numericInput("Sb2",
                   "Between-Cluster Variance (Sb²):",
                   value = 25, min = 1),
      
      numericInput("bias",
                   "Sampling Bias (b):",
                   value = 1, min = 0),
      
      numericInput("d",
                   "Margin of Error (d):",
                   value = 2, min = 0.1),
      
      selectInput("conf",
                  "Confidence Level:",
                  choices = c("90%" = 1.645,
                              "95%" = 1.96,
                              "99%" = 2.576)),
      
      numericInput("cost",
                   "Cost per Cluster:",
                   value = 100, min = 1),
      
      numericInput("time",
                   "Time per Cluster (hours):",
                   value = 2, min = 0.1)
    ),
    
    mainPanel(
      verbatimTextOutput("results"),
      plotOutput("doePlot")
    )
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output) {
  
  calculations <- reactive({
    
    Z <- as.numeric(input$conf)
    
    # Required number of clusters
    k <- (Z^2 * (input$Sb2 + input$bias^2)) / (input$d^2)
    k <- ceiling(k)
    
    total_cost <- k * input$cost
    total_time <- k * input$time
    
    list(
      k = k,
      cost = total_cost,
      time = total_time
    )
  })
  
  # -----------------------------
  # TEXT OUTPUT
  # -----------------------------
  output$results <- renderText({
    r <- calculations()
    paste(
      "Required Number of Clusters (k):", r$k, "\n",
      "Total Survey Cost:", r$cost, "\n",
      "Total Survey Time (hours):", r$time
    )
  })
  
  # -----------------------------
  # DESIGN OF EXPERIMENT PLOT
  # -----------------------------
  output$doePlot <- renderPlot({
    r <- calculations()
    
    plot(r$cost, r$time,
         pch = 19, col = "blue",
         xlab = "Total Cost",
         ylab = "Total Time (hours)",
         main = "Design of Experiment: Cost vs Time")
  })
}

# -----------------------------
# RUN APP
# -----------------------------
shinyApp(ui = ui, server = server)
