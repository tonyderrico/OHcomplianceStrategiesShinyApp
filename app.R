library(shiny)
library(OHcomplianceStrategies)

# Define UI 
ui <- fluidPage(
  titlePanel("ShinyApp for compliance strategies application to test workers exposure to chemical agent(s) with the Occupational Exposure Limit (OEL)"),
  
  # Section for Phase 1EN 2018 k3 Calculation
  sidebarLayout(
    sidebarPanel(
      h3("EN689 Preliminary Test with k = 3"),
      numericInput("OEL_phase1EN2018_k3", "Occupational Exposure Limit:", value = 1),
      numericInput("measurement1_phase1EN2018_k3", "Measurement 1:", value = 0),
      numericInput("measurement2_phase1EN2018_k3", "Measurement 2:", value = 0),
      numericInput("measurement3_phase1EN2018_k3", "Measurement 3:", value = 0),
      actionButton("calculate_phase1EN2018_k3", "Calculate Exposure Compliance (k = 3)")
    ),
    mainPanel(
      textOutput("phase1EN2018_k3_result")
    ) 
  ),
  
  # Section for Phase 1EN 2018 k4 Calculation
  sidebarLayout(
    sidebarPanel(
      h3("EN689 Preliminary Test with k = 4"),
      numericInput("OEL_phase1EN2018_k4", "Occupational Exposure Limit:", value = 1),
      numericInput("measurement1_phase1EN2018_k4", "Measurement 1:", value = 0),
      numericInput("measurement2_phase1EN2018_k4", "Measurement 2:", value = 0),
      numericInput("measurement3_phase1EN2018_k4", "Measurement 3:", value = 0),
      numericInput("measurement4_phase1EN2018_k4", "Measurement 4:", value = 0),
      actionButton("calculate_phase1EN2018_k4", "Calculate Exposure Compliance (k = 4)")
    ),
    mainPanel(
      textOutput("phase1EN2018_k4_result")
    )
  ),
  
  # Section for Phase 1EN 2018 k5 Calculation
  sidebarLayout(
    sidebarPanel(
      h3("EN689 Preliminary Test with k = 5"),
      numericInput("OEL_phase1EN2018_k5", "Occupational Exposure Limit:", value = 1),
      numericInput("measurement1_phase1EN2018_k5", "Measurement 1:", value = 0),
      numericInput("measurement2_phase1EN2018_k5", "Measurement 2:", value = 0),
      numericInput("measurement3_phase1EN2018_k5", "Measurement 3:", value = 0),
      numericInput("measurement4_phase1EN2018_k5", "Measurement 4:", value = 0),
      numericInput("measurement5_phase1EN2018_k5", "Measurement 5:", value = 0),
      actionButton("calculate_phase1EN2018_k5", "Calculate Exposure Compliance (k = 5)")
    ),
    mainPanel(
      textOutput("phase1EN2018_k5_result")
    )
  )
)

# Define Server
server <- function(input, output) {
  # Calculate Phase 1EN 2018 k3
  observeEvent(input$calculate_phase1EN2018_k3, {
    samples <- c(input$measurement1_phase1EN2018_k3, 
                 input$measurement2_phase1EN2018_k3,
                 input$measurement3_phase1EN2018_k3)
    compliance <- phase1EN2018_k3(samples, OEL = input$OEL_phase1EN2018_k3)
    output$phase1EN2018_k3_result <- renderText({
      paste("Compliance result", compliance)
    })
  })
  
  # Calculate Phase 1EN 2018 k4
  observeEvent(input$calculate_phase1EN2018_k4, {
    samples <- c(input$measurement1_phase1EN2018_k4, 
                 input$measurement2_phase1EN2018_k4,
                 input$measurement3_phase1EN2018_k4,
                 input$measurement4_phase1EN2018_k4)
    compliance <- phase1EN2018_k4(samples, OEL = input$OEL_phase1EN2018_k4)
    output$phase1EN2018_k4_result <- renderText({
      paste("Compliance result:", compliance)
    })
  })
  
  # Calculate Phase 1EN 2018 k5
  observeEvent(input$calculate_phase1EN2018_k5, {
    samples <- c(input$measurement1_phase1EN2018_k5, 
                 input$measurement2_phase1EN2018_k5,
                 input$measurement3_phase1EN2018_k5,
                 input$measurement4_phase1EN2018_k5,
                 input$measurement5_phase1EN2018_k5)
    compliance <- phase1EN2018_k5(samples, OEL = input$OEL_phase1EN2018_k5)
    output$phase1EN2018_k5_result <- renderText({
      paste("Compliance result:", compliance)
    })
  })
}

# Run the Shiny app
shinyApp(ui, server)
