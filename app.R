library(shiny)
library(OHcomplianceStrategies)
library(htmltools)
library(shinyuieditor)
library(shinythemes)
library('tolerance')
library('ggplot2')

# Define UI 
ui <- fluidPage( 
  theme = shinytheme("flatly"),
  # Include CSS file
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  #APP start
  
  titlePanel("ShinyApp for compliance strategies application to test workers exposure to chemical agent(s) with the Occupational Exposure Limit (OEL)"),
  
  tabsetPanel(
    # Home Page
    tabPanel("Home",
             h2("Welcome to the OH Compliance Strategies App!"),
             p("This app helps you assessing compliance strategies for testing workers' exposure to chemical agents against the Occupational Exposure Limit (OEL)."),
             p("Use the tabs above to navigate to different sections and perform compliance calculations."),
             p("Get started by selecting one of the options from the tabs above.")
    ),
    # Section for Phase 1EN 2018 k3 Calculation
    tabPanel("EN689 Preliminary Test with k = 3",
             sidebarLayout(
               sidebarPanel(
                 numericInput("OEL_phase1EN2018_k3", "Occupational Exposure Limit:", value = 1),
                 numericInput("measurement1_phase1EN2018_k3", "Measurement 1:", value = 0),
                 numericInput("measurement2_phase1EN2018_k3", "Measurement 2:", value = 0),
                 numericInput("measurement3_phase1EN2018_k3", "Measurement 3:", value = 0),
                 actionButton("calculate_phase1EN2018_k3", "Calculate Exposure Compliance (k = 3)")
               ),
               mainPanel(
                 textOutput("phase1EN2018_k3_result"),
                 plotOutput("boxplot_k3")
               ) 
             )
    ),
    
    # Section for Phase 1EN 2018 k4 Calculation
    tabPanel("EN689 Preliminary Test with k = 4",
             sidebarLayout(
               sidebarPanel(
                 numericInput("OEL_phase1EN2018_k4", "Occupational Exposure Limit:", value = 1),
                 numericInput("measurement1_phase1EN2018_k4", "Measurement 1:", value = 0),
                 numericInput("measurement2_phase1EN2018_k4", "Measurement 2:", value = 0),
                 numericInput("measurement3_phase1EN2018_k4", "Measurement 3:", value = 0),
                 numericInput("measurement4_phase1EN2018_k4", "Measurement 4:", value = 0),
                 actionButton("calculate_phase1EN2018_k4", "Calculate Exposure Compliance (k = 4)")
               ),
               mainPanel(
                 textOutput("phase1EN2018_k4_result"),
                 plotOutput("boxplot_k4")
               )
             )
    ),
    
    # Section for Phase 1EN 2018 k5 Calculation
    tabPanel("EN689 Preliminary Test with k = 5",
             sidebarLayout(
               sidebarPanel(
                 numericInput("OEL_phase1EN2018_k5", "Occupational Exposure Limit:", value = 1),
                 numericInput("measurement1_phase1EN2018_k5", "Measurement 1:", value = 0),
                 numericInput("measurement2_phase1EN2018_k5", "Measurement 2:", value = 0),
                 numericInput("measurement3_phase1EN2018_k5", "Measurement 3:", value = 0),
                 numericInput("measurement4_phase1EN2018_k5", "Measurement 4:", value = 0),
                 numericInput("measurement5_phase1EN2018_k5", "Measurement 5:", value = 0),
                 actionButton("calculate_phase1EN2018_k5", "Calculate Exposure Compliance (k = 5)")
               ),
               mainPanel(
                 textOutput("phase1EN2018_k5_result"),
                 plotOutput("boxplot_k5")
               )
             )
    ),
    
    # Section for Phase 2 UTL Calculation
    tabPanel("EN689 Phase 2 Upper Tolerance Limit (UTL) Calculation",
             sidebarLayout(
               sidebarPanel(
                 column(width = 12,
                        numericInput("OEL_phase2_UTL", "Occupational Exposure Limit:", value = 1),
                        fileInput("samples_phase2_UTL", "Upload CSV file with Workers Exposure Concentration"),
                        actionButton("calculate_phase2_UTL", "Calculate Compliance")
                 ),
                 column(width = 8,
                        h4("Dataset Example in csv"),  # Title for the image
                        img(src = "exampledf.jpg", height = "300px", width = "50%")
                 )
               ),
               mainPanel(
                 textOutput("phase2_UTL_result"),
                 plotOutput("density_plot")
               )
             )
    )
    
    
  )
)

#----------------------------------------------------------------------------------------------------
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
    output$boxplot_k3 <- renderPlot({
      data <- data.frame(Measurements = paste("Worker", 1:3), Agent = samples)
      bp = boxplot(Agent ~ Measurements, data = data, main = "Workers Exposure",
                   ylim = c(0, max(samples) * 1.2))
      abline(h = input$OEL_phase1EN2018_k3, col = "red")
      bp
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
    output$boxplot_k4 <- renderPlot({
      data <- data.frame(Measurements = paste("Worker", 1:4), Agent = samples)
      bp = boxplot(Agent ~ Measurements, data = data, main = "Workers Exposure",
                   ylim = c(0, max(samples) * 1.2))
      abline(h = input$OEL_phase1EN2018_k3, col = "red")
      bp
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
    output$boxplot_k5 <- renderPlot({
      data <- data.frame(Measurements = paste("Worker", 1:5), Agent = samples)
      bp = boxplot(Agent ~ Measurements, data = data, main = "Workers Exposure",
                   ylim = c(0, max(samples) * 1.2))
      abline(h = input$OEL_phase1EN2018_k3, col = "red")
      bp
    })
  })
  
  # Phase 2 UTL calculation
  observeEvent(input$calculate_phase2_UTL, {
    req(input$samples_phase2_UTL)
    df <- read.csv(input$samples_phase2_UTL$datapath, sep = ";")
    
    df <- data.frame(df)
    df$samples = as.numeric(df$samples)

    result <- phase2_UTL(df$samples, input$OEL_phase2_UTL)
    
    output$phase2_UTL_result <- renderText({
      paste("Result:", result)
    })
    
    output$density_plot <- renderPlot({
      ggplot(df, aes(x = log(samples))) +
        geom_density(fill = "skyblue", color = "blue") +
        geom_vline(xintercept = log(input$OEL_phase2_UTL), color = "red", linetype = "dashed", size = 1) +
        labs(title = "Density Plot of Exposure Concentrations", x = "Log(Concentrations)", y = "Density") +
        theme_minimal()
    })
  })
}


# Run the Shiny app
shinyApp(ui, server)

