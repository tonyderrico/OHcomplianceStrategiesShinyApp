library(shiny)
library(OHcomplianceStrategies)
library(htmltools)
library(shinyuieditor)
library(shinythemes)
library('tolerance')
library('ggplot2')
library(lme4)
library(dplyr)

# Define UI 
ui <- fluidPage( 
  theme = shinytheme("flatly"),
  # Include CSS file
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  #APP start
  img(src = "uulogo.png", height = "100px", width = "100px", align="right"),
  titlePanel("ShinyApp for compliance strategies application to test workers exposure to chemical agent(s) with the Occupational Exposure Limit (OEL)"),
  
  
  tabsetPanel(
    # Home Page
    tabPanel("Home",
             h1("Welcome to the OH Compliance Strategies App!"),
             h4("This app helps you assessing compliance strategies for testing workers' exposure to airborne substances with the Occupational Exposure Limit (OEL)."),
             h4("Use the tabs above to navigate to different tests and perform compliance calculations of your exposure data. Based on the number of measurements you have carried out within SEGs (Similar Exposure Groups), you can select the appropriate test"),
             h3("Below you can find the table with the compliance strategies included in the App carried out through the 'OHcomplianceStrategies' package [https://github.com/tonyderrico/OHcomplianceStrategies]"),
             img(src = "table_1.jpg", height = "1300px", width = "60%")
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
                 tableOutput("descriptive_tablek3"),
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
                 tableOutput("descriptive_tablek4"),
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
                 tableOutput("descriptive_tablek5"),
                 plotOutput("boxplot_k5")
               )
             )
    ),
    
    # Section for Phase 2 UTL Calculation
    tabPanel("EN689 Phase 2: Upper Tolerance Limit (UTL) Calculation",
             sidebarLayout(
               sidebarPanel(
                 column(width = 12,
                        numericInput("OEL_phase2_UTL", "Occupational Exposure Limit:", value = 1),
                        fileInput("samples_phase2_UTL", "Upload CSV file with workers exposure concentration"),
                        actionButton("calculate_phase2_UTL", "Calculate Compliance")
                 ),
                 column(width = 8,
                        h4("Dataset Example in csv"),  # Title for the image
                        img(src = "exampledf.jpg", height = "300px", width = "50%")
                 )
               ),
               mainPanel(
                 tableOutput("descriptive_tableUTL"),
                 plotOutput("density_plot1")
               )
             )
    ),
   
     # Tab panel for Phase 3 BoHS.NvVA Calculation
    tabPanel("BoHS.NvVA Phase 3: Individual Compliance",
             sidebarLayout(
               sidebarPanel(
                 column(width = 12,
             numericInput("OEL_individual", "Occupational Exposure Limit:", value = 1),
             fileInput("data_individual", "Upload CSV file with information on workers IDs (with repeated measurements) and their exposure concentrations"),
             actionButton("calculate_individual", "Calculate Compliance")),
             column(width = 8,
                    h4("Dataset Example in csv"),  # Title for the image
                    img(src = "exampledf_IC.jpg", height = "300px", width = "50%"))
             ),
             mainPanel(
             textOutput("individual_result"),
             dataTableOutput("analysis_table"),
             plotOutput("density_plot2")
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
    
    # Calculate descriptive statistics
    mean_val <- mean(samples)
    sd_val <- sd(samples)
    geo_mean <- exp(mean(log(samples))) # Calculate geometric mean
    geo_sd <- exp(sd(log(samples))) # Calculate geometric standard deviation
    variance_val <- var(samples)
    median_val <- median(samples)
    compliance <- phase1EN2018_k3(samples, OEL = input$OEL_phase1EN2018_k3)
    
    # Create a data frame for descriptive statistics
    stats_dfk3 <- data.frame(
      Parameters = c("Compliance Result", "Median","Mean", "Standard Deviation", "Geometric Mean", 
                    "Geometric Standard Deviation", "Variance"),
      Value = c(compliance, median_val,mean_val, sd_val, geo_mean, geo_sd, variance_val)
    )
    
    # Round numeric values to two decimal places
    stats_dfk3$Value <- ifelse(grepl("^\\d+\\.\\d+$", stats_dfk3$Value),
                               format(round(as.numeric(stats_dfk3$Value), 2), nsmall = 2),
                               stats_dfk3$Value)

    
    # Render table
    output$descriptive_tablek3 <- renderTable({
      stats_dfk3 <- stats_dfk3  # Ensure stats_dfk3 is available in the local environment
      stats_dfk3  # Return the data frame
    }, rownames = FALSE)
    

    
    #render boxplot
    output$boxplot_k3 <- renderPlot({
      data <- data.frame(Measurements = paste("Worker", 1:3), Agent = samples)
      bp <- ggplot(data = data, aes(x = Measurements, y = Agent)) +
        geom_bar(stat = "identity") +  # 'stat = "identity"' is needed to plot actual y values
        ggtitle("Workers Exposure") + 
        geom_hline(yintercept = input$OEL_phase1EN2018_k3, color = "red",linetype = 'dashed', size= 1) +
        annotate("text", x = 3, y = input$OEL_phase1EN2018_k3, label = "OEL*0.1", vjust = -1, color = "red")
      bp
    })
  })
  
  # Calculate Phase 1EN 2018 k4
  observeEvent(input$calculate_phase1EN2018_k4, {
    samples <- c(input$measurement1_phase1EN2018_k4, 
                 input$measurement2_phase1EN2018_k4,
                 input$measurement3_phase1EN2018_k4,
                 input$measurement4_phase1EN2018_k4)
    
    # Calculate descriptive statistics
    mean_val <- mean(samples)
    sd_val <- sd(samples)
    geo_mean <- exp(mean(log(samples))) # Calculate geometric mean
    geo_sd <- exp(sd(log(samples))) # Calculate geometric standard deviation
    variance_val <- var(samples)
    median_val <- median(samples)
    compliance <- phase1EN2018_k4(samples, OEL = input$OEL_phase1EN2018_k4)
    
    # Create a data frame for descriptive statistics
    stats_dfk4 <- data.frame(
      Parameters = c("Compliance Result", "Median","Mean", "Standard Deviation", "Geometric Mean", 
                     "Geometric Standard Deviation", "Variance"),
      Value = c(compliance, median_val,mean_val, sd_val, geo_mean, geo_sd, variance_val)
    )
    
    # Round numeric values to two decimal places
    stats_dfk4$Value <- ifelse(grepl("^\\d+\\.\\d+$", stats_dfk4$Value),
                               format(round(as.numeric(stats_dfk4$Value), 2), nsmall = 2),
                               stats_dfk4$Value)
    
    
    # Render table
    output$descriptive_tablek4 <- renderTable({
      stats_dfk4 <- stats_dfk4  # Ensure stats_dfk4 is available in the local environment
      stats_dfk4  # Return the data frame
    }, rownames = FALSE)
    
    
    output$boxplot_k4 <- renderPlot({
      data <- data.frame(Measurements = paste("Worker", 1:4), Agent = samples)
      bp <- ggplot(data = data, aes(x = Measurements, y = Agent)) +
        geom_bar(stat = "identity") +  # 'stat = "identity"' is needed to plot actual y values
        ggtitle("Workers Exposure") + 
        geom_hline(yintercept = input$OEL_phase1EN2018_k4, color = "red",linetype = 'dashed', size= 1) +
        annotate("text", x = 3, y = input$OEL_phase1EN2018_k4, label = "OEL*0.15", vjust = -1, color = "red")
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
    # Calculate descriptive statistics
    mean_val <- mean(samples)
    sd_val <- sd(samples)
    geo_mean <- exp(mean(log(samples))) # Calculate geometric mean
    geo_sd <- exp(sd(log(samples))) # Calculate geometric standard deviation
    variance_val <- var(samples)
    median_val <- median(samples)
    compliance <- phase1EN2018_k5(samples, OEL = input$OEL_phase1EN2018_k5)
    
    # Create a data frame for descriptive statistics
    stats_dfk5 <- data.frame(
      Parameters = c("Compliance Result", "Median","Mean", "Standard Deviation", "Geometric Mean", 
                     "Geometric Standard Deviation", "Variance"),
      Value = c(compliance, median_val,mean_val, sd_val, geo_mean, geo_sd, variance_val)
    )
    
    # Round numeric values to two decimal places
    stats_dfk5$Value <- ifelse(grepl("^\\d+\\.\\d+$", stats_dfk5$Value),
                               format(round(as.numeric(stats_dfk5$Value), 2), nsmall = 2),
                               stats_dfk5$Value)
    
    
    # Render table
    output$descriptive_tablek5 <- renderTable({
      stats_dfk5 <- stats_dfk5  # Ensure stats_dfk5 is available in the local environment
      stats_dfk5  # Return the data frame
    }, rownames = FALSE)
    
    #renderboxplot
    output$boxplot_k5 <- renderPlot({
      data <- data.frame(Measurements = paste("Worker", 1:5), Agent = samples)
      bp = bp <- ggplot(data = data, aes(x = Measurements, y = Agent)) +
        geom_bar(stat = "identity") +  # 'stat = "identity"' is needed to plot actual y values
        ggtitle("Workers Exposure") + 
        geom_hline(yintercept = input$OEL_phase1EN2018_k5, color = "red",linetype = 'dashed', size= 1) +
        annotate("text", x = 3, y = input$OEL_phase1EN2018_k5, label = "OEL*0.2", vjust = -1, color = "red")
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
    
    # Calculate descriptive statistics
    mean_val <- mean(df$samples)
    sd_val <- sd(df$samples)
    geo_mean <- exp(mean(log(df$samples))) # Calculate geometric mean
    geo_sd <- exp(sd(log(df$samples))) # Calculate geometric standard deviation
    variance_val <- var(df$samples)
    median_val <- median(df$samples)
    result <- phase2_UTL(df$samples, input$OEL_phase2_UTL)
    # Calculate percentiles
    p25 <- quantile(df$samples, 0.25)
    p75 <- quantile(df$samples, 0.75)
    
    # Create a data frame for descriptive statistics
    stats_dfUTL <- data.frame(
      Parameters = c("Compliance Result", "25th Percentile","Median","75th Percentile","Mean", "Standard Deviation", "Geometric Mean", 
                     "Geometric Standard Deviation", "Variance"),
      Value = c(result, p25, median_val, p75, mean_val, sd_val, geo_mean, geo_sd, variance_val)
    )
    
    # Round numeric values to two decimal places
    stats_dfUTL$Value <- ifelse(grepl("^\\d+\\.\\d+$", stats_dfUTL$Value),
                               format(round(as.numeric(stats_dfUTL$Value), 2), nsmall = 2),
                               stats_dfUTL$Value)
    
    
    # Render table
    output$descriptive_tableUTL <- renderTable({
      stats_dfUTL <- stats_dfUTL  # Ensure stats_dfUTL is available in the local environment
      stats_dfUTL  # Return the data frame
    }, rownames = FALSE)
    
    
    output$density_plot1 <- renderPlot({
      ggplot(df, aes(x = log(samples))) +
        geom_density(fill = "skyblue", color = "blue") +
        geom_vline(xintercept = log(input$OEL_phase2_UTL), color = "red", linetype = "dashed", size = 1) +
        annotate("text", x = log(input$OEL_phase2_UTL)-0.15, y = 0.9, label = "OEL", color = "red", size = 8) +
        labs(title = "Density Plot of Exposure Concentrations", x = "Log(Concentrations)", y = "Density") +
        theme_minimal()
    })
  })
  
  #phase 3 calculation INDIVIDUAL COMPLIANCE
  
  observeEvent(input$calculate_individual, {
    # Check if data is uploaded and not empty
    if (is.null(input$data_individual) || is.null(input$data_individual$datapath)) {
      return(NULL)
    }
    
    # Read the uploaded CSV file
    df <- read.csv(input$data_individual$datapath, sep = ";")
    df <- data.frame(df)
    
    # Call the Individual_Compliance function with uploaded data and specified exposure limit
    compliance_result <- Individual_Compliance(
      seg = df, 
      workers = df$workers, 
      samples = df$samples, 
      OEL = input$OEL_individual
    )
    
    # Calculate necessary parameters
    M1 <- mean(df$samples)
    t <- lmer(samples ~ 1 + (1 | workers), data = df)
    VCrandom <- VarCorr(t)
    vv <- as.data.frame(VCrandom)
    wwsd <- sqrt(vv$vcov[2])
    bwsd <- sqrt(vv$vcov[1])
    H <- (log(input$OEL_individual) - (M1 + 1.645 * wwsd)) / bwsd
    IE <- 1 - pnorm(H)
    
    M1 <- formatC(M1, format = "f", digits = 2) # Format M1 with 2 decimal places
    wwsd <- formatC(wwsd, format = "f", digits = 2) # Format wwsd with 2 decimal places
    bwsd <- formatC(bwsd, format = "f", digits = 2) # Format bwsd with 2 decimal places
    H <- formatC(H, format = "f", digits = 2) # Format H with 2 decimal places
    IE <- formatC(IE, format = "f", digits = 2) # Format IE with 2 decimal places
    
    #DENSITY FUNCTIONS
    output$density_plot2 <- renderPlot({
      ggplot(data = df, aes(x = log(samples))) +
        geom_density(fill = "skyblue", color = "blue") +
        geom_vline(xintercept = log(input$OEL_individual), color = "red", linetype = "dashed") +
        annotate("text", x = log(input$OEL_individual)-0.15, y = 0.9, label = "OEL", color = "red", size = 8) +
        labs(title = "Density Plot of Exposure Concentrations", x = "Log(Concentrations)", y = "Density") +
        theme_minimal()
    })
    
    # Render the table with analysis summary
    output$analysis_table <- renderDataTable({
      # Create a data frame with the results
      analysis_summary <- data.frame(
        "Parameter" = c("Overall Mean of SEG", 
                        "Within-Workers Variance", 
                        "Between-Workers Variance",
                        "Probability of Exceedance (IE)",
                        "Compliance Result"),
        "Value" = c(M1, wwsd, bwsd, IE, compliance_result)
      )
      
      # Return the data frame
      data.frame(analysis_summary)
    })
  })
  

}


# Run the Shiny app
shinyApp(ui, server)
