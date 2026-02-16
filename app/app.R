library(shiny)

# --- 1. User Interface (UI) ---
ui <- fluidPage(
  
  titlePanel("Statistical Distributions Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown for Sample Size
      selectInput("n", 
                  "Sample Size (n):", 
                  choices = c(15, 30, 50, 100), 
                  selected = 100),
      
      # Dropdown for Distribution Type
      selectInput("dist", 
                  "Distribution:", 
                  choices = c("Normal", "Binomial", "Geometric", "Poisson", "Uniform", "Lognormal"),
                  selected = "Normal"),
      
      tags$hr(),
      
      # Dynamic UI: This area will change based on the distribution selected
      uiOutput("dynamic_params")
    ),
    
    mainPanel(
      # Output: Histogram
      plotOutput("dist_plot"),
      
      tags$hr(),
      h4("Summary Statistics"),
      
      # Output: Summary Table
      tableOutput("summary_table")
    )
  )
)

# --- 2. Server Logic ---
server <- function(input, output, session) {
  
  # Reactive UI: Render input fields based on distribution choice
  output$dynamic_params <- renderUI({
    dist <- input$dist
    
    if (dist == "Normal") {
      tagList(
        numericInput("mu", "Mean (\U03BC):", value = 0),
        numericInput("sigma", "Standard Deviation (\U03C3):", value = 1, min = 0.0001)
      )
    } else if (dist == "Binomial") {
      tagList(
        numericInput("size", "Number of Trials:", value = 10, min = 1, step = 1),
        numericInput("prob", "Probability of Success (p):", value = 0.5, min = 0, max = 1, step = 0.1)
      )
    } else if (dist == "Geometric") {
      tagList(
        numericInput("prob", "Probability of Success (p):", value = 0.5, min = 0.0001, max = 1, step = 0.1)
      )
    } else if (dist == "Poisson") {
      tagList(
        numericInput("lambda", "Rate (\U03BB):", value = 5, min = 0)
      )
    } else if (dist == "Uniform") {
      tagList(
        numericInput("min", "Minimum:", value = 0),
        numericInput("max", "Maximum:", value = 10)
      )
    } else if (dist == "Lognormal") {
      tagList(
        numericInput("meanlog", "Mean of Log:", value = 0),
        numericInput("sdlog", "SD of Log:", value = 1, min = 0.0001)
      )
    }
  })
  
  # Reactive Data: Generate random sample based on inputs
  sampled_data <- reactive({
    # Require the basic inputs to be ready
    req(input$n, input$dist)
    n <- as.numeric(input$n)
    dist <- input$dist
    
    # Generate data conditionally, ensuring parameters are loaded first
    if (dist == "Normal") {
      req(input$mu, input$sigma)
      rnorm(n, mean = input$mu, sd = input$sigma)
      
    } else if (dist == "Binomial") {
      req(input$size, input$prob)
      rbinom(n, size = input$size, prob = input$prob)
      
    } else if (dist == "Geometric") {
      req(input$prob)
      rgeom(n, prob = input$prob)
      
    } else if (dist == "Poisson") {
      req(input$lambda)
      rpois(n, lambda = input$lambda)
      
    } else if (dist == "Uniform") {
      req(input$min, input$max)
      runif(n, min = input$min, max = input$max)
      
    } else if (dist == "Lognormal") {
      req(input$meanlog, input$sdlog)
      rlnorm(n, meanlog = input$meanlog, sdlog = input$sdlog)
    }
  })
  
  # Render the Histogram
  output$dist_plot <- renderPlot({
    # Get the generated data
    data <- sampled_data() 
    
    hist(data, 
         main = paste("Histogram of", input$dist, "Distribution"),
         xlab = "Value", 
         col = "steelblue", 
         border = "white",
         breaks = 15) # Adjust breaks for better bin sizing at small sample sizes
  })
  
  # Render the Summary Table
  output$summary_table <- renderTable({
    data <- sampled_data()
    
    # Calculate the five-number summary + mean and SD
    summary_df <- data.frame(
      Statistic = c("Mean", "Standard Deviation", "Minimum", "1st Quartile (Q1)", "Median", "3rd Quartile (Q3)", "Maximum"),
      Value = c(
        mean(data),
        sd(data),
        min(data),
        quantile(data, 0.25),
        median(data),
        quantile(data, 0.75),
        max(data)
      )
    )
    return(summary_df)
  }, digits = 4) # Round to 4 decimal places for cleanliness
}

# --- 3. Run the App ---
shinyApp(ui = ui, server = server)
