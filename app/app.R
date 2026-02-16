library(shiny)

# --- 1. User Interface (UI) ---
ui <- fluidPage(
  
  titlePanel("Statistical Distributions Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown for Sample Size
      selectInput("n", 
                  "Sample Size (n):", 
                  choices = c(15, 30, 50, 100, 500, 1000), # Added larger sizes to better see the curves!
                  selected = 100),
      
      # Dropdown for Distribution Type
      selectInput("dist", 
                  "Distribution:", 
                  choices = c("Normal", "Binomial", "Geometric", "Poisson", "Uniform", "Lognormal"),
                  selected = "Normal"),
      
      # NEW: Slider for number of Histogram Bins
      sliderInput("bins",
                  "Number of Bins:",
                  min = 5,
                  max = 50,
                  value = 15),
      
      tags$hr(),
      
      # Dynamic UI: Changes based on distribution
      uiOutput("dynamic_params")
    ),
    
    mainPanel(
      # Output: Histogram with Overlay
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
  
  # Reactive Data: Generate random sample
  sampled_data <- reactive({
    req(input$n, input$dist)
    n <- as.numeric(input$n)
    dist <- input$dist
    
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
  
  # Render the Histogram with Overlay
  output$dist_plot <- renderPlot({
    data <- sampled_data() 
    dist <- input$dist
    
    # 1. Plot the Histogram (freq = FALSE scales it to density)
    hist(data, 
         main = paste("Histogram of", dist, "Distribution"),
         xlab = "Value", 
         col = "steelblue", 
         border = "white",
         breaks = input$bins, 
         freq = FALSE) # CRITICAL: This allows the density overlay to fit
    
    # 2. Overlay the Theoretical Density / Mass Function
    if (dist == "Normal") {
      req(input$mu, input$sigma)
      curve(dnorm(x, mean = input$mu, sd = input$sigma), 
            add = TRUE, col = "darkorange", lwd = 3)
      
    } else if (dist == "Uniform") {
      req(input$min, input$max)
      curve(dunif(x, min = input$min, max = input$max), 
            add = TRUE, col = "darkorange", lwd = 3)
      
    } else if (dist == "Lognormal") {
      req(input$meanlog, input$sdlog)
      curve(dlnorm(x, meanlog = input$meanlog, sdlog = input$sdlog), 
            add = TRUE, col = "darkorange", lwd = 3)
      
    } else {
      # Discrete distributions: Use points and lines (PMF) instead of a continuous curve
      x_vals <- min(data):max(data)
      
      if (dist == "Binomial") {
        req(input$size, input$prob)
        y_vals <- dbinom(x_vals, size = input$size, prob = input$prob)
      } else if (dist == "Geometric") {
        req(input$prob)
        y_vals <- dgeom(x_vals, prob = input$prob)
      } else if (dist == "Poisson") {
        req(input$lambda)
        y_vals <- dpois(x_vals, lambda = input$lambda)
      }
      
      # Add "lollipop" style markers for discrete probabilities
      points(x_vals, y_vals, col = "darkorange", pch = 16, cex = 1.5)
      lines(x_vals, y_vals, col = "darkorange", lwd = 3, type = "h")
    }
  })
  
  # Render the Summary Table
  output$summary_table <- renderTable({
    data <- sampled_data()
    
    summary_df <- data.frame(
      Statistic = c("Mean", "Standard Deviation", "Minimum", "1st Quartile (Q1)", "Median", "3rd Quartile (Q3)", "Maximum"),
      Value = c(mean(data), sd(data), min(data), quantile(data, 0.25), median(data), quantile(data, 0.75), max(data))
    )
    return(summary_df)
  }, digits = 4)
}

# --- 3. Run the App ---
shinyApp(ui = ui, server = server)