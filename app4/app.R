library(shiny)

# --- 1. User Interface (UI) ---
ui <- fluidPage(
  
  titlePanel("Statistical Distributions Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown for Sample Size
      selectInput("n", 
                  "Sample Size (n):", 
                  choices = c(15, 30, 50, 100, 500, 1000), 
                  selected = 100),
      
      # Dropdown for Distribution Type
      selectInput("dist", 
                  "Distribution:", 
                  choices = c("Normal", "Binomial", "Geometric", "Negative Binomial", "Poisson", "Uniform", "Lognormal"),
                  selected = "Normal"),
      
      # Slider for number of Histogram Bins
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
      # Output: Side-by-side plots
      plotOutput("dist_plot", height = "400px"),
      
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
        numericInput("geom_prob", "Probability of Success (p):", value = 0.5, min = 0.0001, max = 1, step = 0.1)
      )
    } else if (dist == "Negative Binomial") {
      tagList(
        numericInput("nbinom_size", "Target Number of Successes (r):", value = 5, min = 1, step = 1),
        numericInput("nbinom_prob", "Probability of Success (p):", value = 0.5, min = 0.0001, max = 1, step = 0.1)
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
      req(input$geom_prob)
      rgeom(n, prob = input$geom_prob)
    } else if (dist == "Negative Binomial") {
      req(input$nbinom_size, input$nbinom_prob)
      rnbinom(n, size = input$nbinom_size, prob = input$nbinom_prob)
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
  
  # Render the Side-by-Side Plots
  output$dist_plot <- renderPlot({
    data <- sampled_data() 
    dist <- input$dist
    
    # --- 1. Establish consistent X-axis limits ---
    x_min <- min(data)
    x_max <- max(data)
    
    if (dist == "Normal") {
      req(input$mu, input$sigma)
      x_min <- min(x_min, input$mu - 3.5 * input$sigma)
      x_max <- max(x_max, input$mu + 3.5 * input$sigma)
    } else if (dist == "Binomial") {
      req(input$size)
      x_min <- 0
      x_max <- input$size
    } else if (dist == "Geometric") {
      req(input$geom_prob)
      x_min <- 0
      x_max <- max(x_max, qgeom(0.99, input$geom_prob))
    } else if (dist == "Negative Binomial") {
      req(input$nbinom_size, input$nbinom_prob)
      x_min <- 0
      x_max <- max(x_max, qnbinom(0.99, size = input$nbinom_size, prob = input$nbinom_prob))
    } else if (dist == "Poisson") {
      req(input$lambda)
      x_min <- 0
      x_max <- max(x_max, qpois(0.999, lambda = input$lambda))
    } else if (dist == "Uniform") {
      req(input$min, input$max)
      x_min <- input$min
      x_max <- input$max
    } else if (dist == "Lognormal") {
      req(input$meanlog, input$sdlog)
      x_min <- 0
      x_max <- max(x_max, qlnorm(0.98, meanlog = input$meanlog, sdlog = input$sdlog))
    }
    
    # --- 2. Calculate Empirical & Theoretical Data for Y-axis Limits ---
    hist_info <- hist(data, breaks = input$bins, plot = FALSE)
    max_hist_y <- max(hist_info$density)
    
    if (dist %in% c("Normal", "Uniform", "Lognormal")) {
      # Theoretical Continuous PDF
      x_vals_theo <- seq(x_min, x_max, length.out = 500)
      if (dist == "Normal") y_vals_theo <- dnorm(x_vals_theo, mean = input$mu, sd = input$sigma)
      else if (dist == "Uniform") y_vals_theo <- dunif(x_vals_theo, min = input$min, max = input$max)
      else if (dist == "Lognormal") y_vals_theo <- dlnorm(x_vals_theo, meanlog = input$meanlog, sdlog = input$sdlog)
      
      # Empirical Continuous Density (KDE)
      emp_dens <- density(data)
      max_emp_y <- max(emp_dens$y)
      
    } else {
      # Theoretical Discrete PMF
      x_vals_theo <- max(0, floor(x_min)):ceiling(x_max)
      if (dist == "Binomial") y_vals_theo <- dbinom(x_vals_theo, size = input$size, prob = input$prob)
      else if (dist == "Geometric") y_vals_theo <- dgeom(x_vals_theo, prob = input$geom_prob)
      else if (dist == "Negative Binomial") y_vals_theo <- dnbinom(x_vals_theo, size = input$nbinom_size, prob = input$nbinom_prob)
      else if (dist == "Poisson") y_vals_theo <- dpois(x_vals_theo, lambda = input$lambda)
      
      # Empirical Discrete PMF (Sample Proportions)
      emp_table <- prop.table(table(data))
      emp_x <- as.numeric(names(emp_table))
      emp_y <- as.numeric(emp_table)
      max_emp_y <- max(emp_y)
    }
    
    max_theo_y <- max(y_vals_theo)
    y_max_combined <- max(max_hist_y, max_theo_y, max_emp_y) * 1.1 # Envelope all 3 maxima
    
    # Set up plotting area for 1 row, 2 columns
    par(mfrow = c(1, 2), mar = c(5, 4, 4, 2) + 0.1)
    
    # --- PLOT 1: Empirical Sample Histogram + Empirical Overlay ---
    hist(data, 
         main = paste("Sample & Empirical Density (n =", input$n, ")"),
         xlab = "Value", 
         ylab = "Density / Proportion",
         col = "steelblue", 
         border = "white",
         breaks = input$bins, 
         xlim = c(x_min, x_max),
         ylim = c(0, y_max_combined),
         freq = FALSE) 
    
    # Draw the Empirical overlay
    if (dist %in% c("Normal", "Uniform", "Lognormal")) {
      lines(emp_dens, col = "forestgreen", lwd = 3) # Using green to distinguish empirical
    } else {
      points(emp_x, emp_y, col = "forestgreen", pch = 16, cex = 1.5)
      lines(emp_x, emp_y, col = "forestgreen", lwd = 3, type = "h")
    }
    
    # --- PLOT 2: Theoretical PDF / PMF ---
    if (dist %in% c("Normal", "Uniform", "Lognormal")) {
      plot(x_vals_theo, y_vals_theo, type = "l", col = "darkorange", lwd = 3,
           main = "Theoretical PDF (\U221E Sample)",
           xlab = "Value", ylab = "Probability Density",
           xlim = c(x_min, x_max), ylim = c(0, y_max_combined))
      
    } else {
      plot(x_vals_theo, y_vals_theo, type = "h", col = "darkorange", lwd = 3,
           main = "Theoretical PMF (\U221E Sample)",
           xlab = "Value", ylab = "Probability Mass",
           xlim = c(x_min, x_max), ylim = c(0, y_max_combined))
      points(x_vals_theo, y_vals_theo, col = "darkorange", pch = 16, cex = 1.5)
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