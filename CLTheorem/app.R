#| standalone: true
#| viewerHeight: 900
library(shiny)

ui <- fluidPage(
  titlePanel("Central Limit Theorem: Base R Edition"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "1. Choose Population Distribution:",
                  choices = c("Normal", "Exponential", "Beta", "Lognormal", 
                              "Binomial", "Poisson", "Geometric", 
                              "Negative Binomial", "Chi-square", "F")),
      
      # Dynamic UI for parameters
      uiOutput("dist_params"),
      
      hr(),
      
      sliderInput("n", "2. Sample Size (n):", min = 1, max = 100, value = 30),
      sliderInput("n_samples", "3. Number of Samples:", min = 100, max = 5000, value = 1000, step = 100),
      sliderInput("bins", "4. Histogram Bins:", min = 5, max = 100, value = 30),
      
      actionButton("resample", "Generate New Samples", class = "btn-primary", style="width: 100%;")
    ),
    
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("samplingPlot"),
      wellPanel(
        h4("Statistical Summary"),
        verbatimTextOutput("stats")
      )
    )
  )
)

server <- function(input, output) {
  
  # Reactive UI for distribution-specific parameters
  output$dist_params <- renderUI({
    switch(input$dist,
           "Normal" = tagList(numericInput("mean", "Mean", 0), numericInput("sd", "SD", 1, min = 0.1)),
           "Exponential" = numericInput("rate", "Rate (lambda)", 1, min = 0.1),
           "Beta" = tagList(numericInput("shape1", "Shape 1", 2), numericInput("shape2", "Shape 2", 5)),
           "Lognormal" = tagList(numericInput("logmean", "Meanlog", 0), numericInput("logsd", "SDlog", 1)),
           "Binomial" = tagList(numericInput("size", "Trials", 10), numericInput("prob", "Prob", 0.5, step = 0.1)),
           "Poisson" = numericInput("lambda", "Lambda", 4),
           "Geometric" = numericInput("g_prob", "Prob", 0.2),
           "Negative Binomial" = tagList(numericInput("nb_size", "Target Successes", 5), numericInput("nb_prob", "Prob", 0.5)),
           "Chi-square" = numericInput("df", "Degrees of Freedom", 3),
           "F" = tagList(numericInput("df1", "DF 1", 10), numericInput("df2", "DF 2", 20))
    )
  })
  
  # Generate the data matrix (rows = n, columns = n_samples)
  get_data <- reactive({
    input$resample # Trigger on button click
    n <- input$n
    reps <- input$n_samples
    
    samples <- replicate(reps, {
      switch(input$dist,
             "Normal" = rnorm(n, req(input$mean), req(input$sd)),
             "Exponential" = rexp(n, req(input$rate)),
             "Beta" = rbeta(n, req(input$shape1), req(input$shape2)),
             "Lognormal" = rlnorm(n, req(input$logmean), req(input$logsd)),
             "Binomial" = rbinom(n, req(input$size), req(input$prob)),
             "Poisson" = rpois(n, req(input$lambda)),
             "Geometric" = rgeom(n, req(input$g_prob)),
             "Negative Binomial" = rnbinom(n, req(input$nb_size), req(input$nb_prob)),
             "Chi-square" = rchisq(n, req(input$df)),
             "F" = rf(n, req(input$df1), req(input$df2))
      )
    })
    return(samples)
  })
  
  # Top Plot: Population Distribution (Single observations)
  output$distPlot <- renderPlot({
    raw_data <- as.vector(get_data())
    hist(raw_data, breaks = input$bins, col = "skyblue", border = "white",
         main = "Population Distribution", 
         xlab = "Value of Individual Observations",
         cex.main = 1.5)
  })
  
  # Bottom Plot: Sampling Distribution with Normal Overlay
  output$samplingPlot <- renderPlot({
    means <- colMeans(get_data())
    
    # Calculate histogram
    h <- hist(means, breaks = input$bins, plot = FALSE)
    
    # Plot histogram with probability=TRUE to allow density overlay
    hist(means, breaks = input$bins, col = "orange", border = "white",
         prob = TRUE, main = "Sampling Distribution of the Mean",
         xlab = "Sample Mean Value", cex.main = 1.5)
    
    # Add Theoretical Normal Curve
    x_fit <- seq(min(means), max(means), length = 100)
    y_fit <- dnorm(x_fit, mean = mean(means), sd = sd(means))
    lines(x_fit, y_fit, col = "black", lwd = 3)
    
    # Add Mean line
    abline(v = mean(means), col = "red", lwd = 2, lty = 2)
    
    legend("topright", legend=c("Observed Means", "Normal Curve"),
           fill=c("orange", NA), border=c("white", NA), 
           lty=c(NA, 1), lwd=c(NA, 3), bty="n")
  })
  
  # Summary Statistics
  output$stats <- renderPrint({
    means <- colMeans(get_data())
    cat("Summary of Sample Means:\n")
    print(summary(means))
    cat("\nStandard Error (SD of Means):", round(sd(means), 4))
  })
}

shinyApp(ui, server)