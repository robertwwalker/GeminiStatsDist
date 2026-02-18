#| standalone: true
#| viewerHeight: 800
library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Central Limit Theorem Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Population Distribution:",
                  choices = c("Normal", "Exponential", "Beta", "Lognormal", 
                              "Binomial", "Poisson", "Geometric", 
                              "Negative Binomial", "Chi-square", "F")),
      uiOutput("dist_params"),
      hr(),
      sliderInput("n", "Sample Size (n):", min = 1, max = 100, value = 30),
      sliderInput("n_samples", "Number of Samples:", min = 100, max = 5000, value = 1000, step = 100),
      actionButton("resample", "Generate New Samples", class = "btn-primary")
    ),
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("samplingPlot"),
      verbatimTextOutput("stats")
    )
  )
)

server <- function(input, output) {
  
  # Dynamic UI for distribution parameters
  output$dist_params <- renderUI({
    switch(input$dist,
           "Normal" = tagList(numericInput("mean", "Mean", 0), numericInput("sd", "SD", 1, min = 0.1)),
           "Exponential" = numericInput("rate", "Rate (lambda)", 1, min = 0.1),
           "Beta" = tagList(numericInput("shape1", "Shape 1", 2), numericInput("shape2", "Shape 2", 5)),
           "Lognormal" = tagList(numericInput("logmean", "Meanlog", 0), numericInput("logsd", "SDlog", 1)),
           "Binomial" = tagList(numericInput("size", "Size (Trials)", 10), numericInput("prob", "Probability", 0.5, step = 0.1)),
           "Poisson" = numericInput("lambda", "Lambda", 4),
           "Geometric" = numericInput("g_prob", "Probability", 0.2),
           "Negative Binomial" = tagList(numericInput("nb_size", "Target Successes", 5), numericInput("nb_prob", "Prob", 0.5)),
           "Chi-square" = numericInput("df", "Degrees of Freedom", 3),
           "F" = tagList(numericInput("df1", "DF 1", 10), numericInput("df2", "DF 2", 20))
    )
  })
  
  # Reactive logic to generate data
  get_data <- reactive({
    input$resample
    n <- input$n
    reps <- input$n_samples
    
    # Generate the samples based on selection
    samples <- replicate(reps, {
      switch(input$dist,
             "Normal" = rnorm(n, input$mean, input$sd),
             "Exponential" = rexp(n, input$rate),
             "Beta" = rbeta(n, input$shape1, input$shape2),
             "Lognormal" = rlnorm(n, input$logmean, input$logsd),
             "Binomial" = rbinom(n, input$size, input$prob),
             "Poisson" = rpois(n, input$lambda),
             "Geometric" = rgeom(n, input$g_prob),
             "Negative Binomial" = rnbinom(n, input$nb_size, input$nb_prob),
             "Chi-square" = rchisq(n, input$df),
             "F" = rf(n, input$df1, input$df2)
      )
    })
    
    if(n == 1) return(matrix(samples, nrow=1))
    return(samples)
  })
  
  # Top Plot: Population Distribution
  output$distPlot <- renderPlot({
    data <- as.vector(get_data())
    ggplot(data.frame(x = data), aes(x = x)) +
      geom_histogram(fill = "steelblue", color = "white", bins = 30) +
      labs(title = paste("Population Distribution (Individual Observations)"),
           subtitle = "This shows the shape of the data you are sampling from.") +
      theme_minimal()
  })
  
# Bottom Plot: Sampling Distribution with Normal Overlay
  output$samplingPlot <- renderPlot({
    # Calculate means of each sample
    sample_means <- colMeans(get_data())
    df_means <- data.frame(x = sample_means)
    
    # Calculate parameters for the overlay curve
    m_means <- mean(sample_means)
    s_means <- sd(sample_means)
    
    ggplot(df_means, aes(x = x)) +
      # Use density on y-axis to match the Normal curve scale
      geom_histogram(aes(y = ..density..), fill = "darkorange", 
                     color = "white", bins = 30, alpha = 0.7) +
      # Add the Theoretical Normal Curve
      stat_function(fun = dnorm, 
                    args = list(mean = m_means, sd = s_means), 
                    color = "black", size = 1.2) +
      geom_vline(xintercept = m_means, linetype = "dashed", color = "red") +
      labs(title = "Sampling Distribution of the Mean",
           subtitle = paste("Black line: Normal Distribution N(", 
                            round(m_means, 2), ",", round(s_means, 2), ")"),
           x = "Sample Mean Value",
           y = "Density") +
      theme_minimal()
  })  
  output$stats <- renderPrint({
    means <- colMeans(get_data())
    cat("Theoretical Mean vs Observed Mean:\n")
    cat("Mean of Sample Means:", round(mean(means), 4), "\n")
    cat("SD of Sample Means (Standard Error):", round(sd(means), 4), "\n")
  })
}

shinyApp(ui, server)