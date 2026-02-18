library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "Advanced Distribution Calculator",
  theme = bs_theme(bootswatch = "yeti"),
  
  sidebar = sidebar(
    # 1. Distribution Selection
    selectInput("dist", "Select Distribution:",
                choices = list(
                  "Continuous" = c("Normal", "t", "F", "Exponential", "Uniform", "Beta"),
                  "Discrete" = c("Binomial", "Poisson", "Negative Binomial", "Geometric")
                )),
    
    hr(),
    # 2. Dynamic Parameters
    uiOutput("params_ui"),
    
    hr(),
    # 3. Calculation Logic
    radioButtons("calc_type", "Calculation Type:",
                 choices = list("Find Probability P(X)" = "prob", 
                                "Find Quantile (x)" = "quant")),
    
    numericInput("val", "Input Value (x or p):", value = 0.5, step = 0.1),
    
    # 4. Tail Switch
    radioButtons("tail", "Tail Direction:",
                 choices = list("Lower Tail (P ≤ x)" = "lower", 
                                "Upper Tail (P > x)" = "upper"))
  ),
  
  # Results and Plotting
  layout_column_wrap(
    width = 1,
    card(
      card_header("Statistical Result"),
      div(style = "font-size: 2rem; color: #2c3e50; text-align: center; font-weight: bold;",
          textOutput("result_text"))
    ),
    card(
      card_header("Distribution Visualization"),
      plotOutput("dist_plot", height = "400px")
    )
  )
)

server <- function(input, output, session) {
  
  # Generate UI based on selection
  output$params_ui <- renderUI({
    d <- input$dist
    if (d == "Normal") {
      tagList(numericInput("mu", "Mean (μ):", 0), numericInput("sigma", "SD (σ):", 1, min = 0.1))
    } else if (d == "t") {
      numericInput("df", "Deg. Freedom (v):", 10, min = 1)
    } else if (d == "F") {
      tagList(numericInput("df1", "df1:", 10, min = 1), numericInput("df2", "df2:", 10, min = 1))
    } else if (d == "Exponential") {
      numericInput("rate", "Rate (λ):", 1, min = 0.01)
    } else if (d == "Uniform") {
      tagList(numericInput("min", "Min (a):", 0), numericInput("max", "Max (b):", 1))
    } else if (d == "Beta") {
      tagList(numericInput("s1", "Shape 1 (α):", 2), numericInput("s2", "Shape 2 (β):", 2))
    } else if (d == "Binomial") {
      tagList(numericInput("size", "Trials (n):", 20, min = 1), numericInput("p_b", "Prob (p):", 0.5, min=0, max=1))
    } else if (d == "Poisson") {
      numericInput("lam", "Lambda (λ):", 5, min = 0)
    } else if (d == "Negative Binomial") {
      tagList(numericInput("nb_size", "Successes (r):", 5), numericInput("nb_p", "Prob (p):", 0.5, min=0, max=1))
    } else if (d == "Geometric") {
      numericInput("g_p", "Prob (p):", 0.5, min=0, max=1)
    }
  })
  
  # Core Calculation
  calc_val <- reactive({
    req(input$val)
    d <- input$dist
    v <- input$val
    lt <- (input$tail == "lower")
    is_q <- (input$calc_type == "quant")
    
    tryCatch({
      if (d == "Normal")      { if(is_q) qnorm(v, input$mu, input$sigma, lower.tail=lt) else pnorm(v, input$mu, input$sigma, lower.tail=lt) }
      else if (d == "t")      { if(is_q) qt(v, input$df, lower.tail=lt) else pt(v, input$df, lower.tail=lt) }
      else if (d == "F")      { if(is_q) qf(v, input$df1, input$df2, lower.tail=lt) else pf(v, input$df1, input$df2, lower.tail=lt) }
      else if (d == "Exponential") { if(is_q) qexp(v, input$rate, lower.tail=lt) else pexp(v, input$rate, lower.tail=lt) }
      else if (d == "Uniform") { if(is_q) qunif(v, input$min, input$max, lower.tail=lt) else punif(v, input$min, input$max, lower.tail=lt) }
      else if (d == "Beta")    { if(is_q) qbeta(v, input$s1, input$s2, lower.tail=lt) else pbeta(v, input$s1, input$s2, lower.tail=lt) }
      else if (d == "Binomial") { if(is_q) qbinom(v, input$size, input$p_b, lower.tail=lt) else pbinom(v, input$size, input$p_b, lower.tail=lt) }
      else if (d == "Poisson")  { if(is_q) qpois(v, input$lam, lower.tail=lt) else ppois(v, input$lam, lower.tail=lt) }
      else if (d == "Negative Binomial") { if(is_q) qnbinom(v, input$nb_size, input$nb_p, lower.tail=lt) else pnbinom(v, input$nb_size, input$nb_p, lower.tail=lt) }
      else if (d == "Geometric") { if(is_q) qgeom(v, input$g_p, lower.tail=lt) else pgeom(v, input$g_p, lower.tail=lt) }
    }, error = function(e) return(NA))
  })
  
  output$result_text <- renderText({
    res <- calc_val()
    if (is.na(res)) return("Invalid Calculation")
    label <- if(input$calc_type == "prob") "P = " else "x = "
    paste0(label, round(res, 5))
  })
  
  # Advanced Plotting
  output$dist_plot <- renderPlot({
    res <- calc_val()
    d <- input$dist
    req(!is.na(res))
    
    # Logic to determine x-range and density function
    is_discrete <- d %in% c("Binomial", "Poisson", "Negative Binomial", "Geometric")
    
    if (!is_discrete) {
      # Continuous Plotting
      x_min <- if(d=="Normal") input$mu - 4*input$sigma else if(d=="Uniform") input$min - 0.5 else 0
      x_max <- if(d=="Normal") input$mu + 4*input$sigma else if(d=="Uniform") input$max + 0.5 else if(d=="Beta") 1 else 10
      
      x_seq <- seq(x_min, x_max, length.out = 300)
      y_seq <- switch(d,
                      "Normal" = dnorm(x_seq, input$mu, input$sigma),
                      "t" = dt(x_seq, input$df),
                      "F" = df(x_seq, input$df1, input$df2),
                      "Exponential" = dexp(x_seq, input$rate),
                      "Uniform" = dunif(x_seq, input$min, input$max),
                      "Beta" = dbeta(x_seq, input$s1, input$s2)
      )
      
      plot(x_seq, y_seq, type="l", lwd=2, col="#2c3e50", xlab="x", ylab="Density", main=paste(d, "Distribution"))
      
      # Shading the tail
      x_crit <- if(input$calc_type == "prob") input$val else res
      if(input$tail == "lower") {
        poly_x <- x_seq[x_seq <= x_crit]
        poly_y <- y_seq[x_seq <= x_crit]
      } else {
        poly_x <- x_seq[x_seq >= x_crit]
        poly_y <- y_seq[x_seq >= x_crit]
      }
      polygon(c(min(poly_x), poly_x, max(poly_x)), c(0, poly_y, 0), col=rgb(0.2, 0.6, 0.8, 0.5), border=NA)
    } else {
      # Discrete Plotting
      x_max_d <- if(d=="Binomial") input$size else 20
      x_vals <- 0:x_max_d
      y_vals <- switch(d,
                       "Binomial" = dbinom(x_vals, input$size, input$p_b),
                       "Poisson" = dpois(x_vals, input$lam),
                       "Negative Binomial" = dnbinom(x_vals, input$nb_size, input$nb_p),
                       "Geometric" = dgeom(x_vals, input$g_p)
      )
      
      cols <- rep("#bdc3c7", length(x_vals))
      x_crit <- if(input$calc_type == "prob") input$val else res
      if(input$tail == "lower") cols[x_vals <= x_crit] <- "#3498db"
      else cols[x_vals > x_crit] <- "#3498db"
      
      barplot(y_vals, names.arg=x_vals, col=cols, border="white", main=paste(d, "PMF"), xlab="k", ylab="P(X=k)")
    }
  })
}

shinyApp(ui, server)