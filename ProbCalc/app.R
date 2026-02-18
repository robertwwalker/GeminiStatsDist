library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "Statistical Distribution Workbench",
  theme = bs_theme(preset = "morph", primary = "#2c3e50", version=5),
  
  sidebar = sidebar(
    # 1. Distribution Selection
    selectInput("dist", "Distribution:",
                choices = list(
                  "Continuous" = c("Normal", "t", "F", "Exponential", "Uniform", "Beta"),
                  "Discrete" = c("Binomial", "Poisson", "Negative Binomial", "Geometric")
                )),
    
    uiOutput("params_ui"),
    hr(),
    
    # 2. Calculation Logic
    radioButtons("calc_type", "Objective:", 
                 choices = list("Find Probability" = "prob", "Find Quantile" = "quant")),
    
    selectInput("tail_mode", "Interval Type:", 
                choices = c("One-tailed", "Two-tailed")),
    
    uiOutput("val_inputs"),
    
    conditionalPanel(
      condition = "input.tail_mode == 'One-tailed'",
      radioButtons("tail_dir", "Direction:",
                   choices = list("Lower Tail (≤)" = "lower", "Upper Tail (>)" = "upper"))
    )
  ),
  
  # Main Layout
  layout_column_wrap(
    width = 1/2,
    card(
      card_header("Statistical Claim"),
      div(style = "font-size: 1.4rem; padding: 10px; color: #2c3e50; font-family: 'Courier New', monospace;",
          textOutput("claim_text"))
    ),
    card(
      card_header("Distribution Summary Stats"),
      tableOutput("summary_table")
    ),
    card(
      full_screen = TRUE,
      card_header("Visualization"),
      plotOutput("dist_plot")
    )
  )
)

server <- function(input, output, session) {
  
  # --- Dynamic Inputs ---
  output$params_ui <- renderUI({
    d <- input$dist
    if (d == "Normal") tagList(numericInput("mu", "Mean (μ):", 0), numericInput("sigma", "SD (σ):", 1, min = 0.01))
    else if (d == "t") numericInput("df", "df:", 10, min = 1)
    else if (d == "F") tagList(numericInput("df1", "df1:", 10, min = 1), numericInput("df2", "df2:", 10, min = 1))
    else if (d == "Exponential") numericInput("rate", "Rate (λ):", 1, min = 0.01)
    else if (d == "Uniform") tagList(numericInput("min", "Min (a):", 0), numericInput("max", "Max (b):", 1))
    else if (d == "Beta") tagList(numericInput("s1", "Shape 1 (α):", 2), numericInput("s2", "Shape 2 (β):", 2))
    else if (d == "Binomial") tagList(numericInput("size", "n:", 20), numericInput("p_b", "p:", 0.5, min=0, max=1))
    else if (d == "Poisson") numericInput("lam", "λ:", 5)
    else if (d == "Negative Binomial") tagList(numericInput("nb_r", "Successes (r):", 5), numericInput("nb_p", "Prob (p):", 0.5))
    else if (d == "Geometric") numericInput("g_p", "Prob (p):", 0.5)
  })
  
  output$val_inputs <- renderUI({
    label <- if(input$calc_type == "prob") "Value (x)" else "Prob (p)"
    if (input$tail_mode == "One-tailed") {
      numericInput("v1", label, value = 0.5)
    } else {
      tagList(
        numericInput("v1", paste("Lower", label), value = 0.25),
        numericInput("v2", paste("Upper", label), value = 0.75)
      )
    }
  })
  
  # --- Helpers for Prob/Quantile ---
  dist_func <- function(type, val, lower_tail = TRUE) {
    d <- input$dist
    if (type == "p") {
      switch(d,
             "Normal" = pnorm(val, input$mu, input$sigma, lower.tail=lower_tail),
             "t" = pt(val, input$df, lower.tail=lower_tail),
             "F" = pf(val, input$df1, input$df2, lower.tail=lower_tail),
             "Exponential" = pexp(val, input$rate, lower.tail=lower_tail),
             "Uniform" = punif(val, input$min, input$max, lower.tail=lower_tail),
             "Beta" = pbeta(val, input$s1, input$s2, lower.tail=lower_tail),
             "Binomial" = pbinom(val, input$size, input$p_b, lower.tail=lower_tail),
             "Poisson" = ppois(val, input$lam, lower.tail=lower_tail),
             "Negative Binomial" = pnbinom(val, input$nb_r, input$nb_p, lower.tail=lower_tail),
             "Geometric" = pgeom(val, input$g_p, lower.tail=lower_tail)
      )
    } else {
      switch(d,
             "Normal" = qnorm(val, input$mu, input$sigma, lower.tail=lower_tail),
             "t" = qt(val, input$df, lower.tail=lower_tail),
             "F" = qf(val, input$df1, input$df2, lower.tail=lower_tail),
             "Exponential" = qexp(val, input$rate, lower.tail=lower_tail),
             "Uniform" = qunif(val, input$min, input$max, lower.tail=lower_tail),
             "Beta" = qbeta(val, input$s1, input$s2, lower.tail=lower_tail),
             "Binomial" = qbinom(val, input$size, input$p_b, lower.tail=lower_tail),
             "Poisson" = qpois(val, input$lam, lower.tail=lower_tail),
             "Negative Binomial" = qnbinom(val, input$nb_r, input$nb_p, lower.tail=lower_tail),
             "Geometric" = qgeom(val, input$g_p, lower.tail=lower_tail)
      )
    }
  }
  
  # --- Core Reactive Calculation ---
  calc_res <- reactive({
    req(input$v1)
    is_prob <- (input$calc_type == "prob")
    
    if (input$tail_mode == "One-tailed") {
      lt <- (input$tail_dir == "lower")
      res <- dist_func(ifelse(is_prob, "p", "q"), input$v1, lt)
      return(list(res = res, v1 = input$v1, v2 = NA))
    } else {
      req(input$v2)
      validate(need(input$v2 > input$v1, "Upper bound must be greater than lower bound"))
      if (is_prob) {
        res <- dist_func("p", input$v2) - dist_func("p", input$v1)
        return(list(res = res, v1 = input$v1, v2 = input$v2))
      } else {
        res1 <- dist_func("q", input$v1)
        res2 <- dist_func("q", input$v2)
        return(list(res = paste(round(res1, 3), "to", round(res2, 3)), v1 = res1, v2 = res2))
      }
    }
  })
  
  # --- Summary Statistics Logic ---
  output$summary_table <- renderTable({
    d <- input$dist
    stats <- list(Mean = NA, SD = NA, Var = NA, Median = NA)
    
    try({
      if (d == "Normal") {
        stats$Mean <- input$mu; stats$SD <- input$sigma; stats$Var <- input$sigma^2; stats$Median <- input$mu
      } else if (d == "t") {
        stats$Mean <- if(input$df > 1) 0 else "Undefined"; stats$Var <- if(input$df > 2) input$df/(input$df-2) else "Infinite"; stats$Median <- 0
      } else if (d == "Exponential") {
        stats$Mean <- 1/input$rate; stats$Var <- 1/input$rate^2; stats$Median <- log(2)/input$rate
      } else if (d == "Uniform") {
        stats$Mean <- (input$min+input$max)/2; stats$Var <- (input$max-input$min)^2/12; stats$Median <- stats$Mean
      } else if (d == "Binomial") {
        stats$Mean <- input$size * input$p_b; stats$Var <- stats$Mean * (1-input$p_b); stats$Median <- qbinom(0.5, input$size, input$p_b)
      } else if (d == "Beta") {
        stats$Mean <- input$s1/(input$s1 + input$s2); stats$Var <- (input$s1*input$s2)/((input$s1+input$s2+1)*(input$s1+input$s2)^2); stats$Median <- (input$s1 - (1/3))/(input$s1+input$s2-(2/3))
      } else if (d == "Poisson") {
        stats$Mean <- input$lam; stats$Var <- input$lam; stats$Median <- qpois(0.5, input$lam)
      }
      if(!is.character(stats$Var)) stats$SD <- sqrt(as.numeric(stats$Var))
    }, silent = TRUE)
    
    data.frame(Statistic = names(stats), Value = as.character(round(unlist(stats), 4)))
  }, colnames = FALSE)
  
  # --- Dynamic Claim Generator ---
  output$claim_text <- renderText({
    out <- calc_res()
    d_name <- input$dist
    
    if (input$calc_type == "prob") {
      if (input$tail_mode == "One-tailed") {
        symb <- if(input$tail_dir == "lower") "≤" else ">"
        paste0("P(X ", symb, " ", input$v1, ") ≈ ", round(out$res, 4))
      } else {
        paste0("P(", input$v1, " < X < ", input$v2, ") ≈ ", round(out$res, 4))
      }
    } else {
      if (input$tail_mode == "One-tailed") {
        symb <- if(input$tail_dir == "lower") "≤" else ">"
        paste0("If P(X ", symb, " x) = ", input$v1, ", then x ≈ ", round(out$res, 4))
      } else {
        paste0("For middle probability between ", input$v1, " and ", input$v2, ", interval ≈ [", round(out$v1, 3), ", ", round(out$v2, 3), "]")
      }
    }
  })
  
  # --- Visualization ---
  output$dist_plot <- renderPlot({
    out <- calc_res()
    d <- input$dist
    is_discrete <- d %in% c("Binomial", "Poisson", "Negative Binomial", "Geometric")
    
    if(!is_discrete) {
      # Custom logic for plotting ranges
      x_min <- if(d=="Normal") input$mu - 4*input$sigma else if(d=="Uniform") input$min-0.2 else 0
      x_max <- if(d=="Normal") input$mu + 4*input$sigma else if(d=="Uniform") input$max+0.2 else if(d=="Beta") 1 else 15
      x_seq <- seq(x_min, x_max, length.out=400)
      y_seq <- switch(d, "Normal"=dnorm(x_seq, input$mu, input$sigma), "t"=dt(x_seq, input$df), "Exponential"=dexp(x_seq, input$rate), "Uniform"=dunif(x_seq, input$min, input$max), "Beta"=dbeta(x_seq, input$s1, input$s2), "F"=df(x_seq, input$df1, input$df2))
      
      plot(x_seq, y_seq, type="l", lwd=3, col="#2c3e50", xlab="x", ylab="f(x)", main=paste(d, "Distribution Density"))
      
      x_crit1 <- if(input$calc_type == "prob") input$v1 else out$v1
      x_crit2 <- if(input$calc_type == "prob") input$v2 else out$v2
      
      if(input$tail_mode == "One-tailed") {
        mask <- if(input$tail_dir == "lower") x_seq <= x_crit1 else x_seq >= x_crit1
      } else {
        mask <- x_seq >= x_crit1 & x_seq <= x_crit2
      }
      polygon(c(min(x_seq[mask]), x_seq[mask], max(x_seq[mask])), c(0, y_seq[mask], 0), col="#18bc9c88", border=NA)
    } else {
      x_max_d <- if(d=="Binomial") input$size else 25
      x_vals <- 0:x_max_d
      y_vals <- switch(d, "Binomial"=dbinom(x_vals, input$size, input$p_b), "Poisson"=dpois(x_vals, input$lam), "Negative Binomial"=dnbinom(x_vals, input$nb_r, input$nb_p), "Geometric"=dgeom(x_vals, input$g_p))
      cols <- rep("#bdc3c7", length(x_vals))
      x_crit1 <- if(input$calc_type == "prob") input$v1 else out$v1
      x_crit2 <- if(input$calc_type == "prob") input$v2 else out$v2
      
      if(input$tail_mode == "One-tailed") {
        if(input$tail_dir == "lower") cols[x_vals <= x_crit1] <- "#18bc9c" else cols[x_vals > x_crit1] <- "#18bc9c"
      } else {
        cols[x_vals >= x_crit1 & x_vals <= x_crit2] <- "#18bc9c"
      }
      barplot(y_vals, names.arg=x_vals, col=cols, border="white", main=paste(d, "PMF"))
    }
  })
}

shinyApp(ui, server)