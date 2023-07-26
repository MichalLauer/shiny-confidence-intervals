function(input, output, session) {

  # Setup global variables
  INVALID <- reactive({invalidateLater(isolate(input$sim_speed)*1000)})
  RUN <- reactiveVal(FALSE)
  CI <- reactiveVal(tibble())

  # {waiter}
  # --------------------------------------------------------------------------
  h1 <- Hostess$new()
  h2 <- Hostess$new()
  h3 <- Hostess$new()
  h4 <- Hostess$new()
  w <- Waiter$new(id = c("plot", "plot_distr", "progress", "info", "samples"),
                  html = list(
                    h1$get_loader(preset = "fan"),
                    h2$get_loader(preset = "fan"),
                    h3$get_loader(preset = "fan"),
                    h4$get_loader(preset = "fan")
                  ))
  # ----------------------------------------------------------------------------

  # {shinyvalidate}
  # --------------------------------------------------------------------------
  iv <- InputValidator$new()
  iv$add_rule("sam_size", sv_gte(1))
  iv$add_rule("alpha", sv_between(0, 1))
  iv$add_rule("sim_speed", sv_gte(0.01))
  iv$add_rule("num_samples", sv_between(1, 1000))
  iv$enable()
  # ----------------------------------------------------------------------------
  #

  # Buttons
  # --------------------------------------------------------------------------
  observeEvent(input$distribution_modal, {
    showModal(modalDialog(
      title = "Distributions",
      p("Popluation distribution is a theoretical distribution that represents",
        " the population from which is being sampled. Currently, only possible",
        " distributions are:"),
      h4("Normal distribution"),
      tags$ul(
        tags$li("Normal()"),
        tags$li("Normal(mean = X)"),
        tags$li("Normal(sd = X)"),
        tags$li("Normal(var = X)"),
        tags$li("Normal(mean = X, sd = Y)"),
        tags$li("Normal(mean = X, var = X)"),
      ),
      tags$em("Not that if a parameter is not specified, N(0,1) is assumed"),
      easyClose = TRUE
    ))
  })
  observeEvent(input$start, {
    req(iv$is_valid())
    # Control
    disable("distribution")
    disable("sam_size")
    disable("var_known")
    disable("alpha")
    disable("sim_speed")
    # Buttons
    disable("start")
    enable("stop")
    enable("pause")
    disable("continue")
    disable("num_samples")
    disable("gen_samples")
    # Other
    CI(tibble())
    sample_info <<- list()
    RUN(TRUE)
  })
  observeEvent(input$stop,  {
    RUN(FALSE)
    # Control
    enable("distribution")
    enable("sam_size")
    enable("var_known")
    enable("alpha")
    enable("sim_speed")
    # Buttons
    enable("start")
    disable("stop")
    disable("pause")
    disable("continue")
    enable("num_samples")
    enable("gen_samples")
  })
  observeEvent(input$pause,  {
    RUN(FALSE)
    # Control
    enable("sim_speed")
    # Buttons
    disable("start")
    enable("stop")
    disable("pause")
    enable("continue")
    enable("num_samples")
    enable("gen_samples")
  })
  observeEvent(input$continue,  {
    RUN(TRUE)
    # Control
    disable("sim_speed")
    # Buttons
    disable("start")
    enable("stop")
    enable("pause")
    disable("continue")
    disable("num_samples")
    disable("gen_samples")
  })
  observeEvent(input$gen_samples,  {
    req(iv$is_valid())
    w$show()
    k <- isolate(input$num_samples)
    generate_ci(iterations = k)
  })
  # ----------------------------------------------------------------------------

  # Function that generates CI
  generate_ci <- function(iterations = 1) {
    cis <- tibble()
    # Get sample
    population <- text_to_distr(isolate(input$distribution))
    n <- isolate(input$sam_size)
    # Get parameters
    alpha <- isolate(input$alpha)
    type <- case_when(
      n <= 30 ~ "Small n (<= 30)", # (T dist)
      isolate(input$var_known) ~ "Known variance", # (Normal)
      isolate(!input$var_known) ~ "Unknown variance", # (Normal, because n)
      .default = "Neviem"
    )
    if (type == "Small n (<= 30)") {
      sampling_distribution <- StudentT$new(df = n - 1)
    } else {
      sampling_distribution <- Normal$new()
    }
    # All the stuff above is deterministic, e.g. it does not rely on any
    # random process
    for (i in seq_len(iterations)) {
      sample <- population$rand(n)
      if (isolate(input$var_known)) {
        var <- population$variance()
      } else {
        var <- var(sample)
      }
      x_bar <- mean(sample)
      # CI
      quantile <- sampling_distribution$quantile(1 - alpha/2)
      mult <- var / sqrt(n)
      lower <- x_bar - quantile*mult
      upper <- x_bar + quantile*mult
      # Return
      ret <- tibble(
        i = nrow(isolate(CI())) + nrow(cis) + 1,
        pop = population$strprint(),
        mu = population$mean(),
        sigma2 = population$variance(),
        var_known = isolate(input$var_known),
        sample = list(as.character(sample)),
        alpha = alpha,
        x_bar = x_bar,
        S = var,
        n = n,
        lower = lower,
        upper = upper,
        width = 2*quantile*mult,
        correct = between(mu, lower, upper)
      )
      cis <- bind_rows(cis, ret)
      h1$set(i*100/iterations)
      h2$set(i*100/iterations)
      h3$set(i*100/iterations)
      h4$set(i*100/iterations)
    }

    CI(bind_rows(isolate(CI()), cis))
  }

  # Generative loop
  observe({
    INVALID()
    if (RUN()) generate_ci()
  })

  # Outputs
  # ----------------------------------------------------------------------------
  output$plot <- renderPlotly({
    req(nrow(CI()) > 0)
    # Prepare data
    ci <- CI()
    ci_info <- ci[1,]
    ci_print <-
      ci |>
      mutate(correct = if_else(correct, "Within CI", "Outside CI"),
             correct = factor(correct, levels = c("Within CI", "Outside CI"))) |>
      slice_max(i, n = 10)
    # Generate dynamic limits
    offset   <- max(ci$mu - ci$lower, ci$upper - ci$mu) * 1.2
    xlimits <- c(ci_info$mu - offset, ci_info$mu + offset)
    ylimits <- c(min(ci_print$i) - 0.5, max(ci_print$i, 10) + 0.5)
    # Plot
    plot_ly(ci_print, type = "scatter", mode = "markers",
            colors = c("Within CI" = "green", "Outside CI" = "red")) |>
      # Confidence interval
      add_segments(x = ~lower, xend = ~upper, y = ~i, yend = ~i,
                   line = list(color = "blue", width = 1),
                   hoverinfo = "skip") |>
      # Lower bound
      add_trace(x = ~lower, y = ~i,
                marker = list(color = "blue", symbol = "line-ns-open"),
                hovertemplate = paste(c("Sample: %{y}",
                                        "Lower bound: %{x}",
                                        "<extra></extra>"),
                                      collapse = "\n")) |>
      # Guess
      add_trace(x = ~x_bar, y = ~i,
                color = ~correct,
                marker = list(symbol = "x"),
                hovertemplate = paste(c("Sample: %{y}",
                                        "Estimate: %{x}",
                                        "<extra></extra>"),
                                      collapse = "\n")) |>
      # Upper bound
      add_trace(x = ~upper, y = ~i,
                marker = list(color = "blue", symbol = "line-ns-open"),
                hovertemplate = paste(c("Sample: %{y}",
                                        "Upper bound: %{x}",
                                        "<extra></extra>"),
                                      collapse = "\n")) |>
      layout(
        xaxis = list(
          title = "Estimate",
          range = xlimits,
          showgrid = F,
          zeroline = F
        ),
        yaxis = list(
          title = "Sample",
          range = ylimits,
          dtick = 1,
          tick0 = max(ci_print$i),
          tickmode = "linear",
          showgrid = F,
          zeroline = F
        ),
        shapes = list(list(
          type = "line",
          y0 = 0,
          y1 = max(10.5, ci_print$i + .5),
          x0 = ci_info$mu,
          x1 = ci_info$mu,
          line = list(color = "black", dash = "dot")
        )),
        showlegend = F
      )

  })

  output$plot_distr <- renderPlotly({
    req(nrow(CI()) > 0)
    # Prepare data
    ci <- CI()
    means <- ci$x_bar
    mu <- unique(ci$mu)

    gg <-
      ggplot(data.frame(x = means), aes(x = x)) +
      geom_histogram(aes(y = ..density..), fill = "lightblue", color = "black",
                     linewidth = 0.1,
                     bins = ceiling(1 + 3.3 * log(length(means), 10))) +
      stat_density(geom = "line", position = "identity",
                   bw = "SJ", color = "orange") +
      scale_y_continuous() +
      theme_bw() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )

    ggplotly(gg) |>
      layout(
        # True mean
        shapes = list(list(
          type = "line",
          y0 = 0,
          y1 = 1,
          x0 = mu,
          x1 = mu,
          line = list(color = "black", dash = "dot")
        ))
      )
  })

  output$download_data <- downloadHandler(
    filename = "samples.csv",
    content = function(file) {
      isolate(CI()) |>
        mutate(sample = paste0(unlist(sample), collapse  = ",")) |>
        write.csv2(file)
    }
  )

  output$info <- renderPrint({
    ci <- CI()
    if (nrow(ci) == 0) return()

    corr <- filter(ci, correct)
    fals <- filter(ci, !correct)
    prop <- round(nrow(corr)*100/nrow(ci), 4)
    streak_str <- streak(ci$correct)

    glue(
      "Samples: {nrow(ci)}\n",
      "Correct intervals: {nrow(corr)} ({fmt(prop, 7)} %)\n",
      "Average (SD) guess in correct intervals: {fmt(mean(corr$x_bar), 7)} ",
      "({fmt(sd(corr$x_bar), 7)})\n",
      "{streak_str}"
    )
  })

  output$progress <- renderPlotly({
    ci <- CI()
    if (nrow(ci) == 0) return()

    hline <- (1 - isolate(input$alpha))*100
    cip <-
      ci |>
      mutate(
        across(
          .cols = c("x_bar", "lower", "upper"),
          .fns = ~ fmt(.x, 7)
        ),
        meancor = cumsum(correct)*100/seq_len(nrow(ci)),
        correct = if_else(correct, "Within CI", "Outside CI"),
        correct = factor(correct, levels = c("Within CI", "Outside CI")))

    cip |>
      plot_ly(type = "scatter", mode = 'lines') |>
      add_trace(x = ~i, y = ~meancor, mode = 'lines',
                line = list(color = "blue", width = 1)) |>
      add_trace(x = ~i, y = ~meancor, mode = 'markers',
                symbol = ~correct, symbols = c("circle", "x-thin"),
                hovertemplate = glue("Sample: {cip$i}\n",
                                     "Guess: {cip$x_bar}\n",
                                     "CI: ({cip$lower}, {cip$upper})\n",
                                     "Success: {cip$meancor} %\n",
                                     "Status: {cip$correct}\n",
                                     "<extra></extra>")) |>
      layout(
        yaxis = list(
          title = "Percentage of correct CIs",
          range = list(0, 105),
          ticksuffix = "%",
          showgrid = F
        ),
        xaxis = list(
          title = "Sample",
          range = list(.8, max(ci$i)),
          showgrid = F
        ),
        shapes = list(
          list(
            type = "line",
            y0 = hline,
            y1 = hline,
            x0 = .8,
            x1 = max(ci$i),
            line = list(color = "red", dash = "dot")
          )
        ),
        showlegend = FALSE)


  })

  sample_info <- list()
  output$samples <- renderPrint({
    ci <- CI()
    if (nrow(ci) == 0) return()

    index <- nrow(ci)
    index_seq <- (length(sample_info) + 1):index
    for (i in index_seq) {
      cinfo <- ci[i, ]
      mark <- if_else(cinfo$correct, "√", "X")
      sample_info[i] <<- glue(
        "[{mark}] Sample {str_pad(i, width = 2)}: ",
        "Mean = {fmt(cinfo$x_bar)} ",
        "({fmt(cinfo$lower)}, {fmt(cinfo$upper)})"
      )
    }

    invisible(lapply(rev(sample_info), cat, sep = "\n"))
  })
  # ----------------------------------------------------------------------------
}
