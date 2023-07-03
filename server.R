function(input, output, session) {

  # Setup global variables
  INVALID <- reactive({invalidateLater(isolate(input$sim_speed)*1000)})
  RUN <- reactiveVal(FALSE)
  CI <- reactiveVal(tibble())

  ### Control buttons
  ### --------------------------------------------------------------------------
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
    # Other
    CI(tibble())
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
  })
  ### --------------------------------------------------------------------------

  observe({
    INVALID()
    if (!RUN()) return()
    # Get sample
    population <- text_to_distr(isolate(input$distribution))
    n <- isolate(input$sam_size)
    sample <- population$rand(n)
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
      i = nrow(isolate(CI())) + 1,
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
    CI(bind_rows(isolate(CI()), ret))

  })

  # Outputs
  # ----------------------------------------------------------------------------
  output$plot <- renderPlot({
    ci <- CI()
    if (nrow(ci) == 0) return()
    # Prepare data
    ci_info <- ci[1,]
    ci_print <-
      ci |>
      mutate(correct = if_else(correct, "Within CI", "Outside CI"),
             correct = factor(correct, levels = c("Within CI", "Outside CI"))) |>
      slice_max(i, n = 10)
    # Generate dynamic limits
    offset   <- max(ci$mu - ci$lower, ci$upper - ci$mu) * 1.2
    xlimits <- c(ci_info$mu - offset, ci_info$mu + offset)
    ybreaks <- seq(from = if_else(nrow(ci) <= 10, 1, min(ci$i)),
                   to = max(ci_print$i, 10))
    ylimits <- c(min(ci_print$i), max(ci_print$i, 10))
    # Plot
    ggplot(ci_print) +
      # Real mean
      geom_vline(aes(xintercept = mu), linetype = 4, linewidth = 1, color = "red") +
      # Confidence interval
      geom_segment(aes(x = lower, xend = upper, y = i, yend = i, color = correct),
                   linewidth = 1) +
      # Computed sample mean
      geom_point(aes(x = x_bar, y = i), size = 2) +
      scale_y_continuous(breaks = ybreaks,
                         limits = ylimits) +
      scale_x_continuous(limits = xlimits,
                         expand = c(0, 0)) +
      scale_color_manual(name = "CI Evaluation",
                         values = c("Within CI" = "green", "Outside CI" = "red"),
                         drop = F) +
      theme_bw() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      ) +
      labs(
        title = "Simulation of confidence intervals (CI) and Type I error",
        subtitle = glue("Population: {ci_info$pop} ,",
                        "Variance: {if_else(ci_info$var_known, '', 'un')}known, ",
                        "Sample size: {ci_info$n}, ",
                        "Alpha: {ci_info$alpha}"),
        x = "Mean", y = "Generated sample",
        caption = "Michal Lauer | laumi.me"
      )

  })

  output$download_data <- downloadHandler(
    filename = "samples.csv",
    content = function(file) {
      isolate(CI()) |>
        mutate(sample = paste0(unlist(sample), collapse  = ",")) |>
        write_csv2(file)
    }
  )

  output$info <- renderPrint({
    ci <- CI()
    if (nrow(ci) == 0) return()

    corr <- filter(ci, correct)
    fals <- filter(ci, !correct)
    prop <- round(nrow(corr)*100/nrow(ci))

    frm <- \(x, n = 2) sprintf(paste0('%0.', n, 'f'), x)
    glue(
      "Samples: {nrow(ci)}\n",
      "Correct intervals: {nrow(corr)} ({frm(prop)} %)\n",
      "Average (SD) guess in correct intervals: {frm(mean(corr$x_bar), 4)} ",
      "({frm(sd(corr$x_bar), 4)})\n",
      "Average (SD) guess in incorrect intervals: {frm(mean(fals$x_bar), 4)} ",
      "({frm(sd(fals$x_bar), 4)})\n",
    )
  })
  # ----------------------------------------------------------------------------
}
