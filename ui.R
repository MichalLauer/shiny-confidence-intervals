body <- dashboardBody(
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css")
  ),
  box(status = "primary", width = 12, height = "50vh",
      plotOutput("plot", height = "48vh")),
  box(status = "primary", width = 12, height = "30vh",
      plotOutput("progress", height = "28vh")),
  box(status = "primary", width =  6, height = "12vh",
      verbatimTextOutput(outputId = "info")),
  box(status = "primary", width =  6, height = "12vh",
      verbatimTextOutput(outputId = "samples"))
)

sidebar <- dashboardSidebar(
  textInput(inputId = "distribution",
            label = "Population distribution",
            value = "Normal()"),
  actionButton(inputId = "distribution_modal",
               label = "What is this?",
               icon = icon("magnifying-glass")),
  numericInput(inputId = "sam_size",
               label = "Sample size",
               min = 1, value = 200, step = 1),
  checkboxInput(inputId = "var_known",
                label = "population variance known?"),
  hr(class = "control-splitter"),
  numericInput(inputId = "alpha",
               label = "Alpha",
               min = 0, max = 1, value = 0.05, step = 0.05),
  numericInput(inputId = "sim_speed",
               label = "Simulation speed (s)",
               min = 0.1, max = 5, value = 0.5, step = 0.1),
  hr(class = "control-splitter"),
  div(class = "control-buttons",
      actionButton(inputId = "start",
                   label = "Start",
                   icon = icon("play")),
      actionButton(inputId = "stop",
                   label = "End",
                   icon = icon("stop"),
                   disabled = T)
  ),
  div(class = "control-buttons",
      actionButton(inputId = "pause",
                   label = "Pause",
                   icon = icon("pause"),
                   disabled = T),
      actionButton(inputId = "continue",
                   label = "Continue",
                   icon = icon("circle-play"),
                   disabled = T)
  ),
  hr(class = "control-splitter"),
  div(class = "control-buttons control-download",
      downloadButton(outputId = "download_data",
                     label = "Download samples")
  )
)

ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(sidebar),
  dashboardBody(body)
)
