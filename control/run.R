shiny::stopApp()
cat('\f')
shiny::runApp(appDir = ".",
              launch.browser = F,
              host = "127.0.0.1",
              port = 1234)
