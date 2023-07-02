if (!require(rsconnect)) renv::install("rsconnect")

rsconnect::setAccountInfo(name = 'laumi',
                          token = Sys.getenv("SHINYAPPS_TOKEN"),
                          secret = Sys.getenv("SHINYAPPS_SECRET"))

rsconnect::deployApp(appDir = "./")
