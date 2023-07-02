text_to_distr <- function(text, .debug = F) {
  normal <- c("Normal()",
              "Normal(mean = \\d+)",
              "Normal(mean = \\d+, sd = \\d+)",
              "Normal(mean = \\d+, var = \\d+)",
              "Normal(sd = \\d+)",
              "Normal(var = \\d+)")

  regexes <- c(
    normal
  )
  res <- str_detect(string = text, pattern = regexes)
  if (sum(res) == 0) {
    # TODO: No match
  } else if (sum(res) == 1) {
    name <- str_extract(string = text, pattern = "^[a-zA-Z]{1,}")
    params <- str_extract(string = text, pattern = "\\(.*\\)$")
    call <- paste0("distr6::", name, "$new", params)
    obj <- eval(parse(text = call))
    if (.debug) {
      print(paste("Text:", text))
      print(paste("Regex:", regexes[res]))
      print(paste("Distribution name:", name))
      print(paste("Parameters:", params))
      print(paste("Call:", call))
    }
    return(obj)
  } else {
    # TODO: Multiple matches
  }

}

