source("renv/activate.R")
setHook("rstudio.sessionInit", function(newSession) {
  with(data = NULL, expr = {
    renv::install("rstudioapi", prompt = F)
    # Kill all terminals
    rstudioapi::terminalKill(rstudioapi::terminalList())
    # Open new terminal
    invisible(rstudioapi::terminalCreate(caption = "Git Bash",
                                         show = T,
                                         shellType = "win-git-bash"))
  })
}, action = "append")
