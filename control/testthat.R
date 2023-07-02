# Source functions...
sapply(X = list.files(path = "R/", full.names = T, recursive = T),
       FUN = source)
# ...and test them!
if (!require(testthat)) renv::install("testthat")
test_dir("./tests/testthat/")
