# Source functions...
sapply(X = list.files(path = "R/", full.names = T, recursive = T),
       FUN = source)
# ...and test them!
require(testthat)
test_dir("./tests/testthat/")
