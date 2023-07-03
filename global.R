# Load libraries
# Shiny libs
library(shiny)
library(shinyjs)
library(shinydashboard)
library(waiter)
# Shiny support
library(distr6)
library(ggplot2)
# Data manipulation
library(stringr)
library(dplyr)
library(glue)
# Deploy
library(testthat)
library(rsconnect)
# Load functions
sapply(X = list.files(path = "R/", full.names = T, recursive = T),
       FUN = source)
# ------------------------------------------------------------------------------
