# Load libraries
# Shiny libs
library(shiny)
library(shinyjs)
library(shinyvalidate)
library(shinydashboard)
library(waiter)
# Shiny support
library(distr6)
library(plotly)
# Data manipulation
library(stringr)
library(dplyr)
library(glue)
library(knitr, include.only = "combine_words")
# Deploy
library(testthat)
# Load functions
sapply(X = list.files(path = "R/", full.names = T, recursive = T),
       FUN = source)
# ------------------------------------------------------------------------------
