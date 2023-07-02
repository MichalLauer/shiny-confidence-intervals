# Load libraries
library(shiny)
library(shinyjs)
library(shinydashboard)
library(distr6)
library(stringr)
library(dplyr)
library(ggplot2)
library(glue)
library(DT)
library(readr)
library(testthat)
# Load functions
sapply(X = list.files(path = "R/", full.names = T, recursive = T),
       FUN = source)
# ------------------------------------------------------------------------------
