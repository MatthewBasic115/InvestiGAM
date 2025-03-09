# 
# This file contains helper functions for generating common UI elements.
#

library(shiny)

# Simple function to select whether the user wants the link or response scale for a given scenario
responseScaleSelector <- function(id) {
  selectInput(id, label="Display Results on Link or Response Scale?", choices=c('link','response'))
}
