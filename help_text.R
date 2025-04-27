###
### Help text for the application. Holds long text strings for code readability.
### Generally, the header will be defined by the calling function as it can be stored in one line.
###
getPlotPredHelpText <- function(){
  "Plots predictions on the y-axis againt values of or more predictors. Input features which you want to generate a grid of predictor values for.
    Use the 'by' field to input categorical predictors when you want to get the average prediction for each factor level.
    You can only select ONE of by or cond. Make sure the other input is empty to generate the appropriate graph.
    See https://marginaleffects.com/bonus/plot.html"
}

getSmoothTermsHelpText <- function(){
  "Select the term to be used for the smooth. s is the default. te is tensor."
}