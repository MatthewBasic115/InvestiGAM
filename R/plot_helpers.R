###
### Functions related to plotting for the application.
###

# Given a list of optional params for a plotting function, return whether a particular option is present
checkPlotOption <- function(opt, opt_list){
  return(opt %in% opt_list)
}

getPlotPredictions <- function(model, cond, type, rug, by){
  if(isTruthy(cond)){
    return(plot_predictions(model,condition=cond,type=type, rug=rug))
  } else if (isTruthy(by)){ # just by
    return(plot_predictions(model,by=by,type=type, rug=rug))
  }
}

#' getPlotSlopes 
#' 
#' Takes in UI inputs from the Shiny frontend to produce plots.
#' Checks if either the condition or by UI element is empty and plots accordingly
#' Can't plot both conditional and marginal slopes at the same time,
#' so this function acts as a wrapper.
#' 
#' @importFrom marginaleffects plot_slopes
#'
#' @param model userModel() from Shiny
#' @param var_int Variable of interest, can be a c() or single item
#' @param cond Conditional variable - mutually exclusive with by
#' @param type Whether to plot on link or response scale 
#' @param rug Whether to show tick marks on the axes
#' @param by Marginal predictions - mutually exclusive with cond
#' 
#'
#' @returns marginaleffects plot_slopes function
#' @export
getPlotSlopes <- function(model, var_int, cond, by, type="response", rug=FALSE){
  # If the variable of interest is empty, set it to null.
  req(var_int)
  if(isTruthy(cond)){
    return(plot_slopes(model, variables=var_int,condition=cond,type=type,rug=rug))
  } else if(isTruthy(by)){
    return(plot_slopes(model, variables=var_int,by=by,type=type,rug=rug))
  }
}

getStudentPlotComparisons <- function(model, var_int, cond, by, comp){
  # build out custom value list
  return(getPlotBasicComparisons(model, var_int, cond, by, comp))
}

getPlotBasicComparisons <- function(model, var_int, cond, by, comp, rug=FALSE,type="response"){
  if(isTruthy(cond)){
    return(plot_comparisons(model, variables=var_int, condition=cond, comparison=comp,rug=rug, type=type))
  } else if(isTruthy(by)){
    return(plot_comparisons(model, variables=var_int, by=by, comparison=comp,rug=rug, type=type))
  }
}
