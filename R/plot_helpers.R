###
### Functions related to plotting for the application.
###

# Given a list of optional params for a plotting function, return whether a particular option is present
checkPlotOption <- function(opt, opt_list){
  return(opt %in% opt_list)
}

getPlotPredictions <- function(model, cond, type, rug, by){
  # I didn't want to do that string building crap anymore.
  # If no by provided, just use cond
  if(is.null(by)){
    return(plot_predictions(model,condition=cond,type=type, rug=rug))
  } else if (is.null(cond)){ # just by
    return(plot_predictions(model,by=by,type=type, rug=rug))
  }
  # This will error but I don't have a better solution currently
  return(plot_predictions(model,condition=cond, type=type, rug=rug, by=by))
}