###
### This file contains functions for data manipulation.
###

#' getGamFamilies
#' 
#' Returns a list of valid GAM families that can be used in the application.
#' This is those supported by mgcv with some exclusions.
#'
#' @returns Valid subset of families supported by mgcv offered by the GUI
#' @export
getGamFamilies <- function(){
  # Families in GLM from family
  famList <- c("binomial","gaussian","Gamma","inverse.gaussian","poisson","quasi","quasibinomial","quasipoisson",
               # families in mgcv #TODO: requires theta
               "negbin",
               # families in extended.family
               "betar", "cnorm", "nb", "ocat", "scat", "tw", "ziP",
               # general.family classes which can only be used with REML or NCV
               # Not supporting location scale families
               "cox.ph")
  
  # Not currently supporting (but may time permitting)
  # Tweedie
  return(famList)
}

#' Title getGamMethods
#' 
#' Gets valid smooth parameter estimator for a given distribution family
#' Some that are not explicitly supported are still in the list.
#'
#' @param family Family for distribution provided as a string
#' @export
#' @returns Valid methods for the given family
getGamMethods <- function(family){
  methodList = switch(
    family,
    # Exponential family members.
    "binomial" = ,
    "Gamma" = ,
    "inverse.guassian" = ,
    "poisson" = ,
    "negbin" = ,
    "Tweedie" = ,
    "guassian" = c("REML","GCV.Cp","GACV.Cp","NCV","QNCV","P-REML","ML","P-ML"),
    # Below families have a reduced set of supported methods.
    "betar" = ,
    "cnorm" = ,
    "nb" = ,
    "ocat" = ,
    "scat" = ,
    "tw" = ,
    "ziP" = c("REML","ML","NCV"),
    "cox.ph" = c("REML")
    )
  return(methodList)
}


#' Title getValidLinkFunctions
#'
#' @param family Distribution family to get the valid link functions for
#'
#' @returns Valid link functions for a given family with the first item being the canonical link function.
#' @export
getValidLinkFunction <- function(family){
  linkList = switch(
    family,
    # guassian from family {stats}
    "cnorm" = ,
    "clog" = ,
    "scat" = ,
    "guassian"=c("identity","log","inverse"),
    "betar" = ,
    "binomial"=c("logit","probit","cauchit","log","cloglog"),
    "Gamma"=c("inverse","identity","log"),
    "nb" = ,
    "cpois" = ,
    "poisson"=c("log","identity","sqrt"),
    "inverse.guassian"=c("1/mu^2","inverse","identity","log"),
    "quasi"=c("logit","probit","cloglog","identity","log","inverse","1/mu^2","sqrt"),
    "ziP" = ,
    "cox.ph" = ,
    "ocat" = c("identity"),
    "Tweedie" = ,
    "tw" = c("log","inverse","identity", "1/mu^2")
  )
  return(linkList)
}

# Given a family and link, build a family object to pass into a model

#' Title
#'
#' @param family Selected distribution from Family dropdown in GUI
#' @param link Selected valid link function from dropdown in GUI
#'
#' @returns family object for use in GAM formula.
#' @export
#'
#' @examples 
#' # Gamma family using log link
#' buildFamilyValue("Gamma", "log")
buildFamilyValue <- function(family,link){
  fam_obj = eval(paste0(family,"(link= '",link,"')"))
  return(fam_obj)
}

#' Title buildSmooth
#'
#' @param smooth_term - one of ("s","te","ti","t2"). Defines what smooth term is being used. See mgcv doco for formula.gam
#' @param covars - List of covariates to be included in the smooth
#' @param nknots - Number of knots for the smooth
#' @param penalised - LOGICAL - Whether the spline will use a fixed d.f. regression spline or penalised regression spine (default)
#' @param smooth_class - Which smooth class is being used for the smooth. See smooth.terms in mgcv doco
#' @param by - optional - by in the spline definition. 'Default' is the default GUI input which will be ignored.
#'
#' @returns mgcv smooth object to be passed into a gam() formula for building a model
#' @export
buildSmooth <- function(smooth_term,covars,nknots,penalised,smooth_class, by){
  cleaned_covars = paste(covars,collapse=", ")
  
  # Begin building the string which will be evaluated to build the smooth
  # Smooth term and covars are mandatory, so are immediately added
  string_builder <- paste0(smooth_term,"(",cleaned_covars)
  
  # Slowly build the strings
  if(by!="Default") {string_builder <- paste0(string_builder,",by=", by)}
  if(nknots!=-1) {string_builder <- paste0(string_builder,",k=",nknots)}
  if(smooth_class != "Default") {
    string_builder <- paste0(string_builder,",bs='",smooth_class,"'")
  }
  
  # Penalised defaults to False, do not need to check if supplied
  string_builder <-paste0(string_builder,",fx=",penalised)
  
  # closing bracket
  string_builder <- paste0(string_builder,")")

  return(string_builder)
}

# Build a sequence across the range of values for the selected covariate
# Factor and numeric must be handled differently
# TODO: Datetime?
buildSequence <- function(selection, dtype){
  # Sequences for numerics and factors must be treated separately
  if (dtype=="factor"){
    seq <- unique(selection)
    seq <- lapply(seq, as.character)[[1]]
  } else { #numeric
    seq <- seq(from=min(selection),max(selection), length.out=500)
  }
  return(seq)
}

# Valid smooth classes
#' getSmoothClasses
#'
#' @returns a char vector of valid smooths
#' @export
getSmoothClasses <- function(){
  # Not supporting - sos, so, mrf, twlss, gevlss, multinom, mvn. However, they can be entered into the 'raw formula'
  smooth_classes <- c("tp","ds","cr","cs","cc","bs","ps","re", "sz")
  return(smooth_classes)
}

#' getFactorsForModel
#'
#' @param model mgcv model to get factor terms for
#'
#' @returns char vector containing factor terms for the model
getFactorsForModel <- function(model){
  mv <- gratia::model_vars(model)
  mp <- mapply(function(X) { return(gratia::is_factor_term(model,X))}, mv)
  factors <- mv[mp]
  return(factors)
}

#' Title getValidDataGridMethods
#' 
#' Based on the dtype for a given variable, offer different generation methods for the datagrid.
#' Note that the defaults are mean and mode, which are not provided here.
#' 
#' @param type String - dtype to generate methods for
#'
#' @returns String - Appropriate data generation method for given dtype. Currently only factor and numeric supported.
getValidDataGridMethods <- function(type){
  if(type=="factor") return(c("mode","unique"))
  #if(type=="numeric") return("range")
  methods <- return(c("mean","threenum", "fivenum","minmax","quartile","range"))
  return(methods)
}

getValidCondFunctions <- function(type){
  if(type=="factor") return(c("mode","unique"))
  if(type=="numeric") return(c("mean","threenum", "fivenum","minmax","quartile","range"))
  return(c("mean","threenum", "fivenum","minmax","quartile","range"))
}

getValidCompareAdvVarMethods <- function(type){
  if(type=="factor") return(c("reference", "sequential", "pairwise", "all", "revpairwise", "revsequential", "revreference", "minmax"))
  if(type=="numeric") return(c("reference", "sequential", "pairwise", "all", "revpairwise", "revsequential", "revreference", "minmax", "sd", "2sd"))
  # If in doubt, return numeric
  return(c("reference", "sequential", "pairwise", "all", "revpairwise", "revsequential", "revreference", "minmax", "sd", "2sd"))
}


#' getValidDataGridTypes
#'
#' @returns the three valid arguments for datagrid() grid_type argument
getValidDataGridTypes <- function(){
  types <- c("mean_or_mode", "balanced", "counterfactual")
  return(types)
}

#' getValidCompareArguments
#'
#' @returns Valid functions for the comparisons argument in plot_comparisons. Some have been skipped for brevity.
getValidCompareArguments <- function(){
  vals <- c("difference", "lnor", "differenceavg","ratio", "lnratio", "lift","dydx", "eyex", "eydx", "dyex","dydxavg","eyexavg",
            "eydxavg","dyexavg","ratioavg","lnratioavg","lnoravg","liftavg","expdydx","expdydxavg")
  return(vals)
}

buildDataGridString <- function(seq,add){
  # Collapse list of additional vars to a string
  cadd <- paste0(add,collapse=",")
  # Trick here is that the datagrid requires a model or newdata when called outside of predictions(), comparisons() or slopes()
  sb <- paste0("datagrid2(model=userModel(),",seq,"=seq,",cadd,")")
  return(sb)
}

#' getValidSlopeFunction
#'
#' @returns Valid slopes as per marginaleffects package.
getValidSlopeFunctions <- function(){
  return(c("dydx", "eyex", "eydx", "dyex"))
}