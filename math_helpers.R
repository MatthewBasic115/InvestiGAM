# Get list of valid families
getGamFamilies <- function(){
  # Families in GLM from family
  famList <- c("binomial","guassian","Gamma","inverse.guassian","poisson","quasi","quasibinomial","quasipoisson",
               # families in mgcv
               "Tweedie","negbin",
               # families in extended.family
               "betar", "cnorm", "nb", "ocat", "scat", "tw", "ziP",
               # general.family classes which can only be used with REML or NCV
               "cox.ph","gammals","gaulss","gumbls","gevlss","multinom","mvn","shash","twlss","ziplss")
  return(famList)
}

# Get a list of methods
getGamMethods <- function(){
  # Families in GLM from family
  # Non-exponential
  methodList <- c("REML","ML","NCV","QNCV")
  
  # All methods (exponential)
  methodList <- c("GCV.Cp","GACV.Cp","NCV","QNCV","REML","P-REML","ML","P-ML")
  
  return(methodList)
}

# Based on the given family, return valid link functions
getValidLinkFunction <- function(family){
  linkList = switch(
    family,
    # guassian from family {stats}
    "guassian"=c("identity","log","inverse"),
    "binomial"=c("logit","probit","cauchit","log","cloglog"),
    "Gamma"=c("inverse","identity","log"),
    "poisson"=c("log","identity","sqrt"),
    "inverse.guassian"=c("1/mu^2","inverse","identity","log"),
    "quasi"=c("logit","probit","cloglog","identity","log","inverse","1/mu^2","sqrt")
  )
  return(linkList)
}

# Given a family and link, build a family object to pass into a model
buildFamilyValue <- function(family,link){
  fam_obj = eval(paste0(family,"(link= '",link,"')"))
  return(fam_obj)
}
