# ~~~~~~~~~~~~~~~~~~~~~~~ HELPERS ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file contains wrappers for functions to more easily handle user inputs

# Often for datagrid, we are passing in the name of columns as function arguments.
# e.g. If we want to build a datagrid with the 'conc' feature of a model/df being the variable of interest
# we would call datagrid(conc=seq, model=model_1)
# However, as 'conc' is model and context specific, we need to be able to pass in this argument name dynamically
# Using this wrapper allows us to inject the argument name as a user input using "{name}":=seq where name can be any user input col name
datagrid2 <- function(...){
  do.call("datagrid", list2(...))
}

# aes wrapper
aes2 <- function(...){
  do.call("aes",list2(...))
}

#dataframe wrapper
df2 <- function(...){
  do.call("data.frame",list2(...))
}

gam2 <- function(...){
  do.call("gam",list2(...))
}

# Error function helper
showModalErrorMessage <- function(e){
  showModal(
    modalDialog(
      title="Error",
      size="l",
      easyClose=TRUE,
      e
    )
  )
}