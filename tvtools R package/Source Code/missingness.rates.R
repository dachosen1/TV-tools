missingness.rates.old <- function(dat, id.name, time.names, time=0, variable.list){
  library(Hmisc)
  
  if(sum(id.name %nin% names(dat))>0){
    return("Error:  id.name must be in names(dat).")
  }
  if(sum(time.names %nin% names(dat))>0){
    return("Error: time.names must be in names(dat).")
  }
  
  if(sum(variable.list %nin% names(dat))>0){
    return("Error:  all names in variable.list must be in names(dat).")
  }
  source("create.baseline.R")
  baseline <- create.baseline(dat, id.name, time.names, outcome.names=NA, time)
  
  K <- length(variable.list)
  missing <- numeric(K)
  names(missing) <- variable.list
  
  for(k in 1:K){
    w <- which(names(dat)==variable.list[k])
    missing[k] <- mean(is.na(dat[,w]))
  }
  return(missing)
}

mean.missing <- function(x){
  return(mean(is.na(x)))
}


# This update is complete and ready to test.
missingness.rates <- function(dat, id.name, time.names, time=0, variable.names){
    library(Hmisc)
    
    if(sum(id.name %nin% names(dat))>0){
        return("Error:  id.name must be in names(dat).")
    }
     if(sum(time.names %nin% names(dat))>0){
        return("Error: time.names must be in names(dat).")
    }

    if(sum(variable.names %nin% names(dat))>0){
        return("Error:  all names in variable.list must be in names(dat).")
    }
    source("cross.sectional.data.R")
    baseline <- cross.sectional.data(dat = dat, id.name = id.name, time.names = time.names, outcome.names = NA, time = time)
    
    library(data.table)
    
    baseline <- as.data.table(baseline)
    
    missing <- baseline[, lapply(X = .SD, FUN = "mean.missing"), .SDcols = variable.names]
    
    return(missing)
}

