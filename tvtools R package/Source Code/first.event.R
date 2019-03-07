firstevent.internal.old <- function(dat, wt, wh){
    w <- which(dat[,wh]==1)
    if(length(w)>0){
        return(min(dat[w,wt]))
    }
    else{
        return(Inf)
    }
}

first.event.old <- function(dat, id.name, outcome.names, time.name="t1"){
    library(Hmisc)
    if(sum(id.name %nin% names(dat))>0){
        return("Error:  id.name must be in names(dat).")
    }
    if(sum(outcome.names %nin% names(dat))>0){
        return("Error: outcome.names must all be in names(dat).")
    }
    if(sum(time.name %nin% names(dat))>0){
        return("Error: time.name must be in names(dat).")
    }
    
    wi <- which(names(dat)== id.name)
    wt <- which(names(dat) == time.name)
    ids <- unique(dat[,wi])
    
    tab <- matrix(0, nrow=length(ids), ncol=length(outcome.names))
    for(k in 1:length(outcome.names)){
        wh <- which(names(dat)==outcome.names[k])
        tab[,k] <- sapply(split(dat, dat[,wi]), "firstevent.internal", wt, wh)
    }
    rownames(tab) <- ids
    colnames(tab) <- outcome.names
    
    res <- c()
    res$tab <- tab
    res$caption <- "Time to first event for each patient.  Patients without an event are assigned an infinite time."
    return(res)
}


calc.first.event.time <- function(outcome, time){
  w <- which(outcome == 1)
  if(length(w) > 0){
    return(min(time[w]))
  }
  else{
    if(is.integer(time)){
      return(NA_integer_)
    }
    if(is.numeric(time)){
      return(NA_real_)
    }
  }
}

# time.name is the beginning of the interval for each interval.
# This draft is now updated and ready for testing.

first.event <- function(dat, id.name, outcome.names, time.name, append.to.table = FALSE){
  library(Hmisc)
  if(sum(id.name %nin% names(dat))>0){
    return("Error: id.name must be in names(dat).")
  }
  if(sum(outcome.names %nin% names(dat))>0){
    return("Error: outcome.names must all be in names(dat).")
  }
  if(sum(time.name %nin% names(dat))>0){
    return("Error: time.name must be in names(dat).")
  }
  
  library(data.table)
  dat <- as.data.table(dat)
  
  res <- dat[, lapply(X = .SD, FUN = "calc.first.event.time", time = get(time.name)), .SDcols = outcome.names, by = id.name]
  
  setnames(x = res, old = outcome.names, new = sprintf("%s.first.event.time", outcome.names))
  
  if(append.to.table == TRUE){
    newdat <- merge(x = dat, y = res, by = "id", all.x = TRUE)
    return(newdat)
  }
  else{
    return(res)
  }
  
}
