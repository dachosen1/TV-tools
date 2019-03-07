followup.time.old <- function(dat, id.name, time.name="t2"){
    library(Hmisc)
    if(sum(id.name %nin% names(dat))>0){
        return("Error:  id must be a data set column name.")
    }
    if(sum(time.name %nin% names(dat))>0){
        return("Error: time.name must all be data set column names.")
    }
    
    wi <- which(names(dat)== id.name)
    wt <- which(names(dat) == time.name)
    ids <- unique(dat[,wi])
    
    tab <- as.matrix(sapply(split(dat[,wt], dat[,wi]), "max"))
    rownames(tab) <- ids
    colnames(tab) <- "follow-up"
    
    return(tab)    
}

# Computes the end point of observation (the maximum ending time, e.g. death, loss of follow-up, end of study, etc.) for each patient.)

# This has been updated to include data.table functionality.  It is complete as a draft and ready for testing.
followup.time <- function(dat, id.name, time.name="t2", append.to.data = TRUE){
  library(Hmisc)
  if(sum(id.name %nin% names(dat))>0){
    return("Error:  id must be a data set column name.")
  }
  if(sum(time.name %nin% names(dat))>0){
    return("Error: time.name must all be data set column names.")
  }
  
  library(data.table)
  
  if(append.to.data == TRUE){
    tab <- as.data.table(dat)[, follow.up.time := max(get(time.name)), by = id.name]
  }
  else{
    tab <- as.data.table(dat)[, .(follow.up.time = max(get(time.name))), by = id.name]
  }

  return(tab)    
}
