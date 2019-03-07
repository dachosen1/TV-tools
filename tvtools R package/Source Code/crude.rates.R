

crude.rates.old <- function(dat, tx.name=NA, outcome.names, time.names=c("t1", "t2"), time.units="day", cut.points=NA, result.units="year", era.digits=0){
  
  library(Hmisc)
  if(!is.na(tx.name) & tx.name[1] %nin% names(dat)){
    return("Error:  tx.name must be in names(dat).")
  }
  if(sum(outcome.names %nin% names(dat))>0){
    return("Error: outcome.names must all be in names(dat).")
  }
  if(sum(time.names %nin% names(dat))>0){
    return("Error: time.names must all be in names(dat).")
  }
  if(time.units %nin% c("day", "month", "year")){
    print("Unknown time.units; must be day, month, or year.  Using day by default")
    time.units <- "day"
  }
  if(result.units %nin% c("day", "month", "year")){
    print("Unknown result.units; must be day, month, or year.  Using year by default")
    time.units <- "year"
  }
  
  dat <- era.splits(dat, cut.points, time.names)
  
  tt1 <- which(names(dat)==time.names[1])
  tt2 <- which(names(dat)==time.names[2])
  
  if(dat[1,tt1] > dat[1,tt2]){
    x <- tt2
    tt2 <- tt1
    tt1 <- tt2
  }
  
  year <- 365.25
  month <- year/12
  
  if(length(cut.points)>0){
    cut.points <- sort(unique(c(0, max(dat[,tt2]), cut.points)))
  }
  if(length(cut.points)==0){
    cut.points <- c(0, max(dat[,tt2]))
  }
  if(time.units == "day" & result.units == "month"){
    dat[,tt1] <- dat[,tt1]/month
    dat[,tt2] <- dat[,tt2]/month
    cut.points <- cut.points/month        
  }
  if(time.units == "day" & result.units == "year"){
    dat[,tt1] <- dat[,tt1]/year
    dat[,tt2] <- dat[,tt2]/year
    cut.points <- cut.points/year
  }
  if(time.units == "month" & result.units == "day"){
    dat[,tt1] <- dat[,tt1]*month
    dat[,tt2] <- dat[,tt2]*month
    cut.points <- cut.points*month        
  }
  if(time.units == "month" & result.units == "year"){
    dat[,tt1] <- dat[,tt1]*month/year
    dat[,tt2] <- dat[,tt2]*month/year
    cut.points <- cut.points*month/year
  }
  if(time.units == "year" & result.units == "month"){
    dat[,tt1] <- dat[,tt1]*year/month
    dat[,tt2] <- dat[,tt2]*year/month
    cut.points <- cut.points*year/month        
  }
  if(time.units == "year" & result.units == "day"){
    dat[,tt1] <- dat[,tt1]*year
    dat[,tt2] <- dat[,tt2]*year
    cut.points <- cut.points*year
  }
  
  if(is.na(tx.name)){
    tt <- c()    
  }
  else{
    wt <- which(names(dat)==tx.name)
    tt <- table(dat[,wt])
  }
  for(k in 1:length(outcome.names)){
    wh <- which(names(dat)==outcome.names[k])
    
    if(length(cut.points)<=2){
      rates <- matrix(0, nrow=1, ncol=length(tt)+1)
    }
    if(length(cut.points) > 2){
      rates <- matrix(0, nrow=length(cut.points), ncol=length(tt)+1)
    }
    
    colnames(rates) <- character(ncol(rates))
    rownames(rates) <- character(nrow(rates))
    if(length(tt)>0){
      for(j in 1:length(tt)){
        colnames(rates)[j] <- sprintf("%s = %s", tx.name, names(tt[j]))
      }
    }
    colnames(rates)[ncol(rates)] <- "Total"
    if(length(cut.points)<=2){
      rownames(rates) <- "All follow-up"
    }
    if(length(cut.points)>2){
      for(j in 1:(nrow(rates)-1)){
        if(j == 1){
          tround <- round(cut.points[j+1], era.digits)
          if(tround==1){
            ychar <- ""
          }
          if(tround!=1){
            ychar <- "s"
          }
          rownames(rates)[j] <- sprintf("< %s %s%s", tround, result.units, ychar)
        }
        if(j > 1 & j < nrow(rates)-1){
          rownames(rates)[j] <- sprintf("%s--%s %s%s", round(cut.points[j], era.digits), round(cut.points[j+1], era.digits), result.units, ychar)
        }
        if(j == nrow(rates)-1){
          tround <- round(cut.points[j], era.digits)
          if(tround==1){
            ychar <- ""
          }
          if(tround!=1){
            ychar <- "s"
          }
          rownames(rates)[j] <- sprintf(">= %s %s%s", tround, result.units, ychar)
        }
      }
      rownames(rates)[nrow(rates)] <- "All follow-up"
    }
    
    followup.time <- event.counts <- rates
    
    
    if(nrow(rates)>1){
      for(i in 1:(nrow(rates)-1)){
        if(ncol(rates)>1){
          for(j in 1:(ncol(rates)-1)){
            w <- which(dat[,tt2] > cut.points[i] & dat[,tt1] < cut.points[i+1] & dat[,wt] == names(tt)[j])
            dd <- dat[w,]
            dd[,tt1] <- pmax(dd[,tt1], cut.points[i])
            dd[,tt2] <- pmin(dd[,tt2], cut.points[i+1])
            event.counts[i,j] <- sum(dd[,wh])
            followup.time[i,j] <- sum(dd[,tt2]-dd[,tt1])
            rates[i,j] <- event.counts[i,j] / followup.time[i,j]
          }
        }
        j <- ncol(rates)
        w <- which(dat[,tt2] > cut.points[i] & dat[,tt1] < cut.points[i+1])
        dd <- dat[w,]
        dd[,tt1] <- pmax(dd[,tt1], cut.points[i])
        dd[,tt2] <- pmin(dd[,tt2], cut.points[i+1])
        event.counts[i,j] <- sum(dd[,wh])
        followup.time[i,j] <- sum(dd[,tt2]-dd[,tt1])
        rates[i,j] <- event.counts[i,j] / followup.time[i,j]
      }
    }
    
    i <- nrow(rates)
    if(ncol(rates)>1){
      for(j in 1:(ncol(rates)-1)){
        w <- which(dat[,wt] == names(tt)[j])
        event.counts[i,j] <- sum(dat[w,wh])
        followup.time[i,j] <- sum(dat[w,tt2]-dat[w,tt1])
        rates[i,j] <- event.counts[i,j] / followup.time[i,j]
      }
    }
    
    event.counts[nrow(rates), ncol(rates)] <- sum(dat[,wh])
    followup.time[nrow(rates), ncol(rates)] <- sum(dat[,tt2]-dat[,tt1])
    rates[nrow(rates), ncol(rates)] <- event.counts[nrow(rates),ncol(rates)]/followup.time[nrow(rates), ncol(rates)]
    x <- list(event.counts=event.counts, followup.time=followup.time, rates=rates)
    if(k==1){
      res <- x
      res$caption <- sprintf("Event rates per person--%s of follow--up.", result.units)
    }
  }
  return(res)
}



# This update is complete and ready for testing.
# cut.points must be in same units as result.units
crude.rates <- function(dat, tx.name=NA, outcome.names, time.names=c("t1", "t2"), time.units="day", cut.points=NA, result.units="year", era.digits=0){
    
  library(Hmisc)
  library(data.table)
  if(!is.na(tx.name) & tx.name[1] %nin% names(dat)){
      return("Error:  tx.name must be in names(dat).")
  }
  if(sum(outcome.names %nin% names(dat))>0){
      return("Error: outcome.names must all be in names(dat).")
  }
  if(sum(time.names %nin% names(dat))>0){
      return("Error: time.names must all be in names(dat).")
  }
  if(time.units %nin% c("day", "month", "year")){
      print("Unknown time.units; must be day, month, or year.  Using day by default")
      time.units <- "day"
  }

  if(result.units %nin% c("day", "month", "year")){
      print("Unknown result.units; must be day, month, or year.  Using year by default")
      time.units <- "year"
  }
    
  source("era.splits.R")
  dat <- era.splits(dat, cut.points, time.names)
    
  tt1 <- time.names[1]
  tt2 <- time.names[2]
  
  if(mean(dat[, get(tt1)] > dat[, get(tt2)], na.rm=TRUE) > 0.5){
    tt1 <- tt2
    tt2 <- time.names[1]
  }
  
  year <- 365.25
  month <- year/12

  if(length(cut.points)>0){
      cut.points <- sort(unique(c(0, max(dat[,get(tt2)]), cut.points)))
  }
 if(length(cut.points)==0){
      cut.points <- c(0, max(dat[,get(tt2)]))
  }
 if(time.units == "day" & result.units == "month"){
      dat[, eval(tt1)] = dat[, tt1 = get(tt1)/month]
      dat[, eval(tt2)] <- dat[, get(tt2)/month]
      cut.points <- cut.points/month        
  }
  if(time.units == "day" & result.units == "year"){
      dat[, eval(tt1)] <- dat[, .(tt1 = get(tt1)/year)]
      dat[,eval(tt2)] <- dat[, get(tt2)/year]
      cut.points <- cut.points/year
  }
  if(time.units == "month" & result.units == "day"){
      dat[,eval(tt1)] <- dat[, get(tt1)*month]
      dat[,eval(tt2)] <- dat[, get(tt2)*month]
      cut.points <- cut.points*month        
  }
  if(time.units == "month" & result.units == "year"){
      dat[,eval(tt1)] <- dat[, get(tt1)*month/year]
      dat[,eval(tt2)] <- dat[, get(tt2)*month/year]
      cut.points <- cut.points*month/year
  }
   if(time.units == "year" & result.units == "month"){
      dat[,eval(tt1)] <- dat[, get(tt1)*year/month]
      dat[,eval(tt2)] <- dat[, get(tt2)*year/month]
      cut.points <- cut.points*year/month        
  }
  if(time.units == "year" & result.units == "day"){
      dat[,eval(tt1)] <- dat[, get(tt1)*year]
      dat[,eval(tt2)] <- dat[, get(tt2)*year]
      cut.points <- cut.points*year
  }

  era.name <- "era"
  
  dat[, eval(era.name) := cut2(x = get(tt1), cuts = cut.points)]
  
  
  event.counts <- dat[, lapply(.SD, sum), .SDcols = outcome.names, by= c(tx.name, era.name)]
  
  observation.time <- dat[, .(follow.up = sum(get(tt2) - get(tt1))), by = c(tx.name, era.name)]
  
  rates <- event.counts[, .SD / observation.time$follow.up, .SDcols = outcome.names, by = c(era.name, tx.name)]
  names(rates)[names(rates) %in% outcome.names] <- sprintf("%s.rate", outcome.names)
  names(event.counts)[names(event.counts) %in% outcome.names] <- sprintf("%s.counts", outcome.names)
  
  pieces <- merge(x = observation.time, y = event.counts, by = c(era.name, tx.name))
  res <- merge(x = pieces, y = rates, by = c(era.name, tx.name))
  
  
  
  
  return(res)
}

#cruderates(dat, tx.name="tv_clopidogrel", outcome.names=c("death", "mi.event"), cut.points=c(0.5,1)*365.25, result.units="month")

#cruderates(dat, tx.name=NA, outcome.names=c("death", "mi.event"), result.units="month")


