missingness.plot.old <- function(dat, time.names, times = NA, ntimes =20, variable.list, pdf.name="missingness.pdf"){
  library(Hmisc)
  if(sum(variable.list %nin% names(dat)) > 0){
    return("Error:  variable.list must be in names(dat).")
  }
  if(sum(time.names %nin% names(dat)) > 0){
    return("Error:  time.names must be in names(dat).")
  }
  
  tt1 <- which(names(dat)==time.names[1])
  tt2 <- which(names(dat)==time.names[2])
  m <- min(dat[,tt1])
  M <- max(dat[,tt1])
  
  if(is.na(times)){
    if(is.na(ntimes)){
      ntimes <- min(c(20, length(unique(dat[,tt1]))))
    }
    times <- seq(m, M, length.out=ntimes)
  }
  M <- max(dat[,tt2])
  pdf(pdf.name)
  for(variable in variable.list){
    wh <- which(names(dat)==variable)
    plot(m, 1, type="n", xlim=c(m,M), ylim=c(0,100), xlab="Time", ylab="Percent Missing", main=sprintf("Missingness Rate of %s", variable))
    if(sum(is.na(dat[,wh]))==0){
      lines(c(m,M), c(0,0))
    }
    else{
      rates <- numeric(length(times))
      for(i in 1:length(times)){
        w <- which(dat[,tt1] <= times[i] & dat[,tt2] >= times[i])
        rates[i] <- mean(is.na(dat[w,wh]))
      }
      lines(times, 100*rates, type='b')
    }
  }
  dev.off()
}

# This update is complete and ready for testing.
missingness.plot <- function(dat, time.names, times = NA, ntimes = 20, variable.names, pdf.name="missingness.pdf"){
  
  library(Hmisc)
  if(sum(variable.names %nin% names(dat)) > 0){
    return("Error:  variable.names must be in names(dat).")
  }
  if(sum(time.names %nin% names(dat)) > 0){
    return("Error:  time.names must be in names(dat).")
  }
  
  library(data.table)
  dat <- as.data.table(dat)
  
  tt1 <- time.names[1]
  tt2 <- time.names[2]
  
  if(mean(dat[, get(tt1)] > dat[, get(tt2)], na.rm=TRUE) > 0.5){
    tt1 <- tt2
    tt2 <- time.names[1]
  }
  
  m <- min(dat[,get(tt1)])
  M <- max(dat[,get(tt1)])
  
  if(is.na(times)){
    if(is.na(ntimes)){
      ntimes <- min(c(20, length(unique(dat[,get(tt1)]))))
    }
    times <- seq(m, M, length.out=ntimes)
  }
  M <- max(dat[,get(tt2)])
  pdf(pdf.name)
  for(variable in variable.names){
    plot(m, 1, type="n", xlim=c(m,M), ylim=c(0,100), xlab="Time", ylab="Percent Missing", main=sprintf("Missingness Rate of %s", variable))
    if(sum(is.na(dat[,get(variable)]))==0){
      lines(c(m,M), c(0,0))
    }
    else{
      rates <- numeric(length(times))
      for(i in 1:length(times)){
        w <- which(dat[,get(tt1)] <= times[i] & dat[,get(tt2)] >= times[i])
        rates[i] <- mean(is.na(dat[w,get(variable)]))
      }
      lines(times, 100*rates, type='b')
    }
  }
  dev.off()
}