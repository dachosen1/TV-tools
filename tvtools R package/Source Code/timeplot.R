# Creates a timeplot for one single individual.  The data frame dat will contain only a single value in the id field.
timeplot.internal.old <- function(dat, id.name, time.names, variable.names, variable.names.printed = NA, event.names, event.names.printed = NA, index.name = NA, title, lwd=NA, colors=NA, legend.scale=1.5, xlab="Time"){
    
    if(is.na(colors[1])){
        colors <- rep(1, length(variable.names))
    }
    if(is.na(lwd[1])){
        lwd <- rep(1, length(variable.names))
    }
    tt1 <- which(names(dat)==time.names[1])
    tt2 <- which(names(dat)==time.names[2])
    
    m <- min(dat[,tt1])
    M <- round(max(dat[,tt2]),0)

    lv <- length(variable.names)
    le <- length(event.names)
    
    xlim <- c(m,M)
    ylim=c(0, legend.scale*(2*lv+le))
    
    plot(1,1, xlim=xlim, ylim=ylim, axes=FALSE, xlab=xlab, ylab="Exposure (Yes/No)
         ", main=sprintf("%s for Subject %s", title, unique(dat[,names(dat)==id.name])), type='n')
    axis(1, m:M)
#    axis(2, at=NA, labels=FALSE)
#    lines(rep(-0.22,2), c(-0.25, ylim[2]))   
    
    for(i in 1:length(variable.names)){
        wh <- which(names(dat)==variable.names[i])
        w <- which(dat[,wh]==1)
        if(length(w)>0){
            for(j in 1:length(w)){
                lines(c(dat[w[j], tt1], dat[w[j], tt2]), c(lv-i+1, lv-i+1), col=colors[i], lwd=lwd[i])
            }
        }
    }
    
    lines(c(0,0), c(0, lv+le), lty=3)
    if(!is.na(index.name)){
        wh <- which(names(dat)==index.name)
        text(0, lv+1, dat[1,wh], cex=0.5)
    }
    
    for(i in 1:le){
        wh <- which(names(dat)==event.names[i])
        w <- which(dat[,wh]==1)
        if(length(w)>0){
            for(j in 1:length(w)){
                lines(c(dat[w[j], tt1], dat[w[j], tt1]), c(0, lv+le), lty=3)
                text(dat[w[j], tt1], lv+i+1, event.names.printed[i], cex=0.5)
            }
        }
    }
    
    legend(0, ylim[2]-0.001, variable.names.printed, col=colors, lty=1, lwd=lwd)
}

timeplot.internal <- function(dat, id.name, time.names, variable.names, variable.names.printed, event.names, event.names.printed, index.name = NA, the.title, lwd=NA, colors=NA, legend.scale=1.5, xlab="Time"){
  
  library(data.table)
  dat <- as.data.table(dat)
  
  if(is.na(colors[1])){
    colors <- rep(1, length(variable.names))
  }
  if(is.na(lwd[1])){
    lwd <- rep(1, length(variable.names))
  }
  if(is.na(event.names.printed)[1]){
    event.names.printed <- event.names
  }
  if(is.na(variable.names.printed[1])){
    variable.names.printed <- variable.names
  }

  tt1 <- time.names[1]
  tt2 <- time.names[2]
  
  if(mean(dat[, get(tt1)] > dat[, get(tt2)], na.rm=TRUE) > 0.5){
    tt1 <- tt2
    tt2 <- time.names[1]
  }
  
  m <- min(dat[,get(tt1)])
  M <- round(max(c(m, dat[,get(tt2)])),0)
  
  lv <- length(variable.names)
  le <- length(event.names)
  
  xlim <- c(m,M)
  ylim=c(0, legend.scale*(2*lv+le))
  
  main = sprintf("%s for Subject %s", the.title, as.character(dat[1, get(id.name)]))
  
  plot(1,1, xlim=xlim, ylim=ylim, axes=FALSE, xlab=xlab, ylab="Exposure (Yes/No)", main = main, type='n')
  axis(1, m:M)
  #    axis(2, at=NA, labels=FALSE)
  #    lines(rep(-0.22,2), c(-0.25, ylim[2]))   
  
  
  
  for(i in 1:length(variable.names)){
    w <- which(dat[,get(variable.names[i])]==1)
    if(length(w)>0){
      y = rep(lv - i + 1, 2)
      for(j in 1:length(w)){
        x = c(dat[w[j], get(tt1)], dat[w[j], get(tt2)])
        lines(x = x, y = y, col=colors[i], lwd=lwd[i])
      }
    }
  }
  
  lines(c(0,0), c(0, lv+le), lty=3)
  if(!is.na(index.name)){
    text(0, lv+1, dat[1, get(index.name)], cex=0.5)
  }
  
  for(i in 1:le){
    w <- which(dat[,get(event.names[i])]==1)
    if(length(w)>0){
      for(j in 1:length(w)){
        lines(c(dat[w[j], get(tt1)], dat[w[j], get(tt1)]), c(0, lv+le), lty=3)
        text(dat[w[j], get(tt1)], lv+i+1, event.names.printed[i], cex=0.5, pos = 4)
      }
    }
  }
  source("exposure.R")
  utilization <- exposure(dat = dat, id.name = id.name, time.names = time.names, begin.time = min(dat[, get(tt1)]), end.time = max(dat[, get(tt2)]), variable.names = variable.names, append.to.table = FALSE)
  
  legend.text <- character(length(variable.names))
  
  for(i in 1:length(variable.names)){
    legend.text[i] <- sprintf("%s (%.1f%%)", variable.names.printed[i], 100*utilization[, get(names(utilization)[grep(pattern = variable.names[i], x = names(utilization))])])
  }
  
  legend(x = "topleft", legend = legend.text, col=colors, lty=1, lwd=lwd)
}



timeplot.old <- function(dat, id.name, time.names, variable.names, variable.names.printed=NA, event.names, event.names.printed=NA, index.name=NA, pdf.name=NA, title=NA, howmany=NA, lwd=1, colors=NA, legend.scale=1.5, xlab="Time"){
    if(lwd[1]==1 & length(lwd)==1){
        lwd <- rep(1, length(variable.names))
    }
    
    wi <- which(names(dat)==id.name)
    if(is.na(wi)){
        return("Error:  id.name must be in names(dat).")
    }
    library(Hmisc)
    if(length(time.names)!= 2){
        return("Error:  time.names must be of length 2.")
    }
   if(sum(time.names %nin% names(dat))>0){
        return("Error:  time.names must be in names(dat).")
    }
    if(sum(variable.names %nin% names(dat))>0){
        return("Error:  variable.names must be in names(dat).")
    }
    if(sum(event.names %nin% names(dat))>0){
        return("Error:  event.names must be in names(dat).")
    }
    if(is.na(howmany)){
        howmany <- length(unique(dat[,wi]))
    }
    if(sum(is.na(variable.names.printed))>0){
        variable.names.printed <- variable.names
    }
    if(length(variable.names.printed)!=length(variable.names)){
        return("Error:  length(variable.names.printed) must equal length(variable.names)")
    }    
    if(sum(is.na(event.names.printed))>0){
        event.names.printed <- event.names
    }
    if(length(event.names.printed)!=length(event.names)){
        return("Error:  length(event.names.printed) must equal length(event.names)")
    }    
    ids <- unique(dat[,wi])
    
    dat <- dat[dat[,wi] %in% ids[1:howmany],]
    
    if(!is.na(pdf.name)){
        pdf(pdf.name)
    }
    sapply(split(dat, dat[,wi]), "timeplot.internal", id.name, time.names, variable.names, variable.names.printed, event.names, event.names.printed, index.name, title, lwd, colors, legend.scale, xlab)

    if(!is.na(pdf.name)){
        dev.off()
    }
}

# This is kind of half data.table, half conventional, but it might be reasonable enough for now.

timeplot <- function(dat, id.name, id.values = NA, time.names, variable.names, variable.names.printed=NA, event.names, event.names.printed=NA, index.name=NA, pdf.name=NA, the.title=NA, howmany=NA, lwd=1, colors=NA, legend.scale=1.5, xlab="Time"){
   
  library(Hmisc)
  
  if(id.name %nin% names(dat)){
    return("Error:  id.name must be in names(dat).")
  }
  
  if(length(time.names)!= 2){
    return("Error:  time.names must be of length 2.")
  }
  if(sum(time.names %nin% names(dat))>0){
    return("Error:  time.names must be in names(dat).")
  }
  if(sum(variable.names %nin% names(dat))>0){
    return("Error:  variable.names must be in names(dat).")
  }
  if(sum(event.names %nin% names(dat))>0){
    return("Error:  event.names must be in names(dat).")
  }
  if(is.na(howmany)){
    howmany <- length(unique(dat[,get(id.name)]))
  }
  if(sum(is.na(variable.names.printed))>0){
    variable.names.printed <- variable.names
  }
  if(length(variable.names.printed)!=length(variable.names)){
    return("Error:  length(variable.names.printed) must equal length(variable.names)")
  }    
  if(sum(is.na(event.names.printed))>0){
    event.names.printed <- event.names
  }
  if(length(event.names.printed)!=length(event.names)){
    return("Error:  length(event.names.printed) must equal length(event.names)")
  }    
  
  library(data.table)
  dat <- as.data.table(dat)
  
  if(!is.na(id.values)){
    id.values <- id.values[id.values %in% unique(dat[, get(id.name)])]
    if(length(id.values) == 0){
      return("Error:  no id.values show up in the id.name field.")
    }
  }
  else{
    if(is.na(howmany)){
      id.values <- dat[1, get(id.name)]
    }
    else{
      unique.ids <- unique(dat[, get(id.name)])
      
      id.values <- unique.ids[1:min(c(howmany, length(unique.ids)))]
      
    }
  }
  
  dat <- dat[get(id.name) %in% id.values,]
  
  if(!is.na(pdf.name)){
    pdf(pdf.name)
  }
  if(lwd[1]==1 & length(lwd)==1){
    lwd <- rep(1, length(variable.names))
  }
  
  
  # This needs to be updated...
  
  for(id.value in id.values){
    timeplot.internal(dat = dat, id.name = id.name, time.names = time.names, variable.names = variable.names, variable.names.printed = variable.names.printed, event.names = event.names, event.names.printed = event.names.printed, index.name = index.name, the.title = the.title, lwd=lwd, colors=colors, legend.scale=legend.scale, xlab=xlab)
  }
  
  if(!is.na(pdf.name)){
    dev.off()
  }
}
