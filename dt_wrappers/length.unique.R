## faster alternative to dat[, length(unique(get(value.name))), by = by]

length.unique <- function(dat, value.name, by = NA){
  require(data.table)
  this.dat <- setDT(x = dat)
  
  if(length(by) > 0){
    if(!is.na(by[1])){
      if(by[1] == ""){
        by[1] <- NA
      }
    }
  }
  
  if(is.na(by[1])){
    dat.u <- unique(this.dat, by = value.name)
    ans <- dat.u[, .(N = .N)]
  }
  if(!is.na(by[1])){
    dat.u <- unique(this.dat, by = c(by, value.name))
    ans <- dat.u[, .N, by = by]
  }
  return(ans)
}
