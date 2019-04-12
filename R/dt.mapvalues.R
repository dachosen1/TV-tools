
# data.table equivalent to plyr::mapvalues
dt.mapvalues <- function(dat, variable.name, from, to){
 library(data.table)
 dat <- as.data.table(dat)

 for(i in 1:length(from)){
  set(x = dat, i = dat[get(variable.name) == from[i], .I], j = variable.name, value = to[i])
 }

 return(dat)
}
