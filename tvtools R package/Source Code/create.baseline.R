

create.baseline <- function(dat, id.name, time.names, outcome.names=NA){
    return(create.baseline(dat = dat, id.name = id.name, time.names = time.names, outcome.names = outcome.names, time = 0))
}
