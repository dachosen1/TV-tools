id.panel.overlaps.one.patient <- function(patient.dat, id.name, t1.name, t2.name, row.index.name){
  require(data.table)
  setDT(patient.dat)
  
  beginning.times <- patient.dat[, get(t1.name)]
  ending.times <- patient.dat[, get(t2.name)]
  
  overlapping.results <- patient.dat[, .(is.challenging = sum((get(t1.name) > beginning.times & get(t1.name) < ending.times) | (get(t2.name) < beginning.times & get(t2.name) > ending.times)) > 0), by = row.index.name][, sum(is.challenging) > 0]
  
  return(overlapping.results)
}

id.panel.overlaps <- function(dat, id.name, t1.name, t2.name){
  require(data.table)
  setDT(dat)
  setorderv(x = dat, cols = c(id.name, t1.name), order = 1)
  
  dat[, record.index := 1:.N, by = id.name]
  
  ids.with.overlaps <- dat[, .(V1 = id.panel.overlaps.one.patient(patient.dat = .SD, id.name = id.name, t1.name = t1.name, t2.name = t2.name, row.index.name = "record.index")), by = get(id.name)]
  
  setnames(x = ids.with.overlaps, old = c("get", "V1"), new = c(id.name, "overlapping_panels"))
  
  return(ids.with.overlaps)
}


#dd <- data.table(id = c(1,1,2,2), t1 = c(0, 30, 0, 15), t2 = c(30, 60, 20, 30))

#id.panel.overlaps(dat = dd, id.name = "id", t1.name = "t1", t2.name = "t2")


