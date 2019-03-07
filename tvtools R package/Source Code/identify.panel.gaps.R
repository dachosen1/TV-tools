## Assumes that the data are all for a single patient (same id) and sorted in increasing order of t1.name
identify.panel.gaps.one.patient <- function(patient.dat, t1.name, t2.name, first.value = 0, expected.gap.between = 0){
  require(data.table)
  setDT(patient.dat)
  
  gap.first.row <- (patient.dat[1, get(t1.name) > first.value])
  n <- patient.dat[, .N]
  
  if(n == 1){
    res <- gap.first.row
  }
  if(n > 1){
    t2.values <- patient.dat[1:(n-1), get(t2.name)]
    gaps.other.rows <- patient.dat[2:n, get(t1.name) > t2.values + expected.gap.between]
    res <- c(gap.first.row, gaps.other.rows)
  }
  return(res)
}

identify.panel.gaps <- function(dat, id.name, t1.name, t2.name, gap.name = "gap_before", first.value = 0, expected.gap.between = 0){
  require(data.table)
  setDT(dat)
  setorderv(x = dat, cols = c(id.name, t1.name), order = 1)
  
  dat[, eval(gap.name) := identify.panel.gaps.one.patient(patient.dat = .SD, t1.name = t1.name, t2.name = t2.name, first.value = first.value, expected.gap.between = expected.gap.between), by = get(id.name)]
  
  return(dat[])
}


#dd <- data.table(id = c(1,1,2,2), t1 = c(3, 30, 0, 15), t2 = c(28, 60, 20, 30))

#dd <- identify.gaps(dat = dd, id.name = "id", t1.name = "t1", t2.name = "t2")
#datatable(data = dd, rownames = FALSE)

