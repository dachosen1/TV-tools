read.any.file <- function(filename, output.format = "data.table", header="auto", skip=0L, nrows= -1L, sep = "auto", sep2 = "auto", na.strings = c("NA", ""), stringsAsFactors = FALSE, select=NULL, drop=NULL, colClasses=NULL, strip.white = TRUE, sheet = NULL, range = NULL){
  
  pieces.filename <- strsplit(x = filename, split = ".")[[1]]
  the.extension <- pieces.filename[length(pieces.filename)]
  
  if(the.extension %in% c("csv", "txt")){
    require(data.table)
    dat <- fread(input = filename, sep = sep, sep2 = sep2, nrows = nrows, header = header, na.strings = na.strings, stringsAsFactors = stringsAsFactors, skip = skip, select = select, drop = drop, colClasses = colClasses, strip.white = strip.white)
  }
  if(the.extension %in% c("xls", "xlsx")){
    if(!(header %in% c(TRUE, FALSE))){
      col_names = TRUE
    }
    if(header %in% c(TRUE, FALSE)){
      col_names = header
    }
    if(nrows == -1L){
      n_max = Inf
    }
    if(nrows > 0){
      n_max = floor(nrows)
    }
    require(readxl)
    dat <- read_excel(path = filename, sheet = sheet, range = range, col_names = col_names, col_types = colClasses, na = na.strings, trim_ws = strip.white, skip = skip, n_max = n_max)
  }
  if(the.extension %in% c("json")){
    require(rjson)
    dat <- fromJSON(file = filename)
  }
  if(the.extension %in% c("sav")){
    require(foreign)
    dat <- read.spss(file = filename, to.data.frame = TRUE, trim.factor.names = strip.white, trim_values = strip.white)
  }
  if(the.extension %in% c("sas7bdat")){
    require(sas7bdat)
    dat <- read.sas7bdat(file = filename)
  }
  
  setDT(dat)
  
  if(nrows > 0){
    n_max = min(c(dat[, .N], floor(nrows)))
  }
  
  dat <- dat[1:n_max,]
  
  if(output.format == "tibble"){
    require(tibble)
    dat <- as.tibble(x = dat)
  }
  if(output.format == "data.frame"){
    dat <- as.data.frame(x = dat)
  }
  
  return(dat)
}