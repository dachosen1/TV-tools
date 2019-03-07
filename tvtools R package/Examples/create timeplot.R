setwd("/Users/Dave/Documents/CHD/Aim1")
source("data cleanup.R")
dat <- data.cleanup()

setwd("/Users/Dave/Documents/tvtools")
source("missingness.R")

setwd("/Users/Dave/Documents/JSM/2012")
missingness.plot(dat, time.names=c("t1","t2"), times = NA, ntimes =NA, variable.list=c("ldl_result", "hdl_result", "creactive_result"), pdf.name="missingness.pdf")



ids <- unique(dat$patid[dat$t1==0 & dat$hospital==0])

dat <- dat[dat$patid %in% ids,]
all.dat <- dat

w <- which(dat$death==1)-1

len <- dat$t2[w]-dat$t1[w]

o <- order(len, decreasing=TRUE)
w <- w[o]

id <- dat$patid[w[10]]

dat <- dat[dat$patid %in% id,]

dat <- dat[1:(nrow(dat)-1),]
dat$death[nrow(dat)] <- 1

setwd("/Users/Dave/Documents/tvtools")
source("timeplot.R")
source("exposure.R")

if(dat$init_pres[1]=="CABG or PCI"){
    if(dat$comorb_cabg[1]==1){
        dat$init_pres <- rep("Bypass", nrow(dat))
    }
    else{
        dat$init_pres <- rep("Angioplasty", nrow(dat))
    }
}
variable.names <- c("hospital", "tv_ace_inhibitors", "tv_betablocker", "tv_clopidogrel", "tv_statin", "tv_diuretic_t")

x=exposure(dat, id.name="patid", time.names=c("t1", "t2"), begin.time=0, end.time=max(dat$t2), variable.names=variable.names)

event.names.printed <- c("Death")

variable.names.printed <- c("Hospitalized", "Ace Inhibitor", "Beta Blocker", "Clopidogrel", "Statin", "Thiazide Diuretic")

for(i in 1:length(variable.names.printed)){
    variable.names.printed[i] <- sprintf("%s (%.1f%%)", variable.names.printed[i], 100*x$rates[i])
}

setwd("/Users/Dave/Documents/JSM/2012")
pdf("timeplot.pdf")
timeplot(dat, id.name="patid", time.names=c("t1", "t2"), variable.names=variable.names, variable.names.printed=variable.names.printed, event.names=c("death"), event.names.printed=event.names.printed, index.name="init_pres", pdf.name=NA, title="Medical History", howmany=1, legend.scale=1.5)

text(90, 10, "Patient not initially hospitalized;")
text(90, 9, "violates inclusion criteria.")
text(300, 10, "Survival time")
text(300, 9, " miscoded in data.")
dev.off()



