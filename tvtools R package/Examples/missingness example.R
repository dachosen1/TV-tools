t1 <- rep(0:9, 50)
t2 <- 1 + t1

n <- length(t1)
age <- rnorm(n)
weight <- rnorm(n)

age.missingness <- 1*(runif(n) < 0.1)
weight.missingness <- 1*(runif(n) < 0.2)
age[age.missingness==1] <- NA
weight[weight.missingness==1] <- NA

dat <- as.data.frame(cbind(t1, t2, age, weight))

missingness.plot(dat, time.names=c("t1","t2"), times = NA, ntimes =NA, variable.list=c("age", "weight"), pdf.name="missingness.pdf")
