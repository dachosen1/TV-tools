t1 <- rep(0:9, 50)
t2 <- 1 + t1

n <- length(t1)
id <- 1+floor((0:(n-1))/10)
age <- rnorm(n)
weight <- rnorm(n)

drug1 <- 1*(runif(n) < 0.9)
drug2 <- 1*(runif(n) < 0.2)
drug3 <- 1*(runif(n) < 0.5)
drug4 <- 1*(runif(n) < 0.3)
drug5 <- 1*(runif(n) < 0.4)

event1 <- 1*(runif(n) < 0.1)
event2 <- 1*(runif(n) < 0.05)

dat <- as.data.frame(cbind(id, t1, t2, age, weight, drug1, drug2, drug3, drug4, drug5, event1, event2))


cruderates(dat, tx.name="drug1", outcome.names=c("event1", "event2"), time.names=c("t1", "t2"), time.units="day", cut.points=c(3,7), result.units="day", era.digits=0)
