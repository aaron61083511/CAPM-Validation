# a is the intercept (0, a)
# b is the portfolio point (b1,b2)
# c is the benchmark (c1,c2)
# CML slope: (c2-a)/c1
# portfolio lines slopes: (b2-a)/b1

library(PerformanceAnalytics)
port <- read.csv(file.choose())
index <- read.csv(file.choose())
rf <- read.csv(file.choose())
mu <- Return.annualized(index$vwretd, 252, geometric = TRUE)
sigma <- sd(index$vwretd)*sqrt(252)
itcpt <- ((max(rf$rf)+min(rf$rf))/2)*252
CMLslope <- (mu-itcpt)/sigma
portslope <- (port$mu-itcpt)/port$sigma
length(which(portslope>CMLslope))/length(portslope)

