###
install.packages("splancs")
library(splancs)
data(cardiff)
plot(cardiff)
?khat

### khat example
s <- seq(2,30,2)
plot(s, sqrt(khat(as.points(cardiff), cardiff$poly, s)/pi) - s,
     type="l", xlab="Splancs - polygon boundary", ylab="Estimated L",
     ylim=c(-1,1.5))


newstyle <- khat(as.points(cardiff), cardiff$poly, s, newstyle=TRUE)

str(newstyle)
newstyle
apply(newstyle$khats, 2, sum)
plot(newstyle)

### kenv.csr
?Kenv.csr
UL.khat <- Kenv.csr(length(cardiff$x), cardiff$poly, nsim=29, seq(2,30,2))
plot(seq(2,30,2), sqrt(khat(as.points(cardiff), cardiff$poly, 
                            seq(2,30,2))/pi)-seq(2,30,2), type="l", xlab="Splancs - polygon boundary", 
     ylab="Estimated L", ylim=c(-1,1.5))
lines(seq(2,30,2), sqrt(UL.khat$upper/pi)-seq(2,30,2), lty=2)
lines(seq(2,30,2), sqrt(UL.khat$lower/pi)-seq(2,30,2), lty=2)

#### Dan's scripts for Utah tree-ring data
# Load the software
source("k1d.R.txt")
# Read the example data file
X <- read.csv("utah.csv")
t <- k1d(X,start=1600,end=1650,nstep=51,stepsize=1)
# plot the Lhat function
plot.k1d(t)  

#simulation
s <- k1d.sim(t,scenario="circular",nsim=10,perc=c(0.025,0.975))
lines(t$h,s[1,],col=2)
lines(t$h,s[2,],col=2)
