install.packages("segmented")
library(segmented)
library(stats)

ele <- read.csv("ele.csv")
names(ele)
plot(ele$age,ele$bsi,type="l", xlab="Age (cal yrs BP)", ylab="BSi (%)")

ele <- ele[-273:-289,]
plot(ele$age,ele$bsi,type="l", xlab="Age (cal yrs BP)", ylab="BSi (%)")

hist(ele$age[2:495]-ele$age[1:494])

min(ele$age)
max(ele$age)

bsi.20 <-approx(x=ele$age,y=ele$bsi,xout=seq(-40,10860,20))
names(bsi.20)

plot(ele$age,ele$bsi, xlab="Age (cal yrs BP)", ylab="BSi (%)")
lines(bsi.20$x,bsi.20$y,col=2)

bsi.1 <-approx(x=ele$age,y=ele$bsi,xout=(seq(-47,10870,1)))
xout <- seq(-40,10860,20)
bsi.20i <- data.frame(x=xout,y=rep(NA,length(xout)))

for(i in 1:length(xout)){
  low.bound <- xout[i]-10
  high.bound <- xout[i]+10
  bsi.20i$y[i] <- mean(bsi.1$y[bsi.1$x>low.bound & bsi.1$x<high.bound])
}

lines(bsi.20i$x,bsi.20i$y,col=3)

age <- bsi.20i$x
bsi <- bsi.20i$y
ele.lm <- lm(bsi~age)
lines(age,ele.lm$fitted.values,col=1)

ele.lm.seg <- segmented(ele.lm,seg.Z=~age,psi=c(4000,8000))
lines(xout,ele.lm.seg$fitted.values,col=5)
summary(ele.lm.seg)
AIC(ele.lm.seg)

## autocorrelation function
acf(bsi.20i$y)
## partial autocorrelation? 
pacf(bsi.20i$y)
