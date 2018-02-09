###This exercise at http://members.cbio.mines-paristech.fr/~thocking/change-tutorial/RK-CptWorkshop.html

install.packages('changepoint.np')
library(changepoint.np)

#Done by previous
#install.packages('changepoint')
#library(changepoint)


set.seed(1)
m1=c(rnorm(100,0,1),rnorm(100,5,1))
m1.amoc=cpt.mean(m1)
cpts(m1.amoc)
m1.cusum=cpt.mean(m1,pen.value=1,penalty='Manual',test.stat='CUSUM')
plot(m1.amoc)


set.seed(1)
v1=c(rnorm(100,0,1),rnorm(100,0,2),rnorm(100,0,10), rnorm(100,0,9))
v1.man=cpt.var(v1,method='PELT',penalty='Manual',pen.value='2*log(n)')
cpts(v1.man)
param.est(v1.man)
plot(v1.man,cpt.width=3)

set.seed(1)
mv1=c(rexp(50,rate=1),rexp(50,5),rexp(50,2),rexp(50,7))
mv1.binseg=cpt.meanvar(mv1,test.stat='Exponential',method='BinSeg',Q=10,penalty="SIC")
cpts(mv1.binseg)

param.est(mv1.binseg)

plot(mv1.binseg,cpt.width=3,cpt.col='blue')



data(ftse100)
plot(ftse100,type='l',xlab='Date',ylab='Daily Return')

data(HC1)
ts.plot(HC1)

v1.crops=cpt.var(v1,method="PELT",penalty="CROPS",pen.value=c(5,500))


#Use ftse100 with 'segmented'

