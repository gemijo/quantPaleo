
### A simple R function example
### A randomization test for the difference in medians
### The function below is designed to test the hypothesis that two samples are from populations with the same median value. 
### Copy and paste this function into R.  The function will now be available to use.
### new line

median_test <- function(a,b,nsim=999){
  diff <- abs(median(a)-median(b))  #the absolute value of the difference of the two medians
  z <- append(a,b)
  l.a <- length(a)
  l.b <- length(b)
  l.z <-length(z)
  diff.sim <- array(NA,nsim) #create the array to hold the results of the randomizations.
  for(s in 1:nsim){
    m <- sample(1:l.z,l.z)  #this shuffles the order of the numbers in the combined data set
    a.s <- z[m[1:l.a]] #the first l.a values make up the first sample
    b.s <- z[m[(l.a+1):l.z]] #the remainder of the values make up the second sample
    diff.sim[s] <- abs(median(a.s)-median(b.s)) # save the result in the array
  }
  Pval<- round(mean(diff.sim>=diff),4)
  print("A randomization test for the equivalence of medians of two samples. Ho:median_a=median_b")
  print(paste0("median_a = ",median(a),"; median_b = ",median(b)))
  if(Pval<0.05){
    print(paste0("p = ",Pval,"; Reject Ho"))
  } else {
    print(paste0("p = ",Pval,"; Accept Ho"))
  }
  maxx <- max(diff,diff.sim)
  hist(diff.sim,ylab="Number of randomizations",xlab="Difference in medians",xlim=c(0,maxx))
  lines(c(diff,diff),c(0,100),col=2,lwd=2)
  
}

###Create data for samples a and b, and run the test
a<-sample(1:50,15) ##15 random numbers between 1 and 50
b<-sample(10:60,10) ##10 random numbers between 10 and 60
median_test(a,b,nsim=9999)

a<-sample(20:50,15) ##15 random numbers between 20 and 50
b<-sample(30:60,20) ##10 random numbers between 30 and 60
median_test(a,b,nsim=9999)


### The common method for this test, which you may have heard of, is the Mann-Whitney U test
### The R function for this test is "Wilcox.Test", which requires one vector with the data, and a second vector of equal length indicating the groups 
### In this case, sample a will be group 1, sample b will be group 2, denoted by the vector ind.

z <- append(a,b)
ind <- append(rep(1,length(a)),rep(2,length(b)))
wilcox.test(z~ind)

###The WRS2 package has a similar function for the randomization test (medpb2)

library(WRS2)
medpb2(z~ind)

