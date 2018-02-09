ser <- read.csv("series.csv")
plot(ser)

e1<-c(1007,1714,1376,644,1259,1925,200)

e2<-c(1924,1923,1922,1920,1882,1878,1877,1863,1857,1829,1827,1809,1736,1555,1482,1401,1391,1377,1344,1323,1263,1258,1250,1220,1211,1063,1061,1054,1015,969,967,943,916,874,796,675,643,601,587,581,488,483,458,449,438,264,244,243,232,231,230,200,158,152,141,11,-72)

sea <- function(events,yrs,vals,window,ci){
  # create an empty ('NAs') matrix to hold all data
  # rows=ser values, columns=ages of events, values= time elapsed before or after events.
  sea<-matrix(NA,length(yrs),length(events))
  for(i in 1:length(vals)){
    for(j in 1:length(events)){
      sea[i,j]<-yrs[i]-events[j]
    }
  }
  
  # select out values within 'window' years before and after events
  xvals<-sea[abs(sea)<window]
  yvals<-vals[row(sea)][abs(sea)<window]
  
  #plot values
  plot(xvals,yvals,xlab="Years",ylab="Series values")
  
  # plot lines for series for each of the events in events.  This can look messy and is optional.  
  # for(i in 1:length(events)){
  #   lines(sea[,i][abs(sea[,i])<window],vals[abs(sea[,i])<window])
  #   }
  
  # average values in moving window of size xp: number of events
  xp<-length(events)
  events.mean<-array(NA,((window*2)+1))
  tempdis<-array(NA,length(yvals))
  a<-1
  for(i in -window:window){
    for(j in 1:length(yvals)){
      tempdis[j]<-abs(i-xvals[j])
    }
    events.mean[a]<-mean(yvals[order(tempdis)[1:xp]])
    a<-a+1  #increase a by 1.
  }
  
  #plot mean values as a thick blue line
  lines(-window:window,events.mean,lwd=2,col="blue")
  
  #determine confidence threshold 
  # this uses resampling of the time series with equal length as 'events', determined
  # confidence interval by the percentiles of the distribution of a large number of resampled values.
  sim<-array(NA,10000)  #10,000 resamples
  for(s in 1:10000){
    sim[s]<-mean(sample(vals,xp))
  }
  ci.out <- quantile(sim,ci)
  #plot confidence interval boundaries as gray horizontal lines.
  lines(c(-window,window),c(ci.out[1],ci.out[1]),lwd=2,col="gray")
  lines(c(-window,window),c(ci.out[2],ci.out[2]),lwd=2,col="gray")
}

sea(events=e1,yrs=ser$year,vals=ser$val,window=20,ci=c(0.025,0.975))
