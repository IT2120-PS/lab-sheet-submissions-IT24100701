#exercise
#1
getwd()
setwd("C:\\Users\\IT24100701\\Desktop\\IT24100701\\LAB 5")
Delivery_Times <- read.table("Exercise - Lab 05.txt", header = TRUE, sep = ",")
attach(Delivery_Times)
fix(Delivery_Times)
#2
names(Delivery_Times)<-c("del")
histogram1 <- hist(Delivery_Times$del,breaks=seq(20,70,length = 10),right = TRUE,main = "Histogram of Delivery Times",xlab = "Delivery Times",ylab = "Frequency")
#4
breaks1<-round(histogram1$breaks)
breaks1
freq1<-histogram1$counts
freq1
mids1<-histogram1$mids
mids1
classses1<-c()
for(i in 1:length(breaks1)-1){
  classses1[i]<-paste0("[",breaks1[i],",",breaks1[i+1],")")
}
classses1
cbind(Classes1=classses1,Frequency1=freq1)
lines(mids1,freq1)
plot(mids1,freq1,type='o',main = "freq poly of DELIVERY TIMES",xlab = "TIMES",ylab="freq",ylim=c(0,max(freq1)))

cum.freq1<-cumsum(freq1)
new1<-c()
for(i in 1:length(breaks1)){
  if(i==1){
    new1[i]=0
  }else{
    new1[i]=cum.freq1[i-1]
  }
}
plot(breaks1,new1,type='o',main="cum freq of del time",xlab = "sh",ylab = "freq",ylim=c(0,max(cum.freq1)))
cbind(Upper=breaks1,Cumfreq=new1)


