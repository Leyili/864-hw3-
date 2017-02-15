# 864-hw3-

setwd('C:\\Users\\lileyi\\Desktop\\')
flintlead<-read.csv(file="Flint-water-lead-dataset.csv",
                    header=FALSE)
head(flintlead)
colnames(flintlead)=c("SampleID","Zip Code","Ward", 
                      "Pb Bottle 1(ppb)-First Draw", 
                      "Pb Bottle 2(ppb)- 45 secs flushing", 
                      "Pb Bottle 3(ppb)- 2 mins flusing")
# Q1 
a)flintlead$`Zip Code`=as.factor(flintlead$`Zip Code`)
summary(flintlead$`Zip Code`)
#result:   48502 48503 48504 48505 48506 48507 48529 48532 
             1    69    55    48    44    51     1     2 
#obviously they are not evenly sampled

b)
hist(flintlead[,4], main="Histogram of lead level at first draw", xlab="lead level")
hist(flintlead[,5], main="Histogram of lead level after 45 secs", xlab="lead level")
hist(flintlead[,6], main="Histogram of lead level after 2 mins", xlab="lead level")

c)
#lead level versus sample collecting time
time<-c(0, 45, 120)
matplot(time, t(flintlead[,4:6]),type="o", ylab="lead level")
extremeID<-which(flintlead[,5]>1000)
extremesvalues<-flintlead[extremeID,]

#remove the extreme cases
flintlead2<-flintlead[-extremeID,]
matplot(time,t(flintlead2[,4:6]),type="o")

d)
# zip only include 48503 to 48507
## Plot lead level versus the sampling time for each zip code area

zipcode1<-which(flintlead2[,2]==48503)
matplot(time,t(flintlead2[zipcode1,4:6]),type="o",ylab="lead level",main="zip code 48503")

zipcode2<-which(flintlead2[,2]==48504)
matplot(time,t(flintlead2[zipcode2,4:6]),type="o",ylab="lead level",main="zip code 48504")

zipcode3<-which(flintlead2[,2]==48505)
matplot(time,t(flintlead2[zipcode3,4:6]),type="o",ylab="lead level",main="zip code 48505")

zipcode4<-which(flintlead2[,2]==48506)
matplot(time,t(flintlead2[zipcode4,4:6]),type="o",ylab="lead level",main="zip code 48506")

zipcode5<-which(flintlead2[,2]==48507)
matplot(time,t(flintlead2[zipcode5,4:6]),type="o",ylab="lead level",main="zip code 48507")

Q2:
zipcode6<-which(flintlead2[,2]==48502)
zipcode7<-which(flintlead2[,2]==48529)
zipcode8<-which(flintlead2[,2]==48532)
flintlead3<-flintlead2[-c(zipcode6,),]zipcode7,zipcode8

for (i in 48503:48507) {
subset1<-which(flintlead3[,2]==i)
subsetflintlead<-flintlead3[subset1,]
responses1<-unlist(subsetflintlead[,4:6])
sampletime1<-rep(time,each=dim(subsetflintlead)[1])
plot(sampletime1, responses1)
theta1<-5
theta2<-0.02
plot(sampletime1, theta1*exp(-sampletime1*theta2))

fm[i]<-nls(responses1~theta1*exp(-sampletime1*theta2), start=list(theta1=5,theta2=0.02))
}
summary(fm)










