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

Q3:
according to the different model we could get the different result.
48503 48505 both theta1 and theta2 are significant.
48504 48506 48507 just theta1 is significant.

Q4:
var.test(responses1,responses2)
Since the p-value < 2.2e-16,the hypothesis that the variances of x and y are equal is not accepted.
only var.test(responses4,responses5) 
Since the p-value=0.8679>0.05 ,the hypothesis that the variances of x and y are equal is accepted.

So only 48506 and 48507 have the same lead contamination and others are significantly different.

Q5:
time0<-0
theta1hat<-coef(fm1)[1]
theta2hat<-coef(fm1)[2]
meany<-theta1hat*exp(-time0*theta2hat)
sigmahat<-summary(fm1)$sigma
y90quantile<-qnorm(0.9, meany, sigmahat)

48503:  26.34515>15 
48504:  57.11656>15
48505:  12.29387<15
48506:  33.74771>15
48507:  33.0938>15
just 48505 area is less than 15 ppb


Q6:

the 95% interval for the 90% quantiles of lead level at different sampling time.
48503:  4.331021 48.359277
48504:  -102.3103  216.5435
48505:  8.441714 16.146033
48506:  -7.997447 75.492874
48507:  -11.64721  77.83481

Q7:


Q8:
res=y-yhat
and sample res + into the yhat  get the nwe y value and then we can compute the new res and use it to get the SEhat.



