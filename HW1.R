# load libraries
library(tidyverse)

# set working directory
setwd("/Users/singhh/Downloads/CSCI48900")

######### Data manipulation and plotting in R #########

######### Question 1 #########

# read in data
crime=read.csv("indy_crime.csv")

# creating a tibble for the data
crime=as_tibble(crime)
# grouping the incident by zip code
crime %>% filter(Incident.Name=="Residential Burglary") %>% group_by(Zipcode)
# tally it
crime %>% filter(Incident.Name=="Residential Burglary") %>% group_by(Zipcode) %>% tally(sort = T)

crime %>% filter(Incident.Name=="Residential Burglary") %>% group_by(Zipcode) 


# grouping the incident by zip code
crime %>% filter(Incident.Name=="Motor Vehicle Theft") %>% group_by(Zipcode)
# tally it
crime %>% filter(Incident.Name=="Motor Vehicle Theft") %>% group_by(Zipcode) %>% tally(sort = T)


final_result=as_tibble(data.frame(Incident.Name=c("Residential Burglary", "Motor Vehicle Theft"),Zipcode=c(46268, 46260)))
crime %>% right_join(final_result,by="Zipcode")

######### Question 1 ends #########


######### Question 2  #########

# number of vehicle crashes in the data set
num_crashes=crime %>% filter(Incident.Name=="Vehicle Crash No Influence") %>% count()
print(num_crashes)

library(ggplot2)
ggplot(crime, aes(x=Incident.Code)) + geom_histogram()
ggplot(crime, aes(x=Incident.Code,y=Zipcode)) + geom_point()


# convert to date if not already
crime$Date <- as.Date(crime$Date)
# get months
crime$Month <- months(crime$Date)
# get years
crime$Year <- format(crime$Date,format="%y")
# aggregate on months and year and get mean
aggregate( Date ~ Month + Year , crime , mean )


######### Question 2 ends  #########


######### Stastics Review #########

######### Question 1 #########

phat=(123+63)/800
z=numeric(1000)
diff=abs(123/500-63/300)

x_data=numeric(500)
x_data[1:123]=1

y_data=numeric(300)
y_data[1:63]=1

for(k in 1:1000){
x=rbinom(500,1,phat)
y=rbinom(300,1,phat)
  
x=sample(x_data, length(x_data), replace=T)
y=sample(y_data, length(y_data), replace=T)
z[k]=as.numeric(abs(mean(x)-mean(y))>diff) 
}
mean(z)

xt=replicate(10000,sample(x,50,replace=T))
m1=apply(xt,2,median)
c_low=quantile(m1,.05)
c_up=quantile(m1,.95)

######### Question 1 ends #########


######### Question 2 #########

# read in data
gss=read.csv("GSS2002.csv")
# summary of data
summary(gss)
# make a table
table(gss$Happy, gss$Income)
# perform the test
chisq.test(table(gss$Happy, gss$Income))
# "Happy" and "Income" are independent because we cannot conclude that variables are associated

######### Question 2 ends #########


######### Question 3 #########
library(mosaic)
flight<-read.csv("FlightDelays.csv")
attach(flight)
summary(Delay)
hist(Delay)

tally(~Delay, data=flight)
tally(~Delay, data=flight, format="prop")

phat<-0.0014

rflip(1, prob=0.0014)
rflip(1000, prob=0.0014)

boot_dsn <- do(1000) * rflip(1000, prob=0.0014)
histogram(~prop, data=boot_dsn)

# save the se
se <- sd(~prop, data=boot_dsn)
#plug in ci
0.0014-2*se
0.0014+2*se
# percentile bootstrap ci
confint(boot_dsn, level=.90, method="quantile")

######### Question 3 ends #########

