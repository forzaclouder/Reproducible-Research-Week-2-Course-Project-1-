#Load package
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(magrittr)

#Data download
data<-download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile="data.zip")
data<-read.csv(unzip("data.zip"))

#Data reclassicifation
data<-data %>% 
        mutate(steps=as.integer(steps),
        interval=as.integer(interval),
        date=as.Date(date, format("%Y-%m-%d"))
         )


#Question 1
answer11<-data %>%
        group_by(date) %>%
        summarise(steps=sum(steps))

answer12<-ggplot(answer11,aes(x=steps))+
        geom_histogram(fill="green", color="black", binwidth=1500)+
        ylab("Counts")+
        xlab("The Total Number of Steps Taken Each Day")

answer13.1<-mean(answer11$steps, na.rm=T)
answer13.2<-median(answer11$steps, na.rm=T)

print(answer11)
png(filename = "1.2.png")
answer12
dev.off
print(answer13.1)
print(answer13.2)


#Question 2
q2data<-data %>%
        filter(!is.na(steps)) %>%
        group_by(interval) %>%
        summarise(steps=mean(steps))

answer21<-ggplot(q2data,aes(x=interval, y=steps))+
        geom_line(color="darkgreen")+
        ylab("Average Number of Steps")+
        xlab("5-minute Interval")

answer22<-filter(q2data,steps==max(q2data$steps))

png(filename = "2.1.png")
answer21
dev.off
print(answer22)


#Question 3
answer31<-sum(!complete.cases(data))

fillingdata<-data %>%
        filter(!is.na(steps)) %>%
        group_by(interval) %>%
        summarise(steps=mean(steps))
missingdata<-filter(data,is.na(steps))

for(i in 1:nrow(missingdata)){
        missingdata$steps[i]<-fillingdata$steps[
        missingdata$interval[i]==fillingdata$interval
        ]
}

answer33<-rbind(missingdata,filter(data,!is.na(steps)))

q34data<-answer33%>%
        group_by(date) %>%
        summarise(steps=sum(steps))

answer34.1<-ggplot(q34data,aes(x=steps))+
        geom_histogram(fill="blue", color="black", binwidth=1500)+
        ylab("Counts")+
        xlab("The Total Number of Steps Taken Each Day")
answer34.2<-mean(q34data$steps, na.rm=T)
answer34.3<-median(q34data$steps, na.rm=T)

print(answer31)
png(filename = "3.4.1.png")
answer34.1
dev.off
print(answer34.2)
print(answer34.3)


#Question 4
fillingdata<-data %>%
        filter(!is.na(steps)) %>%
        group_by(interval) %>%
        summarise(steps=mean(steps))
missingdata<-filter(data,is.na(steps))
for(i in 1:nrow(missingdata)){
        missingdata$steps[i]<-fillingdata$steps[
        missingdata$interval[i]==fillingdata$interval
        ]
}
q4data<-rbind(missingdata,filter(data,!is.na(steps)))
answer41<-q4data %>%
        mutate(weekday=ifelse(weekdays(q4data$date)=="Saturday"|weekdays(q4data$date)=="Sunday", "weekend", "weekday"))

q4data<-answer41 %>%
        group_by(weekday,interval) %>%
        summarise(steps=mean(steps,na.rm=T))

answer42<-ggplot(q4data,aes(x=interval, y=steps))+
        geom_line(color="navy")+
        facet_wrap(~weekday, nrow=2, ncol=1)+
        ylab("Average Number of Steps") + 
        xlab("5-minute Interval")

print(answer41)
png(filename = "4.2.png")
answer42
dev.off