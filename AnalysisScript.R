## Reproducible Research Project 1

# load libraries
library(dplyr)
library(ggplot2)
library(DMwR)

# Get Data

d1<-read.csv(unz("activity.zip", "activity.csv"))

# create a vector of complete cases

good<-complete.cases(d1)

# keep only complete cases

d2<-d1[good,]

# summarize to steps taken by day

d3<-d2 %>%
  group_by(date) %>%
  summarize(steps=sum(steps))

# Histogram of steps by day

plot1<-ggplot(data=d3, aes(d3$steps)) + geom_histogram(binwidth = 2500, col="black", fill="blue")
plot1<-plot1+labs(x="Steps", y="Count of Days", title="Histogram of Steps per Day")

plot1

# mean and median steps by day

stepmean<-mean(d3$steps)

stepmedian<-median(d3$steps)

stepmean

stepmedian

# summarize by avg steps taken by interval

d4<-d2

d5<-d4 %>%
  group_by(interval) %>%
  summarize(steps=mean(steps))

# plot avg steps by interval

plot2<-ggplot(d5, aes(x=interval, y=steps))+geom_line(col="blue")+
  labs(x="Interval", y="Average Steps Taken", title="Average Steps Taken by Interval")

plot2

# find interval with most steps taken

max<-max(d5$steps)

maxint<-subset(d5, steps==max)

maxint$interval


# Calculate number of rows with NA's

NAnumb<-nrow(d1)-nrow(d2)

NAnumb

# Impute NAs with knn

d6<-knnImputation(d1, k=5)

d7<-d6 %>%
  group_by(date) %>%
  summarize(steps=sum(steps))

# Histogram of steps by day

plot3<-ggplot(data=d7, aes(d7$steps)) + geom_histogram(binwidth = 2500, col="black", fill="blue")
plot3<-plot1+labs(x="Steps", y="Count of Days", title="Histogram of Steps per Day - Imputed N/A")

plot3

# mean and median steps by day

impstepmean<-mean(d7$steps)

impstepmedian<-median(d7$steps)

impstepmean

impstepmedian

# Convert date column to actual dates

d6$date<-as.Date(d6$date)

# add column for day of week

d6$weekday<-weekdays(d6$date)

# combine weekdays into weekend or weekday

d6<-mutate(d6, DayType = ifelse(test = weekday=="Saturday" | weekday=="Sunday", yes="Weekend", no="Weekday"))

# convert Daytype to factor

d6$DayType<-as.factor(d6$DayType)

d8<-d6 %>%
  group_by(DayType, interval) %>%
  summarize(steps=mean(steps))

# plot avg steps by interval

plot4<-ggplot(d8, aes(x=interval, y=steps))+geom_line(col="blue")+
  labs(x="Interval", y="Average Steps Taken", title="Average Steps Taken by Interval: Weekday vs. Weekend")+
  facet_grid(DayType~.)

plot4
