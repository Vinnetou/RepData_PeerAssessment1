---
title: "Reproducible Research: Peer Assessment 1"
author: "Lukas Kloucek"
date: "1/10/2021"
output: html_document
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
```{r message=FALSE, warning=FALSE}
library(magrittr)
library(dplyr)
library(ggplot2)
```


## 1. Loading and preprocessing the data
```{r}
downloadLink <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(downloadLink, destfile = "activityData.zip", mode="wb")
# unzip raw data and load step data into Rstudio 
unzip("activityData.zip")
stepdata <- read.csv("activity.csv", header = TRUE)
head(stepdata)
```

## 2. What is mean total number of steps taken per day?
``` {r}
stepsByDay <- stepdata %>% select(date,steps) %>% group_by(date) %>%
                summarize(tsteps= sum(steps)) %>% na.omit()
        hist(stepsByDay$tsteps, xlab = "Total daily Steps",
        main="Histogram of Total Steps per day", breaks = 20)
````

## 3. What is the average daily activity pattern?
``` {r}
# Mean
mean(stepsByDay$tsteps)
# Median
median(stepsByDay$tsteps)
```

## 4. Time Series Plot
```{r}
library(ggplot2)
databyinterval <- stepdata%>% select(interval,steps) %>% na.omit() %>%
                group_by(interval) %>% summarize(tsteps= mean(steps)) 
        ggplot(databyinterval, aes(x=interval, y=tsteps))+ geom_line()
```

## 5. The 5-min interval contains the max number of steps on average
```{r}
databyinterval[which(databyinterval$tsteps==max(databyinterval$tsteps)),]
```


## 6. Imputing missing values
```{r}
#subset dataset where steps have NAs
activity_na <- stepdata[which(is.na(stepdata$steps)),]
# Number of NAs in the dataset
nrow(activity_na)

# I will subset NAs with average amount of steps in given interval taken from the
# rest of the dataset where values are NOT missing

# subset dataset without missing values
activity_no_NA <- stepdata[which(!is.na(stepdata$steps)),]
# calculate the average amount of steps for each interval
interval_only <- activity_no_NA %>% group_by(interval) %>% summarise(average=mean(steps))
# convert the average to integer
interval_only$average <- as.integer(interval_only$average)

# fill NAs with average steps based on interval
activity_na$steps <- ifelse(activity_na$interval==interval_only$interval,
                            interval_only$average)
# rbind the dataset that do not have NAs and the dataset where NAs are replaced
# with average values
activity_impute <- rbind(activity_no_NA,activity_na)
```

## 7. Histogram of the total number of steps taken each day with imputed values
```{r}
# Compute the total number of steps per day
stepsByDay_impute <- activity_impute %>% group_by(date) %>% summarise(stepsPerDay = sum(steps))
qplot(stepsPerDay,data=stepsByDay_impute,binwidth=900,xlab='Total steps per day',
      ylab='Frequency',main = 'Total Daily Steps')

# Average mean and median of new and old data
oldmean <- mean(stepsByDay$tsteps, na.rm = TRUE)
newmean <- mean(stepsByDay_impute$stepsPerDay)
oldmean
newmean

oldmedian <- median(stepsByDay$tsteps, na.rm = TRUE)
newmedian <- median(stepsByDay_impute$stepsPerDay)
oldmedian
newmedian
```

## 8. Are there differences in activity patterns between weekdays and weekends?
```{r}
# differences in activity patterns between weekdays and weekends
# slice data into weekdays and weekends
activity_impute$date <- as.Date(activity_impute$date)
activity_impute$weekday <- weekdays(activity_impute$date)
activity_impute$weekend <- ifelse(activity_impute$weekday=="Saturday"|activity_impute$weekday=="Sunday","Weekend","Weekday")
# the plot
meansteps <- activity_impute %>% group_by(interval,weekend) %>%
                summarise(average = mean(steps))
        qplot(interval,average,data=meansteps,geom="line", facets=weekend~.,
        xlab="5-minute interval",ylab="average number of steps",
        main="Average steps pattern comparison Weekdays and Weekend")
```