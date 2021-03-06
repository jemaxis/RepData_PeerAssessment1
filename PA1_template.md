# Reproducible Research: Peer Assessment 1



## Set required library

```r
library(dplyr)
library(ggplot2)
```

## Loading and preprocessing the data
- Store the data "activity.csv" in a working directory (E.g."C:\\Rassignment\\Cse5Project1"")
- The data, a csv file format, is loaded into a dataframe called "data".

```r
setwd("C:\\Rassignment\\Cse5Project1")
data <- read.csv("activity.csv",header = TRUE,stringsAsFactor=FALSE)
```

## What is mean total number of steps taken per day?

```r
nonNaData <- filter(data,!is.na(steps))
byDate <- group_by(nonNaData,date)
result1 <- summarise(byDate,sumsteps=sum(steps))
hist(result1$sumsteps,breaks = 20,col="red",ylim=c(0,12),xlim=c(0,25000), xlab="Total Number of steps per day", main="Histogram of Total Number of Steps Per Day")
```

![](PA1_template_files/figure-html/plotHist-1.png) 

- The mean and median is calculated as follow:

```r
meanStepWithNA <- as.character(round(mean(result1$sumsteps),2))
medianStepWithNA <- as.character(round(median(result1$sumsteps),0))
```
The mean total number of steps per day is 10766.19 and the median is 10765.

## What is the average daily activity pattern?


```r
byInterval <- group_by(nonNaData,interval)
result2 <- summarise(byInterval,avgsteps = mean(steps))
plot(x=result2$interval,y=result2$avgsteps,type="l",xlab="5-Minute Interval",ylab="Average Number Of Steps", main="Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/dailyActivityPattern-1.png) 

- Calculation to identify 5-min interval with highest average steps is:

```r
maxStepInt <- result2$interval[which(result2$avgsteps==max(result2$avgsteps))]
```
The 5-minute interval with highest average steps across all the days is 835.

## Imputing missing values
- Initial strategy was to replace the NA values using the mean of the total sum of steps for that day.  Unfortunately, the NA values occur for an entire day leading to all NA values being replaced with zero value. 
- The strategy finally used is to replace all the NA values at the interval level based on the mean step for each 5-minute interval. 
- Function to replace NA values in the data using mean step for each 5-minute interval is as follow: 

```r
replaceNaStepsByInterval <- function(argData){   
    dataWithNa <- argData[is.na(argData$steps),]
    dataWithoutNa <- argData[!is.na(argData$steps),]
    grpByDate <- group_by(dataWithoutNa,interval)
    avgStepsPerInterval <- summarise(grpByDate,meanstep=mean(steps))
    y = nrow(avgStepsPerInterval)
    result=data.frame()
    for(i in 1:y){
        interval <- avgStepsPerInterval$interval[i]
        meanStep <- avgStepsPerInterval$meanstep[i]
        dataWithNa$steps[dataWithNa$interval==interval]=meanStep
    }
    result <- rbind(dataWithoutNa,dataWithNa)
    return(result)
}
```

- Replace NA values in the original data

```r
completeData <- replaceNaStepsByInterval(data)
```

- Plot histogram of the total number of steps taken each day after missing values were imputed:

```r
dataByDay <- group_by(completeData,date)
result4 <- summarise(dataByDay,sumsteps=sum(steps))
hist(result4$sumsteps,breaks = 20,col="red",ylim=c(0,20),xlim=c(0,25000),xlab="Total Number of steps per day", main="Histogram of Total Number of Steps Per Day\nWith NA Values Replaced")
```

![](PA1_template_files/figure-html/plotHistComplete-1.png) 

- The  mean and median total number of steps taken per day for the above histogram are calculated as follow:

```r
meanStepWithNoNA <- as.character(round(mean(result4$sumsteps),2))
medianStepWithNoNA <- as.character(round(median(result4$sumsteps),0))
```
- The mean is 10766.19 and the median is 10766.  In comparison with the initial mean 10766.19 and median 10765,  the values does not differ much.

## Are there differences in activity patterns between weekdays and weekends?
- Function to calculate if the date falls on a weekday or weekend:

```r
dayOfWeekFunc <- function(arg){
    day <- weekdays(as.POSIXct(arg,format="%Y-%m-%d"))
    if(day == "Saturday" || day == "Sunday")
        return("weekend")
    else
        return("weekday")
}
```

- Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
colDayInd <- sapply(completeData$date,dayOfWeekFunc) # to create a column of "weekend" or "weekday" value.
result5<-cbind(completeData,DayIndicator=colDayInd)
```

- Create a panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends:

```r
grpByInterval <- group_by(result5,interval,DayIndicator)
resultPlot <- summarise(grpByInterval,averagestep = mean(steps))
qplot(interval,averagestep,data=resultPlot,facets=DayIndicator~.,geom="line",xlab="5-minute Interval",ylab="Average Number Of Steps")
```

![](PA1_template_files/figure-html/panelPlot-1.png) 
