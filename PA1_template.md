---
title: "Program Recherche reproductible Week 2"
author: "F.Weinachter"
date: "14/10/2019"
output:
  html_document: 
    keep_md: yes
  pdf_document: default
---




**Loading and preprocessing the data**


```r
# unzip and open the file
unzip(zipfile = "activity.zip", exdir = "activity")
data<-read.csv("activity/activity.csv",header=TRUE,sep=",")

# reformat "date"
library(lubridate)
data$date<-(ymd(data$date))
```

**What is mean total number of steps taken per day?**

<span style="color: #26B260">Question 1 : Total number of steps taken per day</span>


```r
agg = aggregate(data$steps,by = list(data$date),FUN = sum)
names(agg)<-c("Date","Number_of_steps")
print(agg)
```

```
##          Date Number_of_steps
## 1  2012-10-01              NA
## 2  2012-10-02             126
## 3  2012-10-03           11352
## 4  2012-10-04           12116
## 5  2012-10-05           13294
## 6  2012-10-06           15420
## 7  2012-10-07           11015
## 8  2012-10-08              NA
## 9  2012-10-09           12811
## 10 2012-10-10            9900
## 11 2012-10-11           10304
## 12 2012-10-12           17382
## 13 2012-10-13           12426
## 14 2012-10-14           15098
## 15 2012-10-15           10139
## 16 2012-10-16           15084
## 17 2012-10-17           13452
## 18 2012-10-18           10056
## 19 2012-10-19           11829
## 20 2012-10-20           10395
## 21 2012-10-21            8821
## 22 2012-10-22           13460
## 23 2012-10-23            8918
## 24 2012-10-24            8355
## 25 2012-10-25            2492
## 26 2012-10-26            6778
## 27 2012-10-27           10119
## 28 2012-10-28           11458
## 29 2012-10-29            5018
## 30 2012-10-30            9819
## 31 2012-10-31           15414
## 32 2012-11-01              NA
## 33 2012-11-02           10600
## 34 2012-11-03           10571
## 35 2012-11-04              NA
## 36 2012-11-05           10439
## 37 2012-11-06            8334
## 38 2012-11-07           12883
## 39 2012-11-08            3219
## 40 2012-11-09              NA
## 41 2012-11-10              NA
## 42 2012-11-11           12608
## 43 2012-11-12           10765
## 44 2012-11-13            7336
## 45 2012-11-14              NA
## 46 2012-11-15              41
## 47 2012-11-16            5441
## 48 2012-11-17           14339
## 49 2012-11-18           15110
## 50 2012-11-19            8841
## 51 2012-11-20            4472
## 52 2012-11-21           12787
## 53 2012-11-22           20427
## 54 2012-11-23           21194
## 55 2012-11-24           14478
## 56 2012-11-25           11834
## 57 2012-11-26           11162
## 58 2012-11-27           13646
## 59 2012-11-28           10183
## 60 2012-11-29            7047
## 61 2012-11-30              NA
```

<span style="color: #26B260">Question 2 : Histogram of the total number of steps taken each day</span>


```r
p <- ggplot(agg, aes(x=Number_of_steps)) + geom_histogram(color="white", fill="#69b3a2",bins=30)+ theme_bw()+
        xlab("Number of steps") +
        ylab("Count") +ggtitle("total number of steps taken each day")
print(p)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

<span style="color: #26B260">Question 3 : Calculate and report the mean and median of the total number of steps taken per day</span>


```r
meanST<-as.integer(mean(agg$Number_of_steps, na.rm = TRUE))
medianST<-median(agg$Number_of_steps , na.rm = TRUE)
```

<span style="background-color: #ffff99;">The average number of steps per day for this person is 10766 and the median number of steps per day for this person is 10765 <span/>

**What is the average daily activity pattern?**

<span style="color: #26B260">Question 1 : Time series plot of the 5-minute interval and the average number of steps taken<span/>


```r
#calculation of the number of steps per interval
mean_by_interval<-aggregate(data$steps,by = list(data$interval),FUN = mean,na.rm=TRUE)

install.packages("plotly")
```

```
## Error in contrib.url(repos, "source"): trying to use CRAN without setting a mirror
```

```r
library(plotly)
install.packages("hrbrthemes")
```

```
## Error in contrib.url(repos, "source"): trying to use CRAN without setting a mirror
```

```r
library(hrbrthemes)

#plot describing the daily average steps by interval
p <- mean_by_interval %>%
        ggplot( aes(x=Group.1, y=x)) +
        geom_area(fill="#69b3a2", alpha=0.5) +
        geom_line(color="#69b3a2") + 
        ggtitle("Average of daily steps by interval")+
        ylab("Number of steps") +
        xlab("Intervals") +
        theme_ipsum()+
        geom_vline(xintercept = 835,linetype="dotted",color = "red", size=0.2)

p <- ggplotly(p)
p
```


<span style="color: #26B260">Question 2 : Calculate which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?<span/>


```r
l_max_st<-subset(mean_by_interval,x==max(x))
max_st<-l_max_st[,1]
max_dist<-as.integer(l_max_st[,2])
```

<span style="background-color: #ffff99;">The 5-minute interval, on average across all the days in the dataset, which contains the maximum number of steps is the 835th with 206 steps on average<span/>

**Imputing missing values**

<span style="color: #26B260">Question 1 : Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA<span/>


```r
sumNA<-sum(is.na(data))
```

<span style="background-color: #ffff99;">There is 2304 NAs in this data<span/>

<span style="color: #26B260">Question 2 :Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.<span/>

<span style="background-color: #ffff99;">The strategy developed is to impute the NAs by the average of steps of the corresponding interval<span/>

<span style="color: #26B260">Question 3 :Create a new dataset that is equal to the original dataset but with the missing data filled in.<span/>


```r
colnames(mean_by_interval)=c("interval","mean")
data2<-merge(data,mean_by_interval,by="interval")
b<-which(is.na(data2$steps))
data2$steps[b]<-data2$mean[b]
```

<span style="color: #26B260">Question 4 :Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?<span/>


```r
agg2 = aggregate(data2$steps,by = list(data2$date),FUN = sum)
names(agg2)<-c("Date","Number_of_steps")

p2 <- ggplot(agg2, aes(x=Number_of_steps)) + geom_histogram(color="white", fill="#69b3a2",bins=30)+ theme_bw()+
        xlab("Number of steps") +
        ylab("Count") +ggtitle("total number of steps taken each day (after NAs imputation)")
print(p2)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
meanST2<-as.integer(mean(agg2$Number_of_steps, na.rm = TRUE))
medianST2<-as.integer(median(agg2$Number_of_steps , na.rm = TRUE))
```

<span style="background-color: #ffff99;">The average number of steps per day for this person is AFTER NAs IMPUTATION 10766 and the median number of steps per day for this person is 10766.

These values don't vary from the calculation before imputation.<span/>


<span style="background-color: #ffff99;">The imputation changes the mode of the distribution: the number of days the person walks between 10715 steps and 11430 steps from about 4 to more than 11<span/>

**Are there differences in activity patterns between weekdays and weekends?**

<span style="color: #26B260">Question 1 :Create a new factor variable in the dataset with two levels “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.<span/>


```r
data2<-mutate(data2,wd=weekdays(data2$date))
data2$wdi<-ifelse(data2$wd %in% c("lundi","mardi","mercredi","jeudi","vendredi"), "weekday","weekend")
```

<span style="color: #26B260">Question 2 :Make a panel plot with a time series plot of the 5-minute interval (x-axis) and the average number of steps taken (y-axis). <span/>


```r
data3<-aggregate(data2$steps,by=list(data2$interval,data2$wdi),FUN=mean)
colnames(data3)<-c("intervals","weekday_indicator","number_of_steps")


data3 %>%
  ggplot( aes(x=intervals, y=number_of_steps, color=weekday_indicator)) +
  labs(x = "intervals", y = "number of steps",title = "Average number of steps per time interval")+
  geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
