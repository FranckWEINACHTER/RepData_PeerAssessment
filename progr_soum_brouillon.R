install.packages("unzip")
library(unzip)


setwd("E:/module5/devoirS2/RepData_PeerAssessment1")

unzip(zipfile = "activity.zip", exdir = "activity")

data<-read.csv("activity/activity.csv",header=TRUE,sep=",")



lubridate

library(lubridate)
data$date<-(ymd(data$date))
class(data$date)

agg = aggregate(data$steps,by = list(data$date),FUN = sum)

library(ggplot2)

p <- ggplot(agg, aes(x=x)) + geom_histogram(color="white", fill="#69b3a2",bins=30)+ theme_bw()+
        xlab("Number of steps") +
        ylab("Count") +ggtitle("total number of steps taken each day")

library(tidyverse)


mean_by_interval<-aggregate(data$steps,by = list(data$interval),FUN = mean,na.rm=TRUE)


install.packages("plotly")
library(plotly)
install.packages("hrbrthemes")
library(hrbrthemes)

p <- mean_by_interval %>%
        ggplot( aes(x=Group.1, y=x)) +
        geom_area(fill="#69b3a2", alpha=0.5) +
        geom_line(color="#69b3a2") +
        ylab("bitcoin price ($)") +
        theme_ipsum()+
        geom_vline(xintercept = 835,linetype="dotted",color = "red", size=0.2)

p <- ggplotly(p)
p

max_st<-subset(mean_by_interval,x==max(x))
max_st[,1]
library(tidyverse)


sum(is.na(data$steps)==TRUE)  



people_rep <- data %>%
        mutate(steps=replace_na(steps, mean(steps, na.rm=TRUE)))

data$steps[is.na(data$steps)] <- mean(data$steps,na.rm=T)

library(tidyverse)
if (data$steps=="NA"){
        replace(data$steps,555)
}


for (i in 1:2000)        {
        data$steps[data$steps %in% c("NA")] & data$steps[data$Intervals ==i]<-"habitat"
        
        
}

apply(data,1,replace_na(mean))

merge()


data$steps<-replace_na(data$steps,tapply(data$steps, data$interval, mean,na.rm=TRUE))



        merge(data,mean_by_interval,by.x=interval,by.y=Group.1)
         



Age <- c(12,15,23,29) # création de la variable Age
Genre <- c("homme", "homme", "femme", "femme") # création de la variable Genre
data <- data.frame(Age, Genre) # on met les 2 variables dans un tableau
# On souhaite connaître l'âge moyen selon le genre
tapply(data$Age, data$Genre, mean)

tapply(data$steps, data$interval, replace_na,na.rm=TRUE)

group_by(data,interval)

apply(a,1,fn)

if (data$steps=="NA"){
        data$steps=="YES"
}

data$interval<-as.character(data$interval)
mean_by_interval$Group.1<-as.character(mean_by_interval$Group.1)




library(tidyverse)

replace(data$steps,data$mean)
replace(a$steps, which(data$steps %in% "NA"), a$mean)

fn<-function(x) 
  {if (is.na(a$steps)==TRUE) {
  a$steps<-a$mean
  }
}




mean_by_interval<-aggregate(data$steps,by = list(data$interval),FUN = mean,na.rm=TRUE)

colnames(mean_by_interval)=c("interval","mean")

a<-merge(data,mean_by_interval,by="interval")

b<-which(is.na(a$steps))

a$steps[b]<-a$mean[b]


colnames(mean_by_interval)=c("interval","mean")
data2<-merge(data,mean_by_interval,by="interval")
b<-which(is.na(data2$steps))
data2$steps[b]<-data2$mean[b]
head(data2)


par(mfrow = c(1,2))
p
p2
agg2 = aggregate(data2$steps,by = list(data2$date),FUN = sum)
names(agg2)<-c("Date","Number_of_steps")

p2 <- ggplot(agg2, aes(x=Number_of_steps)) + geom_histogram(color="white", fill="#69b3a2",bins=30)+ theme_bw()+
  xlab("Number of steps") +
  ylab("Count") +ggtitle("total number of steps taken each day (after NAs imputation)")
print(p2)

weekdays(data2$date)
library(lubridate)
library(dplyr)
data$date<-ymd(data2$date)


data2<-mutate(data2,wd=weekdays(data2$date))

data2$wdi<-ifelse(data2$wd %in% c("lundi","mardi","mercredi","jeudi","vendredi"), "weekday","weekend")


data3<-aggregate(data2$steps,by=list(data2$interval,data2$wdi),FUN=mean)
colnames(data3)<-c("intervals","weekday_indicator","number_of_steps")

# Libraries
library(ggplot2)

library(dplyr)



# Plot
data3 %>%
  ggplot( aes(x=intervals, y=number_of_steps, color=weekday_indicator)) +
  labs(x = "intervals", y = "number of steps",title = "Average number of steps per time interval")+
  geom_line()
