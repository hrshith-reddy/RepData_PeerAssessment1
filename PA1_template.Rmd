
#Coursera Project 1

##Loading and preprocessing the data


```{r,message=FALSE}
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",destfile="./Activity") # Downloading the required file

if(!dir.exists("./data")) #creating a new directory data to store the csv file
     
  dir.create("./data")

unzip("./Activity",exdir="./data") #unzipping the compressed file

data<-read.csv("./data/activity.csv") #reading the table into R

data$date<-as.POSIXct(data$date,format="%Y-%m-%d") #converting the character format into time format

library("ggplot2") #loading the ggplot2 package

library("dplyr") #loading the dplyr package
```


##Histogram of the total number of steps taken each day


```{r}
histdata<-group_by(data,date)%>%summarise(totalsteps=sum(steps,na.rm=T))%>%filter(totalsteps!=0) # mutating the data into a required format

qplot(histdata$totalsteps,bins=50,fill=I("green"),col=I("blue"),xlab="Total number of steps in a  day",main="histogram of total number of steps in a day") # constructing the required histogram
```


##What is mean/median total number of steps taken per day?



```{r}
mean(histdata$totalsteps) # average number of steps taken in a day

median(histdata$totalsteps) # median number of steps taken in a day
```


##What is the average daily activity pattern?


```{r}
timeseries<-group_by(data,interval)%>%summarise(dailyaverage=mean(steps,na.rm=T)) # mutating the data into a form suitable for the timeseries

ggplot(timeseries,aes(interval,dailyaverage))+geom_line(aes(col=I("orange")),size=1)+xlab("5 minute intervals")+ylab("number of steps averaged over all the days")+ggtitle("Time-Series of the number of steps") # constructing the time series plot
```


##The 5-minute interval that, on average, contains the maximum number of steps


```{r}
timeseries<-arrange(timeseries,desc(dailyaverage)) #arranging the time intervals in decreasing order of average number of steps

timeseries[1,1] # the interval with maximum number of steps on an average
```

##Imputing missing values


```{r}
missing<-is.na(data$steps)# to find missing values

sum(missing)# number of missing values

fix<-grep(T,missing) # positions of missing values

newdataset<-data # creating a replica of the dataset to fill the missing values with sensible values

for(f in fix) # we shall replace the missing value with the average value for that interval averaged over all the days
{
    newdataset[f,"steps"]<-timeseries[which(newdataset[f,"interval"]==timeseries$interval),"dailyaverage"] #replacing the missing value with the average value of steps calculated over all the days for that interval
}
```


##Histogram of the total number of steps taken each day after missing values are imputed


```{r}
histdata<-group_by(newdataset,date)%>%summarise(totalsteps=sum(steps,na.rm=T))%>%filter(totalsteps!=0) # mutating the new data set in order to convert it to a form suitable for histogram construction

qplot(histdata$totalsteps,bins=50,fill=I("green"),col=I("blue"),xlab="Total number of steps in a day",main="histogram of total number of steps in a day") # constructing the histogram

mean(histdata$totalsteps) # new mean after imputing values is the same as old mean

median(histdata$totalsteps) # new median after imputing value exceeds the older median by a miniscule amount 

```


##Are there differences in activity patterns between weekdays and weekends?


```{r}
newdataset<-mutate(newdataset,day=as.character(format(newdataset$date,"%A"))) # mutating the new data set in order to include the comlumn of day
  
for(d in 1:nrow(newdataset)) # classifying the days of the new data set into weekdays and weekends
  {
    
      if(newdataset[d,"day"]=="Saturday" | newdataset[d,"day"]=="Sunday")
        {
           newdataset[d,"day"]<-"Weekend"
        }
      
      else 
        {
          newdataset[d,"day"]<-"Weekday"
        }
  }
newdataset$date<-as.factor(newdataset$date) #factorizing the character vector indicating wether a given day is "weekday" or "weekend"

newtimeseries<-group_by(newdataset,day,interval)%>%summarise(dailyaverage=mean(steps))
#mutating the dataset to calaculate the average number of steps for a given time interval separately for a weekend and a weekday and storing the result in the column dailyaverage

ggplot(newtimeseries,aes(interval,dailyaverage))+geom_line(aes(col=day),)+facet_grid(day~.)+ylab("Number of steps taken")+ggtitle("Time series of number of steps") #constructing the time series plot for weekends and weekdays separately
```
  
  