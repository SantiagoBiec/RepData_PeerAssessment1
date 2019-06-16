---
title: "PA1_template"
author: "Santiago Biec"
date: "16 de junio de 2019"
output: html_document
---

## Loading and preprocessing the data

```{r, echo=TRUE}
raw_df = read.csv("activity.csv")

```

## What is mean total number of steps taken per day?


```{r , echo=TRUE}
tot_steps = tapply(raw_df$steps,raw_df$date,sum,na.rm = TRUE)
hist(tot_steps)
```

The mean and median of total daily steps are:

```{r , echo=TRUE}
print(mean(tot_steps))
print(median(tot_steps))

```

## What is the average daily activity pattern?

```{r , echo=TRUE}
library(ggplot2)
avg_steps = tapply(raw_df$steps,raw_df$interval,mean,na.rm = TRUE)
ts_df = as.data.frame.table(avg_steps)
plot(avg_steps,type = "l")
```

The 5-minute interval with the maximum average number of steps is:

```{r , echo=TRUE}
index_max = which.max(ts_df$Freq)
as.character(ts_df$Var1)[index_max]
```

## Imputing missing values
The total number of missing values in the dataset is:

```{r , echo=TRUE}
sum(is.na(raw_df))
```

I am creating a new dataframe filling the NAs with the average number of steps
of that 5min period

```{r , echo=TRUE}
new_df = raw_df

index_na = is.na(raw_df)[,1]
imp_steps = avg_steps[match(raw_df$interval[index_na],names(avg_steps))]

new_df$steps[index_na] = imp_steps

```


The new histogram of the total number of steps is:

```{r , echo=TRUE}
ntot_steps = tapply(new_df$steps,new_df$date,sum,na.rm = TRUE)
hist(ntot_steps)
```

The new mean and median of the total number of steps is:

```{r , echo=TRUE}
print(mean(ntot_steps))
print(median(ntot_steps))

```

These values have changed significantly. As I have inputed the average value to 
serveral days, the median now matches the average.

## Are there differences in activity patterns between weekdays and weekends?

```{r , echo=TRUE}
wd = weekdays(as.Date(new_df$date))
wd[wd == "Saturday"] = "weekend"
wd[wd == "Sunday"] = "weekend"
wd[wd != "weekend"] = "weekday"
wd = as.factor(wd)

wd_df = new_df[wd == "weekday",]
we_df = new_df[wd == "weekend",]    

avg_steps_wd = tapply(wd_df$steps,wd_df$interval,mean,na.rm = TRUE)
avg_steps_we = tapply(we_df$steps,we_df$interval,mean,na.rm = TRUE)


par(mfrow=c(2,1))
plot(avg_steps_wd, type = "l",main="Weekday")
plot(avg_steps_we, type = "l",main="Weekend")

```
