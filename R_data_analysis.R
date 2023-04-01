
#install and load package tidyverse

install.packages("tidyverse")
library(tidyverse)

#set directory

getwd()
setwd("C:/Users/Utilizador/Desktop/Bellabeat")


#import the data to data frames

daily_activity <- read_csv("dailyActivity_merged.csv")
daily_calories <- read_csv("dailyCalories_merged.csv")
daily_intensities <- read_csv("dailyIntensities_merged.csv")
daily_sleepday <- read_csv("dailySleepDay_merged.csv")
daily_steps <- read_csv("dailySteps_merged.csv")
weight_info <- read_csv("weightLogInfo_merged.csv")
heart_rate <- read_csv("heartrate_seconds_merged.csv")
steps_hour <- read_csv("hourlySteps_merged.csv")


#take a look to the data

colnames(daily_activity)
colnames(weight_info)
colnames(daily_calories)
colnames(daily_intensities)
colnames(daily_sleepday)
colnames(daily_steps)
colnames(heart_rate)
colnames(steps_hour)
str(daily_activity)
str(weight_info)
str(daily_calories)
str(daily_intensities)
str(daily_sleepday)
str(daily_steps)  
str(heart_rate)
str(steps_hour)
  
  
  
#drop null values

drop_na(daily_activity)
drop_na(weight_info)
drop_na(daily_calories)
drop_na(daily_intensities)
drop_na(daily_sleepday)
drop_na(daily_steps)
drop_na(heart_rate)
drop_na(steps_hour)


#check distinct values in order to understand how many participants in each data set

n_distinct(daily_activity$Id)
n_distinct(weight_info$Id)
n_distinct(daily_calories$Id)
n_distinct(daily_intensities$Id)
n_distinct(daily_sleepday$Id)
n_distinct(heart_rate$Id)
n_distinct(steps_hour$Id)


#some summary statistics in order to take a deeper look at the dataset

daily_activity %>%
  select(TotalSteps,TotalDistance,SedentaryMinutes,Calories) %>%
  summary()

daily_intensities %>%
  select(VeryActiveMinutes,
         VeryActiveDistance,
         SedentaryMinutes,
         SedentaryActiveDistance,
         LightlyActiveMinutes,
         LightActiveDistance,
         FairlyActiveMinutes,
         ModeratelyActiveDistance)%>%
  summary()

daily_sleepday %>%  
  select(TotalSleepRecords,
         TotalMinutesAsleep,
         TotalTimeInBed)%>%
  summary()


#to make data more usable, some tables were joined 

join_sleep_activity <-merge(daily_sleepday,daily_activity,by="Id")
join_sleep_activity_heart <- merge(join_sleep_activity, heart_rate, by='Id')
join_calories_steps <- merge(daily_calories,daily_steps,by="Id")
join_calories_intensities <-merge(daily_calories,daily_intensities,by="Id")

View(join_calories_steps)

#visualization: relationship between burnt calories and steps taken 

ggplot(data = join_calories_steps,aes(x=StepTotal,y=Calories))+geom_point(color="orange")

#visualization: what makes burn more calories - time or distance?

ggplot(data = join_calories_intensities, aes(x=VeryActiveMinutes, y=Calories))+geom_point()


#visualization: relationship between daily steps and sedentary minutes

ggplot(data=daily_activity) + geom_point(mapping = aes(x=TotalSteps,y=SedentaryMinutes))
       

#visualization: relationship between daily sleep and daily steps


ggplot(data=join_sleep_activity,aes(x=TotalTimeInBed,y=TotalSteps))+geom_col(color="blue")

