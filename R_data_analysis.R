
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
join_calories_steps <- merge(daily_calories,daily_steps,by="Id")
join_calories_intensities <-merge(daily_calories,daily_intensities,by="Id")

head(join_sleep_activity)


#calculate averages by id

daily_average <- join_sleep_activity %>%
  group_by(Id) %>%
  summarise (mean_daily_steps = mean(TotalSteps), mean_daily_calories = mean(Calories), mean_daily_sleep = mean(TotalMinutesAsleep))

head(daily_average)


#categorize types of users by the number of daily steps

daily_average_steps <- daily_average %>%
  mutate(user_type = case_when(
    mean_daily_steps < 3000 ~ "sedentary",
    mean_daily_steps >= 3000 & mean_daily_steps < 5000 ~ "moderate active", 
    mean_daily_steps >= 5000 & mean_daily_steps < 8000 ~ "very active", 
    mean_daily_steps >= 8000 ~ "super active"))

head(daily_average_steps)


user_percentage<- daily_average_steps %>%
  group_by(user_type) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(user_type) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

user_percentage$user_type <- factor(user_percentage$user_type , levels = c("super active", "very active", "moderate active", "sedentary"))


head(user_percentage)

#visualization: percentage types of users

user_percentage %>%
  ggplot(aes(x="",y=total_percent, fill=user_type)) +
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start=0)+
  theme_minimal()+
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold")) +
  scale_fill_manual(values = c("#9EE2A2","#68E16E", "#0E9C15", "#DA2914")) +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))+
  labs(title="Types of users percentage")

#categorize types of users by the number of daily steps


daily_average_sleep <- daily_average %>%
  mutate(user_type = case_when(
    mean_daily_sleep < 250 ~ "sleep deprived",
    mean_daily_sleep >= 250 & mean_daily_sleep< 425 ~ "little sleep", 
    mean_daily_sleep >= 425 & mean_daily_sleep < 550 ~ "enough sleep", 
    mean_daily_sleep >= 550 ~ "too much sleep"))

head(daily_average_sleep)


user_percentage_sleep<- daily_average_sleep %>%
  group_by(user_type) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(user_type) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

user_percentage_sleep$user_type <- factor(user_percentage_sleep$user_type , levels = c("sleep deprived", "little sleep", "enough sleep", "too much sleep"))

head(user_percentage_sleep)


#visualization: percentage types of users


user_percentage_sleep %>%
  ggplot(aes(x="",y=total_percent, fill=user_type)) +
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start=0)+
  theme_minimal()+
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold")) +
  scale_fill_manual(values = c("#9EE2A2","#68E16E", "#0E9C15", "#DA2914")) +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))+
  labs(title="Types of sleep users percentage")



#visualization: relationship between burnt calories and steps taken 

ggplot(data = join_calories_steps,aes(x=StepTotal,y=Calories))+geom_smooth(color="orange") + xlim(0,20000) + labs(x='Daily Steps', y='Calories', title='Daily Steps vs Calories')

#visualization: relationship between burnt calories and activity duration

ggplot(data = join_calories_intensities, aes(x=VeryActiveMinutes, y=Calories))+geom_smooth() + xlim(0,125) + labs(x='Minutes active', y='Calories', title='Minutes Active vs Calories')


#visualization: relationship between daily sleep and daily stepsTotalSteps

ggplot(data=join_sleep_activity,aes(x=TotalSteps,y=TotalTimeInBed))+geom_smooth(color="blue") + labs(x='Minutes Asleep', y = 'Daily Steps', title = 'Sleep Duration Vs. Daily Steps')



