#Install packages for cleaning
install.packages("tidyverse")
install.packages("skimr")
install.packages("janitor")
install.packages("here")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")

#load packages
library(tidyverse)
library(skimr)
library(janitor)
library(readr)
library(here)
library(dplyr)
library(lubridate)
library(ggplot2)

#import datasets
dailyActivity<- read_csv("~/Desktop/Case Study- Bellabeat/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
dailyCalories<- read_csv("~/Desktop/Case Study- Bellabeat/Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")
dailyIntensities<- read_csv("~/Desktop/Case Study- Bellabeat/Fitabase Data 4.12.16-5.12.16/dailyintensities_merged.csv")
dailySteps<- read_csv("~/Desktop/Case Study- Bellabeat/Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv")
heartrateSeconds<- read_csv("~/Desktop/Case Study- Bellabeat/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")
hourlyCalories<- read_csv("~/Desktop/Case Study- Bellabeat/Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv")
hourlyIntensities<-read_csv("~/Desktop/Case Study- Bellabeat/Fitabase Data 4.12.16-5.12.16/hourlyintensities_merged.csv")
hourlySteps<-read_csv("~/Desktop/Case Study- Bellabeat/Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")
minuteCaloriesNarrow<-read_csv("~/Desktop/Case Study- Bellabeat/Fitabase Data 4.12.16-5.12.16/minuteCaloriesNarrow_merged.csv")
minuteCaloriesWide<-read_csv("~/Desktop/Case Study- Bellabeat/Fitabase Data 4.12.16-5.12.16/minuteCaloriesWide_merged.csv")
minuteIntensitiesNarrow<-read_csv("~/Desktop/Case Study- Bellabeat/Fitabase Data 4.12.16-5.12.16/minuteIntensitiesNarrow_merged.csv")
minuteIntensitiesWide<-read_csv("~/Desktop/Case Study- Bellabeat/Fitabase Data 4.12.16-5.12.16/minuteIntensitiesWide_merged.csv")
minuteMETsNarrow<-read_csv("~/Desktop/Case Study- Bellabeat/Fitabase Data 4.12.16-5.12.16/minuteMetsNarrow_merged.csv")
minuteSleep<-read_csv("~/Desktop/Case Study- Bellabeat/Fitabase Data 4.12.16-5.12.16/minuteSleep_merged.csv")
minuteStepsNarrow<-read_csv("~/Desktop/Case Study- Bellabeat/Fitabase Data 4.12.16-5.12.16/minuteStepsNarrow_merged.csv")
minuteStepsWide<-read_csv("~/Desktop/Case Study- Bellabeat/Fitabase Data 4.12.16-5.12.16/minuteStepsWide_merged.csv")
sleepDay<-read_csv("~/Desktop/Case Study- Bellabeat/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
weightLogInfo<-read_csv("~/Desktop/Case Study- Bellabeat/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")



#Getting to know the Data
#Head of the data
head(dailyActivity)
head(dailyIntensities)
head(dailySteps)
head(dailyCalories)

#column names
colnames(dailyActivity)
colnames(dailyIntensities)
colnames(dailyCalories)
colnames(dailySteps)

#skimming the data
skim_without_charts(dailyActivity)
skim_without_charts(dailyIntensities)
skim_without_charts(dailyCalories)
skim_without_charts(dailySteps)


#DATA PROCESSING
#searching for null values
is.null(dailyActivity)
is.null(dailyIntensities)
is.null(dailyCalories)
is.null(dailySteps)
is.null(heartrateSeconds)
is.null(hourlyCalories)
is.null(hourlyIntensities)
is.null(hourlySteps)
is.null(minuteCaloriesNarrow)
is.null(minuteCaloriesWide)
is.null(minuteIntensitiesNarrow)
is.null(minuteIntensitiesWide)
is.null(minuteMETsNarrow)
is.null(minuteStepsNarrow)
is.null(minuteStepsWide)
is.null(weightLogInfo)

#searching for NA values
sum(is.na(dailyActivity))
sum(is.na(dailyIntensities))
sum(is.na(dailyCalories))
sum(is.na(dailySteps))
sum(is.na(heartrateSeconds))
sum(is.na(hourlyCalories))
sum(is.na(hourlyIntensities))
sum(is.na(hourlySteps))
sum(is.na(minuteCaloriesNarrow))
sum(is.na(minuteCaloriesWide))
sum(is.na(minuteIntensitiesNarrow))
sum(is.na(minuteIntensitiesWide))
sum(is.na(minuteStepsNarrow))
sum(is.na(minuteStepsWide))
sum(is.na(sleepDay))
sum(is.na(weightLogInfo))



#Checking if Identical
#install compare package
install.packages("compare")
library(compare)

#Removing unnecessary data
#compare Id rows
compare(dailyActivity$Id, dailyCalories$Id)

#compare total distance and tracker distance.
compare(dailyActivity$TotalDistance, dailyActivity$TrackerDistance)

#checking how many Unique ID's there are
n_unique(dailyActivity$Id)


#examining the hourly datasets
head(hourlyCalories)
head(hourlyIntensities)
head(hourlySteps)

#compare the activity hours from hourly to check if identical so I can merge
compare(hourlyCalories$ActivityHour, hourlyIntensities$ActivityHour, hourlySteps$ActivityHour)


#Merging hourly df's into an hourlyActivity df
hourlyActivity <- left_join(hourlyCalories, hourlyIntensities) %>%
  left_join(hourlySteps)


#converting the datetime in hourlyActivity and creating a cleaned hourlyActivity DF
hourlyActivity_clean <- hourlyActivity
hourlyActivity_clean$ActivityHour <- as.POSIXct(hourlyActivity_clean$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p", tz = Sys.timezone())
install.packages("hms")
library(hms)
hourlyActivity_clean$Time <- as_hms(hourlyActivity_clean$ActivityHour)
hourlyActivity_clean$Date <- as_date(hourlyActivity_clean$ActivityHour)
hourlyActivity_clean$Weekdate <-weekdays(hourlyActivity_clean$Date)


#convert dailyactivity date column to datetime object 
#and then create new column of specific day
dailyActivity_clean <- dailyActivity
dailyActivity_clean$ActivityDate <- as.POSIXct(dailyActivity_clean$ActivityDate, format = "%m/%d/%Y")
dailyActivity_clean$weekdate <- weekdays(dailyActivity_clean$ActivityDate)
  
#Check for duplicates
sum(duplicated(hourlyActivity_clean))
sum(duplicated(dailyActivity_clean))
sum(duplicated(sleepDay))
sum(duplicated(weightLogInfo))

#identify duplicates
which(duplicated(sleepDay))
which(duplicated(hourlyActivity_clean))

#remove duplicates from sleepDay and work on creating clean sleepDay
sleepDay_clean <- distinct(sleepDay)

#validate if it worked
sum(duplicated(sleepDay_clean))

#convert to datetime, the time stamps are removed because they are all beginning
#at the beginning of the day.
sleepDay_clean$SleepDay <- as.POSIXct(sleepDay_clean$SleepDay, format = "%m/%d/%Y")
sleepDay_clean$weekdate <- weekdays(sleepDay_clean$SleepDay)

#convert weightLog into dates into datetime and add to clean DF and separate into two cols
weightLogInfo_clean <- weightLogInfo

weightLogInfo_clean <- weightLogInfo_clean %>%
  separate(Date, into = c('Date', 'Time'), sep = ' ')

weightLogInfo_clean$Date <- as.POSIXct(weightLogInfo_clean$Date, format = "%m/%d/%Y")
weightLogInfo_clean$weekdate <-weekdays(weightLogInfo_clean$Date)
weightLogInfo_clean$Time_hms <- as_hms(weightLogInfo_clean$Time)

#remove df's that I will no longer be using
rm(dailyActivity)
rm(dailyCalories)
rm(dailyIntensities)
rm(dailySteps)

rm(hourlyActivity)
rm(hourlyCalories)
rm(hourlyIntensities)
rm(hourlySteps)

rm(sleepDay)
rm(weightLogInfo)

##commence some analysis

#So far I have created 4 datasets that I want to work with
#dailyActivity_clean, hourlyActivity_clean, sleepDay_clean,
#and weightLoginfo_clean


#-------------------------------

#playing with sorting/filter/arranging

#average calories burned by day
dailyActivity_clean %>% 
  group_by(weekdate) %>% 
  summarize((mean_cal = mean(Calories)), max_cal_by_day = max(Calories))

#max calories by day
dailyActivity_clean %>% 
  group_by(weekdate) %>% 
  summarize(max_cal_by_day = max(Calories))

#users mean cal and steps by day.
dailyActivity_clean %>% 
  group_by(Id) %>%
  summarize(mean_cal = mean(Calories), mean_steps = mean(TotalSteps))


#number of logged dates per ID 
dailyActivity_clean %>%
  group_by(Id) %>%
  summarize(num_logs = n_unique(ActivityDate))

#creating a df for logged dates to make a plot with later
logged_dates <- dailyActivity_clean %>%
  group_by(Id) %>%
  summarize(num_logs = n_unique(ActivityDate))


#percentage of users that logged weight
n_unique(weightLogInfo_clean$Id)
n_unique(dailyActivity_clean$Id)
(8/33)*100

#avg step per hour
avg_step_per_hour <- hourlyActivity_clean %>%
  group_by(Time) %>%
  summarize(avg_step = sum(StepTotal)/940)
  
#avg cal per hour
avg_cal_per_hour <- hourlyActivity_clean %>%
  group_by(Time) %>%
  summarize(avg_cal = sum(Calories)/940)

# unique sleep entries
n_unique(sleepDay_clean$Id)
User_Sleep_Habits <- sleepDay_clean %>%
  group_by(Id) %>%
  summarize(sleep_entries= sum(TotalSleepRecords),
            TimeAsleep = sum(TotalTimeInBed))
View(User_Sleep_Habits)

#Creating a Table identifying Users' habits focusing on activity minutes
unique_df <- dailyActivity_clean %>%
  group_by(Id) %>%
  summarize(TotalActiveMinutes = sum(VeryActiveMinutes) + sum(LightlyActiveMinutes)
            + sum(FairlyActiveMinutes) + sum(SedentaryMinutes), 
            VeryActive = sum(VeryActiveMinutes),
            LightlyActive = sum(LightlyActiveMinutes),
            FairlyActive = sum(FairlyActiveMinutes),
            SedentaryActive = sum(SedentaryMinutes),
            avg_cal = mean(Calories),
            avg_step = mean(TotalSteps))
           


UserHabits <- left_join(unique_df, entries_df)

View(unique_df)
View(UserHabits)



#Playing with plots-----------------------
ggplot(dailyActivity_clean, aes(x = weekdate, y = TotalSteps, fill= weekdate))+
geom_boxplot()


ggplot(data = dailyActivity_clean, aes(x=TotalSteps, y = Calories))+
  geom_point()+
  geom_smooth()
  

ggplot(hourlyActivity_clean, aes(x = Time_hms, y = StepTotal))+
  geom_point()

ggplot(data = dailyActivity_clean, mapping = aes(x= TotalSteps, y = Calories))+
  geom_point(aes(color = weekdate))+
  facet_wrap(~weekdate)

#avg step per hour plot
ggplot(data = avg_step_per_hour, aes(x = Time, y = avg_step))+
  geom_histogram(stat = "identity")

#avg step per hour plot advanced
ggplot(data = avg_step_per_hour, aes(x = factor(Time), y = avg_step))+
  geom_histogram(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Average Steps per Hour")+
  labs(x = "Time of the Day", y = "Average Steps")

#avg calorie per hour
ggplot(data = avg_cal_per_hour, aes(x = factor(Time), y = avg_cal))+
  geom_histogram(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Average Calories Burned per Hour")+
  labs(x = "Time of the Day", y = "Average Calories Burned")


#can use for viz exploring weight logging
ggplot(data = weightLogInfo_clean)+
  geom_bar(mapping = aes(x = weekdate))



#plot of unique users and their sleep entries
ggplot(data = entries_df, aes(x = factor(Id), y = sleep_entries))+
  geom_histogram(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Unique Users and their Sleep Logging Habits")+
  labs(x = "Unique User", y = "Entries Logged")


#users and step entries (which is basically when they are wearing devices)
ggplot( data = logged_dates, aes(x = factor(Id), y = num_logs,))+ 
  geom_histogram(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Unique Users and their Wearing Habits")+
  labs(x = "Unique User", y = "Entries Logged")
  







#__________________Exporting csv's

write.csv(dailyActivity_clean, "dailyActivity_clean.csv")
write.csv(entries_df, "entries_df.csv")
write.csv(weightLogInfo_clean, "weightLogInfo_clean.csv")
write.csv(UserHabits, "User_Habits.csv")


