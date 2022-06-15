library(readr)

gso.fire <- read_csv("C:/Users/dmbut/Dropbox/PC/Downloads/Greensboro_Fire_Incidents.csv")

class(gso.fire$CallProcessingTime)

library(lubridate)
library(ggplot2)
library(dslabs)
library(dplyr)
library(ggrepel)
library(ggthemes)

?lubridate

#convert to Period object
call_process_period <- lubridate::hms(gso.fire$CallProcessingTime)

class(call_process_period)

#converted call process time from hms to seconds
call_process_seconds <- period_to_seconds(call_process_period)

class(call_process_seconds)

print(call_process_seconds)


call_process_data_day <- data.frame(index = gso.fire$DayOfWeek, var1 = call_process_seconds)

#from this graph Wednesday had the lowest Total Call Process Time
ggplot(df1, aes(index, var1, color = index)) +
  geom_line(size = 2) +
  xlab("Days of Week") +
  ylab("Total Call Process Time") +
  ggtitle("Days of Week vs Call Process Time")

call_process_data_shift <- data.frame(index = gso.fire$shift, var1 = call_process_seconds)

#from this graph shift A looks had the lowest Total Call Process Time
ggplot(df2, aes(index, var1, color = index)) +
  geom_line(size = 2) +
  xlab("Work Shifts") +
  ylab("Total Call Process Time") +
  ggtitle("Work Shifts vs Total Call Process Time")

call_process_data_month <- data.frame(index = gso.fire$Month, var1 = call_process_seconds)

#from this graph the Total Call Process Time is low in May and December
ggplot(df3, aes(index, var1)) +
  geom_line(color = "blue") +
  xlab("Month") +
  ylab("Total Call Process Time") +
  ggtitle("Month vs Total Call Process Time")

#...
#now looking at Response Time
#...

#convert to Period object
response_time_period <- lubridate::hms(gso.fire$ResponseTime)

class(response_time_period)

#converted response time from hms to seconds
response_time_seconds <- period_to_seconds(response_time_period)


response_time_data_day <- data.frame(index = gso.fire$DayOfWeek, var1 = response_time_seconds)

#Sunday has the lowest response time from all the data
ggplot(df1, aes(index, var1, color = index)) +
  geom_line(size = 2) +
  xlab("Days of Week") +
  ylab("Response Time") +
  ggtitle("Days of Week vs Response Time")

response_time_data_shift <- data.frame(index = gso.fire$shift, var1 = response_time_seconds)

#difference not that big... but shift C had the lowest response time
ggplot(df2, aes(index, var1, color = index)) +
  geom_line(size = 2) +
  xlab("Work Shifts") +
  ylab("Response Time") +
  ggtitle("Work Shifts vs Response Time")

response_time_data_month <- data.frame(index = gso.fire$Month, var1 = response_time_seconds)

#June and November had the lowest response time
# 1 row was missing values for this graph so it was automatically removed
ggplot(df3, aes(index, var1)) +
  geom_line(color = "blue") +
  xlab("Month") +
  ylab("Response Time") +
  ggtitle("Month vs Response Time")

#...
#now looking at total response time
#...

#convert to Period object
#some strings failed to parse (where some data NA?)
total_response_period <- lubridate::hms(gso.fire$TotalResponseTime)

#converted total response time from hms to seconds
total_response_seconds <- period_to_seconds(total_response_period)


library(ggplot2)

df1 <- data.frame(index = gso.fire$DayOfWeek, var1 = total_response_seconds)

#Wednesday had the lowest total response time over ten years
ggplot(df1, aes(index, var1, color = index)) +
  geom_line(size = 2) +
  xlab("Days of Week") +
  ylab("Total Response Time") +
  ggtitle("Days of Week vs Total Repsonse Time")

df2 <- data.frame(index = gso.fire$shift, var1 = total_response_seconds)

#Shift A had the lowest total response time over ten years
ggplot(df2, aes(index, var1, color = index)) +
  geom_line(size = 2) +
  xlab("Work Shifts") +
  ylab("Total Response Time") +
  ggtitle("Work Shifts vs Total Response Time")

df3 <- data.frame(index = gso.fire$Month, var1 = total_response_seconds)

print(df3$index)

#January and September and the highest total response time over ten years
ggplot(df3, aes(index, var1)) +
  geom_line(color = "blue") +
  xlab("Month") +
  ylab("Total Response Time") +
  ggtitle("Month vs Total Response Time")

#...
#now making a histogram for total response time
#...


#data frame of week and total response time
total_response_time_data1 <- data.frame(index = gso.fire$Week, var1 = total_response_seconds)

print(total_response_time_data$var1)

#histogram of week in the year vs the count of total response time
ggplot(total_response_time_data1, aes(x = index)) +
  geom_histogram(color = "black", fill = "light blue", bins = 52) +
  xlab("Week in the year") +
  ggtitle("Week in the year vs Count")


#data frame of day of week and total response time
total_response_time_data_day <- data.frame(index = gso.fire$DayOfWeek, var1 = total_response_seconds)

#issue that x is discrete
ggplot(total_response_time_data_day, aes(x = index)) +
  geom_histogram(stat = "count",color = "black", fill = "light blue")


ggplot(total_response_time_data_day, aes(x = index, y = total_response_seconds)) +
  geom_col(color = "light blue") +
  ggtitle("Day of Week vs total response time")
  #scale_x_discrete(limits = total_response_time_data2$index)


#now working on histogram for shift and total response time

#data frame of work shift and total response time
total_response_time_data_shift <- data.frame(index = gso.fire$shift, var1 = total_response_seconds)

#issue that x is discrete
ggplot(total_response_time_data_shift, aes(x = index)) +
  geom_histogram(color = "black", fill = "light blue") +
  xlab("Work Shift") +
  ggtitle("Work Shift vs Count")

ggplot(total_response_time_data_shift, aes(x = index, y = total_response_seconds)) +
  geom_col(color = "light blue") +
  xlab("Work Shift") +
  ggtitle("Work Shift vs total response time")


#now working on histogram for month and total response time

#data frame of month and total response time
#20430 rows containging missing values removed
total_response_time_data_month <- data.frame(index = gso.fire$Month, var1 = total_response_seconds)

#issue that x is discrete
ggplot(total_response_time_data_month, aes(x = index)) +
  geom_histogram(color = "black", fill = "light pink") +
  xlab("Month") +
  ggtitle("Month vs total response time")

ggplot(total_response_time_data_month, aes(x = index, y = total_response_seconds)) +
  geom_col(color = "light pink") +
  xlab("Month") +
  ggtitle("Month vs total response time")


#...
#now looking at number of alarms each day
#...


alarms_per_day_data <- data.frame(index = gso.fire$AlarmDate, var1 = gso.fire$NumberOfAlarms)

#takes awhile to load, lot of data, maybe split it up?
#most days had one alarm, some had two
ggplot(alarms_per_day_data, aes(x = index, y = var1)) +
  geom_point()


#...
#now looking at making some box plots
#...

#making boxplots about call process time

#boxplot of day of week vs call process time
call_process_data_day %>% ggplot(aes(y = var1)) +
  geom_boxplot(coef=1.5, aes(group=index, fill=index)) +
  theme_economist() +
  scale_fill_discrete(name="Day of Week") +
  ggtitle("Day of Week vs call process time")

#boxplot of work shift vs call process time
call_process_data_shift %>% ggplot(aes(y = var1)) +
  geom_boxplot(coef=1.5, aes(group=index, fill=index)) +
  theme_economist() +
  scale_fill_discrete(name="Work Shift") +
  ggtitle("Work Shift vs call process time")

#boxplot of month vs call process time
call_process_data_month %>% ggplot(aes(y = var1)) +
  geom_boxplot(coef=1.5, aes(group=index, fill=index)) +
  theme_economist() +
  scale_fill_discrete(name="Month") +
  ggtitle("Month vs call process time")


#boxplots about total response time

#boxplot of day of week vs total response time
total_response_time_data_day %>% ggplot(aes(y = var1)) +
  geom_boxplot(coef=1.5, aes(group=index, fill=index)) +
  theme_economist() +
  scale_fill_discrete(name="Day of Week") +
  ggtitle("Day of Week vs total response time")

#boxplot of work shift vs total response time
total_response_time_data_shift %>% ggplot(aes(y = var1)) +
  geom_boxplot(coef=1.5, aes(group=index, fill=index)) +
  theme_economist() +
  scale_fill_discrete(name="Work Shift") +
  ggtitle("Work Shift vs total response time")

#boxplot of month vs total response time
total_response_time_data_month %>% ggplot(aes(y = var1)) +
  geom_boxplot(coef=1.5, aes(group=index, fill=index)) +
  theme_economist() +
  scale_fill_discrete(name="Month") +
  ggtitle("Month vs total response time")
