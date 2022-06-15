library(readr)
library(lubridate)
library(ggplot2)
library(dslabs)
library(dplyr)
library(ggrepel)
library(ggthemes)

#loading dataset
gso.fire <- read_csv("Greensboro_Fire_Incidents.csv")

#filtering for incidents prior to June 1st 2022
gso.fire = 
  gso.fire %>%
  mutate(AlarmDate2 = make_date(Year, Month, Day)) %>%
  filter(AlarmDate2 < '2022-06-01')

#get the count of incidents by property type
gso.fire %>%
  count(PropertyUse, sort=TRUE) %>%
  head(10)

#filter for property type 1-2 family dwellings
gso.fire = gso.fire %>%
  filter(PropertyUse == '419 - 1 or 2 family dwelling')

#convert to call process time to seconds and add to gso.fire
gso.fire = gso.fire %>%
  mutate(call_process_period = lubridate::hms(CallProcessingTime), 
         call_process_seconds = period_to_seconds(call_process_period))

#histogram of call process seconds vs count
gso.fire %>% 
  filter(call_process_seconds < 500) %>%
  ggplot(aes(x = call_process_seconds)) +
  geom_histogram()

#density plot of call process seconds
gso.fire %>% 
  filter(call_process_seconds < 500) %>%
  ggplot(aes(x = call_process_seconds)) +
  geom_density()

#box plot of days of the week vs call process time
gso.fire %>%
  ggplot(aes(x = DayOfWeek, y = call_process_seconds, fill = DayOfWeek)) +
  geom_boxplot(show.legend = FALSE) +
  scale_y_log10() +
  xlab("Days of the Week") +
  ylab("Total Call Process Time") +
  ggtitle("Days of the Week vs Call Process Time")

#bar graph of work shift and call process time
gso.fire %>%
  ggplot(aes(x = shift, y = call_process_seconds, fill = shift)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Work Shifts") +
  ggtitle("Work Shifts vs Total Call Process Time")

#bar graph of month and call process time
gso.fire %>%
  ggplot(aes(x = Month, fill = Month)) +
  geom_bar(show.legend = FALSE) +
  xlab("Month") +
  ylab("Total Call Process Time") +
  ggtitle("Month vs Total Call Process Time")

#...
#now looking at Response Time
#...

#convert response time to seconds and add to gso.fire
gso.fire = gso.fire %>%
  mutate(response_time_period = lubridate::hms(ResponseTime),
         response_time_seconds = period_to_seconds(response_time_period))

#bar graph of days of the week vs response time
gso.fire %>%
  ggplot(aes(x = DayOfWeek, y = response_time_seconds, fill = DayOfWeek)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Days of the Week") +
  ylab("Response Time") +
  ggtitle("Days of the Week vs response time")

#bar graph of work shift vs response time
gso.fire %>%
  ggplot(aes(x = shift, y = response_time_seconds, fill = shift)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Work Shifts") +
  ylab("Response Time") +
  ggtitle("Work Shifts vs Response Time")

gso.fire %>%
  ggplot(aes(x = Month, y = response_time_seconds, fill = Month)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Month") +
  ylab("Response Time") +
  ggtitle("Month vs Response Time")

#...
#now looking at total response time
#...

#convert total response time to seconds and add to gso.fire
gso.fire = gso.fire %>%
  mutate(total_response_period = lubridate::hms(TotalResponseTime),
         total_response_seconds = period_to_seconds(total_response_period))

#data frame of day of week and total response time
total_response_time_data_day <- data.frame(index = gso.fire$DayOfWeek, var1 = total_response_seconds)

ggplot(total_response_time_data_day, aes(index, var1, color = index)) +
  geom_line(size = 2) +
  xlab("Days of Week") +
  ylab("Total Response Time") +
  ggtitle("Days of Week vs Total Repsonse Time")

#data frame of work shift and total response time
total_response_time_data_shift <- data.frame(index = gso.fire$shift, var1 = total_response_seconds)

ggplot(total_response_time_data_shift, aes(index, var1, color = index)) +
  geom_line(size = 2) +
  xlab("Work Shifts") +
  ylab("Total Response Time") +
  ggtitle("Work Shifts vs Total Response Time")

#data frame of month and total response time
#20430 rows containging missing values removed
total_response_time_data_month <- data.frame(index = gso.fire$Month, var1 = total_response_seconds)

ggplot(total_response_time_data_month, aes(index, var1)) +
  geom_line(color = "blue") +
  xlab("Month") +
  ylab("Total Response Time") +
  ggtitle("Month vs Total Response Time")

#...
#now making a histogram for total response time
#...


#data frame of week and total response time
total_response_time_data1 <- data.frame(index = gso.fire$Week, var1 = total_response_seconds)


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
#geom_bar

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
