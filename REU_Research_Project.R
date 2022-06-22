library(readr)
library(lubridate)
library(ggplot2)
library(dslabs)
library(dplyr)
library(ggrepel)
library(ggthemes)

#loading data set
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

#convert response time to seconds and add to gso.fire
gso.fire = gso.fire %>%
  mutate(response_time_period = lubridate::hms(ResponseTime),
         response_time_seconds = period_to_seconds(response_time_period))

#convert total response time to seconds and add to gso.fire
gso.fire = gso.fire %>%
  mutate(total_response_period = lubridate::hms(TotalResponseTime),
         total_response_seconds = period_to_seconds(total_response_period))

#...
#histograms
#...

#histogram of call process seconds vs count
gso.fire %>% 
  filter(call_process_seconds < 500) %>%
  ggplot(aes(x = call_process_seconds, fill = "red")) +
  geom_histogram(show.legend = FALSE, color = "black") +
  xlab("Call Process Seconds") +
  ylab("Count") +
  ggtitle("Call Process Seconds vs Count") +
  theme_economist()

#histogram of response time seconds vs count
gso.fire %>%
  filter(response_time_seconds < 800) %>%
  ggplot(aes(x = response_time_seconds, fill = "red")) +
  geom_histogram(show.legend = FALSE, color = "black") +
  xlab("Response Time Seconds") +
  ylab("Count") +
  ggtitle("Response Time Seconds vs Count") +
  theme_economist()

#histogram of total response time seconds vs count
gso.fire %>%
  filter(total_response_seconds < 800) %>%
  ggplot(aes(x = total_response_seconds)) +
  geom_histogram(aes(x = total_response_seconds, fill = "red"), show.legend = FALSE, color = "black") +
  xlab("Total Response Time Seconds") +
  ylab("Count") +
  ggtitle("Total Response Time Seconds vs Count") +
  theme_economist()

#histogram of alarm hour vs count
gso.fire %>%
  ggplot(aes(x = AlarmHour)) +
  geom_histogram(color = "black", fill = "pink") +
  xlab("Alarm Hour") +
  ylab("Count") +
  ggtitle("Alarm Hour vs Count") +
  theme_economist()

#...
#density plots
#...

#density plot of call process seconds
gso.fire %>% 
  filter(call_process_seconds < 500) %>%
  ggplot(aes(x = call_process_seconds)) +
  geom_density(fill = "light blue") +
  geom_vline(aes(xintercept = mean(call_process_seconds)),
             color = "blue", linetype = "dashed") +
  xlab("Call Process Seconds") +
  ylab("Density") +
  ggtitle("Call Process Seconds Density Curve")

#density plot of response time seconds
gso.fire %>%
  filter(response_time_seconds < 750) %>%
  ggplot(aes(x = response_time_seconds)) +
  geom_density(fill = "light green") +
  geom_vline(aes(xintercept = mean(response_time_seconds)),
             color = "dark green", linetype = "dashed") +
  xlab("Response Time Seconds") +
  ylab("Density") +
  ggtitle("Response Time Seconds Density Curve")

#density plot of total response seconds
gso.fire %>%
  filter(total_response_seconds < 1000) %>%
  ggplot(aes(x = total_response_seconds)) +
  geom_density(fill = "light pink") +
  geom_vline(aes(xintercept = mean(total_response_seconds)),
             color = "red", linetype = "dashed") +
  xlab("Total Response Time Seconds") +
  ylab("Density") +
  ggtitle("Total Response Time Seconds Density Curve")


#...
#bar graphs
#...

#bar graph of days of the week vs call process time
gso.fire %>%
  ggplot(aes(x = DayOfWeek, y = call_process_seconds, fill = DayOfWeek)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Days of the Week") +
  ylab("Call Process Time") +
  ggtitle("Days of the Week vs Call Process Time") +
  theme_economist()

#bar graph of work shift vs call process time
gso.fire %>%
  ggplot(aes(x = shift, y = call_process_seconds, fill = shift)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Work Shifts") +
  ylab("Call Process Time") +
  ggtitle("Work Shifts vs Total Call Process Time") +
  theme_economist()

#bar graph of month vs call process time
gso.fire %>%
  ggplot(aes(x = Month, y = call_process_seconds, fill = Month)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Month") +
  ylab("Call Process Time") +
  ggtitle("Month vs Total Call Process Time") +
  theme_economist()

#bar graph of week vs call process time
gso.fire %>%
  ggplot(aes(x = Week, y = call_process_seconds, fill = Week)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Week in the year") +
  ylab("Call Process Time") +
  ggtitle("Week in the year vs Call Process Time") +
  theme_economist()

#bar graph of station vs call process time
gso.fire %>%
  ggplot(aes(x = station, y = call_process_seconds, fill = station)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Station") +
  ylab("Call Process Time") +
  ggtitle("Station vs Call Process Time") +
  theme_economist()

#...
#now looking at Response Time
#...

#bar graph of days of the week vs response time
gso.fire %>%
  ggplot(aes(x = DayOfWeek, y = response_time_seconds, fill = DayOfWeek)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Days of the Week") +
  ylab("Response Time") +
  ggtitle("Days of the Week vs Response Time") +
  theme_economist()

#bar graph of work shift vs response time
gso.fire %>%
  ggplot(aes(x = shift, y = response_time_seconds, fill = shift)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Work Shifts") +
  ylab("Response Time") +
  ggtitle("Work Shifts vs Response Time") +
  theme_economist()

#bar graph of month vs response time
gso.fire %>%
  ggplot(aes(x = Month, y = response_time_seconds, fill = Month)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Month") +
  ylab("Response Time") +
  ggtitle("Month vs Response Time") +
  theme_economist()

#bar graph of week in the year vs response time
gso.fire %>%
  ggplot(aes(x = Week, y = response_time_seconds, fill = Week)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Week in the year") +
  ylab("Response Time") +
  ggtitle("Week in the year vs Response Time") +
  theme_economist()

#bar graph of station vs response time
gso.fire %>%
  ggplot(aes(x = station, y = response_time_seconds, fill = station)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Station") +
  ylab("Response Time") +
  ggtitle("Station vs Response Time") +
  theme_economist()

#...
#now looking at total response time
#...

#bar graph of days of the week vs total response time
gso.fire %>%
  ggplot(aes(x = DayOfWeek, y = total_response_seconds, fill = DayOfWeek)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Days of the Week") +
  ylab("Total Response Time") +
  ggtitle("Days of the Week vs Total Repsonse Time") +
  theme_economist()

#bar graph of work shift vs total response time
gso.fire %>%
  ggplot(aes(x = shift, y = total_response_seconds, fill = shift)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Work Shifts") +
  ylab("Total Response Time") +
  ggtitle("Work Shifts vs Total Response Time") +
  theme_economist()

#bar graph of month vs total response time
gso.fire %>%
  ggplot(aes(x = Month, y = total_response_seconds, fill = Month)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Month") +
  ylab("Total Response Time") +
  ggtitle("Month vs Total Response Time") +
  theme_economist()

#bar graph of week in the year vs total response time
gso.fire %>%
  ggplot(aes(x = Week, y = total_response_seconds, fill = Week)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Week in the year") +
  ylab("Total Response Time") +
  ggtitle("Week in the year vs Total Response Time") +
  theme_economist()
  
#bar graph of station vs total response time
gso.fire %>%
  ggplot(aes(x = station, y = total_response_seconds, fill = station)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Station") +
  ylab("Total Response Time") +
  ggtitle("Station vs Total Response Time") +
  theme_economist()


#...
#now looking at point graphs and number of alarms each day
#...

#point graph of days of the week vs number of alarms
gso.fire %>%
  ggplot(aes(x = DayOfWeek, y = NumberOfAlarms)) +
  geom_point(size = 2) +
  xlab("Days of the Week") +
  ylab("Number of Alarms") +
  ggtitle("Days of the Week vs Number of Alarms") +
  theme_economist()

#point graph of work shift vs number of alarms
gso.fire %>%
  ggplot(aes(x = shift, y = NumberOfAlarms)) +
  geom_point(size = 2) +
  xlab("Work Shift") +
  ylab("Number of Alarms") +
  ggtitle("Work Shift vs Number of Alarms") +
  theme_economist()

#point graph of month vs number of alarms
gso.fire %>%
  ggplot(aes(x = Month, y = NumberOfAlarms)) +
  geom_point(size = 2) +
  xlab("Month") +
  ylab("Number of Alarms") +
  ggtitle("Month vs Number of Alarms") +
  theme_economist()

#...
#box plots
#...

#box plot of days of the week vs call process time
gso.fire %>%
  ggplot(aes(x = DayOfWeek, y = call_process_seconds, fill = DayOfWeek)) +
  geom_boxplot(show.legend = FALSE) +
  scale_y_log10() +
  xlab("Days of the Week") +
  ylab("Total Call Process Time") +
  ggtitle("Days of the Week vs Call Process Time")

#box plot of work shift vs call process time
gso.fire %>%
  ggplot(aes(x = shift, y = call_process_seconds, fill = shift)) +
  geom_boxplot(show.legend = FALSE) +
  scale_y_log10() +
  xlab("Work Shift") +
  ylab("Total Call Process Time") +
  ggtitle("Work Shift vs Call Process Time")

#box plot of month vs call process time
gso.fire %>%
  ggplot(aes(x = Month, y = call_process_seconds, fill = Month)) +
  geom_boxplot(show.legend = FALSE) +
  scale_y_log10() +
  xlab("Month") +
  ylab("Total Call Process Time") +
  ggtitle("Month vs Call Process Time")


#...
#now looking at response time
#...

#box plot of days of the week vs response time
gso.fire %>%
  ggplot(aes(x = DayOfWeek, y = response_time_seconds, fill = DayOfWeek)) +
  geom_boxplot(show.legend = FALSE) +
  scale_y_log10() +
  xlab("Days of the Week") +
  ylab("Response Time") +
  ggtitle("Days of the Week vs Response Time")

#box plot of work shift vs response time
gso.fire %>%
  ggplot(aes(x = shift, y = response_time_seconds, fill = shift)) +
  geom_boxplot(show.legend = FALSE) +
  scale_y_log10() +
  xlab("Work Shift") +
  ylab("Response Time") +
  ggtitle("Work Shift vs Response Time")

#box plot of month vs response time
gso.fire %>%
  ggplot(aes(x = Month, y = response_time_seconds, fill = Month)) +
  geom_boxplot(show.legend = FALSE) +
  scale_y_log10() +
  xlab("Month") +
  ylab("Response Time") +
  ggtitle("Month vs Response Time")

#...
#now looking at total response time
#...

#box plot of day of week vs total response time
gso.fire %>%
  ggplot(aes(x = DayOfWeek, y = total_response_seconds, fill = DayOfWeek)) +
  geom_boxplot(show.legend = FALSE) +
  scale_y_log10() +
  xlab("Days of the Week") +
  ylab("Total Response Time") +
  ggtitle("Days of the Week vs Total Response Time")

#box plot of work shift vs total response time
gso.fire %>%
  ggplot(aes(x = shift, y = total_response_seconds, fill = shift)) +
  geom_boxplot(show.legend = FALSE) +
  scale_y_log10() +
  xlab("Work shift") +
  ylab("Total Response Time") +
  ggtitle("Work Shift vs Total Response Time")

#box plot of month vs total response time
gso.fire %>%
  ggplot(aes(x = Month, y = total_response_seconds, fill = Month)) +
  geom_boxplot(show.legend = FALSE) +
  scale_y_log10() +
  xlab("Month") +
  ylab("Total Response") +
  ggtitle("Month vs Total Response Time")


#...
#violin plots
#...

#violin plot of days of the week vs call process time
gso.fire %>%
  ggplot(aes(x = DayOfWeek, y = call_process_seconds, fill = DayOfWeek)) +
  geom_violin(show.legend = FALSE) +
  stat_summary(fun.y = mean, geom = "point", size = 2, show.legend = FALSE) +
  scale_y_log10() +
  xlab("Days of the Week") +
  ylab("Call Process Time") +
  ggtitle("Days of the Week vs Call Process Time")

#violin plot of work shift vs call process time
gso.fire %>%
  ggplot(aes(x = shift, y = call_process_seconds, fill = shift)) +
  geom_violin(show.legend = FALSE) +
  stat_summary(fun.y = mean, geom = "point", size = 2, show.legend = FALSE) +
  scale_y_log10() +
  xlab("Work Shift") +
  ylab("Call Process Time") +
  ggtitle("Work Shift vs Call Process Time")

#violin plot of month vs call process time
gso.fire %>%
  ggplot(aes(x = Month, y = call_process_seconds, fill = Month)) +
  geom_violin(show.legend = FALSE) +
  stat_summary(fun.y = mean, geom = "point", size = 2, show.legend = FALSE) +
  scale_y_log10() +
  xlab("Month") +
  ylab("Call Process Time") +
  ggtitle("Month vs Call Process Time")
  
#...
#now looking at response time
#...

#violin plot of days of the week vs response time
gso.fire %>%
  ggplot(aes(x = DayOfWeek, y = response_time_seconds, fill = DayOfWeek)) +
  geom_violin(show.legend = FALSE) +
  stat_summary(fun.y = mean, geom = "point", size = 2, show.legend = FALSE) +
  scale_y_log10() +
  xlab("Days of the Week") +
  ylab("Response Time") +
  ggtitle("Days of the Week vs Response Time")

#violin plot of work shift vs response time
gso.fire %>%
  ggplot(aes(x = shift, y = response_time_seconds, fill = shift)) +
  geom_violin(show.legend = FALSE) +
  stat_summary(fun.y = mean, geom = "point", size = 2, show.legend = FALSE) +
  scale_y_log10() +
  xlab("Work Shift") +
  ylab("Response Time") +
  ggtitle("Work Shift vs Response Time")

#violin plot of month vs response time
gso.fire %>%
  ggplot(aes(x = Month, y =  response_time_seconds, fill = Month)) +
  geom_violin(show.legend = FALSE) +
  stat_summary(fun.y = mean, geom = "point", size = 2, show.legend = FALSE) +
  scale_y_log10() +
  xlab("Month") +
  ylab("Response Time") +
  ggtitle("Month vs Response Time")

#...
#now looking at total response time
#...

#violin plot of days of the week vs total response time
gso.fire %>%
  ggplot(aes(x = DayOfWeek, y = total_response_seconds, fill = DayOfWeek)) +
  geom_violin(show.legend = FALSE) +
  stat_summary(fun.y = mean, geom = "point", size = 2, show.legend = FALSE) +
  scale_y_log10() +
  xlab("Days of the Week") +
  ylab("Total Response Time") +
  ggtitle("Days of the Week vs Total Response Time")

#violin plot of work shift vs total response time
gso.fire %>%
  ggplot(aes(x = shift, y = total_response_seconds, fill = shift)) +
  geom_violin(show.legend = FALSE) +
  stat_summary(fun.y = mean, geom = "point", size = 2, show.legend = FALSE) +
  scale_y_log10() +
  xlab("Work Shift") +
  ylab("Total Response Time") +
  ggtitle("Work Shift vs Total Response Time")

#violin plot of month vs total response time
gso.fire %>%
  ggplot(aes(x = Month, y = total_response_seconds, fill = Month)) +
  geom_violin(show.legend = FALSE) +
  stat_summary(fun.y = mean, geom = "point", size = 2, show.legend = FALSE) +
  scale_y_log10() +
  xlab("Month") +
  ylab("Total Response Time") +
  ggtitle("Month vs Total Repsonse Time")
