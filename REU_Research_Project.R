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
  ggtitle("Days of the Week vs Response Time")

#bar graph of work shift vs response time
gso.fire %>%
  ggplot(aes(x = shift, y = response_time_seconds, fill = shift)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Work Shifts") +
  ylab("Response Time") +
  ggtitle("Work Shifts vs Response Time")

#bar graph of month vs  response time
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

#bar graph of days of the week vs total response time
gso.fire %>%
  ggplot(aes(x = DayOfWeek, y = total_response_seconds, fill = DayOfWeek)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Days of the Week") +
  ylab("Total Response Time") +
  ggtitle("Days of the Week vs Total Repsonse Time")

#bar graph of work shift vs total response time
gso.fire %>%
  ggplot(aes(x = shift, y = total_response_seconds, fill = shift)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Work Shifts") +
  ylab("Total Response Time") +
  ggtitle("Work Shifts vs Total Response Time")

#bar graph of month vs total response time
gso.fire %>%
  ggplot(aes(x = Month, y = total_response_seconds, fill = Month)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Month") +
  ylab("Total Response Time") +
  ggtitle("Month vs Total Response Time")

#bar graph of week in the year vs total response time
gso.fire %>%
  ggplot(aes(x = Week, y = total_response_seconds, fill = Week)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Week in the year") +
  ylab("Total Response Time")
  ggtitle("Week in the year vs Total Response Time")


#...
#now looking at number of alarms each day
#...

alarms_per_day_data <- data.frame(index = gso.fire$AlarmDate, var1 = gso.fire$NumberOfAlarms)

#takes awhile to load, lot of data, maybe split it up?
#most days had one alarm, some had two
ggplot(alarms_per_day_data, aes(x = index, y = var1)) +
  geom_point()


#...
#now looking some box plots
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
