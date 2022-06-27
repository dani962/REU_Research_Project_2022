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

cutoff.call_process_time = gso.fire %>% select(call_process_seconds) %>%
  filter(!is.na(call_process_seconds)) %>%
  summarize(c = quantile(call_process_seconds, .75) + 3 * IQR(call_process_seconds)) %>%
  pull(c)

#histogram of call process seconds vs count
gso.fire %>% 
  filter(call_process_seconds < cutoff.call_process_time) %>%
  ggplot(aes(x = call_process_seconds, fill = "red")) +
  geom_histogram(show.legend = FALSE, color = "black") +
  xlab("Call Processing Time (Seconds)") +
  ylab("Count") +
  ggtitle("Distribution of Call Processing Time") +
  theme_economist()

#cutoff point for response time
cutoff.response_time = gso.fire %>% select(response_time_seconds) %>%
  filter(!is.na(response_time_seconds)) %>%
  summarize(c = quantile(response_time_seconds, .75) + 3 * IQR(response_time_seconds)) %>%
  pull(c)

#histogram of response time seconds vs count
gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(x = response_time_seconds, fill = "red")) +
  geom_histogram(show.legend = FALSE, color = "black") +
  xlab("Response Time (Seconds)") +
  ylab("Count") +
  ggtitle("Distribution of Response Time") +
  theme_economist()

#cutoff point for total response time
cutoff.total_response_time = gso.fire %>% select(total_response_seconds) %>% 
  filter(!is.na(total_response_seconds)) %>%
  summarize(c = quantile(total_response_seconds, .75) + 3 * IQR(total_response_seconds)) %>%
  pull(c)

#histogram of distribution of total response time
gso.fire %>%
  filter(total_response_seconds < cutoff.total_response_time) %>%
  ggplot(aes(x = total_response_seconds)) +
  geom_histogram(aes(x = total_response_seconds, fill = "red"), show.legend = FALSE, color = "black") +
  xlab("Total Response Time (Seconds)") +
  ylab("Count") +
  ggtitle("Distribution of Total Response Time") +
  theme_economist()

#histogram of frequency of fire alarms by alarm hour
gso.fire %>%
  ggplot(aes(x = AlarmHour)) +
  geom_bar(color = "black", fill = "pink") +
  scale_x_continuous(breaks = seq(0,23,1)) +
  theme(axis.text.x = element_text(angle = 45)) +
  xlab("Alarm Hour") +
  ylab("Count") +
  ggtitle("Frequency of Fire Alarms by Alarm Hour")


#...
#bar graphs
#...

#bar graph of frequency of calls by fire station
gso.fire %>%
  count(station) %>%
  mutate(station = reorder(station, n)) %>%
  ggplot(aes(x = station, y = n)) +
  geom_bar(stat = "identity", color = "black", fill = "pink") +
  theme(axis.text.y = element_text(size = 6)) +
  xlab("Fire Station (Station Number)") +
  ylab("Count") +
  ggtitle("Frequency of Calls by Fire Station") +
  coord_flip()

#bar graph of frequency of calls by fire district
gso.fire %>%
  count(FireDistrict) %>%
  mutate(FireDistrict = reorder(FireDistrict, n)) %>%
  ggplot(aes(x = FireDistrict, y = n)) +
  geom_bar(stat = "identity", color = "black", fill = "pink") +
  theme(axis.text.y = element_text(size = 6)) +
  xlab("Fire Station (Station Number)") +
  ylab("Count") +
  ggtitle("Frequency of Calls by Fire Station") +
  coord_flip()

#bar graph of frequency of calls by year
gso.fire %>%
  count(Year) %>%
  mutate(Year = reorder(Year, n)) %>%
  ggplot(aes(x = Year, y = n)) +
  geom_bar(stat = "identity", color = "black", fill = "light blue") +
  xlab("Year") +
  ylab("Count") +
  ggtitle("Frequency of Calls by Year")

#bar graph of frequency of calls by shift
gso.fire %>%
  ggplot(aes(x = shift)) +
  geom_bar(aes(fill = shift), color = "black", show.legend = FALSE) +
  xlab("Shift") +
  ylab("Count") +
  ggtitle("Frequency of Calls by Shift")

#bar graph of week in the year vs total response time
gso.fire %>%
  filter(!is.na(total_response_seconds)) %>%
  ggplot(aes(x = Week, y = total_response_seconds, fill = Week)) +
  geom_bar(stat = "identity") +
  xlab("Week in the year") +
  ylab("Total Response Time (Seconds)") +
  ggtitle("Frequency of Total Response Time by Week")

#bar graph of frequency of total response time by year
gso.fire %>%
  filter(!is.na(total_response_seconds)) %>%
  ggplot(aes(x = Year, y = total_response_seconds, fill = Year)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Total Response Time (Seconds)") +
  ggtitle("Frequency of Total Response Time by Year")

#bar graph of frequency of total response time by fire district
gso.fire %>%
  filter(!is.na(total_response_seconds)) %>%
  ggplot(aes(x = FireDistrict, y = total_response_seconds, fill = FireDistrict)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Fire District") +
  ylab("Total Response Time (Seconds)") +
  ggtitle("Frequency of Total Response Time by Fire District") +
  theme_clean() +
  coord_flip()
  
#bar graph of frequency of total response time by week
#had trouble reordering bars
gso.fire %>%
  filter(!is.na(total_response_seconds)) %>%
  ggplot(aes(x = reorder(Week, +total_response_seconds), y = total_response_seconds, fill = Week)) +
  geom_bar(stat = "identity") +
  theme(axis.text.y = element_text(size = 6)) +
  xlab("Week") +
  ylab("Total Response Time (Seconds)") +
  ggtitle("Frequency of Total Response Time by Week") +
  coord_flip()

#bar graph of work shift vs total response time
gso.fire %>%
  filter(!is.na(total_response_seconds)) %>%
  ggplot(aes(x = shift, y = total_response_seconds, fill = shift)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Work Shifts") +
  ylab("Total Response Time (Seconds)") +
  ggtitle("Frequency of Total Response Time by Shift") +
  theme_economist()

#bar graph of month vs total response time
gso.fire %>%
  filter(!is.na(total_response_seconds)) %>%
  ggplot(aes(x = Month, y = total_response_seconds, fill = Month)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Month") +
  ylab("Total Response Time (Seconds)") +
  ggtitle("Frequency of Total Response Time by Month") +
  theme_economist()

#bar graph of station vs total response time
#had trouble trying to reorder bars
gso.fire %>%
  filter(!is.na(total_response_seconds)) %>%
  #mutate(station = reorder(station, total_response_seconds)) %>%
  ggplot(aes(x = station, y = total_response_seconds, fill = station)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme(axis.text.y = element_text(size = 6)) +
  xlab("Station") +
  ylab("Total Response Time (Seconds)") +
  ggtitle("Frequency of Total Response Time by Station") +
  coord_flip()

#bar graph of days of the week vs total response time
gso.fire %>%
  filter(!is.na(total_response_seconds)) %>%
  ggplot(aes(x = DayOfWeek, y = total_response_seconds, fill = DayOfWeek)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Days of the Week") +
  ylab("Total Response Time (Seconds)") +
  ggtitle("Frequency of Total Response Time by Days of the Week") +
  theme_economist()


#...
#box plots
#...

#box plot of days of the week vs total response time
gso.fire %>%
  filter(total_response_seconds < cutoff.total_response_time) %>%
  ggplot(aes(x = DayOfWeek, y = total_response_seconds, fill = DayOfWeek)) +
  geom_boxplot() +
  xlab("Days of the Week") +
  ylab("Total Response Time (Seconds)") +
  ggtitle("Days of the Week vs Total Repsonse Time") +
  theme_economist()

#box plot of week vs total response time
gso.fire %>%
  filter(total_response_seconds < cutoff.total_response_time) %>%
  ggplot(aes(x = Week, y = total_response_seconds, group = Week)) +
  geom_boxplot() +
  xlab("Week") +
  ylab("Total Response Time (Seconds)") +
  ggtitle("Week vs Total Response Time") +
  theme_economist()

#box plot of year vs total response time
gso.fire %>%
  filter(total_response_seconds < cutoff.total_response_time) %>%
  ggplot(aes(x = Year, y = total_response_seconds, group = Year)) +
  geom_boxplot(aes(fill = "red"), show.legend = FALSE) +
  xlab("Year") +
  ylab("Total Response Time (Seconds)") +
  ggtitle("Year vs Total Response Time")

#box plot of work shift vs total response time
gso.fire %>%
  filter(total_response_seconds < cutoff.total_response_time) %>%
  ggplot(aes(x = shift, y = total_response_seconds, fill = shift)) +
  geom_boxplot() +
  xlab("Work shift") +
  ylab("Total Response Time (Seconds)") +
  ggtitle("Work Shift vs Total Response Time") +
  theme_economist()

#box plot of fire district vs total response time
gso.fire %>%
  filter(total_response_seconds < cutoff.total_response_time) %>%
  ggplot(aes(x = FireDistrict, y = total_response_seconds, fill = FireDistrict)) +
  geom_boxplot(show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Fire District") +
  ylab("Total Response Time (Seconds)") +
  ggtitle("Fire District vs Total Response Time")

#box plot of month vs total response time
gso.fire %>%
  filter(total_response_seconds < cutoff.total_response_time) %>%
  ggplot(aes(x = Month, y = total_response_seconds, fill = Month)) +
  geom_boxplot(show.legend = FALSE) +
  xlab("Month") +
  ylab("Total Response Time (Seconds)") +
  ggtitle("Month vs Total Response Time")

#box plot of station vs total response time
gso.fire %>%
  filter(total_response_seconds < cutoff.total_response_time) %>%
  ggplot(aes(x = station, y = total_response_seconds)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Station") +
  ylab("Total Response Time (Seconds)") +
  ggtitle("Station vs Total Response Time")

#box plot of alarm hour vs total response time
gso.fire %>%
  filter(total_response_seconds < cutoff.total_response_time) %>%
  ggplot(aes(x = AlarmHour, y = total_response_seconds, group = AlarmHour)) +
  geom_boxplot() +
  xlab("Alarm Hour") +
  ylab("Total Response Time (Seconds)") +
  ggtitle("Alarm Hour vs Total Response Time")

#box plot of number of alarms vs total response time
gso.fire %>%
  filter(total_response_seconds < cutoff.total_response_time) %>%
  ggplot(aes(x = NumberOfAlarms, y = total_response_seconds, group = NumberOfAlarms)) +
  geom_boxplot() +
  xlab("Number of Alarms") +
  ylab("Total Response Time (Seconds)") +
  ggtitle("Number of Alarms vs Total Response Time")

#box plot of nature code vs total response time
gso.fire %>%
  filter(total_response_seconds < cutoff.total_response_time) %>%
  ggplot(aes(x =  NatureCode, y = total_response_seconds)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_boxplot() +
  xlab("Nature Code") +
  ylab("Total Response Time (Seconds)") +
  ggtitle("Nature Code vs Total Response Time")

#...
#violin plots
#...

#violin plot of days of the week vs total response time
gso.fire %>%
  filter(!is.na(total_response_seconds)) %>%
  ggplot(aes(x = DayOfWeek, y = total_response_seconds, fill = DayOfWeek)) +
  geom_violin(show.legend = FALSE) +
  #stat_summary(fun.y = mean, geom = "point", size = 2, show.legend = FALSE) +
  scale_y_log10() +
  xlab("Days of the Week") +
  ylab("Total Response Time (Seconds)") +
  ggtitle("Violin Plot of Total Response Time Densities by Days of the Week")

#violin plot of work shift vs total response time
gso.fire %>%
  filter(!is.na(total_response_seconds)) %>%
  ggplot(aes(x = shift, y = total_response_seconds, fill = shift)) +
  geom_violin(show.legend = FALSE) +
  scale_y_log10() +
  xlab("Shift") +
  ylab("Total Response Time (Seconds)") +
  ggtitle("Violin Plot of Total Response Time Densities by Shift")

#violin plot of month vs total response time
gso.fire %>%
  filter(!is.na(total_response_seconds)) %>%
  ggplot(aes(x = Month, y = total_response_seconds, fill = Month)) +
  geom_violin(show.legend = FALSE) +
  scale_y_log10() +
  xlab("Month") +
  ylab("Total Response Time (Seconds)") +
  ggtitle("Violin Plot of Total Response Time Densities by Month")

#...
#scatter plots
#...

#Scatter plot of Total Staff on Incident vs Total Response
gso.fire %>%
  filter(total_response_seconds < cutoff.total_response_time) %>%
  ggplot(aes(x = total_response_seconds, y = TotalStaffOnIncident, fill = total_response_seconds)) +
  geom_point(stat = "identity", show.legend = FALSE) +
  ylab("Total Staff On Incident") +
  xlab("Total Response Time") +
  ggtitle("Total Staff on Incident vs Total Response Time") +
  theme_economist()

#Scatter plot of Civilian Injuries vs Total Response Time
gso.fire %>%
  filter(total_response_seconds < cutoff.total_response_time) %>%
  ggplot(aes(x = total_response_seconds, y = CivilianInjuries, fill = total_response_seconds)) +
  geom_point(stat = "identity", show.legend = FALSE) +
  ylab("Civilian Injuries") +
  xlab("Total Response Time") +
  ggtitle("Civilian Injuries vs Total Response Time") +
  theme_economist() 

#Scatter Plot for Civilian Fatalities vs Total Response Time
gso.fire %>%
  filter(total_response_seconds < cutoff.total_response_time) %>%
  ggplot(aes(x = total_response_seconds, y = CivilianFatalities, fill = total_response_seconds)) +
  geom_point(stat = "identity", show.legend = FALSE) +
  ylab("Civilian Fatalities") +
  xlab("Total Response Time") +
  ggtitle("Civilian Fatalities vs Total Response Time") + 
  theme_economist()

#Scatter Plot for Fire Service Injuries vs Total Response Time
gso.fire %>%
  filter(total_response_seconds < cutoff.total_response_time) %>%
  ggplot(aes(x = total_response_seconds, y = FireServiceInjuries, fill = total_response_seconds)) +
  geom_point(stat = "identity", show.legend = FALSE) +
  ylab("Fire Service Injuries") +
  xlab("Total Response Time") +
  ggtitle("Fire Service Injuries Injuries vs Total Response Time") +
  theme_economist()

#Scatter Plot for Fire Service Fatalities vs Total Response Time
gso.fire %>%
  filter(total_response_seconds < cutoff.total_response_time) %>%
  ggplot(aes(x = total_response_seconds, y = FireServiceFatalities, fill = TotalResponseTime)) +
  geom_point(stat = "identity", show.legend = FALSE) +
  ylab("Fire Service Fatalities") +
  xlab("Total Response Time") +
  ggtitle("Fire Service Fatalities vs Total Response Time") +
  theme_economist() 

