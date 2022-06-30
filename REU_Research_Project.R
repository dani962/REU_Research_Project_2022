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


#cutoff point for response time
cutoff.response_time = gso.fire %>% select(response_time_seconds) %>%
  filter(!is.na(response_time_seconds)) %>%
  summarize(c = quantile(response_time_seconds, .75) + 3 * IQR(response_time_seconds)) %>%
  pull(c)

#histogram of distribution of response time
gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(x = response_time_seconds, fill = "red")) +
  geom_histogram(show.legend = FALSE, color = "black") +
  xlab("Response Time (Seconds)") +
  ylab("Count") +
  ggtitle("Distribution of Response Time") +
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
  xlab("Fire District") +
  ylab("Count") +
  ggtitle("Frequency of Calls by Fire District") +
  coord_flip()

#bar graph of frequency of calls by year
gso.fire %>%
  count(Year) %>%
  ggplot(aes(x = Year, y = n)) +
  geom_bar(stat = "identity", fill = "pink", color = "black") +
  scale_x_continuous(breaks = seq(2010,2022,1)) +
  xlab("Year") +
  ylab("Count") +
  ggtitle("Frequency of Calls by Year")

#bar graph of frequency of calls by shift
gso.fire %>%
  ggplot(aes(x = shift)) +
  geom_bar(show.legend = FALSE, fill = "pink", color = "black") +
  xlab("Shift") +
  ylab("Count") +
  ggtitle("Frequency of Calls by Shift")


#...
#box plots
#...

#box plot of week vs response time
gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(x = Week, y = response_time_seconds, group = Week)) +
  geom_boxplot() +
  scale_x_continuous(breaks = seq(1,53,1)) +
  theme(axis.text.x = element_text(angle = 45)) +
  xlab("Week") +
  ylab("Response Time (Seconds)") +
  ggtitle("Week vs Response Time")

#box plot of station vs response time
gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(x = station, y = response_time_seconds)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Station") +
  ylab("Response Time (Seconds)") +
  ggtitle("Station vs Response Time")

#box plot of alarm hour vs response time
gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(x = AlarmHour, y = response_time_seconds, group = AlarmHour)) +
  geom_boxplot() +
  scale_x_continuous(breaks = seq(0,23,1)) +
  xlab("Alarm Hour") +
  ylab("Response Time (Seconds)") +
  ggtitle("Alarm Hour vs Response Time")

#box plot of number of alarms vs response time
gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(x = NumberOfAlarms, y = response_time_seconds, group = NumberOfAlarms)) +
  geom_boxplot(show.legend = FALSE) +
  xlab("Number of Alarms") +
  ylab("Response Time (Seconds)") +
  ggtitle("Number of Alarms vs Response Time")

#box plot of nature code vs response time
gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(x = NatureCode, y = response_time_seconds)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_boxplot() +
  xlab("Nature Code") +
  ylab("Response Time (Seconds)") +
  ggtitle("Nature Code vs Response Time")


#...
#scatter plots
#...

#Scatter plot of Total Staff on Incident vs Response Time
gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(x = response_time_seconds, y = TotalStaffOnIncident)) +
  geom_point(stat = "identity", show.legend = FALSE) +
  ylab("Total Staff On Incident") +
  xlab("Response Time (Seconds)") +
  ggtitle("Total Staff on Incident vs Response Time") +
  theme_economist()

#Scatter plot of Civilian Injuries vs Response Time
gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(x = response_time_seconds, y = CivilianInjuries)) +
  geom_point(stat = "identity", show.legend = FALSE) +
  ylab("Civilian Injuries") +
  xlab("Response Time (Seconds)") +
  ggtitle("Civilian Injuries vs Response Time") +
  theme_economist() 

#Scatter Plot for Civilian Fatalities vs Response Time
gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(x = response_time_seconds, y = CivilianFatalities)) +
  geom_point(stat = "identity", show.legend = FALSE) +
  ylab("Civilian Fatalities") +
  xlab("Response Time (Seconds)") +
  ggtitle("Civilian Fatalities vs Response Time") + 
  theme_economist()

#Scatter Plot for Fire Service Injuries vs Response Time
gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(x = response_time_seconds, y = FireServiceInjuries)) +
  geom_point(stat = "identity", show.legend = FALSE) +
  ylab("Fire Service Injuries") +
  xlab("Response Time (Seconds)") +
  ggtitle("Fire Service Injuries Injuries vs Response Time") +
  theme_economist()

#Scatter Plot for Fire Service Fatalities vs Response Time
gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(x = response_time_seconds, y = FireServiceFatalities)) +
  geom_point(stat = "identity", show.legend = FALSE) +
  ylab("Fire Service Fatalities") +
  xlab("Response Time (Seconds)") +
  ggtitle("Fire Service Fatalities vs Response Time") +
  theme_economist() 


#...
#combined box plot and violin plot
#...

#Box and Violin plot for days of the week vs response time
gso.fire %>%
  filter(!is.na(response_time_seconds)) %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(x = DayOfWeek, y = response_time_seconds)) +
  geom_violin(trim = FALSE, fill = "light blue") +
  geom_boxplot(width = 0.3, fill = "pink") +
  xlab("Days of the Week") +
  ylab("Response Time (Seconds)") +
  ggtitle("Violin & Box Plot of Response Time Densities by Days of the Week") +
  theme(axis.text.x = element_text(angle = 90), legend.position = "hide")

#box and violin plot for year vs response time
gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(x = Year, y = response_time_seconds, group = Year)) +
  geom_violin(trim = FALSE, fill = "light blue") +
  geom_boxplot(show.legend = FALSE, width = .3, fill = "pink") +
  scale_x_continuous(breaks = seq(2010,2022,1)) +
  xlab("Year") +
  ylab("Response Time (Seconds)") +
  ggtitle("Violin & Box Plots of Response Time Densities by Year") 

#box and violin plot for work shift vs response time
gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(x = shift, y = response_time_seconds)) +
  geom_violin(show.legend = FALSE, fill = "light blue") +
  geom_boxplot(width = .5, show.legend = FALSE, fill = "pink") +
  xlab("Work shift") +
  ylab("Response Time (Seconds)") +
  ggtitle("Violin and Box Plot of Response Time Densities by Work Shift")

#box and violin plot for fire district vs response time
gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(x = FireDistrict, y = response_time_seconds)) +
  geom_violin(fill = "light blue") +
  geom_boxplot(show.legend = FALSE, width = .2, fill = "pink") +
  theme(axis.text.x = element_text(angle = 90), legend.position = "hide") +
  xlab("Fire District") +
  ylab("Response Time (Seconds)") +
  ggtitle("Violin and Box Plot of Response Time Densities by Fire District")

#box and violin plot for month vs response time
gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(x = Month, y = response_time_seconds)) +
  geom_violin(show.legend = FALSE, fill = "light blue") +
  geom_boxplot(show.legend = FALSE, width = .3, fill = "pink") +
  xlab("Month") +
  ylab("Response Time (Seconds)") +
  ggtitle("Violin and Box Plot of Response Time Densities by Month")

#box and violin plot for alarm hour vs response time
gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(x = AlarmHour, y = response_time_seconds, group = AlarmHour)) +
  geom_violin(fill = "light blue") +
  geom_boxplot(width = .3, fill = "pink") +
  scale_x_continuous(breaks = seq(0,23,1)) +
  xlab("Alarm Hour") +
  ylab("Response Time (Seconds)") +
  ggtitle("Violin and Box Plot of Response Time Densities by Alarm Hour")

#box and violin plot of number of alarms vs response time
gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(x = NumberOfAlarms, y = response_time_seconds, group = NumberOfAlarms)) +
  geom_violin(fill = "light blue") +
  geom_boxplot(width = .3, fill = "pink") +
  xlab("Number of Alarms") +
  ylab("Response Time (Seconds)") +
  ggtitle("Violin and Box Plot of Response Time Densities by Number of Alarms")

