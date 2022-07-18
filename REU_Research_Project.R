library(readr)
library(lubridate)
library(ggplot2)
library(dslabs)
library(dplyr)
library(ggrepel)
library(ggthemes)
library(broom)
library(glmnet)
library(MASS)
library(tidyverse)
library(Amelia)
library(mice)
library(caret)


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
cutoff.response_time = gso.fire %>% 
  dplyr::select(response_time_seconds) %>%
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

#...
#bar graphs
#...

#bar graph of frequency of fire alarms by alarm hour
gso.fire %>%
  ggplot(aes(x = AlarmHour)) +
  geom_bar(color = "black", fill = "pink") +
  scale_x_continuous(breaks = seq(0,23,1)) +
  theme(axis.text.x = element_text(angle = 45)) +
  xlab("Alarm Hour") +
  ylab("Count") +
  ggtitle("Frequency of Fire Alarms by Alarm Hour")


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

#bar graph of frequency of calls by days of the week
gso.fire %>%
  ggplot(aes(x = DayOfWeek)) +
  geom_bar(fill = "pink", color = "black") +
  xlab("Days of the Week") +
  ylab("Count") +
  ggtitle("Frequency of Calls by Days of the Week")

#bar graph of frequency of calls by week
gso.fire %>%
  ggplot(aes(x = Week)) +
  geom_bar(fill = "pink", color = "black") +
  scale_x_continuous(breaks = seq(1,53,1)) +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Week") +
  ylab("Count") +
  ggtitle("Frequency of Calls by Week")

#bar graph of frequency of calls by month 
gso.fire %>%
  ggplot(aes(x = Month)) +
  geom_bar(fill = "pink", color = "black") +
  xlab("Month") +
  ylab("Count") +
  ggtitle("Frequency of Calls by Month")

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


#Modeling of Response Time

#Filtering data
gso.fire.filtered = gso.fire %>%
  filter(response_time_seconds < cutoff.response_time)

lm.res.time = lm(response_time_seconds ~ TotalStaffOnIncident + FireDistrict + DayOfWeek + shift + Month + AlarmHour , 
                 data = gso.fire.filtered)

#Results from lm.res.time
summary(lm.res.time)

#Variable Selection
lm.res.time.select = stepAIC(lm.res.time)
summary(lm.res.time.select)

missmap(gso.fire)

p = gso.fire%>%
  drop_na(response_time_seconds)

x = p[,-64]
lasso.mod = cv.glmnet(x = as.matrix(x), y = as.matrix(p$response_time_seconds),
                      family = "gaussian", alpha = 1)

#Random Forrest
nr <- nrow(gso.fire)
tr.id <- sample(nr, floor(0.7*nr), replace = FALSE, prob = NULL)
tr.DF <- gso.fire[tr.id, ]
ts.DF <- gso.fire[-tr.id, ]

fitControl <- trainControl(method = 'repeatedcv',
                           number = 10, 
                           repeats = 3,
                           search = 'grid')

m.try <- c(floor(sqrt(ncol(tr.DF))), (ncol(tr.DF) - 1), floor(log(ncol(tr.DF))))
Tune.grid <- expand.grid(.mtry = m.try)
RF.Mod <- train(response_time_seconds~., 
                data = tr.DF, 
                method = 'rf', 
                metric = 'mse',
                tuneGrid = Tune.grid,
                trControl = fitControl,
                importance = TRUE)

randomForest(response_time_seconds~., data = tr.DF, na.action = na.fail)



#daily number of fire incidents
gso.fire.ts = gso.fire %>%
  count(AlarmDate2)

#time series of daily number of fire incidents
gso.fire.ts %>%
  ggplot(aes(x = AlarmDate2, y = n)) +
  geom_line() +
  xlab("Year") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of daily number of fire incidents")

#time series of daily number of fire incidents for 2010
gso.fire.ts %>%
  filter(AlarmDate2 >= "2010-01-01" & AlarmDate2 <= "2010-12-31") %>%
  ggplot(aes(x = AlarmDate2, y = n)) +
  geom_line() +
  xlab("Alarm Date") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of daily number of fire incidents for 2010")

#time series of daily number of fire incidents for 2011
gso.fire.ts %>%
  filter(AlarmDate2 >= "2011-01-01" & AlarmDate2 <= "2011-12-31") %>%
  ggplot(aes(x = AlarmDate2, y = n)) +
  geom_line() +
  xlab("Alarm Date") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of daily number of fire incidents for 2011")

#time series of daily number of fire incidents for 2012
gso.fire.ts %>%
  filter(AlarmDate2 >= "2012-01-01" & AlarmDate2 <= "2012-12-31") %>%
  ggplot(aes(x = AlarmDate2, y = n)) +
  geom_line() +
  xlab("Alarm Date") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of daily number of fire incidents for 2012")

#time series of daily number of fire incidents for 2013
gso.fire.ts %>%
  filter(AlarmDate2 >= "2013-01-01" & AlarmDate2 <= "2013-12-31") %>%
  ggplot(aes(x = AlarmDate2, y = n)) +
  geom_line() +
  xlab("Alarm Date") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of daily number of fire incidents for 2013")

#time series of daily number of fire incidents for 2014
gso.fire.ts %>%
  filter(AlarmDate2 >= "2014-01-01" & AlarmDate2 <= "2014-12-31") %>%
  ggplot(aes(x = AlarmDate2, y = n)) +
  geom_line() +
  xlab("Alarm Date") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of daily number of fire incidents for 2014")

#time series of daily number of fire incidents for 2015
gso.fire.ts %>%
  filter(AlarmDate2 >= "2015-01-01" & AlarmDate2 <= "2015-12-31") %>%
  ggplot(aes(x = AlarmDate2, y = n)) +
  geom_line() +
  xlab("Alarm Date") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of daily number of fire incidents for 2015")

#time series of daily number of fire incidents for 2016
gso.fire.ts %>%
  filter(AlarmDate2 >= "2016-01-01" & AlarmDate2 <= "2016-12-31") %>%
  ggplot(aes(x = AlarmDate2, y = n)) +
  geom_line() +
  xlab("Alarm Date") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of daily number of fire incidents for 2016")

#time series of daily number of fire incidents for 2017
gso.fire.ts %>%
  filter(AlarmDate2 >= "2017-01-01" & AlarmDate2 <= "2017-12-31") %>%
  ggplot(aes(x = AlarmDate2, y = n)) +
  geom_line() +
  xlab("Alarm Date") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of daily number of fire incidents for 2017")

#time series of daily number of fire incidents for 2018
gso.fire.ts %>%
  filter(AlarmDate2 >= "2018-01-01" & AlarmDate2 <= "2018-12-31") %>%
  ggplot(aes(x = AlarmDate2, y = n)) +
  geom_line() +
  xlab("Alarm Date") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of daily number of fire incidents for 2018")

#time series of daily number of fire incidents for 2019
gso.fire.ts %>%
  filter(AlarmDate2 >= "2019-01-01" & AlarmDate2 <= "2019-12-31") %>%
  ggplot(aes(x = AlarmDate2, y = n)) +
  geom_line() +
  xlab("Alarm Date") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of daily number of fire incidents for 2019")

#time series of daily number of fire incidents for 2020
gso.fire.ts %>%
  filter(AlarmDate2 >= "2020-01-01" & AlarmDate2 <= "2020-12-31") %>%
  ggplot(aes(x = AlarmDate2, y = n)) +
  geom_line() +
  xlab("Alarm Date") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of daily number of fire incidents for 2020")

#time series of daily number of fire incidents for 2021
gso.fire.ts %>%
  filter(AlarmDate2 >= "2021-01-01" & AlarmDate2 <= "2021-12-31") %>%
  ggplot(aes(x = AlarmDate2, y = n)) +
  geom_line() +
  xlab("Alarm Date") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of daily number of fire incidents for 2021")

#time series of daily number of fire incidents for 2022
gso.fire.ts %>%
  filter(AlarmDate2 >= "2022-01-01" & AlarmDate2 <= "2022-12-31") %>%
  ggplot(aes(x = AlarmDate2, y = n)) +
  geom_line() +
  xlab("Alarm Date") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of daily number of fire incidents for 2022")

#Monthly number of fire incidents
gso.fire.ts.month = gso.fire %>%
  mutate(Monthly = make_date(Year, Month)) %>%
  group_by(Monthly, Month, Year) %>%
  count()

#time series of monthly number of fire incidents for years 2010-2022
gso.fire.ts.month %>%
  ggplot(aes(x = Monthly, y = n)) +
  geom_line() +
  xlab("Month") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of monthly number of fire incidents for 2010-2022")


#time series of monthly number of fire incidents for 2010
gso.fire.ts.month %>%
  filter(Year == "2010") %>%
  ggplot(aes(x = Month, y = n, group = 1)) +
  geom_line() +
  xlab("Month") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of monthly number of fire incidents for 2010")

#time series of monthly number of fire incidents for 2011
gso.fire.ts.month %>%
  filter(Year == "2011") %>%
  ggplot(aes(x = Month, y = n, group = 1)) +
  geom_line() +
  xlab("Month") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of monthly number of fire incidents for 2011")

#time series of monthly number of fire incidents for 2012
gso.fire.ts.month %>%
  filter(Year == "2012") %>%
  ggplot(aes(x = Month, y = n, group = 1)) +
  geom_line() +
  xlab("Month") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of monthly number of fire incidents for 2012")

#time series of monthly number of fire incidents for 2013
gso.fire.ts.month %>%
  filter(Year == "2013") %>%
  ggplot(aes(x = Month, y = n, group = 1)) +
  geom_line() +
  xlab("Month") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of monthly number of fire incidents for 2013")

#time series of monthly number of fire incidents for 2014
gso.fire.ts.month %>%
  filter(Year == "2014") %>%
  ggplot(aes(x = Month, y = n, group = 1)) +
  geom_line() +
  xlab("Month") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of monthly number of fire incidents for 2014")

#time series of monthly number of fire incidents for 2015
gso.fire.ts.month %>%
  filter(Year == "2015") %>%
  ggplot(aes(x = Month, y = n, group = 1)) +
  geom_line() +
  xlab("Month") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of monthly number of fire incidents for 2015")

#time series of monthly number of fire incidents for 2016
gso.fire.ts.month %>%
  filter(Year == "2016") %>%
  ggplot(aes(x = Month, y = n, group = 1)) +
  geom_line() +
  xlab("Month") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of monthly number of fire incidents for 2016")

#time series of monthly number of fire incidents for 2017
gso.fire.ts.month %>%
  filter(Year == "2017") %>%
  ggplot(aes(x = Month, y = n, group = 1)) +
  geom_line() +
  xlab("Month") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of monthly number of fire incidents for 2017")

#time series of monthly number of fire incidents for 2018
gso.fire.ts.month %>%
  filter(Year == "2018") %>%
  ggplot(aes(x = Month, y = n, group = 1)) +
  geom_line() +
  xlab("Month") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of monthly number of fire incidents for 2018")

#time series of monthly number of fire incidents for 2019
gso.fire.ts.month %>%
  filter(Year == "2019") %>%
  ggplot(aes(x = Month, y = n, group = 1)) +
  geom_line() +
  xlab("Month") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of monthly number of fire incidents for 2019")

#time series of monthly number of fire incidents for 2020
gso.fire.ts.month %>%
  filter(Year == "2020") %>%
  ggplot(aes(x = Month, y = n, group = 1)) +
  geom_line() +
  xlab("Month") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of monthly number of fire incidents for 2020")

#time series of monthly number of fire incidents for 2021
gso.fire.ts.month %>%
  filter(Year == "2021") %>%
  ggplot(aes(x = Month, y = n, group = 1)) +
  geom_line() +
  xlab("Month") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of monthly number of fire incidents for 2021")

#time series of monthly number of fire incidents for 2022
gso.fire.ts.month %>%
  filter(Year == "2022") %>%
  ggplot(aes(x = Month, y = n, group = 1)) +
  geom_line() +
  xlab("Month") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of monthly number of fire incidents for 2022")


#Weekly number of fire incidents
gso.fire.ts.week = gso.fire %>%
  mutate(Weekly = make_date(Year, Week)) %>%
  group_by(Weekly, Year, Week) %>%
  count()

#time series of weekly number of fire incidents for 2010-2022
gso.fire.ts.week %>%
  ggplot(aes(Weekly, y = n)) +
  geom_line() +
  xlab("Week") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of weekly number of fire incidents for 2010-2022")

#time series of weekly number of fire incidents for 2010
gso.fire.ts.week %>%
  filter(Year == "2010") %>%
  ggplot(aes(x = Week, y = n)) +
  geom_line() +
  xlab("Week") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of weekly number of fire incidents for 2010")

#time series of weekly number of fire incidents for 2011
gso.fire.ts.week %>%
  filter(Year == "2011") %>%
  ggplot(aes(x = Week, y = n)) +
  geom_line() +
  xlab("Week") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of weekly number of fire incidents for 2011")

#time series of weekly number of fire incidents for 2012
gso.fire.ts.week %>%
  filter(Year == "2012") %>%
  ggplot(aes(x = Week, y = n)) +
  geom_line() +
  xlab("Week") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of weekly number of fire incidents for 2012")

#time series of weekly number of fire incidents for 2013
gso.fire.ts.week %>%
  filter(Year == "2013") %>%
  ggplot(aes(x = Week, y = n)) +
  geom_line() +
  xlab("Week") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of weekly number of fire incidents for 2013")

#time series of weekly number of fire incidents for 2014
gso.fire.ts.week %>%
  filter(Year == "2014") %>%
  ggplot(aes(x = Week, y = n)) +
  geom_line() +
  xlab("Week") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of weekly number of fire incidents for 2014")

#time series of weekly number of fire incidents for 2015
gso.fire.ts.week %>%
  filter(Year == "2015") %>%
  ggplot(aes(x = Week, y = n)) +
  geom_line() +
  xlab("Week") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of weekly number of fire incidents for 2015")

#time series of weekly number of fire incidents for 2016
gso.fire.ts.week %>%
  filter(Year == "2016") %>%
  ggplot(aes(x = Week, y = n)) +
  geom_line() +
  xlab("Week") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of weekly number of fire incidents for 2016")

#time series of weekly number of fire incidents for 2017
gso.fire.ts.week %>%
  filter(Year == "2017") %>%
  ggplot(aes(x = Week, y = n)) +
  geom_line() +
  xlab("Week") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of weekly number of fire incidents for 2017")

#time series of weekly number of fire incidents for 2018
gso.fire.ts.week %>%
  filter(Year == "2018") %>%
  ggplot(aes(x = Week, y = n)) +
  geom_line() +
  xlab("Week") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of weekly number of fire incidents for 2018")

#time series of weekly number of fire incidents for 2019
gso.fire.ts.week %>%
  filter(Year == "2019") %>%
  ggplot(aes(x = Week, y = n)) +
  geom_line() +
  xlab("Week") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of weekly number of fire incidents for 2019")

#time series of weekly number of fire incidents for 2020
gso.fire.ts.week %>%
  filter(Year == "2020") %>%
  ggplot(aes(x = Week, y = n)) +
  geom_line() +
  xlab("Week") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of weekly number of fire incidents for 2020")

#time series of weekly number of fire incidents for 2021
gso.fire.ts.week %>%
  filter(Year == "2021") %>%
  ggplot(aes(x = Week, y = n)) +
  geom_line() +
  xlab("Week") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of weekly number of fire incidents for 2021")

#time series of weekly number of fire incidents for 2022
gso.fire.ts.week %>%
  filter(Year == "2022") %>%
  ggplot(aes(x = Week, y = n)) +
  geom_line() +
  xlab("Week") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of weekly number of fire incidents for 2022")



#Forecasting for daily number of Fire Incidents

#Time Series Forecasting

#convert gso.fire.ts to official ts object
gso.fire.ts.2 <- ts(gso.fire.ts$n, start = c(2010, 182), end = c(2022, 153),
                 frequency = 365)

#check for stationarity

#determine the number of differences required for time series x to be made stationary
#install.packages("forecast")
library(forecast)
ndiffs(gso.fire.ts.2) #1

nsdiffs(gso.fire.ts.2) #0

#install.packages("urca")
library(urca)
acf(gso.fire.ts.2)
summary(ur.kpss(gso.fire.ts.2))

#attempt at differencing
gso.fire.ts.2.diff <- diff(gso.fire.ts.2)
acf(gso.fire.ts.2.diff)

summary(ur.kpss(gso.fire.ts.2.diff))
ndiffs(gso.fire.ts.2) #1
nsdiffs(gso.fire.ts.2) #0

cbind("Fire Incidents" = gso.fire.ts.2,
      "First differenced" = diff(gso.fire.ts.2)) %>%
  autoplot(facets=TRUE) +
  xlab("Date") + ylab("") +
  ggtitle("Stationarity Transformations of Daily Fire Incidents")


#Forecasting for monthly number of Fire Incidents

#convert gso.fire.ts to official ts object
gso.fire.ts.month.2 <- ts(gso.fire.ts.month$n, start = c(2010, 7), end = c(2022, 6),
                    frequency = 12)

##Looking at stationarity of crashes_mts4 monthly time series 
acf(gso.fire.ts.month.2)
summary(ur.kpss(gso.fire.ts.month.2))

ndiffs(gso.fire.ts.month.2) #1
nsdiffs(gso.fire.ts.month.2) #0
