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
library(forecast)
library(urca)
library(randomForest)
library(ranger)
library(astsa)


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

#cutoff point for response time
cutoff.response_time = gso.fire %>% 
  dplyr::select(response_time_seconds) %>%
  filter(!is.na(response_time_seconds)) %>%
  summarize(c = quantile(response_time_seconds, .75) + 3 * IQR(response_time_seconds)) %>%
  pull(c)

#...
#histograms
#...

#histogram of distribution of response time
gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(x = response_time_seconds)) +
  geom_histogram(show.legend = FALSE, color = "black", fill = "pink") +
  xlab("Response Time (Seconds)") +
  ylab("Count") +
  ggtitle("Distribution of Response Time") +
  theme_economist()

gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  count(CivilianInjuries) %>%
  mutate(nlog = log10(n)) %>%
  ggplot(aes(x = CivilianInjuries, y = nlog)) +
  geom_col(color = "black", fill = "pink") +
  xlab("Civilian Injuries") +
  ylab("Count (Log10 scale)") +
  ggtitle("Distribution of Civilian Injuries") +
  theme_economist()

gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(x = TotalStaffOnIncident)) +
  geom_histogram(bins = 25,show.legend = FALSE, color = "black", fill = "pink") +
  scale_y_log10() +
  xlab("Total Staff on Incident") +
  ylab("Count (Log10 scale)") +
  ggtitle("Distribution of Total Staff on Incident") +
  theme_economist() +
  xlim(0,25)

gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(x = PropertyLoss)) +
  geom_histogram(bins = 20,show.legend = FALSE, color = "black", fill = "pink") +
  scale_x_log10() +
  xlab("Property Loss (Log10 scale)") +
  ylab("Count") +
  ggtitle("Distribution of Property Loss") +
  theme_economist() 

gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(x = TotalLosses)) +
  geom_histogram(bins = 20,show.legend = FALSE, color = "black", fill = "pink") +
  scale_x_log10() +
  xlab("Total Loss (Log10 scale)") +
  ylab("Count") +
  ggtitle("Distribution of Total Loss") +
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

gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(y = TotalLosses, x = as.factor(AlarmHour))) +
  geom_boxplot(show.legend = FALSE) +
  scale_y_log10() +
  ylab("Total Loss (Log10 scale)") +
  xlab("Alarm Hour") +
  ggtitle("Total Loss by Alarm Hour")

gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(y = TotalLosses, x = shift)) +
  geom_boxplot(show.legend = FALSE) +
  scale_y_log10() +
  ylab("Total Loss (Log10 scale)") +
  xlab("Shift") +
  ggtitle("Total Loss by Shift")

gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(y = PropertyLoss, x = as.factor(AlarmHour))) +
  geom_boxplot(show.legend = FALSE) +
  scale_y_log10() +
  ylab("Property Loss (Log10 scale)") +
  xlab("Alarm Hour") +
  ggtitle("Property Loss by Alarm Hour")

gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(y = PropertyLoss, x = shift)) +
  geom_boxplot(show.legend = FALSE) +
  scale_y_log10() +
  ylab("Property Loss (Log10 scale)") +
  xlab("Shift") +
  ggtitle("Property Loss by Shift") +
  theme_economist()

#...
#scatter plots
#...

#Scatter plot of Total Staff on Incident vs Response Time
gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(x = TotalStaffOnIncident, y = response_time_seconds)) +
  geom_point(stat = "identity", show.legend = FALSE) +
  xlab("Total Staff On Incident") +
  ylab("Response Time (Seconds)") +
  ggtitle("Total Staff on Incident vs Response Time") +
  theme_economist()

gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(y = PropertyLoss, x = response_time_seconds)) +
  geom_point(stat = "identity", show.legend = FALSE) +
  scale_y_log10() +
  ylab("Property Loss (Log10 scale)") +
  xlab("Response Time (Seconds)") +
  ggtitle("Property Loss vs Response Time") +
  theme_economist()

gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(y = TotalLosses, x = response_time_seconds)) +
  geom_point(stat = "identity", show.legend = FALSE) +
  scale_y_log10() +
  ylab("Total Loss (Log10 scale)") +
  xlab("Response Time (Seconds)") +
  ggtitle("Total Loss vs Response Time") +
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

gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(x = CivilianInjuries, y = response_time_seconds)) +
  geom_violin(show.legend = FALSE, fill = "light blue") +
  geom_boxplot(width = .5, show.legend = FALSE, fill = "pink") +
  xlab("Civilian Injuries") +
  ylab("Response Time (Seconds)") +
  ggtitle("Violin and Box Plot of Response Time Densities by Civilian Injuries")

gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(x = CivilianFatalities, y = response_time_seconds)) +
  geom_violin(show.legend = FALSE, fill = "light blue") +
  geom_boxplot(width = .5, show.legend = FALSE, fill = "pink") +
  xlab("Civilian Fatalities") +
  ylab("Response Time (Seconds)") +
  ggtitle("Violin and Box Plot of Response Time Densities by Civilian Fatalities")

gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>%
  ggplot(aes(x = FireServiceInjuries, y = response_time_seconds)) +
  geom_violin(show.legend = FALSE, fill = "light blue") +
  geom_boxplot(width = .5, show.legend = FALSE, fill = "pink") +
  xlab("Fire Service Injuries") +
  ylab("Response Time (Seconds)") +
  ggtitle("Violin and Box Plot of Response Time Densities by Fire Service Injuries")

#Time Series

#daily number of fire incidents
gso.fire.ts = gso.fire %>%
  count(AlarmDate2)

#time series of daily number of fire incidents
gso.fire.ts %>%
  ggplot(aes(x = AlarmDate2, y = n)) +
  geom_line() +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 month") + 
  xlab("Year") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of daily number of fire incidents") +
  theme(axis.text.x = element_text(angle = 90))

incidents_annual = gso.fire.ts %>%
  separate(AlarmDate2, sep="-", into = c("Year", "Month", "Day"))

incidents_annual = cbind(incidents_annual, gso.fire.ts$AlarmDate2)
colnames(incidents_annual) = c("Year","Month","Day","n","AlarmDate2")

incidents_annual %>%
  ggplot(aes(x = AlarmDate2, y = n)) + 
  ggtitle("Time Series Plots for Frequency of Daily Fire Incidents Organized by Year") + 
  geom_line() + 
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") + 
  xlab("Date") + ylab("Count") + 
  theme(axis.text.x = element_text(angle = 90, size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(plot.title = element_text(size=22)) +
  theme(axis.title=element_text(size=18)) +
  facet_wrap(~Year, scales = "free_x")


#Monthly number of fire incidents
gso.fire.ts.month = gso.fire %>%
  mutate(Monthly = make_date(Year, Month)) %>%
  group_by(Monthly, Month, Year) %>%
  count()

#time series of monthly number of fire incidents for years 2010-2022
gso.fire.ts.month %>%
  ggplot(aes(x = Monthly, y = n)) +
  geom_line() +
  scale_x_date(date_labels = "%m-%y", date_breaks = "6 month") + 
  xlab("Month") +
  ylab("Number of Fire Incidents") +
  ggtitle("Time series of monthly number of fire incidents for 2010-2022") +
  theme(axis.text.x = element_text(angle = 90))

########
#Time Series Forecasting

#forecasting for daily frequency of fire incidents

#convert gso.fire.ts to official ts object
gso.fire.ts.2 <- ts(gso.fire.ts$n, start = c(2010, 182), end = c(2022, 153),
                 frequency = 365)

#check for stationarity

#determine the number of differences required for time series to be made stationary
ndiffs(gso.fire.ts.2) #1

nsdiffs(gso.fire.ts.2) #0

acf(gso.fire.ts.2)
summary(ur.kpss(gso.fire.ts.2))

#attempt at differencing
gso.fire.ts.2.dif = diff(gso.fire.ts.2)
acf(gso.fire.ts.2.dif)

summary(ur.kpss(gso.fire.ts.2.dif))
ndiffs(gso.fire.ts.2) #1
nsdiffs(gso.fire.ts.2) #0

cbind("Fire Incidents" = gso.fire.ts.2,
      "First differenced" = diff(gso.fire.ts.2)) %>%
  autoplot(facets=TRUE) +
  xlab("Date") + ylab("") +
  ggtitle("Stationarity Transformations of Daily Frequency of Fire Incidents")

#now forecasting daily number of fire incidents
y.dif = msts(gso.fire.ts.2.dif, seasonal.periods=c(7,365.25))
daily.dif.tbats = tbats(y.dif)
daily.dif.tbats.fc = forecast::forecast(daily.dif.tbats, h = 214)

#point forecast values
dif.Yhat = daily.dif.tbats.fc$mean
dif.Yhat.upper = daily.dif.tbats.fc$upper
dif.Yhat.lower = daily.dif.tbats.fc$lower

Yhat = cumsum(c(gso.fire.ts.2[length(gso.fire.ts.2)],dif.Yhat))
Yhat = ts(Yhat, start = c(2022, 152), frequency=365)
Yhat_upper = cumsum(c(gso.fire.ts.2[length(gso.fire.ts.2)],dif.Yhat.upper))
Yhat_upper = ts(Yhat_upper, start = c(2022, 152), end = c(2023,1), frequency=365)
Yhat_lower = cumsum(c(gso.fire.ts.2[length(gso.fire.ts.2)],dif.Yhat.lower))
Yhat_lower = ts(Yhat_lower, start = c(2022, 152), end = c(2023,1), frequency=365)

fire.ts = ts(gso.fire.ts.2[3992:4352],start = c(2021, 25), end = c(2022, 153),
              frequency = 360) # A 360 day trim to see forecast better

#TBAT forecasting
autoplot(fire.ts) + 
  autolayer(Yhat, series="Point Forecasts") +
  ggtitle("TBATS Forecasting Model for Daily Frequency of Fire Incidents") +
  xlab("Date") +
  ylab("Frequency of Fire Incidents") + 
  theme(title = element_text(size = 10), legend.position = "bottom")

#checking accuracy
accuracy(daily.dif.tbats)

#ARIMA forecasting
daily.arima = auto.arima(gso.fire.ts.2)
daily.arima.fc = forecast(daily.arima, h=214)
autoplot(daily.arima.fc) +
  ggtitle("ARIMA Forecasting Model for Daily Frequency of Fire Incidents") +
  xlab("Date") + 
  coord_cartesian(xlim = c(2020, 2023)) +
  ylab("Frequency of Fire Incidents") + 
  theme(title = element_text(size = 10))

#checking accuracy
accuracy(daily.arima)

#this daily data ends three months earlier than true daily data
gso.fire.ts.3 = ts(gso.fire.ts$n, start = c(2010, 182), end = c(2022, 59),
                                 frequency = 365)
#attempt at differencing
gso.fire.ts.3.dif = diff(gso.fire.ts.3)

y.dif.3 = msts(gso.fire.ts.3.dif, seasonal.periods=c(7,365.25))
daily.dif.tbats.3 = tbats(y.dif.3)
daily.dif.tbats.fc.3 = forecast::forecast(daily.dif.tbats.3, h=671)
dif.Yhat.3 = daily.dif.tbats.fc.3$mean #point forecast values 

Yhat.3 = cumsum(c(gso.fire.ts.3[length(gso.fire.ts.3)],dif.Yhat.3))
Yhat.3 = ts(Yhat.3, start = c(2022, 60), frequency=365)

gso.fire.ts.true = ts(gso.fire.ts$n, start = c(2010, 182), end = c(2022, 153),
                    frequency = 365)
#TBAT forecasting
autoplot(gso.fire.ts.3) + 
  autolayer(gso.fire.ts.true, alpha=0.7, series="True Count") +
  autolayer(Yhat.3, alpha=0.6, series="Point Forecasts") + 
  ggtitle("TBATS Forecasting Model for Daily Frequency of Fire Incidents") +
  xlab("Date") +
  ylab("Frequency of Fire Incidents") + 
  theme(title = element_text(size = 10), legend.position = "bottom")

#checking accuracy
accuracy(daily.dif.tbats.3)

#ARIMA forecasting
daily.arima.3 = auto.arima(gso.fire.ts.3)
daily.arima.fc.3 = forecast(daily.arima.3, h=671)
autoplot(daily.arima.fc.3) + 
  ggtitle("ARIMA Forecasting Model for Daily Frequency of Fire Incidents") +
  xlab("Date") +
  coord_cartesian(xlim = c(2020, 2023)) +
  ylab("Frequency of Fire Incidents") + 
  theme(title = element_text(size = 10))

#checking accuracy
accuracy(daily.arima.3)

#######
#Forecasting for monthly frequency of Fire Incidents

#convert gso.fire.ts.month to official ts object
gso.fire.ts.month.2 = ts(gso.fire.ts.month$n, start = c(2010, 07, 01), end = c(2022, 05, 01),
                          frequency = 12)

#looking at stationarity
acf(gso.fire.ts.month.2)
summary(ur.kpss(gso.fire.ts.month.2))

ndiffs(gso.fire.ts.month.2) #1
nsdiffs(gso.fire.ts.month.2) #0

#attempt at differencing
gso.fire.ts.month.2.dif = diff(gso.fire.ts.month.2)

#looking at stationarity for difference
acf(gso.fire.ts.month.2.dif)

summary(ur.kpss(gso.fire.ts.month.2.dif))
ndiffs(gso.fire.ts.month.2.dif) #0
nsdiffs(gso.fire.ts.month.2.dif) #0

cbind("Fire Incidents" = gso.fire.ts.month.2,
      "First differenced" = diff(gso.fire.ts.month.2)) %>%
  autoplot(facets=TRUE) +
  xlab("Date") + ylab("") +
  ggtitle("Stationarity Transformation of Monthly Frequency of Fire Incidents")

y.dif.month = msts(gso.fire.ts.month.2.dif, seasonal.periods=12)
dif.tbats.month = tbats(y.dif.month)
dif.tbats.month.fc = forecast::forecast(dif.tbats.month, h = 8)

#point forecast values
dif.Yhat.month = dif.tbats.month.fc$mean

Yhat.month = cumsum(c(gso.fire.ts.month.2[length(gso.fire.ts.month.2)],dif.Yhat.month))
Yhat.month = ts(Yhat.month, start = c(2022, 05), frequency=12)

#TBATS forecasting
autoplot(gso.fire.ts.month.2) + 
  autolayer(Yhat.month, series="Point Forecasts") + 
  ggtitle("TBATS Forecasting Model for Monthly Frequency of Fire Incidents") +
  xlab("Date") +
  ylab("Frequency of Fire Incidents") + 
  theme(axis.text.x = element_text(angle = 90), title = element_text(size=10), 
        legend.position = "bottom") 

#checking accuracy
accuracy(dif.tbats.month)

#ARIMA forecasting
monthly.arima = auto.arima(gso.fire.ts.month.2)
monthly.arima.fc = forecast(monthly.arima, h=8)
autoplot(monthly.arima.fc) +
  ggtitle("ARIMA Forecasting Model for Monthly Frequency of Fire Incidents") +
  xlab("Date") + 
  coord_cartesian(xlim = c(2020, 2023)) +
  ylab("Frequency of Fire Incidents") + 
  theme(title = element_text(size = 10))

#checking accuracy
accuracy(monthly.arima)

#HoltWinters forecasting
dif.monthly.holt = HoltWinters(gso.fire.ts.month.2.dif)
dif.fcast.monthly = forecast::forecast(dif.monthly.holt, h=8, level=c(80,95))
dYhat.Holt = dif.fcast.monthly$mean

Yhat.Holt = cumsum(c(gso.fire.ts.month.2[length(gso.fire.ts.month.2)],dYhat.Holt))
Yhat.Holt = ts(Yhat.Holt, start = c(2022, 05), frequency=12)

autoplot(gso.fire.ts.month.2) +
  autolayer(Yhat.Holt, series = "Point Forecasts") +
  ggtitle("Forecasts for Monthly Frequency of Fire Incidents using HoltWinters") +
  xlab("Date") +
  ylab("Frequency of Fire Incidents") +
  theme(axis.text.x = element_text(angle = 90), title = element_text(size=9), 
        legend.position = "bottom")

#checking accuracy
accuracy(dif.fcast.monthly)

########
#Multi-Step Forecasting

#Daily Fire Incidents

#getting differenced training and test sets 
y.dif.2 = msts(gso.fire.ts.2.dif, seasonal.periods=c(7,365.25))
dif.training.tbats.2 = subset(y.dif.2, end=length(y.dif.2)-151)
dif.test.tbats.2 = subset(y.dif.2, start=length(y.dif.2)-150)
dif.daily.train.tbats.2 = tbats(dif.training.tbats.2)
dif.train.fc = forecast(dif.daily.train.tbats.2, h=151)

#getting point forecasts 
dYhat.tbats = dif.train.fc$mean

#setting up normal ts training and test sets 
y.2 = msts(gso.fire.ts.2, seasonal.periods=c(7,365.25))
training.tbats.2 = subset(y.2, end=length(y.2)-151)
test.tbats.2 = subset(y.2, start=length(y.2)-150)

#reverting back to original points   
Yhat.tbats = cumsum(c(training.tbats.2[length(training.tbats.2)], dYhat.tbats))
Yhat.tbats = ts(Yhat.tbats, start = c(2022, 1), frequency = 365)

autoplot(training.tbats.2) + 
  autolayer(Yhat.tbats, series="Point Forecasts") + 
  autolayer(test.tbats.2, series="Test Set", alpha = 0.7) + 
  ggtitle("Multi-Step TBATS Daily Forecasts of Frequency of Fire Incidents") + 
  xlab("Date") +
  ylab("Frequency of Fire Incidents") +
  coord_cartesian(xlim = c(2020, 2023)) +
  theme(legend.position = "bottom")

dif.daily.test.tbats.2 = tbats(dif.test.tbats.2)
accuracy(dif.daily.test.tbats.2)

#multi-step ARIMA forecast for daily data
training.arima = subset(gso.fire.ts.2, end=length(gso.fire.ts.2)-151)
test.arima = subset(gso.fire.ts.2, start=length(gso.fire.ts.2)-150)
daily.train.arima = auto.arima(training.arima)
fc.train.arima = forecast(daily.train.arima, h=151)

autoplot(fc.train.arima) + 
  autolayer(test.arima, series="Test Set", alpha = 0.7) + 
  ggtitle("Multi-Step ARIMA Daily Forecasts of Frequency of Fire Incidents") + 
  xlab("Date") +
  ylab("Frequency of Fire Incidents") + 
  coord_cartesian(xlim = c(2020, 2023)) +
  theme(legend.position = "bottom")

daily.test.arima = arima(test.arima)
accuracy(daily.test.arima)

#time series combination
train = window(gso.fire.ts.2, end=c(2019, 12))
h = length(gso.fire.ts.2) - length(train)

#ARIMA model 
ARIMA = forecast::forecast(auto.arima(train, lambda=0, biasadj=TRUE), h=h)

#TBATS model 
train.tbats = msts(gso.fire.ts.2.dif, end=c(2019,12), seasonal.periods = c(7,365.25))
TBATS = forecast::forecast(tbats(train.tbats, biasadj = TRUE), h=h)
dYhat.combo = TBATS$mean

Yhat.combo = cumsum(c(train[length(train)], dYhat.combo))
Yhat.combo = ts(Yhat.combo, start = c(2019, 12), frequency = 365)
Combination = (ARIMA[["mean"]] + Yhat.combo)/2
true.daily = gso.fire.ts %>%
  filter(AlarmDate2 >= as.Date("2019/12/01") & AlarmDate2 <= as.Date("2021/05/31"))
true.daily.ts = ts(true.daily$n, 
                            start = c(2019,12), end = c(2021,153), 
                            frequency = 365)

autoplot(train) +
  autolayer(true.daily.ts) +
  autolayer(Combination, series="Combination", alpha=0.8) +
  autolayer(ARIMA, series="ARIMA", PI=F) +
  autolayer(Yhat.combo, series="TBATS", alpha=0.7) +
  xlab("Date") +
  ylab("Frequency of Fire Incidents") +
  ggtitle("Time Series Combination for Daily Frequency of Fire Incidents") + 
  theme(legend.position = "bottom")

c(ARIMA = accuracy(ARIMA, gso.fire.ts.2)["Test set", "RMSE"],
  TBATS = accuracy(Yhat.combo, gso.fire.ts.2)["Test set", "RMSE"],
  Combination =
    accuracy(Combination, gso.fire.ts.2)["Test set", "RMSE"])

######
#multi-step forecasting for monthly data

#multi-step TBATS
y2.dif.monthly = msts(gso.fire.ts.month.2.dif, seasonal.periods=12)
dif.training.tbats.2.monthly = subset(y2.dif.monthly, end=length(y2.dif.monthly)-5)
dif.test.tbats.2.monthly = subset(y2.dif.monthly, start=length(y2.dif.monthly)-4)
dif.train.tbats.2.monthly = tbats(dif.training.tbats.2.monthly)
dif.fc.train.tbats.2.monthly = forecast(dif.train.tbats.2.monthly, h=5)
dYhat.ms.tbats.monthly = dif.fc.train.tbats.2.monthly$mean

y2.monthly = msts(gso.fire.ts.month.2, seasonal.periods=12)
training.tbats.monthly.2 = subset(y2.monthly, end=length(y2.monthly)-5)
test.tbats.monthly.2 = subset(y2.monthly, start=length(y2.monthly)-4)

Yhat.ms.tbats.monthly = cumsum(c(training.tbats.monthly.2[length(training.tbats.monthly.2)],
                                 dYhat.ms.tbats.monthly))
Yhat.ms.tbats.monthly = ts(Yhat.ms.tbats.monthly, start = c(2022,01), frequency=12)

autoplot(training.tbats.monthly.2) + 
  autolayer(Yhat.ms.tbats.monthly, series="Point Forecasts") + 
  autolayer(test.tbats.monthly.2, series="Test Set", alpha = 0.7) + 
  ggtitle("Multi-Step TBATS Monthly Forecasts of Frequency of Fire Incidents") + 
  xlab("Date") +
  ylab("Frequency of Fire Incidents") + 
  theme(legend.position = "bottom", title=element_text(size=10)) + 
  theme(axis.text.x = element_text(angle = 90))

dif.test.tbats.2.monthly = tbats(dif.test.tbats.2.monthly)
accuracy(dif.test.tbats.2.monthly)

#multi-step ARIMA
training.arima.monthly = subset(gso.fire.ts.month.2, end=length(gso.fire.ts.month.2)-5)
test.arima.monthly = subset(gso.fire.ts.month.2, start=length(gso.fire.ts.month.2)-4)
train.arima.monthly.3 = auto.arima(training.arima.monthly)
fc.train.arima.monthly = forecast(train.arima.monthly.3, h=5)

autoplot(fc.train.arima.monthly) + 
  autolayer(test.arima.monthly, series="Test Set", alpha = 0.7) + 
  ggtitle("Multi-Step ARIMA Monthly Forecasts of Frequency of Fire Incidents") + 
  xlab("Date") +
  ylab("Frequency of Fire Incidents") + 
  coord_cartesian(xlim = c(2018, 2023)) +
  theme(legend.position = "bottom") + 
  theme(axis.text.x = element_text(angle = 90), title = element_text(size=9), 
        legend.position = "bottom")

test.arima.monthly.3 = arima(test.arima.monthly)
accuracy(test.arima.monthly.3)

#multi-step HoltWinters
dif.training.HW.monthly = subset(gso.fire.ts.month.2.dif, end=length(gso.fire.ts.month.2.dif)-5)
dif.test.HW.monthly = subset(gso.fire.ts.month.2.dif, start=length(gso.fire.ts.month.2.dif)-4)
dif.train.HW.monthly = HoltWinters(dif.training.HW.monthly)
dif.fc.HW = forecast::forecast(dif.train.HW.monthly, h=5)

dYhat.ms.HW = dif.fc.HW$mean
Yhat.ms.HW = cumsum(c(gso.fire.ts.month.2[length(gso.fire.ts.month.2)],dYhat.ms.HW))
Yhat.ms.HW = ts(Yhat.ms.HW, start = c(2022,01,01), frequency=12)

training.HW.monthly = subset(gso.fire.ts.month.2, end=length(gso.fire.ts.month.2)-5)
test.HW.monthly = subset(gso.fire.ts.month.2, start=length(gso.fire.ts.month.2)-4)
true.count.monthly = ts(gso.fire.ts.month.2, start=c(2010,07,01), end=c(2021,12,31), frequency=12)

autoplot(training.HW.monthly) + 
  autolayer(Yhat.ms.HW, series = "Point Forecasts") + 
  autolayer(test.HW.monthly, series = "Test Set") + 
  ggtitle("Multi-Step HoltWinters Monthly Forecasts of Frequency of Fire Incidents") + 
  xlab("Date") +
  ylab("Frequency of Fire Incidents") +
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom", 
        title = element_text(size = 9))

accuracy(Yhat.ms.HW, gso.fire.ts.month.2)

#Time series combination for monthly
train.monthly.2 = window(gso.fire.ts.month.2, end=c(2021, 05))
h.monthly.2 = length(gso.fire.ts.month.2) - length(train.monthly.2)

#ARIMA model 
ARIMA.monthly.2 = forecast::forecast(auto.arima(train.monthly.2, lambda=0, biasadj=TRUE),
                                   h=h.monthly.2)

#TBATS model 
train.tbats.monthly.2 = msts(gso.fire.ts.month.2.dif, end=c(2021,05), seasonal.periods = 12)
tbats.monthly.2 = forecast::forecast(tbats(train.tbats.monthly.2, biasadj = TRUE),
                                     h=h.monthly.2)
dYhat.combo.monthly.2 = tbats.monthly.2$mean

Yhat.combo.monthly.2 = cumsum(c(train.monthly.2[length(train.monthly.2)], dYhat.combo.monthly.2))
Yhat.combo.monthly.2 = ts(Yhat.combo.monthly.2, start = c(2021,05), frequency = 12)

#HoltWinters Model 
HW.monthly.2 = forecast::forecast(HoltWinters(gso.fire.ts.month.2.dif), h=h.monthly.2)

dYhat.HW.monthly.2 = HW.monthly.2$mean

Yhat.HW.monthly.2 = cumsum(c(train.monthly.2[length(train.monthly.2)],dYhat.HW.monthly.2))
Yhat.HW.monthly.2 = ts(Yhat.HW.monthly.2, start = c(2021,05), frequency=12)
Combination.monthly.2 = (ARIMA.monthly.2[["mean"]] + Yhat.combo.monthly.2 + 
                           Yhat.HW.monthly.2)/3

y.monthly = msts(gso.fire.ts.month.2, seasonal.periods=12)
test.monthly = subset(y.monthly, start=length(y.monthly)-12)

autoplot(train.monthly.2) +
  autolayer(test.monthly, series="True Count") +
  autolayer(Combination.monthly.2, series="Combination", alpha=0.8) +
  autolayer(ARIMA.monthly.2, series="ARIMA", PI=F) +
  autolayer(Yhat.combo.monthly.2, series="TBATS", alpha=0.7) +
  autolayer(Yhat.HW.monthly.2, series="HoltWinters", alpha=0.7) + 
  xlab("Date") +
  ylab("Frequency of Fire Incidents") +
  ggtitle("Time Series Combination for Monthly Frequency of Fire Incidents") + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom", 
        title = element_text(size = 9)) 

c(ARIMA = accuracy(ARIMA.monthly.2, gso.fire.ts.month.2)["Test set", "RMSE"],
  TBATS = accuracy(Yhat.combo.monthly.2, gso.fire.ts.month.2)["Test set", "RMSE"],
  HW = accuracy(Yhat.HW.monthly.2, gso.fire.ts.month.2)["Test set", "RMSE"],
  Combination =
    accuracy(Combination.monthly.2, gso.fire.ts.month.2)["Test set", "RMSE"])

##########

#######
#Modeling of Response Time

#Filtering data
gso.fire.filtered = gso.fire %>%
  filter(response_time_seconds < cutoff.response_time) %>% 
  dplyr::select(response_time_seconds, everything())

gso.fire.filtered = gso.fire.filtered %>%
  mutate(AlarmTime = case_when(AlarmHour >= 0 & AlarmHour < 6 ~ "0-5", 
                               AlarmHour >= 6 & AlarmHour < 12 ~ "6-11",
                               AlarmHour >= 12 & AlarmHour < 18 ~ "12-17",
                               TRUE ~ "18-23"))

#manual variable selection
lm.res.time = lm(response_time_seconds ~ TotalStaffOnIncident + FireDistrict + DayOfWeek + 
                   shift + Month + AlarmTime + NatureCode, data = gso.fire.filtered)
summary(lm.res.time)
round(coef(lm.res.time), 4)

#Variable Selection
lm.res.time.select = stepAIC(lm.res.time)
summary(lm.res.time.select)

#Lasso variable selection
las.Mod <- glmnet(as.matrix(gso.fire.filtered[,-1]), gso.fire.filtered$response_time_seconds, family = "gaussian")
plot(las.Mod)


#Random Forrest with ranger function
gso.fire.filtered.rf = gso.fire.filtered %>%
  dplyr::select(response_time_seconds,TotalStaffOnIncident , FireDistrict , DayOfWeek , 
           shift , Month , AlarmTime , NatureCode)

RF.Mod.Ranger <- ranger(response_time_seconds~TotalStaffOnIncident + FireDistrict + DayOfWeek + 
                         shift + Month + AlarmTime + NatureCode,
                       data = gso.fire.filtered.rf[complete.cases(gso.fire.filtered.rf),], importance = "impurity", num.trees = 500)

variable = names(RF.Mod.Ranger$variable.importance)
rf.vIMP.df = data.frame(variable = variable, vIMP = RF.Mod.Ranger$variable.importance)

ggplot(rf.vIMP.df, aes(x=reorder(variable,vIMP), y=vIMP,fill=variable))+ 
  geom_bar(stat="identity", position="dodge", show.legend = FALSE)+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("Information Value Summary")
