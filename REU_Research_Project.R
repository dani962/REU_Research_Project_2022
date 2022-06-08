library(readr)

gso.fire <- read_csv("C:/Users/dmbut/Dropbox/PC/Downloads/Greensboro_Fire_Incidents.csv")

class(gso.fire$CallProcessingTime)

library(lubridate)

?lubridate

#convert to Period object
call_process_period <- lubridate::hms(gso.fire$CallProcessingTime)

class(call_process_period)

call_process_seconds <- period_to_seconds(call_process_period)
