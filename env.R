library(tidyverse)
library(jsonlite)
#api key: 4a6ac8c9403740a1aac8c9403750a197
#my_weather_station<-"KMAIPSWI59" #appleton
#my_weather_station(KMASOUTH239) #pingree school

# to create the JSON file (hourly)
#https://api.weather.com/v2/pws/history/hourly?stationId=KMAIPSWI59&format=json
# &units=m&date=20220810&apiKey=4a6ac8c9403740a1aac8c9403750a197

# other options: https://docs.google.com/document/d/1w8jbqfAk0tfZS5P7hYnar1JiitM0gQZB-clxDfG3aD0/edit
#daily: v2/pws/history/daily

myData <- jsonlite::fromJSON(txt="hourly.json", flatten=T)
test<-as.tibble(myData$observations)

myData2 <- as.tibble(jsonlite::fromJSON(txt="https://api.weather.com/v2/pws/history/hourly?stationId=KMAIPSWI59&format=json&units=m&date=20220810&apiKey=4a6ac8c9403740a1aac8c9403750a197", flatten=T)$observations)

#make a list of api calls with a for loop changing the date text. another loop to call the data, then rbind it into a new file. 

#dates in 2022
d2022<-seq(as.Date("2022-01-01"), as.Date("2022-12-31"), by="days", format="%Y%m%d")
d2022<-format(d, "%Y%m%d")

#vector to fill with all the dates
v2022<-vector(mode='character', length=365)
for (i in 1:365){
  v2022[i]=paste("https://api.weather.com/v2/pws/history/daily?stationId=KMASOUTH239&format=json&units=m&date=", d2022[i], "&apiKey=4a6ac8c9403740a1aac8c9403750a197", sep="")
}

#2021
d2021<-format(seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by="days", format="%Y%m%d"), "%Y%m%d")
v2021<-vector(mode='character', length=365)
for (i in 1:365){
  v2021[i]=paste("https://api.weather.com/v2/pws/history/daily?stationId=KMASOUTH239&format=json&units=m&date=", d2021[i], "&apiKey=4a6ac8c9403740a1aac8c9403750a197", sep="")
}

#data frame filled with data for 2022 (pingree school!)
df2022<-tibble()
for(i in 1:273){
  df2022<-rbind(df2022, 
            as.tibble(jsonlite::fromJSON(txt=v2022[i], flatten=T)$observations)
            )
}

#data frame filled with data for 2022 (pingree school!)
df2021<-tibble()
for(i in 1:273){
  df2021<-rbind(df2021, 
                as.tibble(jsonlite::fromJSON(txt=v2021[i], flatten=T)$observations)
  )
}

df2022$date<-d2022[1:222]
df2021$date<-append(d2021[120:129], d2021[141:273])

df2022<-df2022%>%
  mutate(date=substr(date, 5, 8))
df2021<-df2021%>%
  mutate(date=substr(date, 5, 8))

library(lubridate)
df2022$date <- as.Date(parse_date_time(df2022$date, "%m/%d"))
df2021$date <- as.Date(parse_date_time(df2021$date, "%m/%d"))


ggplot(subset(df2022, date>as.Date("0000-05-20")), aes(x=(date), y=cumsum(metric.precipTotal)))+
  geom_line(data=subset(df2021, date>as.Date("0000-05-20")), stat="identity", color="blue", aes(y=cumsum(metric.precipTotal)))+geom_line(stat="identity", color="red")+
scale_x_date(limits = as.Date(c("0000-05-20", "0000-08-10")))+xlab("date")+ylab("Pingree Cumulative Daily Precipitation (mm)")+theme_bw()+
annotate(geom="text", x=as.Date("0000-08-01"), y=180, col="red", label="2022: 123mm")+
  annotate(geom="text", x=as.Date("0000-08-01"), y=450,col="blue",label="2021: 396mm")




