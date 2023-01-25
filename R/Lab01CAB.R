# introduction to R


print("Hello World")

## case sensitive

ThisLanguageIsCaseSensitive = 1:10*pi
ThisLanguageIsCaseSensitive

## install and load packages

install.packages("pacman")

pacman :: p_load (rgdal,parallel, htsr)

pacman :: p_load(ggplot2, dplyr, patchwork, hrbrthemes, rnoaa)

## Graphing example

data <- data.frame(day = as.Date("2019-01-01") + 0:99, temperature = runif(100) + seq(1,100)^2.5 / 10000,
                   price = runif(100) + seq(100,1)^1.5 / 10 )

p1 <- ggplot(data, aes(x=day,y=temperature)) + geom_line(color = "#61a3a2", size = 2) + ggtitle("Temperature: range 1-10") +
  theme_ipsum() + geom_line(aes(x=day,y=price))

p1


## writing functions

vary1 <-  function(a,b) {
  c <-  a*b 
  return(c)
}

vary2 <-  vary1(a,d)

a =  runif(10)
d = a*runif(10)

## complex function Using USGS in class example

source("https://goo.gl/Cb8zGn")
myflowgage_id = "03171000"
myflowgage = get_usgs_gage(myflowgage_id,
                           begin_date="2017-02-01",end_date="2023-02-01")
class(myflowgage)
View(myflowgage$flowdata)

### View data frame from console & Ugly Plot

str(myflowgage)

p2 <- plot(myflowgage$flowdata$mdate,myflowgage$flowdata$flow,
           main=myflowgage$gagename,xlab = "Date",
           ylab="Flow m^3/day",type="l")

p2
## Precipitation data 


station_data <- ghcnd_stations()
meteo_distance(station_data, -33, 151, radius = 10, limit = 10)


stns=meteo_distance(
  station_data=ghcnd_stations(),
  lat=myflowgage$declat,
  long=myflowgage$declon,
  units = "deg",
  radius = 20,
  limit = NULL
)


WXData = meteo_pull_monitors(
  monitors="USC00446999",    # replace the *** with index you find
  keep_flags = FALSE,
  date_min = "2016-01-01",
  date_max = NULL,
  var = c("TMAX","TMIN","PRCP")
)

mean(WXData$prcp,na.rm = T)*365

plot(WXData$date,WXData$tmax,type="l", col="red")
lines(WXData$date,WXData$tmin,type="l", col="blue")
points(WXData$date,WXData$prcp, col="black")


# HW   0

## GIT token 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

# HW 1 

## Set up USGS station near my hometown

### USGS 01674500 MATTAPONI RIVER NEAR BEULAHVILLE, VA

myflowgage_id = "01674500"
myflowgage = get_usgs_gage(myflowgage_id,
                           begin_date="2020-02-01",end_date="2022-12-01")
class(myflowgage)
View(myflowgage$flowdata)

str(myflowgage)

## Call weather data 

station_data <- ghcnd_stations()
meteo_distance(station_data, -33, 151, radius = 10, limit = 10)


stnshw=meteo_distance(
  station_data=ghcnd_stations(),
  lat=myflowgage$declat,
  long=myflowgage$declon,
  units = "deg",
  radius = 20,
  limit = NULL
)


WXDatahw = meteo_pull_monitors(
  monitors="USC00448829",    # replace the *** with index you find
  keep_flags = FALSE,
  date_min = "2020-02-01",
  date_max = "2022-12-01",
  var = c("TMAX","TMIN","PRCP")
)

mean(WXDatahw$prcp,na.rm = T)*365

str(WXDatahw)

?meteo_pull_monitors
## Plot weather data


phw1 <- ggplot(WXDatahw, aes(x=date,y=tmax/10))  + geom_line(color = "#b93444", size = 0.5)+ 
  geom_line(aes(x=date,y=tmin/10),color = "#00ffff", size = 0.3) +
  geom_point(aes(x=date,y=prcp/10),color = "#0000ff", size = 0.3) +
  scale_y_continuous(
    name = "Temperature (C) ",
    sec.axis = sec_axis( trans=~.*1, name="Precipitation (milimeters)")
  ) + theme_ipsum() + ggtitle("Temperature & Precipitation",subtitle = "@ Mattaponi River near Beulahville, VA")


phw1 


# HW 2

phw2 <- ggplot(WXDatahw, aes(x=date,y=tmax/10))  + geom_line(color = "#b93444", size = 0.5)+ 
  geom_line(aes(x=date,y=tmin/10),color = "#00ffff", size = 0.3) +
  geom_point(aes(x=date,y=prcp/10),color = "#0000ff", size = 0.3) +
  scale_y_continuous(
    name = "Temperature (C) & Precipitation (milimeters) ",
    sec.axis = sec_axis( trans=~.*1, name="Flow (m^3/day * 10^5.5)")
  ) + theme_ipsum() + 
  geom_line(aes(x=myflowgage$flowdata$mdate,y=myflowgage$flowdata$flow/10^5.5))+
  ggtitle("Temperature, Precipitation, & Flow",subtitle = "@ Mattaponi River near Beulahville, VA")

phw2

