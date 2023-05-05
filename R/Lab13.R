if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,dplyr,patchwork,rnoaa,meteoForecast)
?meteoForecast::getPointDays

gfsvarstemp=grepVar('temp', service = 'gfs', complete = TRUE)



testDay <- Sys.Date() - 1
library(lattice)
##D   ## Beware: the x-axis labels display time using your local timezone.

today = Sys.Date()
myflowgage_id="01669000"  # USGS 01669000 PISCATAWAY CREEK NEAR TAPPAHANNOCK, VA

myflowgage=get_usgs_gage(myflowgage_id,begin_date = "2015-01-01",
                         end_date = "2022-03-01")

myflowgage$flowdata$Qmm = myflowgage$flowdata$flow/myflowgage$area/10^3


source("https://raw.githubusercontent.com/Rojakaveh/FillMissWX/main/FillMissWX.R")
WXData=FillMissWX(declat=myflowgage$declat, declon=myflowgage$declon,
                  StnRadius=30,minstns=10,date_min="2010-01-01",
                  date_max="2023-05-06",targElev=myflowgage$elev,
                  method = "IDW",alfa=2)
 ## Multiple variables
precipvars <- getPoint(c(myflowgage$declon, myflowgage$declat), vars = "Total_precipitation_surface_Mixed_intervals_Accumulation", day = today, service = "gfs")
GFSPrecip =aggregate(vars,as.Date(time(vars)),sum)

xyplot(precipvars)
plot(precip, xlab = "Date", ylab = "Prepipitation (cm)")

tempvars <- getPoint(c(myflowgage$declon, myflowgage$declat), vars = "Temperature_surface", day = today, service = "gfs")
GFSMaxTemp = aggregate(tempvars,as.Date(time(tempvars)),max)
GFSMinTemp = aggregate(tempvars,as.Date(time(tempvars)),min)
plot(GFSMaxTemp, ylim=c(276,305))
lines(GFSMinTemp)

dir.create("~/pngs")
setwd("~pngs")
graphdir = "~/pngs"
png(paste0(graphdir, "/TempForecast.png"))
plot(GFSMaxTemp, ylim=c(276,305))
lines(GFSMinTemp)
dev.off()
 
plot(GFSMaxTemp)


