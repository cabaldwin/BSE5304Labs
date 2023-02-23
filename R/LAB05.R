# 
# Since everything depends on the libraries you install
# it is worthwhile loading them at the beginning
#
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,dplyr,patchwork,rnoaa)
pacman::p_load(operators,topmodel,DEoptim,soilDB,sp,curl,httr,
               rnoaa,raster,shapefiles,rgdal,elevatr,terra,progress,lubridate)
LabNo="/Lab04"
#
# Getting our organization on for where we want to put
# Data, external programs, and our project files.
# Things are going to get messy if we don't start issolating
# our data files by Lab
#
myhomedir=Sys.getenv("HOME")
datadir=paste0(myhomedir,"/data",LabNo)
dir.create(datadir,recursive = T)
srcdir=paste0(myhomedir,"/src")
dir.create(srcdir,recursive = T)

# Setting the directory for where the GitHub project exists. 
# This depends on where you set up your git, and what you called it locally, 
# but when you start a new git project, it will be the first directory you 
# are placed in... or if later in the project:
# WOOO HOOO... took me a few hours to find this function!
# 
mygitdir=rstudioapi::getActiveProject()
mypdfdir=paste0(mygitdir,"/pdfs",LabNo)
dir.create(mypdfdir)
# 
setwd(mygitdir)
system("git config --global user.email 'cabaldwin2019@vt.edu' ") 
system("git config --global user.name 'cabaldwin' ")
system("git config pull.rebase false")
#
# This week, we discovered some "features" that make removing and 
# re-installing the EcoHydrology Library necessary.
#
setwd(srcdir)
detach("package:EcoHydRology", unload = TRUE)
remove.packages("EcoHydRology", lib="~/R/x86_64-pc-linux-gnu-library/4.2")
system("svn checkout svn://scm.r-forge.r-project.org/svnroot/ecohydrology/"); 
install.packages(c("ecohydrology/pkg/EcoHydRology/"),repos = NULL)
pacman::p_load(EcoHydRology)

setwd(datadir)
#
##USGS 12447390 ANDREWS CREEK NEAR MAZAMA, WA

myflowgage_id="12447390"
myflowgage=get_usgs_gage(myflowgage_id,begin_date = "2015-01-01",
                         end_date = "2022-03-01")

#
# This is where some folks had issues... they forgot to check their 
# watershed areas per the homework... though there were ways to fix
# it later with lower resolution DEM pull
#
print(myflowgage$area)
# For most watershed modelling purposes we normalize Q in mm/day for basins
myflowgage$flowdata$Qmm = myflowgage$flowdata$flow/myflowgage$area/10^3

# In the Lab02, we introduced you to a way to quickly get your WX Data 
# for any location in the world way easier than traditional download and
# parsing methods most old people use.
#
source("https://raw.githubusercontent.com/Rojakaveh/FillMissWX/main/FillMissWX.R")
WXData=FillMissWX(declat=myflowgage$declat, declon=myflowgage$declon,
                  StnRadius=30,minstns=10,date_min="2010-01-01",
                  date_max="2023-02-01",targElev=myflowgage$elev,
                  method = "IDW",alfa=2)

BasinData=merge(WXData,myflowgage$flowdata,by.x="date",by.y="mdate")
#
# Setting the projection information for the specific location
#
proj4_utm = paste0("+proj=utm +zone=", trunc((180+myflowgage$declon)/6+1), " +datum=WGS84 +units=m +no_defs")

# Lat/Lon (_ll) is much easier!
proj4_ll = "+proj=longlat"

# Now we will build our proj4strings which define our “Coordinate 
# Reference Systems” or CRS in future geographic manipulations. 
crs_ll=CRS(proj4_ll)
crs_utm=CRS(proj4_utm)
#
# Double chec

latlon <- cbind(myflowgage$declon,myflowgage$declat)
myflowgage$gagepoint_ll <- SpatialPoints(latlon)
proj4string(myflowgage$gagepoint_ll)=proj4_ll
myflowgage$gagepoint_utm=spTransform(myflowgage$gagepoint_ll,crs_utm)
# Open up maps.google.com to guesstimate area/lengths
url=paste0("https://www.google.com/maps/@",
           myflowgage$declat,",",myflowgage$declon,",18z")
browseURL(url)
# We are going to over estimate our area
# For our search we are going to multiply the area by 6 and
# to get the distance
searchlength=sqrt(myflowgage$area*8)*1000 
pourpoint=SpatialPoints(myflowgage$gagepoint_utm@coords,proj4string = crs_utm)
bboxpts=myflowgage$gagepoint_utm@coords
bboxpts=rbind(bboxpts,bboxpts+searchlength)
bboxpts=rbind(bboxpts,bboxpts-searchlength)
bboxpts
bboxpts=rbind(bboxpts,c(min(bboxpts[,1]),max(bboxpts[,2])))
bboxpts=rbind(bboxpts,c(max(bboxpts[,1]),min(bboxpts[,2])))
bboxpts
bboxpts=SpatialPoints(bboxpts,proj4string = crs_utm)
# From Lab04, get your DEM
mydem=get_aws_terrain(locations=bboxpts@coords, 
                      z = 10, prj = proj4_utm,src ="aws",expand=1)
res(mydem)
plot(mydem)
plot(bboxpts,add=T)
plot(pourpoint,add=T,col="red")

# Write our raster to a geotiff file that can be used with
# OS level hydrological models 
writeRaster(mydem,filename = "mydem.tif",overwrite=T)
# Our quick intro to terminal where the cloud offerings are usually Linux
# ls; cd ~; pwd;  # Linux/Mac 
# dir; cd ; # Windows

#
# I am going to set two different zoom levels so I can inspect 
# the TauDEM Processing below.
#

zoomext=myflowgage$gagepoint_utm@coords
zoomext=rbind(zoomext,zoomext+res(mydem)*100)
zoomext=rbind(zoomext,zoomext-res(mydem)*100)
zoomext=SpatialPoints(zoomext,proj4string = crs_utm)  
zoomext2=myflowgage$gagepoint_utm@coords
zoomext2=rbind(zoomext2,zoomext2+res(mydem)*10)
zoomext2=rbind(zoomext2,zoomext2-res(mydem)*10)
zoomext2=SpatialPoints(zoomext2,proj4string = crs_utm)  
zoom(mydem,ext=zoomext2)
plot(pourpoint,add=T,col="red")

# If you already installed this in your ~/src directory and it 
# worked... you really 
# cd ~/src/      # Set your directory to your home directory
# git clone https://github.com/dtarb/TauDEM.git
# mkdir ~/src/TauDEM/bin
# cd ~/src/TauDEM/src
# sed -i -e 's/MPI_Type_struct/MPI_Type_create_struct/g' linklib.h
## yes, this next line is very small font, but it is one line so...
# sed -i -e 's/MPI_Type_extent(MPI_LONG, \&extent)/MPI_Aint lb\;MPI_Type_get_extent(MPI_LONG, \&lb, \&extent)/g' linklib.h
## Now let's try make again!
# make

rm("old_path")
old_path <- Sys.getenv("PATH")
old_path

if( ! grepl("~/src/TauDEM/bin",old_path)){
  Sys.setenv(PATH = paste(old_path,
                          paste0(Sys.getenv("HOME"),"/src/TauDEM/bin"), 
                          sep = ":"))
}

system("mpirun aread8")

setwd(datadir)
z=raster("mydem.tif")
plot(z)

# Pitremove
system("mpiexec -n 2 pitremove -z mydem.tif -fel mydemfel.tif")
fel=raster("mydemfel.tif")
plot(fel-z)


# D8 flow directions
system("mpiexec -n 2 d8flowdir -p mydemp.tif -sd8 mydemsd8.tif -fel mydemfel.tif",show.output.on.console=F,invisible=F)
p=raster("mydemp.tif")
plot(p)
sd8=raster("mydemsd8.tif")
plot(sd8)

# Contributing area
system("mpiexec -n 2 aread8 -p mydemp.tif -ad8 mydemad8.tif")
ad8=raster("mydemad8.tif")
plot(log(ad8))
zoom(log(ad8),ext=zoomext2)
plot(pourpoint,add=T)

# Grid Network 
system("mpiexec -n 2 gridnet -p mydemp.tif -gord mydemgord.tif -plen mydemplen.tif -tlen mydemtlen.tif")
gord=raster("mydemgord.tif")
plot(gord)
zoom(gord,ext=zoomext2)

# DInf flow directions
system("mpiexec -n 2 dinfflowdir -ang mydemang.tif -slp mydemslp.tif -fel mydemfel.tif",show.output.on.console=F,invisible=F)
ang=raster("mydemang.tif")
plot(ang)
slp=raster("mydemslp.tif")
plot(slp)

# Dinf contributing area
system("mpiexec -n 2 areadinf -ang mydemang.tif -sca mydemsca.tif")
sca=raster("mydemsca.tif")
plot(log(sca))
zoom(log(sca),ext=zoomext2)

targetbasins=3  # Lets figure out some number of sub basins we want
# to break this into
res(mydem)      # Cell Resolution in meters
myflowgage$area # Area in km^2
myflowgage$area * 10^3 * 10^3 / res(mydem)[1]^2 / targetbasins
subthreshold=myflowgage$area * 10^3 * 10^3 / res(mydem)[1]^2 / targetbasins
subthreshold=as.integer(subthreshold)
# Threshold
syscmd=paste0("mpiexec -n 2 threshold -ssa mydemad8.tif -src mydemsrc.tif -thresh ",subthreshold)
system(syscmd)
src=raster("mydemsrc.tif")
plot(src)
zoom(src,ext=zoomext2)
plot(pourpoint,add=T)

outlet=SpatialPointsDataFrame(myflowgage$gagepoint_utm,
                              data.frame(Id=c(1),outlet=paste("outlet",1,sep="")))
writeOGR(outlet,dsn=".",layer="approxoutlets",
         driver="ESRI Shapefile", overwrite_layer=TRUE)
#

# Move Outlets
system("mpiexec -n 2 moveoutletstostrm -p mydemp.tif -src mydemsrc.tif -o approxoutlets.shp -om outlet.shp")

approxpt=readOGR("approxoutlets.shp")
plot(approxpt,add=T, col="blue")
outpt=readOGR("outlet.shp")
plot(outpt,add=T, col="red")

# Contributing area upstream of outlet
# Now that we know the location of an outlet, we can isolate our basin 
#
system("mpiexec -n 2 aread8 -p mydemp.tif -o outlet.shp -ad8 mydemssa.tif")
ssa=raster("mydemssa.tif")
plot(ssa) 

# Threshold
system("mpiexec -n 2 threshold -ssa mydemssa.tif -src mydemsrc1.tif -thresh 2000")
syscmd=paste0("mpiexec -n 2 threshold -ssa mydemssa.tif -src mydemsrc1.tif -thresh ",subthreshold)
system(syscmd)
src1=raster("mydemsrc1.tif")
plot(src1)
zoom(src1,ext=zoomext)

# Stream Reach and Watershed
system("mpiexec -n 2 streamnet -fel mydemfel.tif -p mydemp.tif -ad8 mydemad8.tif -src mydemsrc1.tif -o outlet.shp -ord mydemord.tif -tree mydemtree.txt -coord mydemcoord.txt -net mydemnet.shp -w mydemw.tif")
plot(raster("mydemord.tif"))
zoom(raster("mydemord.tif"),ext=zoomext2)
mydemw=raster("mydemw.tif")
zoom(mydemw,ext=zoomext)
summary(mydemw)
plot(mydemw)

# Trimming, Cropping, and Masking to make life prettier and easier
mydemw=raster("mydemw.tif")
mybasinmask=trim(mydemw,padding=2)
mydem=raster("mydem.tif")
mybasindem=crop(mydem,mybasinmask)
mybasindem=mask(mybasindem,mybasinmask)
plot(mybasindem)

# Make a poly with raster library (slow)
# or from thee command line gdal (fast)
# gdal_polygonize.py -8 mydemw.tif mydemw_poly_gdal.shp
mydemw=rast("mydemw.tif")
mydemw_poly=as.polygons(mydemw,na.rm=T)
plot(mydemw_poly,add=T,col=rainbow(6))

writeVector(mydemw_poly,dsn=".",layer="mydemw",driver="ESRI Shapefile", overwrite_layer=TRUE)
writeVector(mydemw_poly, filename="mydemw.shp", filetype="ESRI Shapefile", layer="mydemw", insert=FALSE,
            overwrite=TRUE)



# Lab 04 Calibration
# Building more complex functions
# 

TMWB=BasinData
#
# Our model will
# 1) Calculate PET for the basin via Function
# 2) Calculate the Snow Accumulation and Melt via Function
# 3) Run TMWB via Function 
#
#
# First functions from last week we already have, Wetting, Drying, 
# and Wetting above capacity 
# 

#
# Lets make one out of our Temperature Index Snow Model
#

SFTmp = 3  # referred to as SFTMP in SWAT input (Table 1)
bmlt6 = 4.5   # referred to as SMFMX in SWAT input (Table 1)
bmlt12 = 0.0  # referred to as SMFMN in SWAT input adjusted for season
Tmlt = SFTmp  # Assumed to be same as SnowFall Temperature
Tlag = 1  # referred to as TIMP in SWAT input (Table 1)
source("https://raw.githubusercontent.com/vtdrfuka/BSE5304Labs/main/R/TISnow.R")



SNO_df=TISnow(TMWB)
TMWB$SNO=SNO_df$SNO
TMWB$SNOmlt=SNO_df$SNOmlt
TMWB$SNOfall=SNO_df$SNOfall
TMWB$Tsno=SNO_df$Tsno
detach(TMWB)
#
# Our PET Model we will borrow from EcoHydrology
#
attach(TMWBdf)
TMWBdf$PET=PET_fromTemp(Jday=(1+as.POSIXlt(date)$yday),Tmax_C = MaxTemp,Tmin_C = MinTemp,
                      lat_radians = myflowgage$declat*pi/180) * 1000
plot(TMWBdf$date,TMWBdf$PET)

TMWBnew= TMWBmodel(TMWB)

plot(TMWB$date,P,col="black")
lines(TMWB$date,TMWB$Qmm,type = "l",col="red")
lines(TMWB$date,TMWB$Qpred,col="blue")
plot(TMWB$Qmm,TMWB$Qpred)

# Calibrate for S

BasinTMWB_JO=TMWBnew[(month(TMWBnew$date) > 5 
                      & month(TMWBnew$date) < 11),]
attach(BasinTMWB_JO)
plot(dP,Qmm)
detach(BasinTMWB_JO)

(1000/85-10)*25.4   # our CN estimate in bold
#[1] 44.82353
(1000/50-10)*25.4   # our CN estimate in bold
#[1] 254
#
# So we are going to visually "guestimate" that S should be somewhere between 
# 45mm and 260mm… repeat plotting until your solution covers the 
# largest Qmm vs dP event (upper right hand corner of plot). 
# 

# Assuming that (P-Ia) ~ dP, we can visually compare 
attach(BasinTMWB_JO)
plot(dP,Qmm)
points(dP,dP^2/(dP+45),col="red")  # S guestimates in bold
points(dP,dP^2/(dP+260),col="blue")# S guestimates in bold

# Now perform a “Calibration” using our method from Lab3 and the NSE
# as the “Objective Function”.  
#
# Vary S to maximize NSE using Eq. 4 of Lyon 2004 as our predictor of Q
#   Qpred=dP^2/(dP+S)
#
NSE(Qmm,dP^2/(dP+260))

NSE(Qmm,dP^2/(dP+45))

#
# Keep iterating until NSE is as high as you can get for your 
# best estimate to S (Sest)
#
f <- function (x) {
  Sest=x
  return(NSE(Qmm,dP^2/(dP+Sest)))
}
optimize(f, c(50,500), tol = 0.0001,maximum = TRUE)$maximum
Sest=50.00006
plot(dP,Qmm)
points(dP,dP^2/(dP+Sest),col="red") 
########
detach(BasinTMWB_JO)

CNnew = CNmodel(BasinData)


