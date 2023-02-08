# Get some knowledge of your work environment
# and do some organizing for future labs. 
# 1) Keep data out of your source project directory
#    a. Source 
#     b. if really desired, results
# 2) Figure out where your project directory is... so you can sync with Git
# 
Sys.getenv()
Sys.getenv("HOME")
myhomedir=Sys.getenv("HOME")
# Next depends on where you set up your git, and what you called it locally, 
# but when you start a new git project, it will be the first directory you 
# are placed in
getwd()
mygitdir=getwd()
# In the future you might want to search around for this
# mygitdir=paste0(myhomedir,"/2023/BSE5304Lab02")
# Mostly, we want to know where we are putting our homework PDFs
mypdfdir=paste0(mygitdir,"/pdfs")
# 
# Packages we think we are going to need today
# https://cran.r-project.org/web/packages/elevatr/elevatr.pdf
# https://cran.r-project.org/web/packages/raster/raster.pdf
# https://cran.r-project.org/web/packages/soilDB/soilDB.pdf
# https://cran.r-project.org/web/packages/rgdal/rgdal.pdf
#We use the R packages "elevatr", which provides access to digital elevation data from various sources including the USGS, “raster” which provides a variety of raster analysis functions, “SoilDB”, which accesses a Web Feature Service (WFS) (https://en.wikipedia.org/wiki/Web_Feature_Service) to access soil maps. 

#
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,dplyr,patchwork,rnoaa)
pacman::p_load(operators,topmodel,DEoptim,soilDB,sp,curl,httr,
                 rnoaa,raster,shapefiles,rgdal,elevatr,terra,progress,lubridate)
system("git config --global user.email 'cabaldwin2019@vt.edu ") 
system("git config --global user.name 'cabaldwin' ")

# We will explore this deeper when we get onto the VT ARC Next Week
# You do not want to, often can't actually, store large datasets in your 
# GitHub repository. This class is about repeatable modelling, not storing 
# large publicdatasets, which is a topic in of itself. 
datadir=paste0(myhomedir,"/data")
dir.create(datadir,recursive = T)
srcdir=paste0(myhomedir,"/src")
dir.create(srcdir,recursive = T)


setwd(srcdir)
system("svn checkout svn://scm.r-forge.r-project.org/svnroot/ecohydrology/"); 
install.packages(c("ecohydrology/pkg/EcoHydRology/"),repos = NULL)
pacman::p_load(EcoHydRology)

setwd(datadir)

#### USGS 01674500 MATTAPONI RIVER NEAR BEULAHVILLE, VA
myflowgage_id="01674500"
myflowgage=get_usgs_gage(myflowgage_id,begin_date = "2015-01-01",
                           end_date = "2019-01-01")

# For most watershed modelling purposes we normalize Q in mm/day for basins
myflowgage$flowdata$Qmm = myflowgage$flowdata$flow/myflowgage$area/10^3

# In the Lab01, we introduced you to a way to quickly get your WX Data 
# for any location in the world way easier than traditional download and
# parsing methods most old people use.
#
# Sorry, but there is even an easier way!
?FillMissWX()
WXData=FillMissWX(declat=myflowgage$declat, declon=myflowgage$declon,
                    StnRadius=30,minstns=10,date_min="2010-01-01",
                    date_max="2023-02-01",targElev=1,
                    method = "IDEW",alfa=2)

BasinData=merge(WXData,myflowgage$flowdata,by.x="date",by.y="mdate")
# A few constants
MinTempCol <- "#0000ff"
  MaxTempCol <- "#ff0000"
  PCol <- "#000000"
  QCol <- PCol

coeff=1
p1= ggplot(BasinData, aes(x=date)) +
  geom_line( aes(y=MaxTemp), linewidth=1, color=MaxTempCol) + 
  geom_line( aes(y=MinTemp), linewidth=1, color=MinTempCol) + 
  geom_line( aes(y=Qmm), linewidth=1, color=QCol) +
  scale_y_continuous(
    # Features of the first axis
    name = "Temp(C)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Depth(mm)")
  ) + 
  theme(
    axis.title.y = element_text(color = "black", size=13),
    axis.title.y.right = element_text(color = QCol, size=13)
  ) +
  ggtitle(myflowgage$gagename)


p1
basestr=format(Sys.time(),"/%Y%m%d%H%M")
filename=paste0(mypdfdir,basestr,"graph01.pdf")
pdf(filename) 
plot(p1)
dev.off()
print("file size")
print(file.size(filename))
print("I finished!")


# We can determine our UTM zone and build our proj4 projection string
trunc((180+myflowgage$declon)/6+1)
proj4_utm = paste0("+proj=utm +zone=", trunc((180+myflowgage$declon)/6+1), " +datum=WGS84 +units=m +no_defs")
print(proj4_utm)

# Lat/Lon (_ll) is much easier!
proj4_ll = "+proj=longlat"

# Now we will build our proj4strings which define our “Coordinate 
# Reference Systems” or CRS in future geographic manipulations. 
crs_ll=CRS(proj4_ll)
crs_utm=CRS(proj4_utm)
print(crs_ll)
print(crs_utm)
#
# Double check against Figure 1 to confirm we are in the correct UTM Zone.


myflowgage$area   # area in km2

# If the watershed was square, which it is not, the size would be the 
# square root of the area. Also, the gage/pour point is not in the center
# so we will search around the gage.
# Build sp point for USGS gage location, in both _ll and _utm
latlon <- cbind(myflowgage$declon,myflowgage$declat)
myflowgage$gagepoint_ll <- SpatialPoints(latlon)
proj4string(myflowgage$gagepoint_ll)=proj4_ll
myflowgage$gagepoint_utm=spTransform(myflowgage$gagepoint_ll,crs_utm)
# Open up maps.google.com to guesstimate area/lengths
url=paste0("https://www.google.com/maps/@",
             myflowgage$declat,",",myflowgage$declon,",18z")
browseURL(url)
# We are going to over estimate our area
sqrt(myflowgage$area)   # guestimating square watershed
# For our search we are going to multiply the area by 6 and
# to get the distance
sqrt(myflowgage$area*8)
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
                        z = 12, prj = proj4_utm,src ="aws",expand=1)
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

# FRAK
# https://runpee.com/frakking-for-fun-7-science-fiction-curse-words-to-learn-and-use/
zoomext=myflowgage$gagepoint_utm@coords
zoomext=rbind(zoomext,zoomext+res(mydem)*100)
zoomext=rbind(zoomext,zoomext-res(mydem)*100)
zoomext=SpatialPoints(zoomext,proj4string = crs_utm)  
zoom(mydem,ext=zoomext)
plot(bboxpts,add=T)
plot(pourpoint,add=T,col="red")

plot(bboxpts,add=T)
plot(pourpoint,add=T,col="red")


#cd ~/src/      # Set your directory to your home directory
#  git clone https://github.com/dtarb/TauDEM.git
#mkdir ~/src/TauDEM/bin
#cd ~/src/TauDEM/src
#make
# sed -i -e 's/MPI_Type_struct/MPI_Type_create_struct/g' linklib.h
# yes, this next line is very small font, but it is one line so...
# sed -i -e 's/MPI_Type_extent(MPI_LONG, \&extent)/MPI_Aint lb\;MPI_Type_get_extent(MPI_LONG, \&lb, \&extent)/g' linklib.h
# Now let's try make again!
# make


## cd ~/src/      # Set your directory to your home directory
# git clone https://github.com/dtarb/TauDEM.git
# mkdir ~/src/TauDEM/bin
# cd ~/src/TauDEM/src
# sed -i -e 's/MPI_Type_struct/MPI_Type_create_struct/g' linklib.h
## yes, this next line is very small font, but it is one line so...
# sed -i -e 's/MPI_Type_extent(MPI_LONG, \&extent)/MPI_Aint lb\;MPI_Type_get_extent(MPI_LONG, \&lb, \&extent)/g' linklib.h
# make

rm("old_path")
old_path <- Sys.getenv("PATH")
old_path
if(!grepl("~/src/TauDEM/bin",old_path)){
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
plot(fel)


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
zoom(log(ad8), ext = zoomext)
plot(pourpoint, add=T)

# Grid Network 
system("mpiexec -n 2 gridnet -p mydemp.tif -gord mydemgord.tif -plen mydemplen.tif -tlen mydemtlen.tif")
gord=raster("mydemgord.tif")
plot(gord)
zoom(gord, zoomext)

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
zoom(log(sca))


system("mpiexec -n 2 threshold -ssa mydemad8.tif -src mydemsrc.tif -thresh 100")
src=raster("mydemsrc.tif")
plot(src)
zoom(src)

outlet=SpatialPointsDataFrame(myflowgage$gagepoint_utm,
                                data.frame(Id=c(1),outlet=paste("outlet",1,sep="")))
writeOGR(outlet,dsn=".",layer="approxoutlets",
           driver="ESRI Shapefile", overwrite_layer=TRUE)

plot(pourpoint,add=T)


system("mpiexec -n 2 moveoutletstostrm -p mydemp.tif -src mydemsrc.tif -o approxoutlets.shp -om Outlet.shp")
approxpt=readOGR("approxoutlets.shp")
plot(approxpt,add=T, col = "blue")

plot(src)
points(outpt$shp[2],outpt$shp[3],pch=19,col=2)
points(approxpt$shp[2],approxpt$shp[3],pch=19,col=4)

zoom(src)


# Contributing area upstream of outlet
system("mpiexec -n 2 Aread8 -p mydemp.tif -o Outlet.shp -ad8 mydemssa.tif")
ssa=raster("mydemssa.tif")
plot(ssa) 


# Threshold
system("mpiexec -n 2 threshold -ssa mydemssa.tif -src mydemsrc1.tif -thresh 2000")
src1=raster("mydemsrc1.tif")
plot(src1)
zoom(src1)

# Stream Reach and Watershed
system("mpiexec -n 2 Streamnet -fel mydemfel.tif -p mydemp.tif -ad8 mydemad8.tif -src mydemsrc1.tif -o outlet.shp -ord mydemord.tif -tree mydemtree.txt -coord mydemcoord.txt -net mydemnet.shp -w mydemw.tif")
plot(raster("mydemord.tif"))
zoom(raster("mydemord.tif"))
plot(raster("mydemw.tif"))




