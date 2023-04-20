#
# Since everything depends on the libraries you install
# it is worthwhile loading them at the beginning
#
# A useful function to make sure you don't have anything residually attached
# from your last session.
 search()
# Or to see what might be attached
 intersect(search(), objects())
 objects()  # This will list the objects you have.
 rm(list=objects()) # Removes ALL the objects… so be careful here.

#
# What is going to change from use case to use case 
 Sys.getenv('Cabaldwin')
 LabNo="/Lab09"
#
# What needs to be loaded
#
 if (!require("pacman")) install.packages("pacman")
 myhomedir=Sys.getenv("HOME")
 datadir=paste0(myhomedir,"/data",LabNo)
 dir.create(datadir,recursive = T)
 srcdir=paste0(myhomedir,"/src")
 dir.create(srcdir,recursive = T)
# Setting the directory for where the GitHub project exists. 
# This depends on where you set up your git, and what you called it locally, 
# but when you start a new git project, it will be the first directory you 
# are placed in... or if later in the project:
# 
 mygitdir=rstudioapi::getActiveProject()
 mypdfdir=paste0(mygitdir,"/pdfs",LabNo)
 dir.create(mypdfdir)
# 
 setwd(mygitdir)
 system("git config --global user.email 'cabaldwin2019@vt.edu' ") 
 system("git config --global user.name 'Cabaldwin' ")
 system("git config pull.rebase false")

 if (!require("pacman")) install.packages("pacman")
 pacman::p_load(httr,EcoHydRology,curl,elevatr,raster,rgdal,
                 data.table,foreign,maptools,dataRetrieval,gdistance)
 setwd(datadir)
#
# Note we have a new library to access USGS Waterdata
# https://owi.usgs.gov/R/dataRetrieval.html
# https://owi.usgs.gov/R/training-curriculum/usgs-packages/dataRetrieval-readNWIS/
#
 ?dataRetrieval  # Review the man page for this package
 ?readNWISuv
 ?readNWISdv
 ?readNWISdata
 #
 # Need to figure out which data to download. 
 # https://nwis.waterdata.usgs.gov/nwis/pmcodes?radio_pm_search=param_group&pm_group=All+--+include+all+parameter+groups&pm_search=&casrn_search=&srsname_search=&format=html_table&show=parameter_group_nm&show=parameter_nm&show=casrn&show=srsname&show=parameter_units
 # 
  url="https://nwis.waterdata.usgs.gov/nwis/pmcodes?radio_pm_search=param_group&pm_group=All+--+include+all+parameter+groups&pm_search=&casrn_search=&srsname_search=&format=html_table&show=parameter_group_nm&show=parameter_nm&show=casrn&show=srsname&show=parameter_units"
  browseURL(url)
 #
 # Yeah, these databases are complex to get to know, remember our 
 # SSURGO?
 #
 # Before you begin your modeling project, confirm what your model outputs 
 # has a value to calibrate against, i.e. match parameter and units. For 
 # this lab we are looking for Gage Height, while historically, we have been 
 # looking at Discharge. NOT ALL PARAMETERS ARE AVAILABLE!
 #
  url="https://help.waterdata.usgs.gov/parameter_cd?group_cd=%"
  browseURL(url)
  View(parameterCdFile)
 
 ##############################################
 # USGS 01669520 DRAGON SWAMP AT MASCOT, VA
 ##############################################
 make_usgs_gage_list=function(
  siteNo = "01669520",
  parameterCd = c("00060","00065"),
  start.date = "2017-05-01",  # Not frozen to not frozen
  end.date = "2017-11-01"    # to still not frozen
  ){
    USGS=list()   # Organize the data in a nice list as in previous labs
    USGS[["flowdata"]]<- readNWISuv(siteNumbers = siteNo,parameterCd = parameterCd,startDate = start.date,endDate = end.date)
    #head(USGS$flowdata)  # Note that we have 00060 and 00065...
  
   # And of course we want to work in SI units so:
    USGS$flowdata$depth_m=USGS$flowdata$X_00065_00000*0.3048
   # m/ft depth
    USGS$flowdata$cms=USGS$flowdata$X_00060_00000*.02832
   # m3/ft3 flow
   #
   # Let's add in the USGS gage site information to the list and inspect
    USGS[["site"]]=readNWISsite(siteNo)
    #head(USGS$site)
    #class(USGS$site$dec_lat_va)
  
    USGS$site$man_n=.035/1.49
   #
   # Create a SpatialPointsDataFrame out of the site dataframe in the USGS list
    coordinates(USGS$site)=~dec_long_va+dec_lat_va
   #
  return(USGS)
  }
  
  
  USGS02042500=make_usgs_gage_list(siteNo = "02042500")
  USGS0205551460=make_usgs_gage_list(siteNo = "0205551460")
  
###  Seperate Watershed PAMUNKEY RIVER NEAR HANOVER, VA
  USGS01673000=make_usgs_gage_list(siteNo = "01673000")
  USGS01672500=make_usgs_gage_list(siteNo = "01672500")
  USGS01671100=make_usgs_gage_list(siteNo = "01671100")

   ab_ll=rbind(USGS01673000$site,
                USGS01672500$site,
                USGS01671100$site)
               
   class(ab_ll)
   ab_ll@proj4string
   proj4_utm = paste0("+proj=utm +zone=",
                       trunc((180+coordinates(USGS01673000$site)[1])/6+1), 
                       " +datum=WGS84 +units=m +no_defs")
   print(proj4_utm)
  # Lat/Lon (_ll) is much easier!
   proj4_ll = "+proj=longlat"
   crs_ll=CRS(proj4_ll)
   crs_utm=CRS(proj4_utm)
   proj4string(ab_ll)=proj4_ll
   ab_utm=spTransform(ab_ll,crs_utm)
   ab_utm@coords
   mydem=get_aws_terrain(locations=ab_utm@coords, 
                          z = 12, prj = proj4_utm,expand=1)
  #
  # Lets plot the DEM and the gage locations so we can guess 
  # what gages connect with what gages
  #
   plot(mydem)
   plot(ab_utm,add=T)
   text(ab_utm, labels=ab_utm@data$site_no, cex=0.6, font=2,pos=1)
  # Streams as USGS sees them, I know I can get an overview of streams with the 
  # USGS H
  url="https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU8/Shape/NHD_H_03010101_HU8_Shape.zip"
   curl_download(url,"NHD_H_03010101_HU8_Shape.zip")
   unzip("NHD_H_03010101_HU8_Shape.zip",exdir="03010101")
   streams=readOGR("03010101/Shape/NHDFlowline.dbf")
   streams_utm=spTransform(streams,crs_utm)
   plot(streams_utm,col="blue",add=T)
   
   ##
   # Assume in the inventory link that for this gage, our Gage height is missing. 
    head(USGS01673000$flowdata,2)  # Note that we have 00060 but missing 00065...
   #  agency_cd  site_no            dateTime X_00060_00000 X_00060_00000_cd tz_cd
   #1      USGS 02056000 2017-05-01 04:00:00           876                A   UTC
   #2      USGS 02056000 2017-05-01 04:15:00           876                A   UTC
   # Hrm? No depth associated with the flow? BUT USGS maintains rating curves
   # explain what a rating curve is: https://en.wikipedia.org/wiki/Rating_curve
   # and use the readNWISrating() function to grab it for this gage
    USGS01673000[["rating"]]=readNWISrating(USGS01673000$site$site_no)
    USGS01673000$flowdata$depth_m=USGS01673000$flowdata$X_00065_00000*0.3048
    # m/ft depth
    #
    plot(USGS01673000$rating$DEP,USGS01673000$rating$INDEP,xlab="Flow (cfs)",ylab="Gage Height (ft)", main="PAMUNKEY RIVER NEAR HANOVER, VA Flow-Head")
   #
   
   # Note that this is very similar to what we saw in the previous gage's results
   # and as it turns out, we can use it to estimate a 00065 measurement as 
   # we did for the previous gage.
  
    USGS01673000$flowdata$X_00065_00000=approx(USGS01673000$rating$DEP,
                                                USGS01673000$rating$INDEP, xout = USGS01673000$flowdata$X_00060_00000, ties = min)$y
    points(USGS01673000$flowdata$X_00060_00000,USGS01673000$flowdata$X_00065_00000,
            col="red", xlab = "Flow (cms)")
   #

   # m/ft depth
   #
   


   ### gage distance
   
    A=SpatialPoints(USGS01671100$site)# Up gradient site
    B=SpatialPoints(USGS01673000$site) # Down gradient site 
    proj4string(A)=proj4_ll
    proj4string(B)=proj4_ll
    A_utm=spTransform(A,crs_utm)
    B_utm=spTransform(B,crs_utm)
   # Cut the DEM down to a more manageable size
    cropmydem=crop(mydem,extend(extent(ab_utm),600))
    cropmydem=trim(cropmydem)
    cropmydem=cropmydem*1000.0
    plot(cropmydem)
    plot(ab_utm,add=T)
   # Set up the weighting functions
    altDiff <- function(x){x[2] - x[1]}
    hd <- transition(cropmydem, altDiff, 8, symm=FALSE)
    slope <- geoCorrection(hd)
    adj <- adjacent(cropmydem, cells=1:ncell(cropmydem), pairs=TRUE, directions=8)
    speed <- slope
    speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
    Conductance <- geoCorrection(speed)
   # Find and plot the flow path
    AtoB <- shortestPath(Conductance, A_utm, B_utm, output="SpatialLines")
    plot(AtoB,add=T)
    plot(streams_utm,col="blue",add=T)
    plot(AtoB,add=T)
    SpatialLinesLengths(AtoB)
    USGS01671100$site$L=SpatialLinesLengths(AtoB) # km to m
    USGS01671100$site$L # reach length in m
   #
   #
   # Getting slope, we will extract the slope for points A and B from the DEM and # divide the difference by the length in m, this gives us a much better 
   # estimate of slope than taking the point slopes at the gage site
   #
    USGS01671100$site$slope=(extract(mydem,A_utm)-
                                  extract(mydem,B_utm))/USGS01671100$site$L
    USGS01671100$site$slope
    
    ## BREAK IT
    
    # So now we have flow depth (y "$depth_m"), manning's n ("$man_n"), Q ("$cms"), and slope ("$slope") rearrange to solve for B
    # B=(n*Q)/(y^(5/3)*sqrt(So))
     USGS01671100$flowdata$B=(USGS01671100$site$man_n*
                                   USGS01671100$flowdata$cms)/(USGS01671100$flowdata$depth_m^(5/3)*
                                                                   sqrt(USGS01671100$site$slope))
     head(USGS01671100$flowdata)
    #  agency_cd	site_no        	dateTime X_00060_00000 X_00060_00000_cd
    #1  	USGS 05267000 2017-05-01 04:00:00      	6.38            	A
    #2  	USGS 05267000 2017-05-01 04:05:00      	6.38            	A
    #  X_00065_00000 X_00065_00000_cd tz_cd   	cms  depth_m    	B
    #1      	2.74            	A   UTC 0.1806816 0.835152 0.103032
    #2      	2.74            	A   UTC 0.1806816 0.835152 0.103032
    #
    # Lets look at how B changes with flow.    
     plot(USGS01671100$flowdata$dateTime,USGS01671100$flowdata$B, main="PAMUNKEY RIVER NEAR HANOVER, VA")
    # Does this seem reasonable (...like order of magnitude reasonable)? You can 
    # perform a quick and dirty check using google earth and measuring the channel 
    # width in a few places.
    #
     plot(USGS01671100$flowdata$cms,USGS01671100$flowdata$depth_m, main="PAMUNKEY RIVER NEAR HANOVER, VA Flow-Head",
          xlab = "Flow (CMS)", ylab = "Gage Depth (M)")
    
    
    # ck
     USGS01671100$flowdata$ck= 5/3*((sqrt(USGS01671100$site$slope))/USGS01671100$site$man_n)*USGS01671100$flowdata$depth_m^(2/3)
      # ANS
       mean(USGS01671100$flowdata$ck,na.rm=T)
    # [1] 2.547238 for this example, confirm this result
     USGS01671100$flowdata$dt = USGS01671100$site$L/USGS01671100$flowdata$ck
       mean(USGS01671100$flowdata$dt,na.rm=T)
    # [1] 6328.655  for this example, confirm this result
     plot(USGS01671100$flowdata$dateTime,USGS01671100$flowdata$dt, xlab = "dt", ylab ="Date", main = "dt vs Date")
     USGS01671100$flowdata$outTime=USGS01671100$flowdata$dateTime+
      USGS01671100$flowdata$dt
    
    # Find the beginning of  Waves assuming a new wave starts at 110% of prior 
    # flow. This might need to change for your homework
     WaveStartDecPercent=1.10
     USGS01671100$flowdata$newwave=
      USGS01671100$flowdata$cms *WaveStartDecPercent <
      data.table::shift(USGS01671100$flowdata$cms)
     summary(USGS01671100$flowdata$newwave)
    # Add plot of the point found
     len=length(USGS01671100$flowdata$newwave)
     USGS01671100$flowdata$newwave[is.na(USGS01671100$flowdata$newwave)]=F
    # Removes repeated finds by going through loop backwords
     for (i in seq(len,2)){
      print(i)
      if(USGS01671100$flowdata$newwave[i]==T &
         USGS01671100$flowdata$newwave[i-1]==T){
        USGS01671100$flowdata$newwave[i]=F
      }
    }
     plot(USGS01671100$flowdata$dateTime,USGS01671100$flowdata$cms,type="l")
     points(USGS01671100$flowdata$dateTime[USGS01671100$flowdata$newwave],
             USGS01671100$flowdata$cms[USGS01671100$flowdata$newwave],col=2)
    
    # Find the time locations where waves begin
     which(USGS01671100$flowdata$newwave == TRUE)
     plot(USGS01671100$flowdata$dateTime,USGS01671100$flowdata$cms,xlab = "Date",ylab = "Flow (cms)", main = "Flow vs Date",
           type="l",xlim=c(USGS01671100$flowdata$dateTime[1109],
                           USGS01671100$flowdata$dateTime[1109+200]))
     lines(USGS01671100$flowdata$outTime,USGS01671100$flowdata$cms,col=2)
    
  
  
  
  