#
 url="https://raw.githubusercontent.com/vtdrfuka/BSE5304Labs/main/R/UglyLab10.R"
 download.file(url,"UglyLab10.R")
 file.edit("UglyLab10.R")
# Gonna need to change this I am sure… but
 LabNo="/Lab11"
# Wait… this really needs some cleanup.
 # Cleaning up
 search()
 objects()  # This will list the objects you have.
 rm(list=objects()) # Removes ALL the objects… so be careful here.
 #
 # What is going to change from use case to use case 
 LabNo="/Lab10"
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
 system("git config --global user.name 'cabaldwin' ")
 system("git config pull.rebase false")
 
 if (!require("pacman")) install.packages("pacman")
 #
 # WAIT! I do that every freakn time! Let's build SetMeUp.R 
 #
 url="https://raw.githubusercontent.com/vtdrfuka/BSE5304Labs/main/R/Lab04.R"
 download.file(url,"Lab04_10.R")
 file.edit("Lab04_10.R")
 
 # Start of Lab 8
 #
 # Note that in the rasterize command, the TIC raster layer is being used as 
 # a reference raster. Any of the matching UTM raster layers could be used in 
 # its place.
 # First, remember we are in a transition between spatial libraries
 # and need to convert from SpatVect to sp, sf, raster, etc. until 
 # the geostats world settles down at the end of the year
 #
 mysoil_utm <- as(ssurgo.geom_utm_crop, "Spatial")
 # Rasterizing for categorical analysis! 
 rmysoil_utm=rasterize(mysoil_utm,TIC,field=as.numeric(mysoil_utm$mukey))
 unique(rmysoil_utm)
 pacman::p_load(circlize)
 plot(rmysoil_utm,col=rand_color(length(unique(values(rmysoil_utm)))))
 unique(rmysoil_utm)
 #
 # Create and write a hydrologic response unit (HRU) table using the 
 # "ratify" function to combine our raster layers and build a "Raster 
 # Attribute Table" (RAT, great acronym right?).
 #
 # In watershed modeling, it is common to break up the watershed into areas 
 # with like characteristics. These are referred to as HRUs, we talked about 
 # them in class. We are familiar with the map of the TI Classes, 
 # where for example, we break up the watershed into 3 different groups of
 # like topographic characteristics. If that area has 3 different soils types
 # you can estimate that the 3 soils are distributed through the 3 TI Classes
 # giving at most 9 (3*3) areas (HRUs). 
 
 
 
 #
 # Traditional raster layers only support numeric values. 
 # The RAT allows a dataframe to be associated with the
 # raster values. We build unique "Hydrological Response Units" (HRUs)
 # by concatenating Map Unit Keys with the the TIClasses (1-2 digits)
 # with the slope*100 (rise over run) (between 1 and 999)
 #
 ratify( TIC*10^9)
 
 mybasinslp=mask(crop(slp,rmysoil_utm),rmysoil_utm)
 unique(ratify(round(mybasinslp*10+1)))
 
 unique(ratify((rmysoil_utm*10^3)))
 
 #
 # Now build an HRU table with the combination of the 1) raster Soils, 2) TIC,
 # and 3) slope layers. 
 #
 hru=ratify(TIC*10^9 + (rmysoil_utm*10^3) + round(mybasinslp*10+1))
 unique(values(hru))
 length(unique(values(hru)))
 
 plot(hru,col=rand_color(length(unique(values(hru)))))
 # Think of how you will color this plot based on the sediment runoff you will
 # calculate later.
 #
 # Build an HRU attribute table
 hru_table = levels(hru)[[1]]
 origID = hru_table$ID # preserve data order for later reordering
 # metadata parameters from a string... this will make more sense
 # after the next "head()" command
 hru_table$TIclass = as.numeric(substr(sprintf("%10.0f", hru_table$ID), 1,1))
 hru_table$mukey = as.numeric(substr(sprintf("%10.0f", hru_table$ID), 2,7))
 hru_table$slp = (as.numeric(substr(sprintf("%10.0f", 
                                            hru_table$ID), 8,10))-1)/10
 #
 # Calculate the area for each unique soil (mukey) X TIClass combination
 # using res(raster) for x and y resolution in units of m
 # Note that table() function returns the count of the occurrences of
 # unique values in the hru raster cells.
 hru_table$areaSQKM = as.vector(round(res(hru)[1]*res(hru)[2]*
                                        table(values(hru))/10^6, 3))
 #
 # To better understand what happened, look at the new hru_table
 head(hru_table)
 summary(hru_table)
 #
 # Read this USDA link:
 # https://sdmdataaccess.sc.egov.usda.gov/documents/TableColumnDescriptionsReport.pdf
 # http://www.nrcs.usda.gov/wps/PA_NRCSConsumption/download?cid=stelprdb1241115&ext=pdf
 # Note that variable names with _h, _r, and _l indicate high,
 # representative, and low values for the range of the variable
 # or parameter in the SSURGO database.
 #
 # Second associate cokey with ksat_r,awc_r,hzdepb_r from chorizon (remember we 
 # have done all this in previous labs
 #
 # mukey_statement = format_SQL_in_statement(unique(hru_table$mukey))
 # q_mu2co = paste("SELECT mukey,cokey FROM component WHERE mukey IN ", mukey_statement, sep="")
 # mu2co = SDA_query(q_mu2co)
 rm("mu2co")
 for (mymukey in unique(hru_table$mukey)){
   print(mymukey)
   mukey_statement = format_SQL_in_statement(mymukey)
   q_mu2co = paste("SELECT mukey,cokey FROM component 
           WHERE mukey IN ", mukey_statement, sep="")
   if(!exists("mu2co")){
     mu2co=SDA_query(q_mu2co)} 
   else{
     mu2co=rbind(mu2co,SDA_query(q_mu2co))
   } 
 }
 View(mu2co)
 # Second associate cokey with ksat_r,awc_r,hzdepb_r from chorizon
 # cokey_statement = format_SQL_in_statement(unique(mu2co$cokey))
 # q_co2ch = paste("SELECT cokey,ksat_r,awc_r,hzdepb_r,frag3to10_r  
 # FROM chorizon WHERE cokey IN ", cokey_statement, sep="")
 # co2ch = SDA_query(q_co2ch)
 rm("co2ch")
 for (mycokey in unique(mu2co$cokey)){
   print(mycokey)
   cokey_statement = format_SQL_in_statement(mycokey)
   q_co2ch = paste("SELECT cokey,ksat_r,awc_r,hzdepb_r,frag3to10_r FROM chorizon WHERE cokey IN ", cokey_statement, sep="")
   print(q_co2ch)
   if(!exists("co2ch")){
     co2ch=SDA_query(q_co2ch)
   } else{
     try((co2ch=rbind(co2ch,SDA_query(q_co2ch))))
   } 
 }
 View(co2ch)
 rm("co2co")
 for (mycokey in unique(mu2co$cokey)){
   print(mycokey)
   cokey_statement = format_SQL_in_statement(mycokey)
   q_co2co = paste("SELECT cokey,slopelenusle_r FROM component WHERE cokey IN ", cokey_statement, sep="")
   print(q_co2co)
   if(!exists("co2co")){
     co2co=SDA_query(q_co2co)} 
   else{
     try((co2co=rbind(co2co,SDA_query(q_co2co))))} 
 }
 View(co2co)
 # Last, bring them back together, and aggregate based on max values
 # of ksat_r,awc_r, and hzdepb_r
 mu2ch=merge(mu2co,co2ch)
 mu2ch=merge(mu2ch,co2co)
 View(mu2ch)
 #
 # Complete the table for running the MUSLE (Modified
 # Universal Soil Loss Equation) to determine daily sediment
 # loss, SWAT Theory eq. 4:1.1.1 . Assume:
 # Kusle=.28
 # Cusle=.2
 # Pusle=.5
 #
 # Merge and then aggregate spatially derived hru_table with
 # SSURGO summary table
 MUSLE_mrg=merge(hru_table,mu2ch)   
 MUSLE_mrg$ksat_r=as.numeric(MUSLE_mrg$ksat_r)
 MUSLE_mrg$awc_r=as.numeric(MUSLE_mrg$awc_r)
 MUSLE_mrg$hzdepb_r=as.numeric(MUSLE_mrg$hzdepb_r)
 MUSLE_mrg$slopelenusle_r=as.numeric(MUSLE_mrg$slopelenusle_r)
 MUSLE_mrg$frag3to10_r=as.numeric(MUSLE_mrg$frag3to10_r)
 MUSLE=aggregate(MUSLE_mrg,list(MUSLE_mrg$TIclass),mean,na.rm=T)
 #
 # Easiest first! Eq. 4:1.1.15 Course Fragment Factor
 MUSLE$CFRG=exp(-0.053*MUSLE$frag3to10_r)
 MUSLE
 #
 # LSusle is calculated using eq. 4.1.12
 MUSLE$alpha=atan(MUSLE$slp/100)
 MUSLE$LSm=.6*(1-exp(-35.835*MUSLE$slp/100))
 MUSLE$LS=(MUSLE$slopelenusle_r/22.1)^MUSLE$LSm * (65.41*sin(MUSLE$alpha)^2+4.56*sin(MUSLE$alpha)+0.065)
 #
 # Pusle
 MUSLE$Pusle=.50
 #
 # Cusle
 MUSLE$Cusle=.20
 #
 # Kusle
 MUSLE$Kusle=0.28
 #
 # Build a constant for those we are not changing day to day
 attach(MUSLE)
 MUSLE$KCPLSCFRG118=11.8*Kusle*Cusle*Pusle*LS*CFRG
 detach(MUSLE)
 MUSLE # Make sure values look correct, Pusle, Cusle, Kusle
 #
 # Now we need to use each of the TIClass Q solutions from Lab06 to calculate
 # peak flows (qpeak) and complete the MUSLE Sediment Loss for each class.
 # Run Model
 #
 # Now we need to use Q solutions from Lab06 to calculate
 # peak flows (qpeak) and complete the MUSLE Sediment Loss for each class.
 # Run Model
 #source CNmodel function
 source("https://raw.githubusercontent.com/vtdrfuka/BSE5304_2022/main/functions/CNmodel")
 pacman::p_load(data.table)
 # We will split into 5 VSA areas represented by 5 TI Classes
 nTIclass=5
 VSAsol=data.table(TIClass=seq(from=nTIclass,to=1),
                   As=seq(1:nTIclass)*(1/nTIclass),Wetfrac=(1/nTIclass))
 VSAsol[,sSratio:=2*(sqrt(1-shift(As))-sqrt(1-As))/Wetfrac-1]
 #
 VSAsol$sSratio[1]=2*(sqrt(1-0)-sqrt(1-VSAsol$As[1]))/VSAsol$Wetfrac[1]-1
 # Calculate TI Class localized sigma and Curve Number
 VSAsol[,sigma:=347*sSratio]
 VSAsol[,CN:=25400/(sigma+254)]
 VSAsol
 
 VSAParams=merge(VSAsol,MUSLE,by.x="TIClass",by.y="TIclass")
 View(VSAParams)
 
 TIC01=TMWB
 TIC02=TMWB
 TIC03=TMWB
 TIC04=TMWB
 TIC05=TMWB
 # For TIC01 CNavg=VSAParams$CN[1] but confirm
 TIC01 = CNmodel(CNmodeldf = TIC01, CNavg=VSAParams$CN[1], 
                 declat=myflowgage$declat,declon=myflowgage$declon)
 TIC01$qpeak=TIC01$Qpred/3600/24/1000*myflowgage$area/nTIclass*10^6 #m^3/sec
 
 TIC01$sed=(TIC01$Qpred*TIC01$qpeak*71.424/nTIclass*100)^.56*MUSLE$KCPLSCFRG118[1]    # Eq. 4:1.1.1 SWAT Theory
 plot(TIC01$sed, xlab="Sediment loss", ylab="Time", main="Sediment Loss Over Time TI 1") 
 ##
 
 TIC02 = CNmodel(CNmodeldf = TIC02, CNavg=VSAParams$CN[2], 
                 declat=myflowgage$declat,declon=myflowgage$declon)
 TIC02$qpeak=TIC02$Qpred/3600/24/1000*myflowgage$area/nTIclass*10^6 #m^3/sec
 
 TIC02$sed=(TIC02$Qpred*TIC02$qpeak*71.424/nTIclass*100)^.56*MUSLE$KCPLSCFRG118[2]    # Eq. 4:1.1.1 SWAT Theory
 plot(TIC02$sed, xlab="Sediment loss", ylab="Time", main="Sediment Loss Over Time TI 2")  
 ##
 
 TIC03 = CNmodel(CNmodeldf = TIC03, CNavg=VSAParams$CN[3], 
                 declat=myflowgage$declat,declon=myflowgage$declon)
 TIC03$qpeak=TIC03$Qpred/3600/24/1000*myflowgage$area/nTIclass*10^6 #m^3/sec
 
 TIC03$sed=(TIC03$Qpred*TIC03$qpeak*71.424/nTIclass*100)^.56*MUSLE$KCPLSCFRG118[3]    # Eq. 4:1.1.1 SWAT Theory
 plot(TIC03$sed, xlab="Sediment loss", ylab="Time", main="Sediment Loss Over Time TI 3")  
 ##
 
 TIC04 = CNmodel(CNmodeldf = TIC04, CNavg=VSAParams$CN[4], 
                 declat=myflowgage$declat,declon=myflowgage$declon)
 TIC04$qpeak=TIC04$Qpred/3600/24/1000*myflowgage$area/nTIclass*10^6 #m^3/sec
 
 TIC04$sed=(TIC04$Qpred*TIC04$qpeak*71.424/nTIclass*100)^.56*MUSLE$KCPLSCFRG118[4]    # Eq. 4:1.1.1 SWAT Theory
 plot(TIC04$sed, xlab="Sediment loss", ylab="Time", main="Sediment Loss Over Time TI 4") 
 ##
 
 TIC05 = CNmodel(CNmodeldf = TIC05, CNavg=VSAParams$CN[5], 
                 declat=myflowgage$declat,declon=myflowgage$declon)
 TIC05$qpeak=TIC05$Qpred/3600/24/1000*myflowgage$area/nTIclass*10^6 #m^3/sec
 
 TIC05$sed=(TIC05$Qpred*TIC01$qpeak*71.424/nTIclass*100)^.56*MUSLE$KCPLSCFRG118[5]    # Eq. 4:1.1.1 SWAT Theory
 plot(TIC05$sed, xlab="Sediment loss", ylab="Time", main="Sediment Loss Over Time TI 5")
# Note we have the solutions from last week's lab in objects TIC05,
# TIC04, etc. 
ls(pattern = "TIC*")


# 
# Build a dataframe for your DP Load model in TI Class 05,(you will need to do 
#this 5 times for TIC 1-4 where you will need the date, Qpred, and Average 
# Temperature
DPTI05=data.frame(date=TIC05$date,
                  Rt=TIC05$Qpred/1000, 
                  Tavg=(TIC05$MaxTemp+TIC05$MinTemp)/2)
# Take it to volumetric water content rather # than volumetric soil content, 
# should be in m of water
tau=2  # days
dt=1     # days time step
kF=.015  # Table 2
# Initialize MF and DF
DPTI05$MF=0
DPTI05$DF=0
# Spread your P Fertilizer on ~May 1, ~August 1, and ~October 1
DPTI05$MF[(format(DPTI05$date,"%j") %in% c(121,213,274))]=5.4*10^-4
# Remember what we say about attaching! 
attach(DPTI05)
#
# Loop to solve MF and DF
for (i in 2:length(date)){
  if(MF[i]<=MF[i-1]){
    MF[i]=MF[i-1]*exp(-dt/tau)-DF[i-1]
  }
  DF[i]=MF[i]*(kF*MF[i]*Rt[i]/(1+kF*MF[i]*Rt[i]))
}
DPTI05$MF=MF
DPTI05$DF=DF
detach(DPTI05)
rm(list=c("MF","DF")) # Clean up the environment 
dev.off() #reset graphics device
plot(DPTI05$date,DPTI05$MF)
plot(DPTI05$date,DPTI05$DF)

# Calculate your Export Coef from Easton et al 2007 Figure 2 using TI Class 5
# and note that the bold gives the TI Class. This figure gives a range of 
# 0 - 520 micrograms/litre 
# For TIC=5
muTS_TI05=(((520-0)/5)*VSAsol$TIClass[1]+0) # use VSAsol$TIClass table to 
# assign TIC to calc (remember TIC05 is in location 1 in the VSAsol$TIClass 
# table
# Setting range of Soil P values (MS) using the range given on page 7 of 
# Easton et al. 2007 (3.7-18.5mg/kg), then 
# Moore 1993… assume soil density is 2000kg/m^3 of soil
MS_TI05=(((18.5-3.7)/5)*VSAsol$TIClass[1]+3.7)*2000  # range from Easton et 
# al. 2007, pg 7. Moore suggests a linear relationship
# We will take care of all of TIClass 05 now as will so 
# it makes sense when you repeat for TI Classes 1-4
# You will use muTS_TI01 thru muTS_TI04 to finish this lab
#
QS= 3.0 # A guess using the middle of the range 1-5
TR=20   # reference Temperature from Table 2.
DPTI05$muS= muTS_TI05*QS^((DPTI05$Tavg-TR)/10)  # Eq. 5
DPTI05$DS=(DPTI05$muS*MS_TI05*DPTI05$Rt)/10^6          # Eq. 4
plot(DPTI05$date,DPTI05$DS)
# We will assume a simple base flow model where the stream baseflow 
# B equals the minimum measured flow of the basin. We will build the 
# Base Flow solution directly into the Stream Load dataframe (DPLT).
# Note that we only consider TIC05 here because TIC05 is the class that 
# directly contributes to the stream.
DPLT=data.frame(date=TIC05$date,
                Rt=TIC05$Qpred/1000.0,
                Tavg=(TIC05$MaxTemp+TIC05$MinTemp)/2)
DPLT$B=min(TIC05$Qmm)*myflowgage$area*1000*1000/1000 # m^3/day
muTB=2.1*10^(-5) # Easton Table 2
QB=2.2           # Easton Table 2
TB=17            # Easton Table 2
DPLT$muB=muTB*QB^((DPLT$Tavg-TB)/10)  # Easton eq. 10
DPLT$LB=DPLT$muB*DPLT$B     # Easton eq. 9
plot(DPLT$date,DPLT$LB)

# Temperature
DPTI04=data.frame(date=TIC04$date,
                  Rt=TIC04$Qpred/1000, 
                  Tavg=(TIC04$MaxTemp+TIC04$MinTemp)/2)
tau=2  # days
dt=1     # days time step
kF=.015  # Table 2
# Initialize MF and DF
DPTI04$MF=0
DPTI04$DF=0
# Spread your P Fertilizer on ~May 1, ~August 1, and ~October 1
DPTI04$MF[(format(DPTI04$date,"%j") %in% c(121,213,274))]=5.4*10^-4
# Remember what we say about attaching! 
attach(DPTI04)
#
# Loop to solve MF and DF
for (i in 2:length(date)){
  if(MF[i]<=MF[i-1]){
    MF[i]=MF[i-1]*exp(-dt/tau)-DF[i-1]
  }
  DF[i]=MF[i]*(kF*MF[i]*Rt[i]/(1+kF*MF[i]*Rt[i]))
}
DPTI04$MF=MF
DPTI04$DF=DF
detach(DPTI04)
rm(list=c("MF","DF")) # Clean up the environment 
dev.off() #reset graphics device
plot(DPTI04$date,DPTI04$MF)
plot(DPTI04$date,DPTI04$DF)


muTS_TI04=(((520-0)/5)*VSAsol$TIClass[1]+0) # use VSAsol$TIClass table to 

MS_TI04=(((18.5-3.7)/5)*VSAsol$TIClass[1]+3.7)*2000  # range from Easton et 

QS= 3.0 # A guess using the middle of the range 1-5
TR=20   # reference Temperature from Table 2.
DPTI04$muS= muTS_TI04*QS^((DPTI04$Tavg-TR)/10)  # Eq. 5
DPTI04$DS=(DPTI04$muS*MS_TI04*DPTI04$Rt)/10^6          # Eq. 4
plot(DPTI04$date,DPTI04$DS)

DPLT=data.frame(date=TIC04$date,
                Rt=TIC04$Qpred/1000.0,
                Tavg=(TIC04$MaxTemp+TIC04$MinTemp)/2)
DPLT$B=min(TIC04$Qmm)*myflowgage$area*1000*1000/1000 # m^3/day
muTB=2.1*10^(-5) # Easton Table 2
QB=2.2           # Easton Table 2
TB=17            # Easton Table 2
DPLT$muB=muTB*QB^((DPLT$Tavg-TB)/10)  # Easton eq. 10
DPLT$LB=DPLT$muB*DPLT$B     # Easton eq. 9
plot(DPLT$date,DPLT$LB)

### 3 
DPTI03=data.frame(date=TIC03$date,
                  Rt=TIC03$Qpred/1000, 
                  Tavg=(TIC03$MaxTemp+TIC03$MinTemp)/2)
tau=2  # days
dt=1     # days time step
kF=.015  # Table 2
# Initialize MF and DF
DPTI03$MF=0
DPTI03$DF=0
# Spread your P Fertilizer on ~May 1, ~August 1, and ~October 1
DPTI03$MF[(format(DPTI03$date,"%j") %in% c(121,213,274))]=5.4*10^-4
# Remember what we say about attaching! 
attach(DPTI03)
#
# Loop to solve MF and DF
for (i in 2:length(date)){
  if(MF[i]<=MF[i-1]){
    MF[i]=MF[i-1]*exp(-dt/tau)-DF[i-1]
  }
  DF[i]=MF[i]*(kF*MF[i]*Rt[i]/(1+kF*MF[i]*Rt[i]))
}
DPTI03$MF=MF
DPTI03$DF=DF
detach(DPTI03)
rm(list=c("MF","DF")) # Clean up the environment 
dev.off() #reset graphics device
plot(DPTI03$date,DPTI03$MF)
plot(DPTI03$date,DPTI03$DF)


muTS_TI03=(((520-0)/5)*VSAsol$TIClass[1]+0) # use VSAsol$TIClass table to 

MS_TI03=(((18.5-3.7)/5)*VSAsol$TIClass[1]+3.7)*2000  # range from Easton et 

QS= 3.0 # A guess using the middle of the range 1-5
TR=20   # reference Temperature from Table 2.
DPTI03$muS= muTS_TI03*QS^((DPTI03$Tavg-TR)/10)  # Eq. 5
DPTI03$DS=(DPTI03$muS*MS_TI03*DPTI03$Rt)/10^6          # Eq. 4
plot(DPTI03$date,DPTI03$DS)

DPLT=data.frame(date=TIC03$date,
                Rt=TIC03$Qpred/1000.0,
                Tavg=(TIC03$MaxTemp+TIC03$MinTemp)/2)
DPLT$B=min(TIC03$Qmm)*myflowgage$area*1000*1000/1000 # m^3/day
muTB=2.1*10^(-5) # Easton Table 2
QB=2.2           # Easton Table 2
TB=17            # Easton Table 2
DPLT$muB=muTB*QB^((DPLT$Tavg-TB)/10)  # Easton eq. 10
DPLT$LB=DPLT$muB*DPLT$B     # Easton eq. 9
plot(DPLT$date,DPLT$LB)

### 2 
DPTI02=data.frame(date=TIC02$date,
                  Rt=TIC02$Qpred/1000, 
                  Tavg=(TIC02$MaxTemp+TIC02$MinTemp)/2)
tau=2  # days
dt=1     # days time step
kF=.015  # Table 2
# Initialize MF and DF
DPTI02$MF=0
DPTI02$DF=0
# Spread your P Fertilizer on ~May 1, ~August 1, and ~October 1
DPTI02$MF[(format(DPTI02$date,"%j") %in% c(121,213,274))]=5.4*10^-4
# Remember what we say about attaching! 
attach(DPTI02)
#
# Loop to solve MF and DF
for (i in 2:length(date)){
  if(MF[i]<=MF[i-1]){
    MF[i]=MF[i-1]*exp(-dt/tau)-DF[i-1]
  }
  DF[i]=MF[i]*(kF*MF[i]*Rt[i]/(1+kF*MF[i]*Rt[i]))
}
DPTI02$MF=MF
DPTI02$DF=DF
detach(DPTI02)
rm(list=c("MF","DF")) # Clean up the environment 
dev.off() #reset graphics device
plot(DPTI02$date,DPTI02$MF)
plot(DPTI02$date,DPTI02$DF)


muTS_TI02=(((520-0)/5)*VSAsol$TIClass[1]+0) # use VSAsol$TIClass table to 

MS_TI02=(((18.5-3.7)/5)*VSAsol$TIClass[1]+3.7)*2000  # range from Easton et 

QS= 3.0 # A guess using the middle of the range 1-5
TR=20   # reference Temperature from Table 2.
DPTI02$muS= muTS_TI02*QS^((DPTI02$Tavg-TR)/10)  # Eq. 5
DPTI02$DS=(DPTI02$muS*MS_TI02*DPTI02$Rt)/10^6          # Eq. 4
plot(DPTI02$date,DPTI02$DS, xlab = "Year", ylab = "Phosphurus Loss")

DPLT=data.frame(date=TIC02$date,
                Rt=TIC02$Qpred/1000.0,
                Tavg=(TIC02$MaxTemp+TIC02$MinTemp)/2)
DPLT$B=min(TIC02$Qmm)*myflowgage$area*1000*1000/1000 # m^3/day
muTB=2.1*10^(-5) # Easton Table 2
QB=2.2           # Easton Table 2
TB=17            # Easton Table 2
DPLT$muB=muTB*QB^((DPLT$Tavg-TB)/10)  # Easton eq. 10
DPLT$LB=DPLT$muB*DPLT$B     # Easton eq. 9
plot(DPLT$date,DPLT$LB)

### 1 
DPTI01=data.frame(date=TIC01$date,
                  Rt=TIC01$Qpred/1000, 
                  Tavg=(TIC01$MaxTemp+TIC01$MinTemp)/2)
tau=2  # days
dt=1     # days time step
kF=.015  # Table 2
# Initialize MF and DF
DPTI01$MF=0
DPTI01$DF=0
# Spread your P Fertilizer on ~May 1, ~August 1, and ~October 1
DPTI01$MF[(format(DPTI01$date,"%j") %in% c(121,213,274))]=5.4*10^-4
# Remember what we say about attaching! 
attach(DPTI01)
#
# Loop to solve MF and DF
for (i in 2:length(date)){
  if(MF[i]<=MF[i-1]){
    MF[i]=MF[i-1]*exp(-dt/tau)-DF[i-1]
  }
  DF[i]=MF[i]*(kF*MF[i]*Rt[i]/(1+kF*MF[i]*Rt[i]))
}
DPTI01$MF=MF
DPTI01$DF=DF
detach(DPTI01)
rm(list=c("MF","DF")) # Clean up the environment 
dev.off() #reset graphics device
plot(DPTI01$date,DPTI01$MF)
plot(DPTI01$date,DPTI01$DF)


muTS_TI01=(((520-0)/5)*VSAsol$TIClass[1]+0) # use VSAsol$TIClass table to 

MS_TI01=(((18.5-3.7)/5)*VSAsol$TIClass[1]+3.7)*2000  # range from Easton et 

QS= 3.0 # A guess using the middle of the range 1-5
TR=20   # reference Temperature from Table 2.
DPTI01$muS= muTS_TI01*QS^((DPTI01$Tavg-TR)/10)  # Eq. 5
DPTI01$DS=(DPTI01$muS*MS_TI01*DPTI01$Rt)/10^6          # Eq. 4
plot(DPTI01$date,DPTI01$DS)

DPLT=data.frame(date=TIC01$date,
                Rt=TIC01$Qpred/1000.0,
                Tavg=(TIC01$MaxTemp+TIC01$MinTemp)/2)
DPLT$B=min(TIC01$Qmm)*myflowgage$area*1000*1000/1000 # m^3/day
muTB=2.1*10^(-5) # Easton Table 2
QB=2.2           # Easton Table 2
TB=17            # Easton Table 2
DPLT$muB=muTB*QB^((DPLT$Tavg-TB)/10)  # Easton eq. 10
DPLT$LB=DPLT$muB*DPLT$B     # Easton eq. 9
plot(DPLT$date,DPLT$LB)

TIC05$area=myflowgage$area/5
TIC04$area=myflowgage$area/4
TIC03$area=myflowgage$area/3
TIC02$area=myflowgage$area/2
TIC01$area=myflowgage$area/1
DPLT$LT=DPLT$LB +
  
  TIC05$area*(DPTI05$DF + DPTI05$DS)+
  TIC04$area*(DPTI04$DF + DPTI04$DS)+
  TIC03$area*(DPTI03$DF + DPTI03$DS)+
  TIC02$area*(DPTI02$DF + DPTI02$DS)+
  TIC01$area*(DPTI01$DF + DPTI01$DS)
plot(DPLT$date,DPLT$LT, type="l",ylim=c(1,10), ylab = "Phosphorous Loss", xlab = "Year", main = "Stream Loss")

