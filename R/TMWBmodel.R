TMWBmodel = function(TMWBdf,fcres=0.3,FldCap=.45,WiltPt=.15,Z=1000){

# Our TMWBdf Model

detach(TMWB)


TMWBdf$ET = TMWBdf$PET # in mm/day
TMWBdf$AWC=(0.45-0.15)*1000 #Fld Cap = .45, Wilt Pt = .15, z=1000mm
TMWBdf$dP = TMWBdf$P-TMWBdf$ET -TMWBdf$SNO + TMWBdf$SNOmlt 

attach(TMWBdf)# Remember to detach or it gets ugly
plot(date,Qmm,type = "l",col="black")
lines(date,P,type = "l",col="red")
lines(date,Qmm,type = "l",col="black") # We repeat to have Qmm on top of P
lines(date,ET,type = "l",col="blue")
legend("topright", c("P", "Qmm", "ET"), col = c("red", "black", "blue"),
       lty = 1:2, cex = 0.8)
detach(TMWBdf) # IMPORTANT TO DETACH


TMWBdf$AWC=(0.45-0.15)*1000 #Fld Cap = .45, Wilt Pt = .15, z=1000mm


TMWBdf$AW=NA  #Assigns all values in column with “NA” (Not available)
TMWBdf$AW[1]=250
TMWBdf$Excess=NA
TMWBdf$Excess[1]=0
head(TMWBdf)

# Here we go looping through our functions….

attach(TMWBdf)
for (t in 2:length(date)){
  if (dP[t]< 0) {  
    values<-soildrying(AW[t-1],dP[t],AWC[t])
  } else if (AW[t-1]+dP[t]>AWC[t]) {
    values<-soil_wetting_above_capacity(AW[t-1],dP[t],AWC[t])
  } else {
    values<-soilwetting (AW[t-1],dP[t],AWC[t])
  }
  AW[t]<-values[1]
  Excess[t]<-values[2]
}

detach(TMWBdf)
TMWBdf$AW <-AW
TMWBdf$Excess<-Excess
rm(list=c("AW","Excess"))

# Calculate Watershed Storage and River Discharge: 
TMWBdf$Qpred=NA
TMWBdf$Qpred[1]=0
TMWBdf$S=NA
TMWBdf$S[1]=0

attach(TMWBdf)
fcres=.3   # reservoir coefficient
for (t in 2:length(date)){
  S[t]=S[t-1]+Excess[t]     
  Qpred[t]=fcres*S[t]
  S[t]=S[t]-Qpred[t]
}
detach(TMWBdf) # IMPORTANT TO DETACH
TMWBdf$S=S
TMWBdf$Qpred=Qpred # UPDATE vector BEFORE DETACHING
rm(list=c("S","Qpred"))
View(TMWBdf)
dev.off()
plot(TMWBdf$date,TMWBdf$Qmm,col="black",ylab ="Qmm(mm)",xlab="date",type="l")
lines(TMWBdf$date,TMWBdf$Qpred,col="blue",type="l", 
      xlab = "", ylab = "")
legend("topright", c("Qmm(mm)", "Qpred(mm)"), col = c("black", "blue"),
       lty = 1:2, cex = 0.8)


TMWBdf$AWC=(FldCap-WiltPt)*Z # 
TMWBdf$dP = 0 # Initializing Net Precipitation
TMWBdf$ET = 0 # Initializing ET
TMWBdf$AW = 0 # Initializing AW
TMWBdf$Excess = 0 # Initializing Excess


# Loop to calculate AW and Excess
attach(TMWBdf)
for (t in 2:length(AW)){
  # This is where Net Precipitation is now calculated
  # Do you remember what Net Precip is? Refer to week 2 notes
  # Update this to reflect the ET model described above
  
  ET[t] = (AW[t-1]/AWC[t-1])*PET[t] # New Model
  dP[t] = P[t] - ET[t] + SNOmlt[t] - SNOfall[t] 
  # From here onward, everything is the same as Week2’s lab
  if (dP[t]<=0) {
    values<-soildrying(AW[t-1],dP[t],AWC[t])
  } else if((dP[t]>0) & (AW[t-1]+dP[t])<=AWC[t]) {
    values<-soilwetting(AW[t-1],dP[t],AWC[t])
  } else {
    values<-soil_wetting_above_capacity(AW[t-1],dP[t],AWC[t])
  }
  AW[t]<-values[1]
  Excess[t]<-values[2]
  print(t)
}
TMWBdf$AW=AW
TMWBdf$Excess=Excess
TMWBdf$dP=dP
TMWBdf$ET=ET
rm(list=c("AW","dP","ET", "Excess"))
detach(TMWBdf) # IMPORTANT TO DETACH

# Calculate Watershed Storage and River Discharge, S and Qpred, playing with the reservoir coefficient to try to get Qpred to best match Qmm

TMWBdf$Qpred=NA
TMWBdf$Qpred[1]=0
TMWBdf$S=NA
TMWBdf$S[1]=0
attach(TMWBdf)
fcres=.3
for (t in 2:length(date)){
  S[t]=S[t-1]+Excess[t]     
  Qpred[t]=fcres*S[t]
  S[t]=S[t]-Qpred[t]
}
TMWBdf$S=S
TMWBdf$Qpred=Qpred # UPDATE vector BEFORE DETACHING
detach(TMWBdf) # IMPORTANT TO DETACH
rm(list=c("Qpred","S"))

#Make a plot that has Qmm, P,and Qpred over time
plot(TMWBdf$date,P,col="black")
lines(TMWBdf$date,TMWBdf$Qmm,type = "l",col="red")
lines(TMWBdf$date,TMWBdf$Qpred,col="blue")
plot(TMWBdf$Qmm,TMWBdf$Qpred)
NSE=function(Yobs,Ysim){
  return(1-sum((Yobs-Ysim)^2,na.rm=TRUE)/sum((Yobs-mean(Yobs, na.rm=TRUE))^2, na.rm=TRUE))}
NSE(TMWBdf$Qmm,TMWBdf$Qpred)
return(TMWBdf)
}
