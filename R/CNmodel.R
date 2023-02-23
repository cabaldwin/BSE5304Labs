CNmodel = function(BasinData,CNavg = 254,IaFrac = 0.05,fnc_slope=0,fnc_aspect=0,func_DAWC=.3,func_z=1000,fnc_fcres=.3){
  
  # 
  #CNModel
  #
  
  
  
  # Energy Balance based Snow Accumulation 
  # and Melt model from the EcoHydRology package.
  attach(CNmodeldf)
  SNO_Energy=SnowMelt(date, P, MaxTemp-3, MinTemp-3, myflowgage$declat, 
                      slope = fnc_slope, aspect = fnc_aspect, tempHt = 1, 
                      windHt = 2, groundAlbedo = 0.25,SurfEmissiv = 0.95, windSp = 2, 
                      forest = 0, startingSnowDepth_m = 0,startingSnowDensity_kg_m3=450)
  # We will update the -3 in the above to be a lapse rate adjustment
  detach(CNmodeldf)
  CNmodeldf$SNO=SNO_Energy$SnowWaterEq_mm
  CNmodeldf$SNOmlt=SNO_Energy$SnowMelt_mm
  CNmodeldf$SnowfallWatEq_mm=SNO_Energy$SnowfallWatEq_mm
  CNmodeldf$SnowMelt_mm=SNO_Energy$SnowMelt_mm
  attach(CNmodeldf)
  CNmodeldf$Albedo=.23
  CNmodeldf$Albedo[CNmodeldf$SNO>0]=.95
  PET=PET_fromTemp(Jday=(1+as.POSIXlt(date)$yday),
                   Tmax_C = MaxTemp,Tmin_C = MinTemp,
                   lat_radians = myflowgage$declat*pi/180) * 1000
  CNmodeldf$PET=PET
  detach(CNmodeldf)
  rm(list="PET")
  
  CNmodeldf$AWC=func_DAWC*func_z
  # Oh, this we want to vary some of these around our watershed!
  CNmodeldf$dP = 0 # Initializing Net Precipitation
  CNmodeldf$ET = 0 # Initializing ET
  CNmodeldf$AW = 0 # Initializing AW
  CNmodeldf$Excess = 0 # Initializing Excess
  CNmodeldf$S =0 # Initializing S
  CNmodeldf$Qpred=0 # Initializing Qpred
  attach(CNmodeldf)
  SSCNavg=(1000/CNavg-10)*25.4
  SSCN=SoilStorage(S_avg=SSCNavg, field_capacity=func_DAWC*.9,
                   soil_water_content=0.1*func_DAWC, porosity=func_DAWC)
  Ia_init=IaFrac*SSCN   
  CNmodeldf$CNavg = CNavg
  CNmodeldf$SSCNavg = SSCNavg
  CNmodeldf$SSCN = SSCN
  detach(CNmodeldf)
  rm(list=c("CNavg", "SSCN", "SSCNavg"))
  CNmodeldf$Ia = Ia_init
  attach(CNmodeldf)
  # Those processes that are dependant on prior days conditions, we run as a 
  # loop through each of the days.
  for (t in 2:length(AW)){
    ET[t] = AW[t-1]/AWC[t-1]*PET[t]
    # Calculating Net Precipitation which adds in slope above's Excess
    dP[t] = SNO_Energy$Rain_mm[t] - ET[t] + 
      SNO_Energy$SnowMelt_mm[t]    # CN Solution
    # Is the soil saturated, and thus can't take more dP? 
    if (AW[t-1] + dP[t]>=AWC[t]){
      Excess[t]=AW[t-1] + dP[t] -AWC[t]
      AW[t]=AWC[t]
      # Otherwise, if dP is less than the initial abstraction? 
      # https://en.wikipedia.org/wiki/Runoff_curve_number#Definition
    } else if (dP[t]<=Ia[t]) {
      Excess[t]=0.0
      AW[t]=AW[t-1] + dP[t]
    } else {
      Excess[t]=(dP[t]-Ia[t])^2/(dP[t]-Ia[t]+SSCN[t])
      AW[t]=AW[t-1] + dP[t] -Excess[t]
    }
    S[t]=S[t-1]+Excess[t]
    Qpred[t]=fnc_fcres*S[t]
    S[t]=S[t]-Qpred[t]
  }
  CNmodeldf$ET=ET
  CNmodeldf$dP=dP
  CNmodeldf$AW=AW
  CNmodeldf$Excess=Excess
  CNmodeldf$S=S
  CNmodeldf$Qpred=Qpred # UPDATE vector BEFORE DETACHING
  rm(list=c("AW", "dP", "ET", "Excess", "Qpred", "S"))
  detach(CNmodeldf)
  
  NSeff(CNmodeldf$Qpred,CNmodeldf$Qmm)
  
  plot(CNmodeldf$Qpred)
  plot(CNmodeldf$Qmm)
  mean(CNmodeldf$Qmm)
  
  mean(CNmodeldf$Qpred)
}
