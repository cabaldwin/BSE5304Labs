# 
# Since everything depends on the libraries you install
# it is worthwhile loading them at the beginning
#
objects()  # This will list the objects you have.
rm(list=objects()) # Removes ALL the objects… so be careful here.
#
# What is going to change from use case to use case 
LabNo="/Lab06"
myflowgage_id="0205551460"  # Old Friendly Gage
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

setwd(srcdir)
# Watch your package load to make sure EcoHydRology Loads else come 
# back to here
#
# detach("package:EcoHydRology", unload = TRUE)
# remove.packages("EcoHydRology", lib="~/R/x86_64-pc-linux-gnu-library/4.2")
# system("svn checkout svn://scm.r-forge.r-project.org/svnroot/ecohydrology/"); 
# install.packages(c("ecohydrology/pkg/EcoHydRology/"),repos = NULL)
# 
# Load your packages!
pacman::p_load(ggplot2,dplyr,patchwork,rnoaa, EcoHydRology)
pacman::p_load(operators,topmodel,DEoptim,soilDB,sp,curl,httr,
                 rnoaa,raster,shapefiles,rgdal,elevatr,terra,progress,lubridate)
#
#
#--------------source CN model function from a previous lab----------
setwd(datadir)
source("https://raw.githubusercontent.com/vtdrfuka/BSE5304_2022/main/functions/CNmodel")
#LITTLE OTTER CREEK AT FERRISBURG, VT.
#---------Getting streamflow data from USGS-------------------
myflowgage_id="04282650"
myflowgage=get_usgs_gage(myflowgage_id, begin_date = "2010-01-01",
                         end_date = "2023-01-01")
myflowgage$flowdata$Qmm = myflowgage$flowdata$flow/myflowgage$area/10^3
#--------Getting weather data from FillMissWX (noaa) function
WXData=FillMissWX(declat=myflowgage$declat, declon=myflowgage$declon,30,
                  date_min="2010-01-01",
                  date_max="2023-02-01",method = "IDEW")
AllDays=data.frame(date=seq(min(myflowgage$flowdata$mdate), by = "day", 
                            length.out = max(myflowgage$flowdata$mdate)-min(myflowgage$flowdata$mdate)))
WXData=merge(AllDays,WXData,all=T)
WXData$PRECIP=WXData$P
WXData$P[is.na(WXData$P)]=0
WXData$PRECIP[is.na(WXData$PRECIP)]=0
WXData$TMX=WXData$MaxTemp
WXData$TMX[is.na(WXData$TMX)]=1
WXData$MaxTemp[is.na(WXData$MaxTemp)]=1
WXData$TMN=WXData$MinTemp
WXData$TMN[is.na(WXData$TMN)]=0
WXData$MinTemp[is.na(WXData$MinTemp)]=0
WXData$DATE=WXData$date

modeldata=merge(WXData,myflowgage$flowdata,by.x="date",by.y="mdate")

CN_DF=CNmodel(CNmodeldf = modeldata,CNavg = 70,
                IaFrac = 0.05,fnc_slope =  0.05,
                fnc_aspect =0.0,func_DAWC = 0.35,
                func_z=1000,fnc_fcres=0.20,
                declat=myflowgage$declat,
                declon=myflowgage$declon)
NSeff(CN_DF$Qmm,CN_DF$Qpred)
#-----------getting ready for optimization using DEoptim----------------------
f <- function (x) {
  CNopt=x[1]
  IaOpt=x[2]
  fnc_slopeOpt=x[3]
  fnc_aspectOpt=x[4]
  func_DAWCOpt=x[5]
  func_zOpt=x[6]
  fnc_fcresOpt=x[7]
  
  CNmodelnew=CNmodel(CNmodeldf =modeldata,
                     CNavg = CNopt,IaFrac = IaOpt,
                     fnc_slope=fnc_slopeOpt,fnc_aspect=fnc_aspectOpt,
                     func_DAWC=func_DAWCOpt,func_z=func_zOpt,
                     fnc_fcres=fnc_fcresOpt,declat=myflowgage$declat,
                     declon=myflowgage$declon)
  mNSE=1-NSeff(CNmodelnew$Qmm,CNmodelnew$Qpred)
  return(mNSE)
}
#            Educated guesses for ranges… or not.        
#            CN,  Ia, Slp, Asp, AWC,Depth,ResCoef
lower <- c(50,0.05, 0.0, 0.01, 0.2,  500,   0.1)
upper <- c(99,0.20, 0.05, 0.1, 0.4, 2000,   0.5)

# run DEoptim and set a seed first for replicability
set.seed(1234)
#
# The cluster will show you 128 cores, but the management software
# will limit you to running on however many cores you requested.
#
cl <- parallel::makeCluster(16)
# In your command line check out all the versions of R that are now 
# running
# top -u {youruserid}  # like top -u drfuka for me
# ps -auxwww | grep drfuka
outDEoptim=DEoptim(f, lower, upper,
                     DEoptim.control(cluster=cl,strategy = 6,
                                     NP = 16,itermax=100,parallelType = 1,
                                     packages = c("EcoHydRology"),
                                     parVar=c("CNmodel","SnowMelt","PET_fromTemp",
                                              "SoilStorage","NSeff","modeldata","myflowgage")))

# This set of parameters should fail… because one (or more) of the parameters
# in the model is causing one of the 16 * 100, 160 runs, to fail with an 
# error. 
#
# Lets figure this out, but first:
# Stop your cluster and confirm in your terminal
# top -u {userid}
parallel::stopCluster(cl)
#
#
outDEoptim$optim$bestmem
# NSE = 1- bestval
1-outDEoptim$optim$bestval
x=outDEoptim$optim$bestmem
f(x)
names(outDEoptim$member$lower)=c("CNavg","IaFrac","fnc_slope", 
                                   "fnc_aspect","func_DAWC","func_z",
                                   "fnc_fcres")
plot(outDEoptim)
plot(outDEoptim,plot.type="bestvalit")
#plot(outDEoptim,plot.type="bestmemit")
dev.off()

##-------plot NSE vs iteration------------
NSE=data.frame(1-outDEoptim$member$bestvalit)
colnames(NSE)="NSE"
plot(NSE$NSE,type = "l", ylab = "NSE", xlab = "Iteration number")
#----another way to plot all parameters of differnt iteration----
BestMemit=data.frame(outDEoptim$member$bestmemit)
colnames(BestMemit)=c("CNavg","IaFrac","fnc_slope", 
                        "fnc_aspect","func_DAWC","func_z",
                        "fnc_fcres")
#plotting the progression of improvements similar to above
for(i in 1:length(BestMemit)){
  plot(BestMemit[,i], ylab= "",xlab="",ylim=c(lower[i],upper[i]),cex=0.3)
  mtext(side=1, line=2, "iteration", col="black",cex=1)
  mtext(side=2, line=3,colnames(BestMemit)[i], col="black", font=2, cex=1)
}
#---------merging NSE and BestMemit dataframe ---------------------------
NSEs_df=cbind(BestMemit,NSE)
#---calculate changes between two succesive rows-----------------------------
deltaparams=(tail(NSEs_df,-1)-NSEs_df[1:(length(NSEs_df[,1])-1),])
# 
#----calculate the relative variability of each parameter------
shifted_NSEs_df=NSEs_df[1:(length(NSEs_df[,1]))-1,]
pacman::p_load(fastmatch)
for (i in colnames(deltaparams)){
  #i="CNavg"
  nam = paste0("junkpar_",i)
  assign(nam,deltaparams[!deltaparams[,fmatch(i,names(deltaparams))]==0,] )
  nam2=paste0("initialval_",i)
  assign(nam2,shifted_NSEs_df[!deltaparams[,fmatch(i,names(deltaparams))]==0,])
  nam3=paste0("Sr_",i)
  assign(nam3,abs(((get(paste0("junkpar_",i))$NSE)/
                     (get(paste0("junkpar_",i))[fmatch(i,names(deltaparams))]))*
                    (get(paste0("initialval_",i))[fmatch(i,names(deltaparams))]/
                       get(paste0("initialval_",i))$NSE)))
}

# 
# With sr for each params and you can get the mean of them to compare the sr
# Making a dataframe with bbox and violin plots
RelSensi=base::as.data.frame(stack(list(CNavg=Sr_CNavg$CNavg,
                                          IaFrac=Sr_IaFrac$IaFrac,
                                          fnc_slope=Sr_fnc_slope$fnc_slope, 
                                          fnc_aspect=Sr_fnc_aspect$fnc_aspect,
                                          func_DAWC=Sr_func_DAWC$func_DAWC,
                                          func_z=Sr_func_z$func_z,
                                          fnc_fcres=Sr_fnc_fcres$fnc_fcres
)))
names(RelSensi)[2]="Parameter"
# Basic violin plot
p <- ggplot(RelSensi, aes(x=Parameter, y=values, color=Parameter)) + 
  coord_cartesian(ylim = c(0,25))+
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_boxplot(width=0.1) + theme(legend.position = "None")

p
p + stat_summary(fun.y=mean, geom="point", shape=8, size=4,col="black") +
  xlab("Parameter")+
  ylab("Relative Variability")+
  theme(axis.text = element_text(size =10))+
  theme(axis.text = element_text(face="bold"))+
  theme(axis.title = element_text(size =10))+
  theme(axis.title = element_text(face="bold"))

## NEUEUEUEUEUE


#===================================================
# install and load basic neural net builder, ggplot2, and 
pacman::p_load(neuralnet,ggplot2,Metrics,reshape2)
#===================================================
# set random seed 
set.seed(2)
#===================================================
#                       Step 1. Make Some Example Data
#===================================================
# Make Some Example Data
# archemedian spiral with noise 
t <- (seq(0, 10, by=0.01))
x <- t*cos(t)
y <- t*sin(t)
y <- (y + runif(length(t), -1, 1)) + 20
outliers <- 100
y[sample(length(y), outliers)] <- y[sample(length(y), outliers)] + 
  runif(outliers, -8, 8)

# add one more variable
z <- -.8*t^2 + 2.1*t
z[seq(1, 0.3*length(z))] <- z[seq(1,0.3*length(z))] - 40

# build the dataframe
dat <- data.frame(x = x, y = y, z = z)
#===================================================
#                      Plot the example Data
#===================================================
# look at the data
ggplot(dat) + 
  geom_point(aes(x = x, y = y, color = z)) + 
  ggtitle('Spiral Pattern Data With z Variable')
#===================================================
#                Try linear Regression to model y ~ x + z
#===================================================
# Try Linear Regression to get y from x and z
lin_test <- lm(y ~ x + z, data = dat)

# look at the results
summary(lin_test)
#===================================================
#                       Plot the result of linear regression
#===================================================
# build a y vs y hat figure to compare 
fit = data.frame(x = dat$x, y = dat$y, y_hat = lin_test$fitted.values)

gg_dat <- melt(fit, 1)

# look at just the x, y and y hat
ggplot(gg_dat) + 
  geom_point(aes(x = x, y = value, color = variable)) + 
  ggtitle('Linear Regression Fails to Capture the Structure ')
#===================================================
#                   Prepare Data for the neural network
#===================================================
# prepare the data for the neural net
datmax <- apply(dat, 2, max)
datmin <- apply(dat, 2, min)
?scale
# scaled = as.data.frame(scale(dat, center = datmin, scale = datmax - datmin))

# now split the datat into train, validation and test sets. 
scaled <- scale(dat, center = datmin, scale = datmax - datmin)

# sub sample to get the test and train set 
index <- seq(1, nrow(dat))
?sample
train_index <- sample(index, 0.8*length(index))
train <- scaled[train_index,]
test <- scaled[-train_index,]
#===================================================
#                 Plot the Scaled Data
#===================================================
# view the Data change
original_x <- dat[,1]
original_y <- dat[,2]

scaled_x <- scaled[,1]
scaled_y <- scaled[,2]

# make before and after data
original <- data.frame(x = original_x, y = original_y)
scaled <- data.frame(x = scaled_x, y = scaled_y)

# look at original and scaled data. 
ggplot() +
  geom_point(data = original, aes(x = x, y = y), color = 'red') + 
  geom_point(data = scaled, aes(x = x, y = y), color = 'orange') +
  ggtitle('Original (in red) and Scaled  (in orange) Data')
#===================================================
#                     Plot the Train and Test Data 
#===================================================
# look at train and test data
# make a train / test data frame for plotting
variable = as.factor(c(rep('train', nrow(train)), rep('test', nrow(test))))
x <- c(train[,1], test[,1])
y <- c(train[,2], test[,2])

train_test_compare <- data.frame(x, y, variable)

# Plot the train and test data 
ggplot(train_test_compare) + 
  geom_point(aes(x = x, y = y, color = variable)) + 
  ggtitle('Train and Test Data')
#===================================================
#             Now, setup and train the neural network!!!
#===================================================
# first set the random seed to a lucky number
set.seed(2)

# setup and train the neural network 
net <- neuralnet(y ~ x + z, 
                   data = train, 
                   hidden = c(6, 3), 
                   threshold = 0.03, # if you network does not run, try turning this up a little
                   stepmax = 1e+05, 
                   rep = 1, 
                   learningrate = 0.001, 
                   lifesign = "full",
                   algorithm = "backprop",
                   err.fct = 'sse', 
                   act.fct = "logistic",
                   linear.output = FALSE, 
                   exclude = NULL,
                   constant.weights = NULL)
#===================================================
#                    Plot the Structure of the Neural Net
#===================================================
# View the structure of the neural net
plot(net)
#===================================================
#             Make Predictions to See how the neural network did 
#===================================================
# look at predictions from the train and test sets
#  to see how the network did

# make the predictions on the train data with "predict()"
?predict.nn
nn_prediction = predict(net, train[,c(1, 3)])

# un-scale the predictions
unscaled_train_prediction = (nn_prediction * (max(dat$y) - min(dat$y))) + min(dat$y)

train_x = (train[,1] * (max(dat$x) - min(dat$x))) + min(dat$x)
train_y = (train[,2] * (max(dat$y) - min(dat$y))) + min(dat$y)

# make predictions on the test data
nn_prediction = predict(net, test[, c(1, 3)])

# un-scale
unscaled_test_prediction = (nn_prediction * (max(dat$y) - min(dat$y))) + min(dat$y)

test_x = (test[,1] * (max(dat$x) - min(dat$x))) + min(dat$x)
test_y = (test[,2] * (max(dat$y) - min(dat$y))) + min(dat$y)
#===================================================
#           Plot the predictions for both train and test
#===================================================
# plot the train and test and predictions
train_fit <- data.frame(x = train_x, y = train_y, 
                          nn_prediction = unscaled_train_prediction, data_set = 'train')
test_fit = data.frame(x = test_x, y = test_y, 
                        nn_prediction = unscaled_test_prediction, data_set = 'test')

result <- rbind(train_fit, test_fit)
result$data_set <- as.factor(result$data_set)

plot_frame <- melt(result, c(1, 4))

ggplot(plot_frame) + 
  geom_point(aes(x = x, y = value, color = variable)) + 
  ggtitle('The Neural Net is able to Capture the Structure') +
  facet_grid(~data_set)

#===================================================

