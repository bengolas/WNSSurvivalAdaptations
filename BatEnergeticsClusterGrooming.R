###################################################################################
# The following code is designed to simulate bats with varying parameterization
# to compare survival expectations in a spatiotemporally heterogeneous environment.
# This set of equations was modified to allow for energy loss mitigation via 
# clustering. We incorporate conservatively for now only energy loss in torpor.
###################################################################################



########################
# Import data
########################

rm(list=ls())

#Setting up logger info as in initial report
library(tidyverse)
library(lubridate)
library(reshape2)
library(ggplot2)
library(schoolmath)
library(plyr)
library(gridExtra)
library(cowplot)

#Set the working directory for the folder of interest on your computer
setwd("C:/Users/bengo/Documents/WNS/2017Barton/DataCleaning")

getwd()
#Identify your iButton files in RHreads
#Not including faulty loggers 28,33,80

RHreads <- c("003_RH.csv","004_RH.csv","005_RH.csv","006_RH.csv","007_RH.csv",
             "013_RH.csv","020_RH.csv","022_RH.csv","023_RH.csv","026_RH.csv","027_RH.csv",
             "031_RH.csv","038_RH.csv","040_RH.csv","045_RH.csv",
             "046_RH.csv","050_RH.csv","058_RH.csv","064_RH.csv","065_RH.csv","067_RH.csv",
             "068_RH.csv","072_RH.csv","074_RH.csv","075_RH.csv","078_RH.csv",
             "088_RH.csv","094_RH.csv","096_RH.csv","101_RH.csv","103_RH.csv","110_RH.csv",
             "116_RH.csv","117_RH.csv")

Treads <- c("003_T.csv","004_T.csv","005_T.csv","006_T.csv","007_T.csv",
            "013_T.csv","020_T.csv","022_T.csv","023_T.csv","026_T.csv","027_T.csv",
            "031_T.csv","038_T.csv","040_T.csv","045_T.csv",
            "046_T.csv","050_RH.csv","058_T.csv","064_T.csv","065_T.csv","067_T.csv",
            "068_T.csv","072_T.csv","074_T.csv","075_T.csv","078_T.csv",
            "088_T.csv","094_T.csv","096_T.csv","101_T.csv","103_T.csv","110_T.csv",
            "116_T.csv","117_T.csv")



IDs <- as.numeric(gsub("_RH.csv","",RHreads))

#Load data that is cleaned in DataCleaning file
setwd("C:/Users/bengo/Documents/WNS/2017Barton/DataCleaning")
load("df.trim") #Data for 2017-18 data loggers
load("dwt") #Data files for 2017-18 averaged outside data
load("dwh")
load("dww")
load("df.pred.trim") #Data for 2018-19 data loggers
load("dwt.18") #Data for 2018-19 averaged outside data
load("dwh.18")
load("dww.18")
load("Temp14_19")
load("Outside.mean.daily")

# Make sure all time points are present and equally spaced
test <- as.data.frame(cbind(unique(df.trim$Time),1:length(unique(df.trim$Time))))
# ggplot(test,aes(as_datetime(V1, format="%Y-%m-%d %H"),as.numeric(as.character(V2)),group=1)) + geom_line()
ID.drum <- c("020","022","023","026","027","028","031","033")
ID.upper <- c("003","004","005","006","007","008","013","038","094","096","101","103","110","116","117")
ID.lower <- c("040","045","046","050","058","064","065","067","068","072","074","075","078","080","088")
df.trim$Room <- NA
df.trim$Room[which(match(df.trim$ID,ID.drum)!="NA")] <- "drum"
df.trim$Room[which(match(df.trim$ID,ID.upper)!="NA")] <- "upper"
df.trim$Room[which(match(df.trim$ID,ID.lower)!="NA")] <- "lower"
df.trim$Drum <- 0
df.trim$Drum[which(match(df.trim$ID,ID.drum)!="NA")] <- 1
df.trim$Upper <- 0
df.trim$Upper[which(match(df.trim$ID,ID.upper)!="NA")] <- 1
df.trim$Lower <- 0
df.trim$Lower[which(match(df.trim$ID,ID.lower)!="NA")] <- 1

# ggplot(df.drum,aes(Time,Temp))+geom_line(aes(group=iButton, color=Depth)) +
# theme_minimal() + scale_color_viridis_c()



RHreads <- c("020_RH.csv", "028_RH.csv", "040_RH.csv")

Treads <- c("020_T.csv", "028_T.csv", "040_T.csv")

IDs.18 <- as.numeric(gsub("_RH.csv","",RHreads))

#Below will plot line graphs for each iButton separately.
# ggplot(df.pred.trim,aes(Time,Temp))+geom_line(aes(group=iButton,color=ID)) + theme_minimal()
# ggplot(df.trim,aes(Time,Temp))+geom_line(aes(group=iButton,color=Depth)) +
#   theme_minimal() + ylim(-10,25)
# ggplot(df.trim,aes(Time,RH))+geom_line(aes(group=iButton,color=Depth)) + theme_minimal()

df.RH.test <- df.trim[df.trim$Time>="2017-12-01 00" & df.trim$Time<="2018-04-15 22",]
# ggplot(df.RH.test, aes(Time,RH)) + geom_line(aes(group=iButton,color=Depth)) +
# theme_minimal() + ylim(40,120)


# ggplot(df.RH.test,aes(Time,(RH/100)*0.611*exp(17.503*Temp/(Temp+240.97))))+geom_line(aes(group=iButton,color=Depth)) + theme_minimal() + ylim(-0.5,1.4)
# #Make sure all time points are present and equally spaced
# test <- as.data.frame(cbind(unique(df.pred.trim$Time),1:length(unique(df.pred.trim$Time))))
# ggplot(test,aes(as_datetime(V1),as.numeric(as.character(V2)),group=1)) + geom_line()


setwd("C:/Users/bengo/Documents/WNS/2017Barton")
df.location <- read.csv("LocationInfo.csv")
df.location <- df.location[match(IDs,df.location$iButton),]
df.location <- subset(separate(df.location,"Approx..Elevation.Above.Lake.Champlain",into=c("a1","a2"),sep="/"))
df.location$a1 <- as.numeric(df.location$a1)
df.location$a2 <- as.numeric(df.location$a2)
df.location$Elevation <- rowMeans(df.location[,4:5],na.rm=TRUE)

df.location$is.MYLU <- ifelse(grepl("MYLU",df.location$Bats),1,0)
df.location$is.MYSO <- ifelse(grepl("MYSO",df.location$Bats),1,0)
df.location$is.MYLE <- ifelse(grepl("MYLE",df.location$Bats),1,0)

df.location$Elevation[which(df.location$iButton==88)] <- 1325
df.location$Elevation[which(df.location$iButton==20)] <- 1321


xycoords <- cbind(IDs,subset(df.location, select=c(x_coord,y_coord,Elevation)))

df.location <- subset(df.location, select = -c(x_coord,y_coord,a1,a2,Bats,Region))

#Suspect some logger depths are incorrectly recorded



df.trim <- df.trim[which(as.numeric(as.character(df.trim$ID)) %in% IDs),]
df.trim$Elevation <- rep(df.location$Elevation, length(df.trim[,1])/length(df.location[,1]))

# #Remove problematic loggers
#
# rejects <- c("28","33","80")
# df.location <- df.location[-which(as.numeric(df.location$iButton %in% rejects)==1),]


#Clean data to remove unusable values
#Reset values associated with temperature errors
for(i in 1:5){
  df.trim$RH[which(df.trim$Temp>=20)] <- df.trim$RH[(which(df.trim$Temp>=20)-length(unique(df.trim$ID)))]
  df.trim$RH[which(df.trim$Temp<=-6)] <- df.trim$RH[(which(df.trim$Temp<=-6)-length(unique(df.trim$ID)))]
}
#Reset erroneous values
df.trim$RH <- df.trim$RH/100
df.trim$RH[which(df.trim$Time <= "2017-11-30 22")] <- 1 #100% RH in warmer seasons
df.trim$RH[which(df.trim$Time >= "2018-04-20 00")] <- 1
df.trim$RH[which(df.trim$RH>=1)] <- 1 #Set truncate measurements to be realistic
df.trim$RH[which(df.trim$RH<=0)] <- 0 #i.e. >100% = 100%, <0% = 0%

#Remove errored temperature values
for(i in 1:50){
  df.trim$Temp[which(df.trim$Temp>=20)] <- df.trim$Temp[(which(df.trim$Temp>=20)-length(unique(df.trim$ID)))]
  df.trim$Temp[which(df.trim$Temp<=-6)] <- df.trim$Temp[(which(df.trim$Temp<=-6)-length(unique(df.trim$ID)))]

  df.trim$RH[which(df.trim$RH<=.4)] <- df.trim$RH[(which(df.trim$RH<=.4)-length(unique(df.trim$ID)))]
}


df.trim$RH[which(df.trim$Time >= "2017-12-15 22" & df.trim$Time <= "2018-03-15" & df.trim$Temp>=8)] <- df.trim$RH[which(df.trim$Time >= "2017-11-30 22" & df.trim$Time <= "2018-03-31" & df.trim$Temp>=9)-length(unique(df.trim$ID))]
df.trim$Temp[which(df.trim$Time >= "2017-12-15 22" & df.trim$Time <= "2018-03-15" & df.trim$Temp>=8)] <- df.trim$Temp[which(df.trim$Time >= "2017-12-15 22" & df.trim$Time <= "2018-03-15" & df.trim$Temp>=8)-length(unique(df.trim$ID))]

T.tormin <- 2

df.microclim <- df.trim[which(df.trim$Time >= "2017-08-16 00" & df.trim$Time <= "2018-05-14 22"),]

df.microclim$d.WVP.bat <- 0.611*exp(17.503*pmax(df.microclim$Temp,T.tormin)/(pmax(df.microclim$Temp,T.tormin)+240.97)) - df.microclim$RH*0.611*exp((17.503*df.microclim$Temp)/(df.microclim$Temp+240.97))

df.microclim$d.WVP.a <- 0.611*exp((17.503*df.microclim$Temp)/(df.microclim$Temp+240.97)) - df.microclim$RH*0.611*exp((17.503*df.microclim$Temp)/(df.microclim$Temp+240.97))

ggplot(df.microclim,aes(Time,Temp))+geom_line(aes(group=iButton,color=Depth)) +
  theme_minimal()
ggplot(df.microclim,aes(Time,RH))+geom_line(aes(group=iButton,color=Depth)) + theme_minimal()



df.location$T.mean <- with(df.microclim,tapply(Temp,list(ID=ID),mean))
df.location$T.sd <- with(df.microclim,tapply(Temp,list(ID=ID),sd))
df.location$T.max <- with(df.microclim,tapply(Temp,list(ID=ID),max))
df.location$T.min <- with(df.microclim,tapply(Temp,list(ID=ID),min))
df.location$T.median <- with(df.microclim,tapply(Temp,list(ID=ID),median))
# df.location$RH.mean <- with(df.microclim[which(df.microclim$Time >= "2017-12-01 00" & df.microclim$Time <= "2018-04-15 22"),],tapply(RH,list(ID=ID),mean))
# df.location$RH.sd <- with(df.microclim[which(df.microclim$Time >= "2017-12-01 00" & df.microclim$Time <= "2018-04-15 22"),],tapply(RH,list(ID=ID),sd))
# df.location$RH.min <- with(df.microclim[which(df.microclim$Time >= "2017-12-01 00" & df.microclim$Time <= "2018-04-15 22"),],tapply(RH,list(ID=ID),min))
# df.location$RH.median <- with(df.microclim[which(df.microclim$Time >= "2017-12-01 00" & df.microclim$Time <= "2018-04-15 22"),],tapply(RH,list(ID=ID),median))
df.location$T.Sept.mean <- with(df.microclim[which(df.microclim$Time >= "2017-09-01 00" & df.microclim$Time <= "2017-09-30 22"),],tapply(Temp,list(ID=ID),mean))
df.location$T.Sept.sd <- with(df.microclim[which(df.microclim$Time >= "2017-09-01 00" & df.microclim$Time <= "2017-09-30 22"),],tapply(Temp,list(ID=ID),sd))
df.location$T.Oct.mean <- with(df.microclim[which(df.microclim$Time >= "2017-10-01 00" & df.microclim$Time <= "2017-10-31 22"),],tapply(Temp,list(ID=ID),mean))
df.location$T.Oct.sd <- with(df.microclim[which(df.microclim$Time >= "2017-10-01 00" & df.microclim$Time <= "2017-10-31 22"),],tapply(Temp,list(ID=ID),sd))
df.location$T.Nov.mean <- with(df.microclim[which(df.microclim$Time >= "2017-11-01 00" & df.microclim$Time <= "2017-11-30 22"),],tapply(Temp,list(ID=ID),mean))
df.location$T.Nov.sd <- with(df.microclim[which(df.microclim$Time >= "2017-11-01 00" & df.microclim$Time <= "2017-11-30 22"),],tapply(Temp,list(ID=ID),sd))
# df.location$d.WVP.a.mean <- with(df.microclim,tapply(d.WVP.a,list(ID=ID),mean))
# df.location$d.WVP.a.sd <- with(df.microclim,tapply(d.WVP.a,list(ID=ID),sd))
# df.location$d.WVP.a.median <- with(df.microclim,tapply(d.WVP.a,list(ID=ID),median))
# df.location$d.WVP.a.min <- with(df.microclim,tapply(d.WVP.a,list(ID=ID),min))
df.location$d.WVP.a.winter.mean <- with(df.microclim[which(df.microclim$Time >= "2017-12-01 00" & df.microclim$Time <= "2018-04-15 22"),],tapply(d.WVP.a,list(ID=ID),mean))
df.location$d.WVP.a.winter.sd <- with(df.microclim[which(df.microclim$Time >= "2017-12-01 00" & df.microclim$Time <= "2018-04-15 22"),],tapply(d.WVP.a,list(ID=ID),sd))
df.location$d.WVP.a.winter.min <- with(df.microclim[which(df.microclim$Time >= "2017-12-01 00" & df.microclim$Time <= "2018-04-15 22"),],tapply(d.WVP.a,list(ID=ID),min))
df.location$d.WVP.bat.mean <- with(df.microclim,tapply(d.WVP.bat,list(ID=ID),mean))
df.location$d.WVP.a.winter.median <- with(df.microclim[which(df.microclim$Time >= "2017-12-01 00" & df.microclim$Time <= "2018-04-15 22"),],tapply(d.WVP.a,list(ID=ID),median))

df.location$Elevation.scaled <- (df.location$Elevation-mean(df.location$Elevation))/sd(df.location$Elevation)



df.location$Room <- "NA"
df.location$Room[which(match(df.trim$ID[1:length(unique(df.location$iButton))],ID.drum)!="NA")] <- "Drum"
df.location$Room[which(match(df.trim$ID[1:length(unique(df.location$iButton))],ID.upper)!="NA")] <- "Upper"
df.location$Room[which(match(df.trim$ID[1:length(unique(df.location$iButton))],ID.lower)!="NA")] <- "Lower"

is.MYLU <- df.location$is.MYLU










#################################################################################
# Set up LHS matrix
#################################################################################


# library(fitur)
library(lhs)


n <- 25000
set.seed(n+1)


Barton.Mbody <- c(8.08,8.58,10.51,9.79,9.87,8.06,10.09,10.11,7.46,9.19)
Barton.fat <- c(2.32,2.385,2.97,2.795,3.17,1.815,3.385,3.18,1.135,2.625)
Barton.pfat <- Barton.fat/Barton.Mbody
m.pf <- mean(Barton.pfat)
sd.pf <- sd(Barton.pfat)

LHS <- randomLHS(n,9)

T.tormin <- 2
t.tormax <- 1300
rEWL.body <- 0.1
TMR.min <- 0.03
C.t <- 0.2
groom <- 1

LHS <- data.frame(M.body = qunif(LHS[,1],min(Barton.Mbody),max(Barton.Mbody)),
                  p.fat = qunif(LHS[,2],min(Barton.pfat),max(Barton.pfat)),
                  T.tormin = qunif(LHS[,3],min=0.5,max=5),
                  t.tormax = qunif(LHS[,4],min=t.tormax*0.5,max=t.tormax*1.5),
                  rEWL.body = qunif(LHS[,5],min=rEWL.body*0.01,max=rEWL.body*2),
                  TMR.min = qunif(LHS[,6],min=TMR.min*0.01,max=TMR.min*5),
                  C.t = qunif(LHS[,7],min=C.t*0.01,max=C.t*5),
                  clusterfac = qunif(LHS[,8], min=0.5, max=1),
                  groom = qunif(LHS[,9], min=0, max=groom*100)
)


#######################################
# Define survival function
#######################################

SURVIVAL.TIME <- function(
  M.body,
  p.fat,
  T.tormin,
  t.tormax,
  rEWL.body,
  TMR.min,
  C.t,
  clusterfac,
  groom){

  library(lubridate)


  df.all.loggers <- NA
  df.all.loggers.Pd <- NA


  #Parameter list for Hayman energetics equations
  B.1.Pd <- 1.15*10^(-3)
  B.2.Pd <- 0.27
  T.min <- 0
  T.max <- 19.7
  mu.1.Pd <- 1.51*10^(-4)
  mu.2.Pd <- -9.92*10^(-3)
  RMR <- 2.6
  T.lc <- 32
  C.eu <- 0.26

  #Parameter list from Haase
  Q10.cool <- 3.82-0.507*log10(M.body)
  S <- 0.1728
  T.eu <- 37
  WR <- 0.8*60
  t.eu <- 1.1
  SA.body <- 10*M.body^(0.67)
  SA.wing <- SA.body*19.68/39.36
  rEWL.wing <- rEWL.body*.33/.1
  p.lean <- 0.55
  M.fat <- M.body*p.fat
  M.lean <- M.body*p.lean

  E.winter <- M.fat*37.6*1000/19.6

  # Grooming per Brownlee-Bouboulis 2013
  p.groom <- 0.219 # Percent time in arousal spent grooming

  #Start and end times for evaluation
  # start.date <- seq(as_datetime("2017-09-16"),as_datetime("2017-11-15"),by="day")
  # start.date <- format(as.POSIXct(strptime(start.date,"%Y-%m-%d",tz="")) ,format = "%Y-%m-%d %H")
  #
  # end.date <- seq(as_datetime("2018-03-16"),as_datetime("2018-05-14"),by="day")
  # end.date <- format(as.POSIXct(strptime(end.date,"%Y-%m-%d",tz="")) ,format = "%Y-%m-%d %H")


  start.date <- as_datetime("2017-10-01")
  start.date <- format(as.POSIXct(strptime(start.date,"%Y-%m-%d",tz="")) ,format = "%Y-%m-%d %H")

  end.date <- as_datetime("2018-04-15")
  end.date <- format(as.POSIXct(strptime(end.date,"%Y-%m-%d",tz="")) ,format = "%Y-%m-%d %H")


  #Calculate energy use from start date to end date for each logger

  for(loc in 1:length(unique(df.trim$ID))){
    df.bat <- df.trim[which(df.trim$ID==unique(df.trim$ID)[loc]),] #choose single location


    SVP <- 0.611*exp(17.503*df.bat$Temp/(df.bat$Temp+240.97))
    WVP.a <- SVP*df.bat$RH
    WVP.bat <- 0.611*exp(17.503*pmax(df.bat$Temp,T.tormin)/(pmax(df.bat$Temp,T.tormin)+240.97))
    d.WVP.hold <- WVP.bat-WVP.a
    d.WVP.hold[which(d.WVP.hold<=0.0001)] <- 0.0001


    df.logger <- NA #empty matrix to record output

    #Loop through energetic evaluation of each scenario for each start and end date

    for(a in 1:length(start.date)){
      for(b in 1:length(end.date)){

        t.start <- start.date[a]
        t.end <- end.date[b]

        #specify hourly temperature and humidity deficit starting at t.start
        T.a <- rep(df.bat$Temp[which(df.bat$Time==t.start):which(df.bat$Time==t.end)],each=2)
        RH <- rep(df.bat$RH[which(df.bat$Time==t.start):which(df.bat$Time==t.end)],each=2)
        d.WVP <- rep(d.WVP.hold[which(df.bat$Time==t.start):which(df.bat$Time==t.end)],each=2)
        WVP.a.hib <- rep(WVP.a[which(df.bat$Time==t.start):which(df.bat$Time==t.end)],each=2)
        WVP.bat.hib <- rep(WVP.bat[which(df.bat$Time==t.start):which(df.bat$Time==t.end)],each=2)


        #Calculate energy to get from start to end based on Haase2019
        X <- 10000 #potential events
        TIME <- rep(NA, X)

        E.hib <- rep(NA, X) #Keep track of energy expenditure


        #First event is cooling down into torpor
        CR <- C.eu*(M.body^0.67)*log10(T.eu-T.a[1])/(S*M.body)

        t.event <- log10(T.eu-T.a[1])/CR #Time for event

        TIME[1] <- 0
        TIME[2] <- t.event #Track timeline

        E.event <- t.event*(TMR.min+(RMR*Q10.cool^((T.a[1]-T.eu)/10)))
        E.hib[1] <- 0
        E.hib[2] <- E.event

        Marker <- "cooling" #Start with bat cooling to hibernation mode
        arousal.count <- 0


        for(t in 3:X){

          time.mark <- round(TIME[t-1], digits=0) #current time stamp relative to T.a/d.WVP
          if(time.mark>=length(T.a)) break #break if we reach the end of hibernation

          if(Marker=="cooling"){
            Marker <- "torpor"

            E.bout <- 0 #Set an energy recording for this torpor bout
            CEWL <- 0
            REWL <- 0
            t.event <- 0
            for(j in 1:10000){ #Determine torpor length based on hourly conditions
              #Temperature-based arousal
              Temp.torpor <- mean(T.a[time.mark:(time.mark+j)]) #torpor bouts proceed in 1 hour increments

              if(Temp.torpor > T.tormin){
                Q10 <- 1.6+0.26*Temp.torpor-0.006*Temp.torpor^2
                t.tor.TMR <- t.tormax/(Q10^((Temp.torpor-T.tormin)/10))
                #assume a 30% reduction in energy use when clustered in torpor
                E.hour <- (TMR.min*Q10^((Temp.torpor-T.tormin)/10))
              }else{
                t.tor.TMR <- t.tormax/((1+(T.tormin-Temp.torpor)*(C.t/TMR.min)))
                #assume a 30% reduction in energy use when clustered in torpor
                E.hour <- (TMR.min+(T.tormin-Temp.torpor)*C.t)
              }

              #Humidity-based arousal using CEWL and REWL rates averaged over the torpor
              CEWL <- ((SA.body*rEWL.body+SA.wing*rEWL.wing)*d.WVP[time.mark+j] + CEWL*(j-1))/(j)
              rav <- TMR.min*M.body/(0.2095*0.30*10^3)
              Sat.deficit <- ((WVP.bat.hib[time.mark+j]*0.00986923)/(0.0821*(max(T.a[time.mark+j],T.tormin)+273.15)))*18015.28 - ((RH[time.mark+j]*WVP.a.hib[time.mark+j]*0.00986923)/(0.0821*(max(T.a[time.mark+j],T.tormin)+273.15)))*18015.28
              REWL <- (rav*Sat.deficit + REWL*(j-1))/(j)

              t.tor.EWL <- 0.027*M.lean*1000/(CEWL+REWL)

              E.bout <- E.bout+E.hour
              t.event <- t.event+1
              if(t.tor.TMR<=j) break
              if(t.tor.EWL<=j) break
              if (TIME[t-1]+j+1>=length(T.a)) break
            }

            E.event <- E.bout

          }else{ #end torpor


            if(Marker=="torpor"){
              Marker <- "arousal"

              t.event <- (T.eu-T.a[time.mark])/WR

              E.event <- S*(T.eu-T.a[time.mark])+t.event*(C.eu*(T.eu-T.a[time.mark]))

              arousal.count <- arousal.count+1

            }else{ #end arousal

              if(Marker=="arousal"){
                Marker <- "euthermic"

                t.event <- t.eu

                E.event <- t.event*(RMR+C.eu*(T.lc-T.a[time.mark]))

              }else{ #end euthermic

                if(Marker=="euthermic"){
                  Marker <- "cooling"

                  CR <- C.eu*(M.body^0.67)*log10(T.eu-T.a[time.mark])/(S*M.body)

                  t.event <- log10(T.eu-T.a[time.mark])/CR #Time for event

                  E.event <- t.event*(TMR.min+(RMR*Q10.cool^((T.a[time.mark]-T.eu)/10)))

                }}}} #end cooling and elses


          TIME[t] <- TIME[t-1] + t.event
          E.hib[t] <- E.hib[t-1] + E.event

          # if(E.hib[t]<=E.winter){t.die <- TIME[t]/24} #Will stop updating t.die when energy required is greater than what is available

          if(TIME[t]>=length(T.a)) break

        }#end hibernation

        #Add in a final arousal if necessary
        if(Marker=="cooling" || Marker=="torpor"){
          time.mark <- length(T.a)
          t.event <- (T.eu-T.a[time.mark])/WR

          E.event <- S*(T.eu-T.a[time.mark])+t.event*(C.eu*(T.eu-T.a[time.mark]))

          arousal.count <- arousal.count+1


          TIME[t+1] <- TIME[t] + t.event
          E.hib[t+1] <- E.hib[t] + E.event
        } #end conditional final arousal

        TIME <- TIME[which(TIME!="NA")]
        E.hib <- E.hib[which(TIME!="NA")]
        E.reserve <- E.winter-E.hib[length(E.hib)]
        survive <- ifelse(E.reserve >=0, 1, 0)


        vec.logger <- c(unique(df.trim$ID)[loc],
                        E.hib[length(E.hib)],
                        E.reserve,
                        arousal.count,
                        t.start,
                        t.end,
                        survive,
                        is.MYLU[loc])





        ##########################################
        # Repeat with clustering
        ##########################################
        t.start <- start.date[a]
        t.end <- end.date[b]

        #specify hourly temperature and humidity deficit starting at t.start
        T.a <- rep(df.bat$Temp[which(df.bat$Time==t.start):which(df.bat$Time==t.end)],each=2)
        RH <- rep(df.bat$RH[which(df.bat$Time==t.start):which(df.bat$Time==t.end)],each=2)
        d.WVP <- rep(d.WVP.hold[which(df.bat$Time==t.start):which(df.bat$Time==t.end)],each=2)
        WVP.a.hib <- rep(WVP.a[which(df.bat$Time==t.start):which(df.bat$Time==t.end)],each=2)
        WVP.bat.hib <- rep(WVP.bat[which(df.bat$Time==t.start):which(df.bat$Time==t.end)],each=2)


        #Calculate energy to get from start to end based on Haase2019
        X <- 10000 #potential events
        TIME <- rep(NA, X)

        E.hib <- rep(NA, X) #Keep track of energy expenditure


        #First event is cooling down into torpor
        CR <- C.eu*(M.body^0.67)*log10(T.eu-T.a[1])/(S*M.body)

        t.event <- log10(T.eu-T.a[1])/CR #Time for event

        TIME[1] <- 0
        TIME[2] <- t.event #Track timeline

        E.event <- t.event*(TMR.min+(RMR*Q10.cool^((T.a[1]-T.eu)/10)))
        E.hib[1] <- 0
        E.hib[2] <- E.event

        Marker <- "cooling" #Start with bat cooling to hibernation mode
        arousal.count <- 0


        for(t in 3:X){

          time.mark <- round(TIME[t-1], digits=0) #current time stamp relative to T.a/d.WVP
          if(time.mark>=length(T.a)) break #break if we reach the end of hibernation

          if(Marker=="cooling"){
            Marker <- "torpor"

            E.bout <- 0 #Set an energy recording for this torpor bout
            CEWL <- 0
            REWL <- 0
            t.event <- 0
            for(j in 1:10000){ #Determine torpor length based on hourly conditions
              #Temperature-based arousal
              Temp.torpor <- mean(T.a[time.mark:(time.mark+j)]) #torpor bouts proceed in 1 hour increments

              if(Temp.torpor > T.tormin){
                Q10 <- 1.6+0.26*Temp.torpor-0.006*Temp.torpor^2
                t.tor.TMR <- t.tormax/(Q10^((Temp.torpor-T.tormin)/10)*clusterfac)
                #assume a 30% reduction in energy use when clustered in torpor
                E.hour <- (TMR.min*Q10^((Temp.torpor-T.tormin)/10))*clusterfac
              }else{
                t.tor.TMR <- t.tormax/((1+(T.tormin-Temp.torpor)*(C.t/TMR.min))*clusterfac)
                #assume a 30% reduction in energy use when clustered in torpor
                E.hour <- (TMR.min+(T.tormin-Temp.torpor)*C.t)*clusterfac
              }

              #Humidity-based arousal using CEWL and REWL rates averaged over the torpor
              CEWL <- ((SA.body*rEWL.body+SA.wing*rEWL.wing)*d.WVP[time.mark+j]*clusterfac + CEWL*(j-1))/(j)
              rav <- TMR.min*M.body/(0.2095*0.30*10^3)
              Sat.deficit <- ((WVP.bat.hib[time.mark+j]*0.00986923)/(0.0821*(max(T.a[time.mark+j],T.tormin)+273.15)))*18015.28 - ((RH[time.mark+j]*WVP.a.hib[time.mark+j]*0.00986923)/(0.0821*(max(T.a[time.mark+j],T.tormin)+273.15)))*18015.28
              REWL <- (rav*Sat.deficit + REWL*(j-1))/(j)

              t.tor.EWL <- 0.027*M.lean*1000/(CEWL+REWL)

              E.bout <- E.bout+E.hour
              t.event <- t.event+1
              if(t.tor.TMR<=j) break
              if(t.tor.EWL<=j) break
              if (TIME[t-1]+j+1>=length(T.a)) break
            }

            E.event <- E.bout

          }else{ #end torpor


            if(Marker=="torpor"){
              Marker <- "arousal"

              t.event <- (T.eu-T.a[time.mark])/WR

              E.event <- S*(T.eu-T.a[time.mark])+t.event*(C.eu*(T.eu-T.a[time.mark]))

              arousal.count <- arousal.count+1

            }else{ #end arousal

              if(Marker=="arousal"){
                Marker <- "euthermic"

                t.event <- t.eu

                E.event <- t.event*(RMR+C.eu*(T.lc-T.a[time.mark]))

              }else{ #end euthermic

                if(Marker=="euthermic"){
                  Marker <- "cooling"

                  CR <- C.eu*(M.body^0.67)*log10(T.eu-T.a[time.mark])/(S*M.body)

                  t.event <- log10(T.eu-T.a[time.mark])/CR #Time for event

                  E.event <- t.event*(TMR.min+(RMR*Q10.cool^((T.a[time.mark]-T.eu)/10)))

                }}}} #end cooling and elses


          TIME[t] <- TIME[t-1] + t.event
          E.hib[t] <- E.hib[t-1] + E.event

          # if(E.hib[t]<=E.winter){t.die <- TIME[t]/24} #Will stop updating t.die when energy required is greater than what is available

          if(TIME[t]>=length(T.a)) break

        }#end hibernation

        #Add in a final arousal if necessary
        if(Marker=="cooling" || Marker=="torpor"){
          time.mark <- length(T.a)
          t.event <- (T.eu-T.a[time.mark])/WR

          E.event <- S*(T.eu-T.a[time.mark])+t.event*(C.eu*(T.eu-T.a[time.mark]))

          arousal.count <- arousal.count+1


          TIME[t+1] <- TIME[t] + t.event
          E.hib[t+1] <- E.hib[t] + E.event
        } #end conditional final arousal

        TIME <- TIME[which(TIME!="NA")]
        E.hib <- E.hib[which(TIME!="NA")]
        E.reserve <- E.winter-E.hib[length(E.hib)]
        survive <- ifelse(E.reserve >=0, 1, 0)



        vec.logger <- append(vec.logger,c(E.hib[length(E.hib)],
                                          E.reserve,
                                          arousal.count,
                                          survive))



    ################################
    #Repeat with Pd
    ################################


    #Loop through energetic evaluation for each start and end date


        t.start <- start.date[a]
        t.end <- end.date[b]

        #specify hourly temperature and humidity deficit starting at t.start
        T.a <- rep(df.bat$Temp[which(df.bat$Time==t.start):which(df.bat$Time==t.end)],each=2)
        RH <- rep(df.bat$RH[which(df.bat$Time==t.start):which(df.bat$Time==t.end)],each=2)
        d.WVP <- rep(d.WVP.hold[which(df.bat$Time==t.start):which(df.bat$Time==t.end)],each=2)
        WVP.a.hib <- rep(WVP.a[which(df.bat$Time==t.start):which(df.bat$Time==t.end)],each=2)
        WVP.bat.hib <- rep(WVP.bat[which(df.bat$Time==t.start):which(df.bat$Time==t.end)],each=2)

        #Calculate energy to get from start to end based on Haase2019
        X <- 10000 #potential events
        TIME <- rep(NA, X)

        E.hib <- rep(NA, X) #Keep track of energy expenditure

        Pd <- 0 #start with Pd=0 and add more with each time step

        #First event is cooling down into torpor
        CR <- C.eu*(M.body^0.67)*log10(T.eu-T.a[1])/(S*M.body)

        t.event <- log10(T.eu-T.a[1])/CR #Time for event

        TIME[1] <- 0
        TIME[2] <- t.event #Track timeline

        E.event <- t.event*(TMR.min+(RMR*Q10.cool^((T.a[1]-T.eu)/10)))
        E.hib[1] <- 0
        E.hib[2] <- E.event

        Pd <- Pd + max(B.1.Pd*(max(T.a[1],T.tormin)-T.min)*(1-exp(B.2.Pd*(max(T.a[1],T.tormin)-T.max)))*(mu.1.Pd*RH[1]/(1+mu.2.Pd*RH[1])),0)*t.event

        Marker <- "cooling" #Start with bat cooling to hibernation mode
        arousal.count <- 0


        for(t in 3:X){

          time.mark <- round(TIME[t-1], digits=0) #current time stamp relative to T.a/d.WVP
          if(time.mark>=length(T.a)) break #break if we reach the end of hibernation

          if(Marker=="cooling"){
            Marker <- "torpor"

            E.bout <- 0 #Set an energy recording for this torpor bout
            CEWL <- 0
            REWL <- 0
            t.event <- 0
            for(j in 1:10000){ #Determine torpor length based on hourly conditions
              #Temperature-based arousal
              Temp.torpor <- mean(T.a[time.mark:(time.mark+j)]) #torpor bouts proceed in 1 hour increments

              if(Temp.torpor > T.tormin){
                Q10 <- 1.6+0.26*Temp.torpor-0.006*Temp.torpor^2
                t.tor.TMR <- t.tormax/(Q10^((Temp.torpor-T.tormin)/10))
                #assume a 30% reduction in energy use when clustered in torpor
                E.hour <- (TMR.min*Q10^((Temp.torpor-T.tormin)/10))
              }else{
                t.tor.TMR <- (t.tormax/(1+((T.tormin-Temp.torpor)*(C.t/TMR.min))))/max(Pd,1)
                #assume a 30% reduction in energy use when clustered in torpor
                E.hour <- (TMR.min+(T.tormin-Temp.torpor)*C.t)
              }

              #Humidity-based arousal using CEWL and REWL rates averaged over the torpor
              #assume a 30% reduction in cutaneous evaporative water loss when clustered in torpor
              CEWL <- ((SA.body*rEWL.body+SA.wing*(rEWL.wing+0.16*min(Pd/SA.wing,1)*100))*d.WVP[time.mark+j] + CEWL*(j-1))/(j)
              rav <- (TMR.min+0.015*min(Pd/SA.wing,1)*100)*M.body/(0.2095*0.30*10^3)
              Sat.deficit <- ((WVP.bat.hib[time.mark+j]*0.00986923)/(0.0821*(max(T.a[time.mark+j],T.tormin)+273.15)))*18015.28 - ((RH[time.mark+j]*WVP.a.hib[time.mark+j]*0.00986923)/(0.0821*(max(T.a[time.mark+j],T.tormin)+273.15)))*18015.28
              REWL <- (rav*Sat.deficit + REWL*(j-1))/(j)

              t.tor.EWL <- 0.027*M.lean*1000/(CEWL+REWL)

              E.bout <- E.bout+E.hour
              t.event <- t.event+1

              #Add more fungus each hour
              Pd <- Pd + max(B.1.Pd*(max(T.a[time.mark+j],T.tormin)-T.min)*(1-exp(B.2.Pd*(max(T.a[time.mark+j],T.tormin)-T.max)))*(mu.1.Pd*RH[time.mark+j]*100/(1+mu.2.Pd*RH[time.mark+j]*100)),0)*1

              if(t.tor.TMR<=j) break
              if(t.tor.EWL<=j) break
              if (TIME[t-1]+j+1>=length(T.a)) break
            }

            E.event <- E.bout

          }else{ #end torpor


            if(Marker=="torpor"){
              Marker <- "arousal"

              t.event <- (T.eu-T.a[time.mark])/WR

              E.event <- S*(T.eu-T.a[time.mark])+t.event*(C.eu*(T.eu-T.a[time.mark]))

              arousal.count <- arousal.count+1

              Pd <- Pd + max(B.1.Pd*(max(T.a[time.mark],T.tormin)-T.min)*(1-exp(B.2.Pd*(max(T.a[time.mark],T.tormin)-T.max)))*(mu.1.Pd*RH[time.mark]/(1+mu.2.Pd*RH[time.mark])),0)*t.event

            }else{ #end arousal

              if(Marker=="arousal"){
                Marker <- "euthermic"

                t.event <- t.eu

                E.event <- t.event*(RMR+C.eu*(T.lc-T.a[time.mark]))


                Pd <- Pd + max(B.1.Pd*(T.eu-T.min)*(1-exp(B.2.Pd*(T.eu-T.max)))*(mu.1.Pd*RH[time.mark]/(1+mu.2.Pd*RH[time.mark])),0)*t.event
                Pd <- ifelse(Pd<0, 0, Pd)
              }else{ #end euthermic

                if(Marker=="euthermic"){
                  Marker <- "cooling"

                  CR <- C.eu*(M.body^0.67)*log10(T.eu-T.a[time.mark])/(S*M.body)

                  t.event <- log10(T.eu-T.a[time.mark])/CR #Time for event

                  E.event <- t.event*(TMR.min+(RMR*Q10.cool^((T.a[time.mark]-T.eu)/10)))

                  Pd <- Pd + max(B.1.Pd*(max(T.a[time.mark],T.tormin)-T.min)*(1-exp(B.2.Pd*(max(T.a[time.mark],T.tormin)-T.max)))*(mu.1.Pd*RH[time.mark]/(1+mu.2.Pd*RH[time.mark])),0)*t.event

                }}}} #end cooling and elses


          TIME[t] <- TIME[t-1] + t.event
          E.hib[t] <- E.hib[t-1] + E.event

          # if(E.hib[t]<=E.winter){t.die <- TIME[t]/24} #Will stop updating t.die when energy required is greater than what is available

          if(TIME[t]>=length(T.a)) break

        }#end hibernation

        #Add in a final arousal if necessary
        if(Marker=="cooling" || Marker=="torpor"){
          time.mark <- length(T.a)
          t.event <- (T.eu-T.a[time.mark])/WR

          E.event <- S*(T.eu-T.a[time.mark])+t.event*(C.eu*(T.eu-T.a[time.mark]))

          arousal.count <- arousal.count+1


          TIME[t+1] <- TIME[t] + t.event
          E.hib[t+1] <- E.hib[t] + E.event
        } #end conditional final arousal

        TIME <- TIME[which(TIME!="NA")]
        E.hib <- E.hib[which(TIME!="NA")]
        E.reserve <- E.winter-E.hib[length(E.hib)]
        survive <- ifelse(E.reserve>=0, 1, 0)

        #Single logger vector output
        vec.logger <- append(vec.logger,c(E.hib[length(E.hib)],
                                          E.reserve,
                                          arousal.count,
                                          survive))










        ################################
        #Repeat with Pd and clustering
        ################################


        #Loop through energetic evaluation for each start and end date


        t.start <- start.date[a]
        t.end <- end.date[b]

        #specify hourly temperature and humidity deficit starting at t.start
        T.a <- rep(df.bat$Temp[which(df.bat$Time==t.start):which(df.bat$Time==t.end)],each=2)
        RH <- rep(df.bat$RH[which(df.bat$Time==t.start):which(df.bat$Time==t.end)],each=2)
        d.WVP <- rep(d.WVP.hold[which(df.bat$Time==t.start):which(df.bat$Time==t.end)],each=2)
        WVP.a.hib <- rep(WVP.a[which(df.bat$Time==t.start):which(df.bat$Time==t.end)],each=2)
        WVP.bat.hib <- rep(WVP.bat[which(df.bat$Time==t.start):which(df.bat$Time==t.end)],each=2)

        #Calculate energy to get from start to end based on Haase2019
        X <- 10000 #potential events
        TIME <- rep(NA, X)

        E.hib <- rep(NA, X) #Keep track of energy expenditure

        Pd <- 0 #start with Pd=0 and add more with each time step

        #First event is cooling down into torpor
        CR <- C.eu*(M.body^0.67)*log10(T.eu-T.a[1])/(S*M.body)

        t.event <- log10(T.eu-T.a[1])/CR #Time for event

        TIME[1] <- 0
        TIME[2] <- t.event #Track timeline

        E.event <- t.event*(TMR.min+(RMR*Q10.cool^((T.a[1]-T.eu)/10)))
        E.hib[1] <- 0
        E.hib[2] <- E.event

        Pd <- Pd + max(B.1.Pd*(max(T.a[1],T.tormin)-T.min)*(1-exp(B.2.Pd*(max(T.a[1],T.tormin)-T.max)))*(mu.1.Pd*RH[1]/(1+mu.2.Pd*RH[1])),0)*t.event

        Marker <- "cooling" #Start with bat cooling to hibernation mode
        arousal.count <- 0


        for(t in 3:X){

          time.mark <- round(TIME[t-1], digits=0) #current time stamp relative to T.a/d.WVP
          if(time.mark>=length(T.a)) break #break if we reach the end of hibernation

          if(Marker=="cooling"){
            Marker <- "torpor"

            E.bout <- 0 #Set an energy recording for this torpor bout
            CEWL <- 0
            REWL <- 0
            t.event <- 0
            for(j in 1:10000){ #Determine torpor length based on hourly conditions
              #Temperature-based arousal
              Temp.torpor <- mean(T.a[time.mark:(time.mark+j)]) #torpor bouts proceed in 1 hour increments

              if(Temp.torpor > T.tormin){
                Q10 <- 1.6+0.26*Temp.torpor-0.006*Temp.torpor^2
                t.tor.TMR <- t.tormax/(Q10^((Temp.torpor-T.tormin)/10)*clusterfac)
                #assume a 30% reduction in energy use when clustered in torpor
                E.hour <- (TMR.min*Q10^((Temp.torpor-T.tormin)/10))*clusterfac
              }else{
                t.tor.TMR <- (t.tormax/(1+((T.tormin-Temp.torpor)*(C.t/TMR.min)*clusterfac)))/max(Pd,1)
                #assume a 30% reduction in energy use when clustered in torpor
                E.hour <- (TMR.min+(T.tormin-Temp.torpor)*C.t)*clusterfac
              }

              #Humidity-based arousal using CEWL and REWL rates averaged over the torpor
              #assume a 30% reduction in cutaneous evaporative water loss when clustered in torpor
              CEWL <- ((SA.body*rEWL.body+SA.wing*(rEWL.wing+0.16*min(Pd/SA.wing,1)*100))*d.WVP[time.mark+j]*clusterfac + CEWL*(j-1))/(j)
              rav <- (TMR.min+0.015*min(Pd/SA.wing,1)*100)*M.body/(0.2095*0.30*10^3)
              Sat.deficit <- ((WVP.bat.hib[time.mark+j]*0.00986923)/(0.0821*(max(T.a[time.mark+j],T.tormin)+273.15)))*18015.28 - ((RH[time.mark+j]*WVP.a.hib[time.mark+j]*0.00986923)/(0.0821*(max(T.a[time.mark+j],T.tormin)+273.15)))*18015.28
              REWL <- (rav*Sat.deficit + REWL*(j-1))/(j)

              t.tor.EWL <- 0.027*M.lean*1000/(CEWL+REWL)

              E.bout <- E.bout+E.hour
              t.event <- t.event+1

              #Add more fungus each hour
              Pd <- Pd + max(B.1.Pd*(max(T.a[time.mark+j],T.tormin)-T.min)*(1-exp(B.2.Pd*(max(T.a[time.mark+j],T.tormin)-T.max)))*(mu.1.Pd*RH[time.mark+j]*100/(1+mu.2.Pd*RH[time.mark+j]*100)),0)*1

              if(t.tor.TMR<=j) break
              if(t.tor.EWL<=j) break
              if (TIME[t-1]+j+1>=length(T.a)) break
            }

            E.event <- E.bout

          }else{ #end torpor


            if(Marker=="torpor"){
              Marker <- "arousal"

              t.event <- (T.eu-T.a[time.mark])/WR

              E.event <- S*(T.eu-T.a[time.mark])+t.event*(C.eu*(T.eu-T.a[time.mark]))

              arousal.count <- arousal.count+1

              Pd <- Pd + max(B.1.Pd*(max(T.a[time.mark],T.tormin)-T.min)*(1-exp(B.2.Pd*(max(T.a[time.mark],T.tormin)-T.max)))*(mu.1.Pd*RH[time.mark]/(1+mu.2.Pd*RH[time.mark])),0)*t.event

            }else{ #end arousal

              if(Marker=="arousal"){
                Marker <- "euthermic"

                t.event <- t.eu

                E.event <- t.event*(RMR+C.eu*(T.lc-T.a[time.mark]))


                Pd <- Pd + max(B.1.Pd*(T.eu-T.min)*(1-exp(B.2.Pd*(T.eu-T.max)))*(mu.1.Pd*RH[time.mark]/(1+mu.2.Pd*RH[time.mark])),0)*t.event
              }else{ #end euthermic

                if(Marker=="euthermic"){
                  Marker <- "cooling"

                  CR <- C.eu*(M.body^0.67)*log10(T.eu-T.a[time.mark])/(S*M.body)

                  t.event <- log10(T.eu-T.a[time.mark])/CR #Time for event

                  E.event <- t.event*(TMR.min+(RMR*Q10.cool^((T.a[time.mark]-T.eu)/10)))

                  Pd <- Pd + max(B.1.Pd*(max(T.a[time.mark],T.tormin)-T.min)*(1-exp(B.2.Pd*(max(T.a[time.mark],T.tormin)-T.max)))*(mu.1.Pd*RH[time.mark]/(1+mu.2.Pd*RH[time.mark])),0)*t.event

                }}}} #end cooling and elses


          TIME[t] <- TIME[t-1] + t.event
          E.hib[t] <- E.hib[t-1] + E.event

          # if(E.hib[t]<=E.winter){t.die <- TIME[t]/24} #Will stop updating t.die when energy required is greater than what is available

          if(TIME[t]>=length(T.a)) break

        }#end hibernation

        #Add in a final arousal if necessary
        if(Marker=="cooling" || Marker=="torpor"){
          time.mark <- length(T.a)
          t.event <- (T.eu-T.a[time.mark])/WR

          E.event <- S*(T.eu-T.a[time.mark])+t.event*(C.eu*(T.eu-T.a[time.mark]))

          arousal.count <- arousal.count+1


          TIME[t+1] <- TIME[t] + t.event
          E.hib[t+1] <- E.hib[t] + E.event
        } #end conditional final arousal

        TIME <- TIME[which(TIME!="NA")]
        E.hib <- E.hib[which(TIME!="NA")]
        E.reserve <- E.winter-E.hib[length(E.hib)]
        survive <- ifelse(E.reserve>=0, 1, 0)

        #Single logger vector output
        vec.logger <- append(vec.logger,c(E.hib[length(E.hib)],
                                          E.reserve,
                                          arousal.count,
                                          survive))







        ################################
        #Repeat with Pd and grooming
        ################################


        #Loop through energetic evaluation for each start and end date


        t.start <- start.date[a]
        t.end <- end.date[b]

        #specify hourly temperature and humidity deficit starting at t.start
        T.a <- rep(df.bat$Temp[which(df.bat$Time==t.start):which(df.bat$Time==t.end)],each=2)
        RH <- rep(df.bat$RH[which(df.bat$Time==t.start):which(df.bat$Time==t.end)],each=2)
        d.WVP <- rep(d.WVP.hold[which(df.bat$Time==t.start):which(df.bat$Time==t.end)],each=2)
        WVP.a.hib <- rep(WVP.a[which(df.bat$Time==t.start):which(df.bat$Time==t.end)],each=2)
        WVP.bat.hib <- rep(WVP.bat[which(df.bat$Time==t.start):which(df.bat$Time==t.end)],each=2)

        #Calculate energy to get from start to end based on Haase2019
        X <- 10000 #potential events
        TIME <- rep(NA, X)

        E.hib <- rep(NA, X) #Keep track of energy expenditure

        Pd <- 0 #start with Pd=0 and add more with each time step

        #First event is cooling down into torpor
        CR <- C.eu*(M.body^0.67)*log10(T.eu-T.a[1])/(S*M.body)

        t.event <- log10(T.eu-T.a[1])/CR #Time for event

        TIME[1] <- 0
        TIME[2] <- t.event #Track timeline

        E.event <- t.event*(TMR.min+(RMR*Q10.cool^((T.a[1]-T.eu)/10)))
        E.hib[1] <- 0
        E.hib[2] <- E.event

        Pd <- Pd + max(B.1.Pd*(max(T.a[1],T.tormin)-T.min)*(1-exp(B.2.Pd*(max(T.a[1],T.tormin)-T.max)))*(mu.1.Pd*RH[1]/(1+mu.2.Pd*RH[1])),0)*t.event

        Marker <- "cooling" #Start with bat cooling to hibernation mode
        arousal.count <- 0


        for(t in 3:X){

          time.mark <- round(TIME[t-1], digits=0) #current time stamp relative to T.a/d.WVP
          if(time.mark>=length(T.a)) break #break if we reach the end of hibernation

          if(Marker=="cooling"){
            Marker <- "torpor"

            E.bout <- 0 #Set an energy recording for this torpor bout
            CEWL <- 0
            REWL <- 0
            t.event <- 0
            for(j in 1:10000){ #Determine torpor length based on hourly conditions
              #Temperature-based arousal
              Temp.torpor <- mean(T.a[time.mark:(time.mark+j)]) #torpor bouts proceed in 1 hour increments

              if(Temp.torpor > T.tormin){
                Q10 <- 1.6+0.26*Temp.torpor-0.006*Temp.torpor^2
                t.tor.TMR <- t.tormax/(Q10^((Temp.torpor-T.tormin)/10))
                #assume a 30% reduction in energy use when clustered in torpor
                E.hour <- (TMR.min*Q10^((Temp.torpor-T.tormin)/10))
              }else{
                t.tor.TMR <- (t.tormax/(1+((T.tormin-Temp.torpor)*(C.t/TMR.min))))/max(Pd,1)
                #assume a 30% reduction in energy use when clustered in torpor
                E.hour <- (TMR.min+(T.tormin-Temp.torpor)*C.t)
              }

              #Humidity-based arousal using CEWL and REWL rates averaged over the torpor
              #assume a 30% reduction in cutaneous evaporative water loss when clustered in torpor
              CEWL <- ((SA.body*rEWL.body+SA.wing*(rEWL.wing+0.16*min(Pd/SA.wing,1)*100))*d.WVP[time.mark+j] + CEWL*(j-1))/(j)
              rav <- (TMR.min+0.015*min(Pd/SA.wing,1)*100)*M.body/(0.2095*0.30*10^3)
              Sat.deficit <- ((WVP.bat.hib[time.mark+j]*0.00986923)/(0.0821*(max(T.a[time.mark+j],T.tormin)+273.15)))*18015.28 - ((RH[time.mark+j]*WVP.a.hib[time.mark+j]*0.00986923)/(0.0821*(max(T.a[time.mark+j],T.tormin)+273.15)))*18015.28
              REWL <- (rav*Sat.deficit + REWL*(j-1))/(j)

              t.tor.EWL <- 0.027*M.lean*1000/(CEWL+REWL)

              E.bout <- E.bout+E.hour
              t.event <- t.event+1

              #Add more fungus each hour
              Pd <- Pd + max(B.1.Pd*(max(T.a[time.mark+j],T.tormin)-T.min)*(1-exp(B.2.Pd*(max(T.a[time.mark+j],T.tormin)-T.max)))*(mu.1.Pd*RH[time.mark+j]*100/(1+mu.2.Pd*RH[time.mark+j]*100)),0)*1

              if(t.tor.TMR<=j) break
              if(t.tor.EWL<=j) break
              if (TIME[t-1]+j+1>=length(T.a)) break
            }

            E.event <- E.bout

          }else{ #end torpor


            if(Marker=="torpor"){
              Marker <- "arousal"

              t.event <- (T.eu-T.a[time.mark])/WR

              E.event <- S*(T.eu-T.a[time.mark])+t.event*(C.eu*(T.eu-T.a[time.mark]))

              arousal.count <- arousal.count+1

              Pd <- Pd + max(B.1.Pd*(max(T.a[time.mark],T.tormin)-T.min)*(1-exp(B.2.Pd*(max(T.a[time.mark],T.tormin)-T.max)))*(mu.1.Pd*RH[time.mark]/(1+mu.2.Pd*RH[time.mark])),0)*t.event

            }else{ #end arousal

              if(Marker=="arousal"){
                Marker <- "euthermic"

                t.event <- t.eu

                E.event <- t.event*(RMR+C.eu*(T.lc-T.a[time.mark]))


                Pd <- Pd + max(B.1.Pd*(T.eu-T.min)*(1-exp(B.2.Pd*(T.eu-T.max)))*(mu.1.Pd*RH[time.mark]/(1+mu.2.Pd*RH[time.mark])),0)*t.event - groom*t.eu*p.groom
                Pd <- ifelse(Pd<0, 0, Pd)
              }else{ #end euthermic

                if(Marker=="euthermic"){
                  Marker <- "cooling"

                  CR <- C.eu*(M.body^0.67)*log10(T.eu-T.a[time.mark])/(S*M.body)

                  t.event <- log10(T.eu-T.a[time.mark])/CR #Time for event

                  E.event <- t.event*(TMR.min+(RMR*Q10.cool^((T.a[time.mark]-T.eu)/10)))

                  Pd <- Pd + max(B.1.Pd*(max(T.a[time.mark],T.tormin)-T.min)*(1-exp(B.2.Pd*(max(T.a[time.mark],T.tormin)-T.max)))*(mu.1.Pd*RH[time.mark]/(1+mu.2.Pd*RH[time.mark])),0)*t.event

                }}}} #end cooling and elses


          TIME[t] <- TIME[t-1] + t.event
          E.hib[t] <- E.hib[t-1] + E.event

          # if(E.hib[t]<=E.winter){t.die <- TIME[t]/24} #Will stop updating t.die when energy required is greater than what is available

          if(TIME[t]>=length(T.a)) break

        }#end hibernation

        #Add in a final arousal if necessary
        if(Marker=="cooling" || Marker=="torpor"){
          time.mark <- length(T.a)
          t.event <- (T.eu-T.a[time.mark])/WR

          E.event <- S*(T.eu-T.a[time.mark])+t.event*(C.eu*(T.eu-T.a[time.mark]))

          arousal.count <- arousal.count+1


          TIME[t+1] <- TIME[t] + t.event
          E.hib[t+1] <- E.hib[t] + E.event
        } #end conditional final arousal

        TIME <- TIME[which(TIME!="NA")]
        E.hib <- E.hib[which(TIME!="NA")]
        E.reserve <- E.winter-E.hib[length(E.hib)]
        survive <- ifelse(E.reserve>=0, 1, 0)

        #Single logger vector output
        vec.logger <- append(vec.logger,c(E.hib[length(E.hib)],
                                          E.reserve,
                                          arousal.count,
                                          survive))



        ################################
        #Repeat with Pd and clustering and grooming
        ################################


        #Loop through energetic evaluation for each start and end date


        t.start <- start.date[a]
        t.end <- end.date[b]

        #specify hourly temperature and humidity deficit starting at t.start
        T.a <- rep(df.bat$Temp[which(df.bat$Time==t.start):which(df.bat$Time==t.end)],each=2)
        RH <- rep(df.bat$RH[which(df.bat$Time==t.start):which(df.bat$Time==t.end)],each=2)
        d.WVP <- rep(d.WVP.hold[which(df.bat$Time==t.start):which(df.bat$Time==t.end)],each=2)
        WVP.a.hib <- rep(WVP.a[which(df.bat$Time==t.start):which(df.bat$Time==t.end)],each=2)
        WVP.bat.hib <- rep(WVP.bat[which(df.bat$Time==t.start):which(df.bat$Time==t.end)],each=2)

        #Calculate energy to get from start to end based on Haase2019
        X <- 10000 #potential events
        TIME <- rep(NA, X)

        E.hib <- rep(NA, X) #Keep track of energy expenditure

        Pd <- 0 #start with Pd=0 and add more with each time step

        #First event is cooling down into torpor
        CR <- C.eu*(M.body^0.67)*log10(T.eu-T.a[1])/(S*M.body)

        t.event <- log10(T.eu-T.a[1])/CR #Time for event

        TIME[1] <- 0
        TIME[2] <- t.event #Track timeline

        E.event <- t.event*(TMR.min+(RMR*Q10.cool^((T.a[1]-T.eu)/10)))
        E.hib[1] <- 0
        E.hib[2] <- E.event

        Pd <- Pd + max(B.1.Pd*(max(T.a[1],T.tormin)-T.min)*(1-exp(B.2.Pd*(max(T.a[1],T.tormin)-T.max)))*(mu.1.Pd*RH[1]/(1+mu.2.Pd*RH[1])),0)*t.event

        Marker <- "cooling" #Start with bat cooling to hibernation mode
        arousal.count <- 0


        for(t in 3:X){

          time.mark <- round(TIME[t-1], digits=0) #current time stamp relative to T.a/d.WVP
          if(time.mark>=length(T.a)) break #break if we reach the end of hibernation

          if(Marker=="cooling"){
            Marker <- "torpor"

            E.bout <- 0 #Set an energy recording for this torpor bout
            CEWL <- 0
            REWL <- 0
            t.event <- 0
            for(j in 1:10000){ #Determine torpor length based on hourly conditions
              #Temperature-based arousal
              Temp.torpor <- mean(T.a[time.mark:(time.mark+j)]) #torpor bouts proceed in 1 hour increments

              if(Temp.torpor > T.tormin){
                Q10 <- 1.6+0.26*Temp.torpor-0.006*Temp.torpor^2
                t.tor.TMR <- t.tormax/(Q10^((Temp.torpor-T.tormin)/10)*clusterfac)
                #assume a 30% reduction in energy use when clustered in torpor
                E.hour <- (TMR.min*Q10^((Temp.torpor-T.tormin)/10))*clusterfac
              }else{
                t.tor.TMR <- (t.tormax/(1+((T.tormin-Temp.torpor)*(C.t/TMR.min)*clusterfac)))/max(Pd,1)
                #assume a 30% reduction in energy use when clustered in torpor
                E.hour <- (TMR.min+(T.tormin-Temp.torpor)*C.t)*clusterfac
              }

              #Humidity-based arousal using CEWL and REWL rates averaged over the torpor
              #assume a 30% reduction in cutaneous evaporative water loss when clustered in torpor
              CEWL <- ((SA.body*rEWL.body+SA.wing*(rEWL.wing+0.16*min(Pd/SA.wing,1)*100))*d.WVP[time.mark+j]*clusterfac + CEWL*(j-1))/(j)
              rav <- (TMR.min+0.015*min(Pd/SA.wing,1)*100)*M.body/(0.2095*0.30*10^3)
              Sat.deficit <- ((WVP.bat.hib[time.mark+j]*0.00986923)/(0.0821*(max(T.a[time.mark+j],T.tormin)+273.15)))*18015.28 - ((RH[time.mark+j]*WVP.a.hib[time.mark+j]*0.00986923)/(0.0821*(max(T.a[time.mark+j],T.tormin)+273.15)))*18015.28
              REWL <- (rav*Sat.deficit + REWL*(j-1))/(j)

              t.tor.EWL <- 0.027*M.lean*1000/(CEWL+REWL)

              E.bout <- E.bout+E.hour
              t.event <- t.event+1

              #Add more fungus each hour
              Pd <- Pd + max(B.1.Pd*(max(T.a[time.mark+j],T.tormin)-T.min)*(1-exp(B.2.Pd*(max(T.a[time.mark+j],T.tormin)-T.max)))*(mu.1.Pd*RH[time.mark+j]*100/(1+mu.2.Pd*RH[time.mark+j]*100)),0)*1

              if(t.tor.TMR<=j) break
              if(t.tor.EWL<=j) break
              if (TIME[t-1]+j+1>=length(T.a)) break
            }

            E.event <- E.bout

          }else{ #end torpor


            if(Marker=="torpor"){
              Marker <- "arousal"

              t.event <- (T.eu-T.a[time.mark])/WR

              E.event <- S*(T.eu-T.a[time.mark])+t.event*(C.eu*(T.eu-T.a[time.mark]))

              arousal.count <- arousal.count+1

              Pd <- Pd + max(B.1.Pd*(max(T.a[time.mark],T.tormin)-T.min)*(1-exp(B.2.Pd*(max(T.a[time.mark],T.tormin)-T.max)))*(mu.1.Pd*RH[time.mark]/(1+mu.2.Pd*RH[time.mark])),0)*t.event

            }else{ #end arousal

              if(Marker=="arousal"){
                Marker <- "euthermic"

                t.event <- t.eu

                E.event <- t.event*(RMR+C.eu*(T.lc-T.a[time.mark]))


                Pd <- Pd + max(B.1.Pd*(T.eu-T.min)*(1-exp(B.2.Pd*(T.eu-T.max)))*(mu.1.Pd*RH[time.mark]/(1+mu.2.Pd*RH[time.mark])),0)*t.event - groom*t.eu*p.groom
                Pd <- ifelse(Pd<0, 0, Pd)
              }else{ #end euthermic

                if(Marker=="euthermic"){
                  Marker <- "cooling"

                  CR <- C.eu*(M.body^0.67)*log10(T.eu-T.a[time.mark])/(S*M.body)

                  t.event <- log10(T.eu-T.a[time.mark])/CR #Time for event

                  E.event <- t.event*(TMR.min+(RMR*Q10.cool^((T.a[time.mark]-T.eu)/10)))

                  Pd <- Pd + max(B.1.Pd*(max(T.a[time.mark],T.tormin)-T.min)*(1-exp(B.2.Pd*(max(T.a[time.mark],T.tormin)-T.max)))*(mu.1.Pd*RH[time.mark]/(1+mu.2.Pd*RH[time.mark])),0)*t.event

                }}}} #end cooling and elses


          TIME[t] <- TIME[t-1] + t.event
          E.hib[t] <- E.hib[t-1] + E.event

          # if(E.hib[t]<=E.winter){t.die <- TIME[t]/24} #Will stop updating t.die when energy required is greater than what is available

          if(TIME[t]>=length(T.a)) break

        }#end hibernation

        #Add in a final arousal if necessary
        if(Marker=="cooling" || Marker=="torpor"){
          time.mark <- length(T.a)
          t.event <- (T.eu-T.a[time.mark])/WR

          E.event <- S*(T.eu-T.a[time.mark])+t.event*(C.eu*(T.eu-T.a[time.mark]))

          arousal.count <- arousal.count+1


          TIME[t+1] <- TIME[t] + t.event
          E.hib[t+1] <- E.hib[t] + E.event
        } #end conditional final arousal

        TIME <- TIME[which(TIME!="NA")]
        E.hib <- E.hib[which(TIME!="NA")]
        E.reserve <- E.winter-E.hib[length(E.hib)]
        survive <- ifelse(E.reserve>=0, 1, 0)

        #Single logger vector output
        vec.logger <- append(vec.logger,c(E.hib[length(E.hib)],
                                          E.reserve,
                                          arousal.count,
                                          survive))

        #Combine loggers to form vector
        df.logger <- rbind(df.logger, vec.logger)

      }#end start date loop
    }#end end date loop


    #Data frame for single logger, multiple start/end dates
    df.logger <- as.data.frame(df.logger)
    df.logger <- df.logger[-1,]

    colnames(df.logger) <- c("Site",
                             "E.hib",
                             "E.reserve",
                             "arousal.count",
                             "start",
                             "end",
                             "survive",
                             "is.MYLU",
                             "E.hib.clust",
                             "E.reserve.clust",
                             "arousal.count.clust",
                             "survive.clust",
                             "E.hib.Pd",
                             "E.reserve.Pd",
                             "arousal.count.Pd",
                             "survive.Pd",
                             "E.hib.Pd.clust",
                             "E.reserve.Pd.clust",
                             "arousal.count.Pd.clust",
                             "survive.Pd.clust",
                             "E.hib.Pd.groom",
                             "E.reserve.Pd.groom",
                             "arousal.count.Pd.groom",
                             "survive.Pd.groom",
                             "E.hib.Pd.clust.groom",
                             "E.reserve.Pd.clust.groom",
                             "arousal.count.Pd.clust.groom",
                             "survive.Pd.clust.groom"
                             )
    df.logger$start <- as.POSIXct(df.logger$start, format="%Y-%m-%d %H", tz="")
    df.logger$end <- as.POSIXct(df.logger$end, format="%Y-%m-%d %H", tz="")





    #############################
    #Visualize results
    #######################

    # p1 <- ggplot(data=df.logger) + theme_minimal() +
    #   geom_tile(aes(start, end, fill=as.numeric(as.character(E.reserve))*19.6/376000)) +
    #   scale_fill_gradient2(low="#833437",mid="#8F8093",high="#67B9E9",midpoint=0) +
    #   ggtitle(unique(df.trim$ID[loc])) +
    #   xlab("Hibernation start date") +
    #   ylab("Hibernation end date") +
    #   guides(fill=guide_legend(title="Energy residual\n(g fat)"))
    #
    #
    # p2 <- ggplot(data=df.logger.Pd) + theme_minimal() +
    #   geom_tile(aes(start, end, fill=as.numeric(as.character(E.reserve))*19.6/376000)) +
    #   scale_fill_gradient2(low="#833437",mid="#8F8093",high="#67B9E9",midpoint=0) +
    #   ggtitle(paste(unique(df.trim$ID[loc])," with Pd")) +
    #   xlab("Hibernation start date") +
    #   ylab("Hibernation end date") +
    #   guides(fill=guide_legend(title="Energy residual\n(g fat)"))

    # print(plot_grid(p1,p2))


    df.all.loggers <- rbind(df.all.loggers,df.logger)

  }#end per logger forloop

  df.all.loggers <- df.all.loggers[-1,]

  TP <- length(which(df.all.loggers$is.MYLU==1 & df.all.loggers$survive.Pd==1))/length(df.all.loggers$is.MYLU)
  TN <- length(which(df.all.loggers$is.MYLU==0 & df.all.loggers$survive.Pd==0))/length(df.all.loggers$is.MYLU)
  FP <- length(which(df.all.loggers$is.MYLU==0 & df.all.loggers$survive.Pd==1))/length(df.all.loggers$is.MYLU)
  FN <- length(which(df.all.loggers$is.MYLU==1 & df.all.loggers$survive.Pd==0))/length(df.all.loggers$is.MYLU)

  specif <- TN/(TN+FP)
  sensit <- TP/(TP+FN)

  summary_output <- c(TP, TN, FP, FN, specif, sensit)

  # add summary output to df.all.loggers as repeated values to fit into data
  # frame output

  df.all.loggers$TP <- rep(TP,nrow(df.all.loggers))
  df.all.loggers$TN <- rep(TN,nrow(df.all.loggers))
  df.all.loggers$FP <- rep(FP,nrow(df.all.loggers))
  df.all.loggers$FN <- rep(FN,nrow(df.all.loggers))
  df.all.loggers$specif <- rep(specif,nrow(df.all.loggers))
  df.all.loggers$sensit <- rep(sensit,nrow(df.all.loggers))

  df.all.loggers$M.body <- rep(M.body,nrow(df.all.loggers))
  df.all.loggers$p.fat <- rep(p.fat,nrow(df.all.loggers))
  df.all.loggers$T.tormin <- rep(T.tormin,nrow(df.all.loggers))
  df.all.loggers$t.tormax <- rep(t.tormax,nrow(df.all.loggers))
  df.all.loggers$rEWL.body <- rep(rEWL.body,nrow(df.all.loggers))
  df.all.loggers$TMR.min <- rep(TMR.min,nrow(df.all.loggers))
  df.all.loggers$C.t <- rep(C.t,nrow(df.all.loggers))
  df.all.loggers$clusterfac <- rep(clusterfac,nrow(df.all.loggers))
  df.all.loggers$groom <- rep(groom,nrow(df.all.loggers))
  df.all.loggers$n.run <- rep(n.run,nrow(df.all.loggers))

  return(df.all.loggers)
}#end function



# #######################################################
# # Run function
# #######################################################
# 
# 
# library(doParallel)
# detectCores()
# c <- makeCluster(detectCores()-1)
# registerDoParallel(c)
# getDoParWorkers()
# 
# start_time <- Sys.time()
# 
# df.output <- NA
# LHS <- as.data.frame(LHS)
# df.output <- foreach(n.run = 1:nrow(LHS), .combine=cbind) %dopar% {
#   output <- t(SURVIVAL.TIME(
#     M.body=as.numeric(as.character(LHS$M.body[n.run])),
#     p.fat=as.numeric(as.character(LHS$p.fat[n.run])),
#     T.tormin=as.numeric(as.character(LHS$T.tormin[n.run])),
#     t.tormax=as.numeric(as.character(LHS$t.tormax[n.run])),
#     rEWL.body=as.numeric(as.character(LHS$rEWL.body[n.run])),
#     TMR.min=as.numeric(as.character(LHS$TMR.min[n.run])),
#     C.t=as.numeric(as.character(LHS$C.t[n.run])),
#     clusterfac=as.numeric(as.character(LHS$clusterfac[n.run])),
#     groom=as.numeric(as.character(LHS$groom[n.run]))))
#   return(output)
# }
# df.output <- as.data.frame(t(df.output))
# end_time <- Sys.time()
# end_time - start_time
# 
# 
# setwd("C:/Users/bengo/Documents/WNS/Chapter3")
# write.csv(df.output,"output_Oct01start_ClusterGroom.csv")

##############################################
# Visualize results
##############################################
min.healthy <- 9
max.healthy <- 31
min.survive.Pd <- 6.8
max.survive.Pd <- 21.6
min.die.Pd <- 3.5
max.die.Pd <- 12

n.days <- length(seq(as.Date("2017-10-01"), as.Date("2018-04-15"), by="day"))


rm(dwt,
   dwt.18,
   dww,
   dww.18,
   dwh,
   dwh.18,
   LHS,
   df.pred.trim,
   df.RH.test,
   xycoords,
   test)
gc()

setwd("C:/Users/bengo/Documents/WNS/Chapter3")
df.output <- read.csv("df.fulldata.csv")
n <- 200000
# 
# df.output <- rbind(read.csv("output_Oct01start_ClusterGroom.csv"),
#                    read.csv("output_Oct01start_ClusterGroom_25000.csv"),
#                    read.csv("output_Oct01start_ClusterGroom_50000.csv"),
#                    read.csv("output_Oct01start_ClusterGroom_100000.csv"))
# 
# # # Calculate sensitivities for cluster and cluster/groom scenarios
# df.output$sensit.clust <- rep(NA, nrow(df.output))
# df.output$specif.clust <- rep(NA, nrow(df.output))
# df.output$sensit.groom <- rep(NA, nrow(df.output))
# df.output$specif.groom <- rep(NA, nrow(df.output))
# df.output$sensit.clust.groom <- rep(NA, nrow(df.output))
# df.output$specif.clust.groom <- rep(NA, nrow(df.output))
n.loggers <- length(unique(df.output$Site))
# 
# df.output$n.run <- rep(1:200000, each=n.loggers)
# for(i in 1:max(df.output$n.run)){
#   TP <- length(which(df.output$is.MYLU[((i-1)*n.loggers+1):(i*n.loggers)]==1 & df.output$survive.Pd.clust[((i-1)*n.loggers+1):(i*n.loggers)]==1))/length(df.output$is.MYLU[((i-1)*n.loggers+1):(i*n.loggers)])
#   FN <- length(which(df.output$is.MYLU[((i-1)*n.loggers+1):(i*n.loggers)]==1 & df.output$survive.Pd.clust[((i-1)*n.loggers+1):(i*n.loggers)]==0))/length(df.output$is.MYLU[((i-1)*n.loggers+1):(i*n.loggers)])
#   df.output$sensit.clust[((i-1)*n.loggers+1):(i*n.loggers)] <- rep(TP/(TP+FN), n.loggers)
#   FP <- length(which(df.output$is.MYLU[((i-1)*n.loggers+1):(i*n.loggers)]==0 & df.output$survive.Pd.clust[((i-1)*n.loggers+1):(i*n.loggers)]==1))/length(df.output$is.MYLU[((i-1)*n.loggers+1):(i*n.loggers)])
#   TN <- length(which(df.output$is.MYLU[((i-1)*n.loggers+1):(i*n.loggers)]==0 & df.output$survive.Pd.clust[((i-1)*n.loggers+1):(i*n.loggers)]==0))/length(df.output$is.MYLU[((i-1)*n.loggers+1):(i*n.loggers)])
#   df.output$specif.clust[((i-1)*n.loggers+1):(i*n.loggers)] <- rep(TN/(FP+TN), n.loggers)
# 
#   TP <- length(which(df.output$is.MYLU[((i-1)*n.loggers+1):(i*n.loggers)]==1 & df.output$survive.Pd.groom[((i-1)*n.loggers+1):(i*n.loggers)]==1))/length(df.output$is.MYLU[((i-1)*n.loggers+1):(i*n.loggers)])
#   FN <- length(which(df.output$is.MYLU[((i-1)*n.loggers+1):(i*n.loggers)]==1 & df.output$survive.Pd.groom[((i-1)*n.loggers+1):(i*n.loggers)]==0))/length(df.output$is.MYLU[((i-1)*n.loggers+1):(i*n.loggers)])
#   df.output$sensit.groom[((i-1)*n.loggers+1):(i*n.loggers)] <- rep(TP/(TP+FN), n.loggers)
#   FP <- length(which(df.output$is.MYLU[((i-1)*n.loggers+1):(i*n.loggers)]==0 & df.output$survive.Pd.groom[((i-1)*n.loggers+1):(i*n.loggers)]==1))/length(df.output$is.MYLU[((i-1)*n.loggers+1):(i*n.loggers)])
#   TN <- length(which(df.output$is.MYLU[((i-1)*n.loggers+1):(i*n.loggers)]==0 & df.output$survive.Pd.groom[((i-1)*n.loggers+1):(i*n.loggers)]==0))/length(df.output$is.MYLU[((i-1)*n.loggers+1):(i*n.loggers)])
#   df.output$specif.groom[((i-1)*n.loggers+1):(i*n.loggers)] <- rep(TN/(FP+TN), n.loggers)
# 
#   TP <- length(which(df.output$is.MYLU[((i-1)*n.loggers+1):(i*n.loggers)]==1 & df.output$survive.Pd.clust.groom[((i-1)*n.loggers+1):(i*n.loggers)]==1))/length(df.output$is.MYLU[((i-1)*n.loggers+1):(i*n.loggers)])
#   FN <- length(which(df.output$is.MYLU[((i-1)*n.loggers+1):(i*n.loggers)]==1 & df.output$survive.Pd.clust.groom[((i-1)*n.loggers+1):(i*n.loggers)]==0))/length(df.output$is.MYLU[((i-1)*n.loggers+1):(i*n.loggers)])
#   df.output$sensit.clust.groom[((i-1)*n.loggers+1):(i*n.loggers)] <- rep(TP/(TP+FN), n.loggers)
#   FP <- length(which(df.output$is.MYLU[((i-1)*n.loggers+1):(i*n.loggers)]==0 & df.output$survive.Pd.clust.groom[((i-1)*n.loggers+1):(i*n.loggers)]==1))/length(df.output$is.MYLU[((i-1)*n.loggers+1):(i*n.loggers)])
#   TN <- length(which(df.output$is.MYLU[((i-1)*n.loggers+1):(i*n.loggers)]==0 & df.output$survive.Pd.clust.groom[((i-1)*n.loggers+1):(i*n.loggers)]==0))/length(df.output$is.MYLU[((i-1)*n.loggers+1):(i*n.loggers)])
#   df.output$specif.clust.groom[((i-1)*n.loggers+1):(i*n.loggers)] <- rep(TN/(FP+TN), n.loggers)
# }
# 
# write.csv(df.output, "df.fulldata.csv")

ggplot(df.output) +theme_classic() +
  ylab("Torpor bout length") + 
  geom_boxplot(aes(as.factor(Site), 
                   n.days/as.numeric(as.character(arousal.count)), 
                   color=as.factor(is.MYLU)))


##############################################################################
# Select sets of runs that demonstrate appropriate survival without clustering
##############################################################################
Selection <- NA
is.MYLU <- which(df.output$is.MYLU[1:n.loggers]==1)

for(i in 1:length(unique(df.output$n.run))){
  df.test <- df.output[((i-1)*n.loggers+is.MYLU),]
  if(
    # length(unique(n.days/as.numeric(as.character(df.test$arousal.count))<=max.healthy))==1 &
    #  unique(n.days/as.numeric(as.character(df.test$arousal.count))<=max.healthy)==TRUE &
    #  length(unique(n.days/as.numeric(as.character(df.test$arousal.count))>=min.healthy))==1 &
    #  unique(n.days/as.numeric(as.character(df.test$arousal.count))>=min.healthy)==TRUE &
    #  length(unique(n.days/as.numeric(as.character(df.test$arousal.count.Pd))<=max.survive.Pd))==1 &
    #  unique(n.days/as.numeric(as.character(df.test$arousal.count.Pd))<=max.survive.Pd)==TRUE &
    #  length(unique(n.days/as.numeric(as.character(df.test$arousal.count.Pd))>=min.survive.Pd))==1 &
    #  unique(n.days/as.numeric(as.character(df.test$arousal.count.Pd))>=min.survive.Pd)==TRUE &
     unique(df.test$sensit==1)
    ) {
    Selection <- append(Selection, unique(df.test$n.run))
  }
}
Selection <- Selection[-1]

df.output.narrowed <- df.output[which(df.output$n.run %in% Selection),]

ggplot(df.output.narrowed) +theme_classic() +
  ylab("Torpor bout length") + ylim(0,100) +
  geom_boxplot(aes(as.factor(Site), 
                   n.days/as.numeric(as.character(arousal.count)), 
                   color=as.factor(is.MYLU)))
ggplot(df.output.narrowed) +theme_classic() +
  ylab("Torpor bout length") + ylim(0,100) +
  geom_boxplot(aes(as.factor(Site), 
                   n.days/as.numeric(as.character(arousal.count.Pd)), 
                   group=Site,
                   color=as.factor(is.MYLU)))


hist(as.numeric(as.character(df.output.narrowed$T.tormin)), breaks=50, main="T.tormin")
abline(v=T.tormin, col="red")
hist(as.numeric(as.character(df.output.narrowed$t.tormax)), breaks=50, main="t.tormax")
abline(v=t.tormax, col="red")
hist(as.numeric(as.character(df.output.narrowed$TMR.min)), breaks=50, main="TMR.min")
abline(v=TMR.min, col="red")
hist(as.numeric(as.character(df.output.narrowed$rEWL.body)), breaks=50, main="rEWL.body")
abline(v=rEWL.body, col="red")
hist(as.numeric(as.character(df.output.narrowed$C.t)), breaks=50, main="C.t")
abline(v=C.t, col="red")
hist(as.numeric(as.character(df.output.narrowed$M.body)), breaks=50, main="M.body")
abline(v=M.body, col="red")
hist(as.numeric(as.character(df.output.narrowed$p.fat)), breaks=50, main="p.fat")
abline(v=p.fat, col="red")
hist(as.numeric(as.character(df.output.narrowed$clusterfac)), breaks=50, main="clusterfac")
abline(v=clusterfac, col="red")
hist(as.numeric(as.character(df.output.narrowed$groom)), breaks=50, main="groom")
abline(v=groom, col="red")



###########################################################################
# Select sets of runs that demonstrate appropriate survival with clustering
###########################################################################
Selection <- NA

for(i in 1:length(unique(df.output$n.run))){
  df.test <- df.output[((i-1)*n.loggers+is.MYLU),]
  if(
    # length(unique(n.days/as.numeric(as.character(df.test$arousal.count.clust))<=max.healthy))==1 &
    # unique(n.days/as.numeric(as.character(df.test$arousal.count.clust))<=max.healthy)==TRUE &
    # length(unique(n.days/as.numeric(as.character(df.test$arousal.count.clust))>=min.healthy))==1 &
    # unique(n.days/as.numeric(as.character(df.test$arousal.count.clust))>=min.healthy)==TRUE &
    #  length(unique(n.days/as.numeric(as.character(df.test$arousal.count.Pd.clust))<=max.survive.Pd))==1 &
    #  unique(n.days/as.numeric(as.character(df.test$arousal.count.Pd.clust))<=max.survive.Pd)==TRUE &
    #  length(unique(n.days/as.numeric(as.character(df.test$arousal.count.Pd.clust))>=min.survive.Pd))==1 &
    #  unique(n.days/as.numeric(as.character(df.test$arousal.count.Pd.clust))>=min.survive.Pd)==TRUE &
     unique(df.test$sensit.clust==1)
     ) {
    Selection <- append(Selection, unique(df.test$n.run))
  }
}
Selection <- Selection[-1]
df.cluster <- df.output[which(df.output$n.run %in% Selection),]

ggplot(df.cluster) +theme_classic() +
  ylab("Torpor bout length") + ylim(0,100) +
  geom_boxplot(aes(as.factor(Site), 
                   n.days/as.numeric(as.character(arousal.count.clust)), 
                   color=as.factor(is.MYLU)))
ggplot(df.cluster) +theme_classic() +
  ylab("Torpor bout length") + ylim(0,100) +
  geom_boxplot(aes(Site, 
                   n.days/as.numeric(as.character(arousal.count.Pd.clust)), 
                   group=Site,
                   color=as.factor(is.MYLU)))


hist(as.numeric(as.character(df.cluster$T.tormin)), breaks=50)
hist(as.numeric(as.character(df.cluster$t.tormax)), breaks=50)
hist(as.numeric(as.character(df.cluster$TMR.min)), breaks=50)
hist(as.numeric(as.character(df.cluster$rEWL.body)), breaks=50)
hist(as.numeric(as.character(df.cluster$C.t)), breaks=50)
hist(as.numeric(as.character(df.cluster$M.body)), breaks=50)
hist(as.numeric(as.character(df.cluster$p.fat)), breaks=50)
hist(as.numeric(as.character(df.cluster$clusterfac)), breaks=50)
hist(as.numeric(as.character(df.cluster$groom)), breaks=50)


###########################################################################
# Select sets of runs that demonstrate appropriate survival with grooming
###########################################################################
Selection <- NA

for(i in 1:length(unique(df.output$n.run))){
  df.test <- df.output[((i-1)*n.loggers+is.MYLU),]
  if(
    length(unique(n.days/as.numeric(as.character(df.test$arousal.count))<=max.healthy))==1 &
     unique(n.days/as.numeric(as.character(df.test$arousal.count))<=max.healthy)==TRUE &
     length(unique(n.days/as.numeric(as.character(df.test$arousal.count))>=min.healthy))==1 &
     unique(n.days/as.numeric(as.character(df.test$arousal.count))>=min.healthy)==TRUE &
     length(unique(n.days/as.numeric(as.character(df.test$arousal.count.Pd.groom))<=max.survive.Pd))==1 &
     unique(n.days/as.numeric(as.character(df.test$arousal.count.Pd.groom))<=max.survive.Pd)==TRUE &
     length(unique(n.days/as.numeric(as.character(df.test$arousal.count.Pd.groom))>=min.survive.Pd))==1 &
     unique(n.days/as.numeric(as.character(df.test$arousal.count.Pd.groom))>=min.survive.Pd)==TRUE &
     unique(df.test$sensit.groom==1)) {
    Selection <- append(Selection, unique(df.test$n.run))
  }
}
Selection <- Selection[-1]
df.groom <- df.output[which(df.output$n.run %in% Selection),]

ggplot(df.groom) +theme_classic() +
  ylab("Torpor bout length") + ylim(0,100) +
  geom_boxplot(aes(as.factor(Site), 
                   n.days/as.numeric(as.character(arousal.count)), 
                   group=as.factor(Site),
                   color=as.factor(is.MYLU)))
ggplot(df.groom) +theme_classic() +
  ylab("Torpor bout length") + ylim(0,100) +
  geom_boxplot(aes(as.factor(Site), 
                   n.days/as.numeric(as.character(arousal.count.Pd.groom)), 
                   color=as.factor(is.MYLU)))


hist(as.numeric(as.character(df.groom$T.tormin)), breaks=50)
hist(as.numeric(as.character(df.groom$t.tormax)), breaks=50)
hist(as.numeric(as.character(df.groom$TMR.min)), breaks=50)
hist(as.numeric(as.character(df.groom$rEWL.body)), breaks=50)
hist(as.numeric(as.character(df.groom$C.t)), breaks=50)
hist(as.numeric(as.character(df.groom$M.body)), breaks=50)
hist(as.numeric(as.character(df.groom$p.fat)), breaks=50)
hist(as.numeric(as.character(df.groom$clusterfac)), breaks=50)
hist(as.numeric(as.character(df.groom$groom)), breaks=50)



##############################################################################
# Select sets of runs that demonstrate appropriate survival with cluster/groom
##############################################################################
Selection <- NA

for(i in 1:length(unique(df.output$n.run))){
  df.test <- df.output[((i-1)*n.loggers+is.MYLU),]
  if(
    length(unique(n.days/as.numeric(as.character(df.test$arousal.count.clust))<=max.healthy))==1 &
     unique(n.days/as.numeric(as.character(df.test$arousal.count.clust))<=max.healthy)==TRUE &
     length(unique(n.days/as.numeric(as.character(df.test$arousal.count.clust))>=min.healthy))==1 &
     unique(n.days/as.numeric(as.character(df.test$arousal.count.clust))>=min.healthy)==TRUE &
     length(unique(n.days/as.numeric(as.character(df.test$arousal.count.Pd.clust.groom))<=max.survive.Pd))==1 &
     unique(n.days/as.numeric(as.character(df.test$arousal.count.Pd.clust.groom))<=max.survive.Pd)==TRUE &
     length(unique(n.days/as.numeric(as.character(df.test$arousal.count.Pd.clust.groom))>=min.survive.Pd))==1 &
     unique(n.days/as.numeric(as.character(df.test$arousal.count.Pd.clust.groom))>=min.survive.Pd)==TRUE &
     unique(df.test$sensit.clust.groom==1)) {
    Selection <- append(Selection, unique(df.test$n.run))
  }
}
Selection <- Selection[-1]
df.clustergroom <- df.output[which(df.output$n.run %in% Selection),]

ggplot(df.clustergroom) +theme_classic() +
  ylab("Torpor bout length") + ylim(0,100) +
  geom_boxplot(aes(as.factor(Site), 
                   n.days/as.numeric(as.character(arousal.count.Pd.clust.groom)), 
                   color=as.factor(is.MYLU)))
ggplot(df.clustergroom) +theme_classic() +
  ylab("Torpor bout length") + ylim(0,100) +
  geom_boxplot(aes(as.factor(Site), 
                   n.days/as.numeric(as.character(arousal.count.clust)), 
                   group=Site,
                   color=as.factor(is.MYLU)))


hist(as.numeric(as.character(df.clustergroom$T.tormin)), breaks=50, main="T.tormin")
abline(v=T.tormin, col="red")
hist(as.numeric(as.character(df.clustergroom$t.tormax)), breaks=50, main="t.tormax")
abline(v=t.tormax, col="red")
hist(as.numeric(as.character(df.clustergroom$TMR.min)), breaks=50, main="TMR.min")
abline(v=TMR.min, col="red")
hist(as.numeric(as.character(df.clustergroom$rEWL.body)), breaks=50, main="rEWL.body")
abline(v=rEWL.body, col="red")
hist(as.numeric(as.character(df.clustergroom$C.t)), breaks=50, main="C.t")
abline(v=C.t, col="red")
hist(as.numeric(as.character(df.clustergroom$M.body)), breaks=50, main="M.body")
abline(v=M.body, col="red")
hist(as.numeric(as.character(df.clustergroom$p.fat)), breaks=50, main="p.fat")
abline(v=p.fat, col="red")
hist(as.numeric(as.character(df.clustergroom$clusterfac)), breaks=50, main="clusterfac")
abline(v=clusterfac, col="red")
hist(as.numeric(as.character(df.clustergroom$groom)), breaks=50, main="groom")
abline(v=groom, col="red")



##############################################################
# # Density plots of parameters for selected sets
##############################################################


df.density <- as.data.frame(rbind(df.groom, df.clustergroom))
df.density$Model <- c(rep("M/G", nrow(df.groom)), rep("M/C/G", nrow(df.clustergroom)))
df.density$Model <- factor(df.density$Model, levels=c("M/G","M/C/G"))
df.density$cluster.rewl <- df.density$clusterfac*df.density$rEWL.body
df.density$CtTMR <- df.density$C.t/df.density$TMR.min
df.density$clusterfac[1:nrow(df.groom)] <- 1

df.density <- df.density[,c("Model", "T.tormin", "rEWL.body", 
                                 "TMR.min", "C.t", "cluster.rewl", "CtTMR")]
df.95 <- matrix(ncol=6, nrow=12)
df.95 <- as.data.frame(df.95)
colnames(df.95) <- c("variable","Model","lower","upper","value","Literature")
df.95$Model <- rep(c("M/G","M/C/G"), each=6)
df.95$variable <- rep(c("T.tormin", "rEWL.body", 
                         "TMR.min", "C.t", "cluster.rewl", "CtTMR"), 2)
df.95$Literature <- rep(c(T.tormin, rEWL.body, 
                         TMR.min, C.t, rEWL.body, C.t/TMR.min), 2)
df.95$lower <- c(quantile(c(df.groom$T.tormin), 0.025),
                 quantile(c(df.groom$rEWL.body), 0.025),
                 quantile(c(df.groom$TMR.min), 0.025), 
                 quantile(c(df.groom$C.t), 0.025), 
                 quantile(c(df.groom$rEWL.body*df.groom$clusterfac), 0.025),
                 quantile(c(df.groom$C.t/df.groom$TMR.min), 0.025),
                 quantile(c(df.clustergroom$T.tormin), 0.025),
                 quantile(c(df.clustergroom$rEWL.body), 0.025),
                 quantile(c(df.clustergroom$TMR.min), 0.025), 
                 quantile(c(df.clustergroom$C.t), 0.025), 
                 quantile(c(df.clustergroom$rEWL.body*df.clustergroom$clusterfac), 0.025),
                 quantile(c(df.clustergroom$C.t/df.clustergroom$TMR.min), 0.025))
df.95$upper <- c(quantile(c(df.groom$T.tormin), 0.975),
                 quantile(c(df.groom$rEWL.body), 0.975),
                 quantile(c(df.groom$TMR.min), 0.975), 
                 quantile(c(df.groom$C.t), 0.975), 
                 quantile(c(df.groom$rEWL.body*df.groom$clusterfac), 0.975),
                 quantile(c(df.groom$C.t/df.groom$TMR.min), 0.975),
                 quantile(c(df.clustergroom$T.tormin), 0.975),
                 quantile(c(df.clustergroom$rEWL.body), 0.975),
                 quantile(c(df.clustergroom$TMR.min), 0.975), 
                 quantile(c(df.clustergroom$C.t), 0.975), 
                 quantile(c(df.clustergroom$rEWL.body*df.clustergroom$clusterfac), 0.975),
                 quantile(c(df.clustergroom$C.t/df.clustergroom$TMR.min), 0.975))
df.95$value <- c(median(df.groom$T.tormin),
                  median(df.groom$rEWL.body),
                  median(df.groom$TMR.min), 
                  median(df.groom$C.t), 
                  median(df.groom$rEWL.body*df.groom$clusterfac),
                  median(df.groom$C.t/df.groom$TMR.min),
                  median(df.clustergroom$T.tormin),
                  median(df.clustergroom$rEWL.body),
                  median(df.clustergroom$TMR.min), 
                  median(df.clustergroom$C.t), 
                  median(df.clustergroom$rEWL.body*df.clustergroom$clusterfac),
                  median(df.clustergroom$C.t/df.clustergroom$TMR.min))

df.density <- melt(df.density, id.vars = "Model")
line2 <- data.frame(y=df.95$Literature[1:6], df.95$variable[1:6])
colnames(line2) <- c("value","variable")
df.density$variable <- factor(df.density$variable, levels=c("T.tormin","rEWL.body",
                                                           "cluster.rewl",
                                                           "C.t", "TMR.min",
                                                           "CtTMR"))

df.95$variable <- factor(df.95$variable, levels=c("T.tormin","rEWL.body",
                                                            "cluster.rewl",
                                                            "C.t", "TMR.min",
                                                            "CtTMR"))

line2$variable <- factor(line2$variable, levels=c("T.tormin","rEWL.body",
                                                            "cluster.rewl",
                                                            "C.t", "TMR.min",
                                                            "CtTMR"))

parm.labs <- c("(a) Min. body temp.", "(b) Evap. water loss rate", "(c) Effective EWL rate",
               "(d) Conductance", "(e) Min. torpid metab. rate", "(f) Conduct. / metab. rate")
names(parm.labs) <- c("T.tormin","rEWL.body",
                      "cluster.rewl",
                      "C.t", "TMR.min",
                      "CtTMR")
  
ggplot(df.density) + theme_classic() +
  xlab("") + ylab("Parameter value") +
  geom_violin(aes(x=Model, y=value, color=Model, fill=Model)) +
  geom_errorbar(data=df.95,
                aes(x=Model, ymin=lower, ymax=upper, group=Model), 
                width=0.25) +
  geom_point(data=df.95,
                aes(x=Model, y=value, group=Model), 
                size=2) +
  facet_wrap(~ variable, scales="free_y",
             labeller = labeller(variable=parm.labs)) +
  geom_hline(data=line2, aes(yintercept=value), linetype="dashed") + 
  scale_fill_manual(values=c("#67B8D6", "#1C77A3"),
                    labels=c("Microclimate +\nGrooming", 
                             "Microclimate + \nClustering + Grooming"))  + 
  scale_color_manual(values=c("#67B8D6", "#1C77A3"),
                    labels=c("Microclimate +\nGrooming", 
                             "Microclimate + \nClustering + Grooming")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.text = element_text(hjust = 0)) 





p.Ttormin <- ggplot(df.density) + theme_classic() +
  xlab("Minimum torpid body temp. (C)") +
  ylab("Density") +
  geom_density(aes(T.tormin, fill=Model), alpha=0.8) + 
  scale_fill_manual(values=c("#67B8D6", "#1C77A3"),
                    labels=c("Microclimate +\nGrooming", 
                             "Microclimate + \nClustering + Grooming")) +
  geom_vline(xintercept=T.tormin, linetype="dashed") +
  theme(legend.position="none")
p2 <- ggplot(df.density) + theme_classic() +
  xlab("Maximum torpor length (Hours)") +
  ylab("Density") +
  geom_density(aes(t.tormax, fill=Model), alpha=0.9) + 
  scale_fill_manual(values=c("#67B8D6", "#1C77A3")) +
  geom_vline(xintercept=t.tormax, linetype="dashed") +
  theme(legend.position="none")
p.Ct <- ggplot(df.density) + theme_classic() +
  xlab("Torpor conductance (mL O2/gC)") +
  ylab("Density") +
  geom_density(aes(C.t, fill=Model), alpha=0.8) + 
  scale_fill_manual(values=c("#67B8D6", "#1C77A3")) +
  geom_vline(xintercept=C.t, linetype="dashed") +
  theme(legend.position="none")
p.TMR <- ggplot(df.density) + theme_classic() +
  xlab("Torpid metabolic rate (mg O2/[g*hour])") +
  ylab("Density") +
  geom_density(aes(TMR.min, fill=Model), alpha=0.8) + 
  scale_fill_manual(values=c("#67B8D6", "#1C77A3")) +
  geom_vline(xintercept=TMR.min, linetype="dashed") +
  theme(legend.position="none")
p5 <- ggplot(df.density) + theme_classic() +
  xlab("Body evap. loss rate (mg H2O/[hour*dWVP*cm2])") +
  ylab("Density") +
  geom_density(aes(rEWL.body, fill=Model), alpha=0.9) + 
  # geom_density(data=df.density[which(df.density$Model=="M/C/G"),],
  #                                    aes(rEWL.body*clusterfac, fill=Model), alpha=0.9) + 
  scale_fill_manual(values=c("#67B8D6", "#1C77A3")) +
  geom_vline(xintercept=rEWL.body, linetype="dashed") +
  theme(legend.position="none")
p6 <- ggplot(df.density[which(df.density$Model=="M/C/G"),]) + theme_classic() +
  xlab("Clustering factor (%)") +
  ylab("Density") +
  geom_density(aes(clusterfac*100, fill=Model), alpha=0.9) + 
  scale_fill_manual(values=c("#1C77A3")) +
  theme(legend.position="none")
p7 <- ggplot(df.density) + theme_classic() +
  xlab("Fungal removal rate from grooming (cm2/hour)") +
  ylab("Density") +
  geom_density(aes(groom, fill=Model), alpha=0.9) + 
  scale_fill_manual(values=c("#67B8D6", "#1C77A3")) +
  theme(legend.position="none")
p8 <- ggplot(df.density) + theme_classic() +
  xlab("Body mass (grams)") +
  ylab("Density") +
  geom_density(aes(M.body, fill=Model), alpha=0.9) + 
  scale_fill_manual(values=c("#67B8D6", "#1C77A3")) +
  theme(legend.position="none")
p9 <- ggplot(df.density) + theme_classic() +
  xlab("Fat as percentage of body mass (%)") +
  ylab("Density") +
  geom_density(aes(p.fat*100, fill=Model), alpha=0.9) + 
  scale_fill_manual(values=c("#67B8D6", "#1C77A3")) +
  theme(legend.position="none")

# legend.density <- get_legend(p1)
# p1 <- p1+theme(legend.position="none")
# plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9,legend.density, ncol=2)





  
#   
# ggplot(df.groom, aes(Room, 
#                        n.days/as.numeric(as.character(arousal.count)), 
#                        group_by=as.factor(is.MYLU),
#                        color=as.factor(is.MYLU))) + 
#   theme_classic() +
#   ylab("Torpor bout length") +
#   geom_boxplot()
# ggplot(df.clustergroom, aes(Room, 
#                             n.days/as.numeric(as.character(arousal.count.clust)), 
#                             group_by=as.factor(is.MYLU),
#                             color=as.factor(is.MYLU))) + 
#   theme_classic() +
#   ylab("Torpor bout length") +
#   geom_boxplot()


df.cgewl <- df.clustergroom
df.cgewl$rEWL.body <- df.cgewl$rEWL.body*df.cgewl$clusterfac
df.cgewl$C.t <- df.cgewl$C.t/df.cgewl$TMR.min
df.cgewl$Model <- "cgewl"
df.density2 <- rbind(df.density, df.cgewl)
df.density$Model <- factor(df.density$Model, levels=c("M/G","M/C/G","cgewl"))

p.rewl <- ggplot(df.density2) + theme_classic() +
  xlab("Evap. loss rate (mg H2O/[hour*dWVP*cm2])") +
  ylab("Density") +
  geom_density(aes(rEWL.body, fill=Model), alpha=0.8) + 
  scale_fill_manual(values=c("#67B8D6", "#1C77A3","#5E2D30"),
                    labels=c("Microclimate +\nGrooming", "Microclimate +\nClustering + Grooming", "M+C+G\nEWL rate * cluster")) +
  geom_vline(xintercept=rEWL.body, linetype="dashed") +
  theme(legend.position="none")


df.gewl <- df.groom
df.gewl$C.t <- df.gewl$C.t/df.gewl$TMR.min
df.gewl$Model <- "gewl"
df.density2 <- rbind(df.gewl,df.cgewl)
df.density2$Model <- factor(df.density2$Model, levels=c("gewl","cgewl"))

p.CtTMR <- ggplot(df.density2) + theme_classic() +
  xlab("Conductance / Torpid metab. rate") +
  ylab("Density") + xlim(0,25) +
  geom_density(aes(C.t, fill=Model), alpha=0.8) + 
  scale_fill_manual(values=c("#C5A387","#5E2D30"),
                    labels=c("Microclimate +\nGrooming", "Microclimate +\nClustering + Grooming")) +
  geom_vline(xintercept=C.t/TMR.min, linetype="dashed") +
  theme(legend.position="none")



df.legend <- rbind(df.density,df.gewl,df.cgewl)
df.legend$Model <- factor(df.legend$Model, levels=c("M/G","M/C/G","gewl","cgewl"))
p.legend <- ggplot(df.legend) + theme_classic() +
  xlab("Conductance / Torpid metab. rate") +
  ylab("Density") + xlim(0,25) +
  geom_density(aes(C.t, fill=Model), alpha=0.7) + 
  scale_fill_manual(values=c("#67B8D6", "#1C77A3","#C5A387","#5E2D30"),
                    labels=c("Microclimate +\nGrooming", "Microclimate +\nClustering + Grooming", "M + G\ncombined parameters", "M + C + G\ncombined parameters")) +
  geom_vline(xintercept=C.t/TMR.min, linetype="dashed")


legend.density <- get_legend(p.legend)
plot_grid(p.Ttormin, p.rewl, legend.density, p.Ct, p.TMR, p.CtTMR, ncol=3, labels=c("a","b","","c","d","e"))

##################################################################
# # Comparing chambers
##################################################################
library(ghibli)
library(ggsignif)

df.clustergroom$Room <- rep(df.location$Room, nrow(df.clustergroom)/n.loggers)

df.chamber.groom <- NA
df.collapsed.groom <- NA
for(i in 1:(nrow(df.groom)/n.loggers)){
  df.test <- df.clustergroom[((i-1)*n.loggers+1):(i*n.loggers),]
  df.chamber.groom <- rbind(df.chamber.groom, c(mean(n.days/as.numeric(as.character(df.test$arousal.count))[which(df.test$Room=="Upper" & df.test$is.MYLU==1)]),
                                                "Upper",
                                                1,
                                                "t.u.mylu"))
  df.chamber.groom <- rbind(df.chamber.groom, c(mean(n.days/df.test$arousal.count[which(df.test$Room=="Lower" & df.test$is.MYLU==1)]),
                                                "Lower",
                                                1,
                                                "t.l.mylu"))
  df.chamber.groom <- rbind(df.chamber.groom, c(mean(n.days/df.test$arousal.count[which(df.test$Room=="Drum" & df.test$is.MYLU==1)]),
                                                "Side",
                                                1,
                                                "t.s.mylu"))
  df.chamber.groom <- rbind(df.chamber.groom, c(mean(n.days/df.test$arousal.count[which(df.test$Room=="Upper" & df.test$is.MYLU==0)]),
                                                "Upper",
                                                0,
                                                "t.u.na"))
  df.chamber.groom <- rbind(df.chamber.groom, c(mean(n.days/df.test$arousal.count[which(df.test$Room=="Lower" & df.test$is.MYLU==0)]),
                                                "Lower",
                                                0,
                                                "t.l.na"))
  df.chamber.groom <- rbind(df.chamber.groom, c(mean(n.days/df.test$arousal.count[which(df.test$Room=="Drum" & df.test$is.MYLU==0)]),
                                                "Side",
                                                0,
                                                "t.s.na"))
  df.collapsed.groom <- rbind(df.collapsed.groom, c(mean(n.days/df.test$arousal.count[which(df.test$Room=="Upper" & df.test$is.MYLU==1)]),
                                                    mean(n.days/df.test$arousal.count[which(df.test$Room=="Lower" & df.test$is.MYLU==1)]),
                                                    mean(n.days/df.test$arousal.count[which(df.test$Room=="Drum" & df.test$is.MYLU==1)]),
                                                    mean(n.days/df.test$arousal.count[which(df.test$Room=="Upper" & df.test$is.MYLU==0)]),
                                                    mean(n.days/df.test$arousal.count[which(df.test$Room=="Lower" & df.test$is.MYLU==0)]),
                                                    mean(n.days/df.test$arousal.count[which(df.test$Room=="Drum" & df.test$is.MYLU==0)])))
}
df.chamber.groom <- as.data.frame(df.chamber.groom[-1,])
df.collapsed.groom <- as.data.frame(df.collapsed.groom[-1,])
colnames(df.chamber.groom) <- c("torpor","Room","is.MYLU","category")
colnames(df.collapsed.groom) <- c("t.u.mylu","t.l.mylu","t.s.mylu","t.u.na","t.l.na","t.s.na")
test.upper <- t.test(df.collapsed.groom$t.u.mylu, df.collapsed.groom$t.u.na, paired=TRUE)
test.lower <- t.test(df.collapsed.groom$t.l.mylu, df.collapsed.groom$t.l.na, paired=TRUE)
test.side <- t.test(df.collapsed.groom$t.s.mylu, df.collapsed.groom$t.s.na, paired=TRUE)


df.chamber.groom$is.MYLU <- as.factor(df.chamber.groom$is.MYLU)
groom.aov <- aov(torpor ~ Room * is.MYLU, data = df.chamber.groom)


ggplot(df.chamber.groom, aes(Room, 
                             as.numeric(as.character(torpor)), 
                             group_by=as.factor(is.MYLU),
                             color=as.factor(is.MYLU))) + 
  theme_classic() +
  ylab("Torpor bout length") + xlab("Chamber") +
  geom_boxplot() +
  # geom_jitter() +
  scale_color_manual(values=c(ghibli_palettes$LaputaMedium[3],ghibli_palettes$LaputaMedium[5]),
                     name="M. lucifugus\nwinter roost",
                     labels=c("No","Yes")) +
  geom_signif(comparisons=list(c("Lower","Side")), color="black", annotation="*", y_position=32) +
  geom_signif(comparisons=list(c("Lower","Upper")), color="black" , y_position=34, annotation="*") +
  geom_signif(comparisons=list(c("Side","Upper")), color="black", annotation="*", y_position=32) +
  geom_signif(y_position = c(26,29,29), xmin = c(0.8, 1.8, 2.8), xmax = c(1.2, 2.2, 3.2),
              annotation="**", tip_length=0) +
  theme(text = element_text(size = 16))  







#################################################
# # Example figure
#################################################



df.example <- as.data.frame(rbind(cbind(read.csv("example1.csv")[,-1], Model=rep("Microclimate",nrow(read.csv("example1.csv")))),
                    cbind(read.csv("example2.csv")[,-1], Model=rep("Microclimate +\nClustering",nrow(read.csv("example2.csv")))),
                    cbind(read.csv("example3.csv")[,-1], Model=rep("Microclimate + Pd",nrow(read.csv("example3.csv")))),
                    cbind(read.csv("example4.csv")[,-1], Model=rep("Microclimate +\nClustering + Pd",nrow(read.csv("example4.csv")))),
                    cbind(read.csv("example5.csv")[,-1], Model=rep("Microclimate +\nGrooming + Pd",nrow(read.csv("example5.csv")))),
                    cbind(read.csv("example6.csv")[,-1], Model=rep("Microclimate +\nClustering *\nGrooming + Pd",nrow(read.csv("example6.csv"))))
                    ))
df.example$TIME <- as.numeric(as.character(df.example$TIME))/24
df.example$E.hib <- as.numeric(as.character(df.example$E.hib))*20.1/(39.3*100)
colnames(df.example) <- c("TIME", "g.fat", "Model")
df.example$Model <- factor(df.example$Model, levels=c("Microclimate",
                                                      "Microclimate +\nClustering",
                                                      "Microclimate + Pd",
                                                      "Microclimate +\nClustering + Pd",
                                                      "Microclimate +\nGrooming + Pd",
                                                      "Microclimate +\nClustering *\nGrooming + Pd"))
col_marnie <- c("#E9D097", "#5E2D30", "#C5A387",
                "#008E90", "#67B8D6", "#1C77A3")

ggplot(df.example) + theme_classic() + 
  ylab("Fat consumed (grams)") + xlab("Time in hibernation (days)") + 
  geom_line(aes(TIME, g.fat, group=Model, color=Model), size=2) +
  scale_color_manual(values=col_marnie) +
  geom_hline(yintercept=9*0.24, linetype="dashed") +
  annotate("text", label="Fat available", x=30, y=2.4)


for(i in 1:6){
  print(n.days/(length(df.example$Model[which(df.example$Model==unique(df.example$Model)[i])])))
}


#################################################################
# # Correlation
#################################################################
df.corr <- df.groom[seq(34,32470, by=34),c("T.tormin", "t.tormax", "C.t", "TMR.min",
                              "rEWL.body", "M.body", "p.fat", "clusterfac",
                              "groom")]

library(PerformanceAnalytics)
chart.Correlation(df.corr,
                  method="spearman",
                  histogram=TRUE,
                  pch=16)



#################################################
# # Junk code for appendix figure
#################################################
#first generate df.all.loggers using just one run of literature-based values
df.all.loggers$is.MYLU  <- c(0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,1,1,1,1,1)
df.test <- df.all.loggers[which(df.all.loggers$is.MYLU==1),]

ggplot(df.test) + theme_classic() + 
  xlab("Microsite") + ylab("Torpor bout length") +
  geom_point(aes(as.factor(Site), n.days/as.numeric(as.character(arousal.count)), pch="Microclimate", color="Without Pd"), 
             size=2) + 
  geom_point(aes(as.factor(Site), n.days/as.numeric(as.character(arousal.count.Pd)), pch="Microclimate", color="With Pd"), 
             size=2) + 
  geom_point(aes(as.factor(Site), n.days/as.numeric(as.character(arousal.count.clust)), pch="Microclimate +\nClustering", color="Without Pd"), 
             size=2) + 
  geom_point(aes(as.factor(Site), n.days/as.numeric(as.character(arousal.count.Pd.clust)), pch="Microclimate +\nClustering", color="With Pd"), 
             size=2) + 
  geom_point(aes(as.factor(Site), n.days/as.numeric(as.character(arousal.count.Pd.groom)), pch="Microclimate +\nGrooming", color="With Pd"), 
             size=2) + 
  geom_point(aes(as.factor(Site), n.days/as.numeric(as.character(arousal.count.Pd.clust.groom)), pch="Microclimate +\nClustering +\nGrooming", color="With Pd"), 
             size=2) +
  geom_hline(yintercept=max.healthy, linetype="dashed") +
  geom_hline(yintercept=min.healthy, linetype="dashed") + 
  scale_color_manual(breaks=c("Without Pd","With Pd"),
                     values=c("Without Pd" = "blue", "With Pd" = "red"),
                     name="Pd presence",
                     drop=FALSE) +
  scale_shape_manual(breaks=c("Microclimate",
                              "Microclimate +\nClustering",
                              "Microclimate +\nGrooming",
                              "Microclimate +\nClustering +\nGrooming"),
                     values=c("Microclimate" = 16,
                              "Microclimate +\nClustering" = 17,
                     "Microclimate +\nGrooming" = 15,
                     "Microclimate +\nClustering +\nGrooming" = 18),
                     name="Behavioral scenario",
                     drop=FALSE) 
