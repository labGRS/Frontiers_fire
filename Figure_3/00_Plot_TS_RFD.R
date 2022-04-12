# Library
library(readxl)

#-------------------------------------------------------------------------------------------------
# Time sieries
t <- read_excel("~/YourFolder/Datos_Ambientales_Colliguay.xlsx")
date <- as.Date(t$date,format="%d-%m-%Y")
t2 <- read.csv("~/YourFolder/PP_monthly.csv")[267:496,]
date2 <- as.Date(paste(substr(t2$time,1,8),"01",sep=""))
## Temperature
plot(x=date,y=t$Temperature_2m_C,pch=20, ylim=c(0,35),ylab="Monthly Temperature (°C)",xlab="", font.lab=2)
lines(x=date,y=t$Temperature_2m_C)
## RH
plot(x=date,y=t$HRP,pch=20, ylim=c(65,95),ylab="HRP",xlab="", font.lab=2)
lines(x=date,y=t$HRP)
## FDFM
plot(x=date,y=t$HCFM,pch=20, ylim=c(10,20),ylab="HCFM",xlab="", font.lab=2)
lines(x=date,y=t$HCFM)
## SM
plot(x=date,y=t$HS_2cm,pch=20, ylim=c(0,30),ylab="SM",xlab="", font.lab=2)
lines(x=date,y=t$HS_2cm)
## EVI
plot(x=date,y=t$EVI,pch=20, ylim=c(0,0.4),ylab="SM",xlab="", font.lab=2)
lines(x=date,y=t$EVI)
## PP
plot(x=date2,y=t2$data,pch=20, ylim=c(0,400),ylab="Monthly Precipitation (mm)",xlab="", font.lab=2)
lines(x=date2,y=t2$data)

#-------------------------------------------------------------------------------------------------
# RFD Phenology
library(npphen)

## Temperatura
PhenKplot(x=t$Temperature_2m_C[1:214],dates=date[1:214],h=1,nGS=12,xlab="DOY",ylab="Monthly Temperature (?C)", rge=c(0,35))
doy_1year <- yday(date[227:238])
ano_data <- cbind(doy_1year, as.vector(t$Temperature_2m_C[227:238]))
points(ano_data[,1], ano_data[,2], pch=16)
## RH
PhenKplot(x=t$HRP[1:214],dates=date[1:214],h=1,nGS=12,xlab="DOY",ylab="HRP", rge=c(65,95))
doy_1year <- yday(date[227:238])
ano_data <- cbind(doy_1year, as.vector(t$HRP[227:238]))
points(ano_data[,1], ano_data[,2], pch=16)
## FDFM
PhenKplot(x=t$HCFM[1:214],dates=date[1:214],h=1,nGS=12,xlab="DOY",ylab="HCFM", rge=c(10,20))
doy_1year <- yday(date[227:238])
ano_data <- cbind(doy_1year, as.vector(t$HCFM[227:238]))
points(ano_data[,1], ano_data[,2], pch=16)
## EVI
PhenKplot(x=t$EVI[1:214],dates=date[1:214],h=1,nGS=12,xlab="DOY",ylab="EVI", rge=c(0,0.4))
doy_1year <- yday(date[227:238])
ano_data <- cbind(doy_1year, as.vector(t$EVI[227:238]))
points(ano_data[,1], ano_data[,2], pch=16)
## PP
PhenKplot(x=t2$data[1:214],dates=date2[1:214],h=1,nGS=12,xlab="DOY",ylab="PP", rge=c(0,400))
doy_1year <- yday(date2[227:238])
ano_data <- cbind(doy_1year, as.vector(t2$data[227:238]))
points(ano_data[,1], ano_data[,2], pch=16)


# RFD Phenology year -1
library(npphen)

## Temperature
PhenKplot(x=t$Temperature_2m_C[1:214],dates=date[1:214],h=1,nGS=12,xlab="DOY",ylab="Monthly Temperature (?C)", rge=c(0,35))
doy_1year <- yday(date[215:226])
ano_data <- cbind(doy_1year, as.vector(t$Temperature_2m_C[215:226]))
points(ano_data[,1], ano_data[,2], pch=16)
## RH
PhenKplot(x=t$HRP[1:214],dates=date[1:214],h=1,nGS=12,xlab="DOY",ylab="HRP", rge=c(65,95))
doy_1year <- yday(date[215:226])
ano_data <- cbind(doy_1year, as.vector(t$HRP[215:226]))
points(ano_data[,1], ano_data[,2], pch=16)
## FDFM
PhenKplot(x=t$HCFM[1:214],dates=date[1:214],h=1,nGS=12,xlab="DOY",ylab="HCFM", rge=c(10,20))
doy_1year <- yday(date[215:226])
ano_data <- cbind(doy_1year, as.vector(t$HCFM[215:226]))
points(ano_data[,1], ano_data[,2], pch=16)
## EVI
PhenKplot(x=t$EVI[1:214],dates=date[1:214],h=1,nGS=12,xlab="DOY",ylab="EVI", rge=c(0,0.4))
doy_1year <- yday(date[215:226])
ano_data <- cbind(doy_1year, as.vector(t$EVI[215:226]))
points(ano_data[,1], ano_data[,2], pch=16)
## PP
PhenKplot(x=t2$data[1:214],dates=date2[1:214],h=1,nGS=12,xlab="DOY",ylab="PP", rge=c(0,400))
doy_1year <- yday(date2[215:226])
ano_data <- cbind(doy_1year, as.vector(t2$data[215:226]))
points(ano_data[,1], ano_data[,2], pch=16)
