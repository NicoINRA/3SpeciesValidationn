# plotting sim vs Obs water potential  ------------------------------------------

# O3HP
# Output loading an plotting  ------------------------------------------
# Set paths  -----------------------------------------------------------------
library(lubridate)
mainDir <-   dirname(dirname(rstudioapi::getActiveDocumentContext()$path))  

output_path               <-  paste0(mainDir,'/scripts_base_simulations/test_Valid_O3HP.csv')

DATA      = read.csv(output_path,header=T, dec='.', sep="")
DATA$Time = as.POSIXct(DATA$Time,format='%Y-%m-%d/%H:%M:%S')
DATA_day = aggregate(DATA$Psi_LSym,by=list(yday(DATA$Time),year(DATA$Time)),min)
colnames(DATA_day) <- c('DOY','YEAR','Psi_min')
DATA_day$Date = as.Date(paste(DATA_day$DOY,DATA_day$YEAR,sep='/'),format='%j/%Y')
DATA_day$Psi_base = aggregate(DATA$Psi_LSym,by=list(yday(DATA$Time),year(DATA$Time)),max)$x
head(DATA_day)


#---
#/!! Attention date modifiée en 2018 et en 2020 et 2021 inventée car pas les dates prcises indiquées...
#Refaire tourner avec vrai climat et vrai dates de potentiel
O3HP_Psi = read.csv("validation_data/O3HP_Psi2.csv", sep=";", head=T)
O3HP_Psi =O3HP_Psi[O3HP_Psi$V7!=2015,]
O3HP_Psi =O3HP_Psi[O3HP_Psi$V7!=2013,]
O3HP_Psi$Date = as.Date(O3HP_Psi$Date,format='%d/%m/%Y')
O3HP_Psi_base=O3HP_Psi[O3HP_Psi$type=="base",]
O3HP_Psi_min=O3HP_Psi[O3HP_Psi$type=="midi",]
PbaseO3HP=tapply(O3HP_Psi_base$Psi,O3HP_Psi_base$Date, mean, na.rm=T)
PminO3HP=tapply(O3HP_Psi_min$Psi,O3HP_Psi_min$Date, mean, na.rm=T)

names(PbaseO3HP)
names(PminO3HP)
Mean_Psi_O3HP = cbind.data.frame( Date=as.Date(names(PbaseO3HP)),Psi_base_obs=PbaseO3HP,Psi_min_obs=PminO3HP)

DATAAll_O3HP=merge(DATA_day, Mean_Psi_O3HP, by="Date")
quartz()
plot(DATAAll_O3HP$Psi_base_obs~DATAAll_O3HP$Psi_base, ylim=c(-4,0),xlim=c(-4,0), cex=1.5, col=4, pch=16)
points(DATAAll_O3HP$Psi_min_obs~DATAAll_O3HP$Psi_min, ylim=c(-4,0),xlim=c(-4,0), cex=1.5, col=2, pch=16)
abline(0,1)

##### 
quartz()
plot(DATA_day$Date,DATA_day$Psi_base, type='l', col=1, ylab="Water potential Leaf (MPa)", ylim=c(-4,0))
lines(DATA_day$Date,DATA_day$Psi_min, type='l', col=3, ylab="Water potential Leaf (MPa)")
points(as.Date(names(PbaseO3HP)), PbaseO3HP, bg=1, pch=21)
points(as.Date(names(PminO3HP)), PminO3HP, bg=3, pch=21)



#---------------
#FontBlanche
output_path               <-  paste0(mainDir,'/scripts_base_simulations/test_Valid_FontBlanche.csv')
DATA      = read.csv(output_path,header=T, dec='.', sep="")
DATA$Time = as.POSIXct(DATA$Time,format='%Y-%m-%d/%H:%M:%S')
DATA_day = aggregate(DATA$Psi_LSym,by=list(yday(DATA$Time),year(DATA$Time)),min)
colnames(DATA_day) <- c('DOY','YEAR','Psi_min')
DATA_day$Date = as.Date(paste(DATA_day$DOY,DATA_day$YEAR,sep='/'),format='%j/%Y')
DATA_day$Psi_base = aggregate(DATA$Psi_LSym,by=list(yday(DATA$Time),year(DATA$Time)),max)$x
head(DATA_day)

FontBlanche_Psi = read.csv("validation_data/FontBlanche_potentiel_foliaire_corrige2.csv", sep=";", head=T)
FontBlanche_Psi$Date = as.Date(FontBlanche_Psi$Date,format='%d/%m/%Y')
PinPsi = FontBlanche_Psi[FontBlanche_Psi$Espece=="P",]

PbasePin_FB=tapply(PinPsi$Pb,PinPsi$Date, mean, na.rm=T)
PbasePin_FB=cbind.data.frame(Date=as.Date(names(PbasePin_FB)),PbasePin_FB)

PminPin_FB=tapply(PinPsi$Pm,PinPsi$Date, mean, na.rm=T)
PminPin_FB=cbind.data.frame(Date=as.Date(names(PminPin_FB)),PminPin_FB)

DATAAll_FB_Pbase_Pin=merge(DATA_day, PbasePin_FB, by="Date")
DATAAll_FB_Pmin_Pin=merge(DATA_day, PminPin_FB, by="Date")

quartz()
plot(DATAAll_FB_Pbase_Pin$PbasePin_FB ~ DATAAll_FB_Pbase_Pin$Psi_base, ylim=c(-4,0),xlim=c(-4,0), cex=1.5, col=4, pch=16)
points(DATAAll_FB_Pmin_Pin$PminPin_FB ~ DATAAll_FB_Pbase_Pin$Psi_min, ylim=c(-4,0),xlim=c(-4,0), cex=1.5, col="orange", pch=16)
#points(DATAAll$Psi_min_obs~DATAAll$Psi_min, ylim=c(-4,0),xlim=c(-4,0), cex=1.5, col=2, pch=16)
abline(0,1)

quartz()
plot(DATA_day$Date,DATA_day$Psi_base, type='l', col=1, ylab="Water potential Leaf (MPa)", ylim=c(-4,0))
lines(DATA_day$Date,DATA_day$Psi_min, type='l', col=3, ylab="Water potential Leaf (MPa)")
points(PbasePin_FB$Date, PbasePin_FB$PbasePin_FB, bg=1, pch=21)
points(PminPin_FB$Date, PminPin_FB$PminPin_FB, bg="orange", pch=21)



#------------------
#Puechabon
filename  = paste0(mainDir,"/scripts_base_simulations/Puechabon_VG_LFMC_AdjustedAllYears.csv")
DATA      = read.csv(filename,header=T, dec='.', sep="")
DATA$Time = as.POSIXct(DATA$Time,format='%Y-%m-%d/%H:%M:%S')
# Loading and computing  plant water potential data --------
data_potential = read.csv(paste0(mainDir,'/validation_data/Water_Potential_MIND_Control-1.csv'),dec=',',sep=';')  
data_potential = data_potential[data_potential$Treatment=='Control',]
data_PDpot     = aggregate(data_potential$Pd_pot,by=list(data_potential$Date),FUN=function(x) mean(x,na.rm=T))# conversion en une données jour par le PD_pot et le Md_pot
colnames(data_PDpot) <- c('Date','PDpot_measured')
data_PDpot$Date      <- as.Date(data_PDpot$Date,format='%d/%m/%Y')
data_MDpot = aggregate(data_potential$Md_pot,by=list(data_potential$Date),FUN=function(x) mean(x,na.rm=T))# conversion en une données jour par le PD_pot et le Md_pot
colnames(data_MDpot) <- c('Date','MDpot_measured')
data_MDpot$Date <- as.Date(data_MDpot$Date,format='%d/%m/%Y')

DATA_day_Puech = aggregate(DATA$Psi_LSym,by=list(yday(DATA$Time),year(DATA$Time)),min)
colnames(DATA_day_Puech) <- c('DOY','YEAR','Psi_min')
DATA_day_Puech$Psi_base = aggregate(DATA$Psi_LSym,by=list(yday(DATA$Time),year(DATA$Time)),max)$x
DATA_day_Puech$Date = as.Date(paste(DATA_day_Puech$DOY,DATA_day_Puech$YEAR,sep='/'),format='%j/%Y')

simu_DD = merge(DATA_day_Puech, data_PDpot, by='Date', all.x=T)
simu_DD = merge(simu_DD, data_MDpot, by='Date', all.x=T)










quartz()
plot(DATAAll_FB_Pbase_Pin$PbasePin_FB ~ DATAAll_FB_Pbase_Pin$Psi_base, ylim=c(-6,0),xlim=c(-6,0), cex=1.5, bg=adjustcolor("orange", 0.4), pch=21, ylab="Observed", xlab="Predicted", las=1, tck=0.02)
points(DATAAll_FB_Pmin_Pin$PminPin_FB ~ DATAAll_FB_Pbase_Pin$Psi_min, ylim=c(-4,0),xlim=c(-4,0), cex=1.5, bg=adjustcolor("orange", 0.4), pch=22)
points(DATAAll_O3HP$Psi_base_obs~DATAAll_O3HP$Psi_base, ylim=c(-4,0),xlim=c(-4,0), cex=1.5, bg=adjustcolor(4, 0.4), pch=21)
points(DATAAll_O3HP$Psi_min_obs~DATAAll_O3HP$Psi_min, ylim=c(-4,0),xlim=c(-4,0), cex=1.5, bg=adjustcolor(4, 0.4), pch=22)
points(simu_DD$PDpot_measured~simu_DD$Psi_base, ylim=c(-4,0),xlim=c(-4,0), cex=1.5, bg=adjustcolor("dark green", 0.4), pch=21)
points(simu_DD$MDpot_measured~simu_DD$Psi_min, ylim=c(-4,0),xlim=c(-4,0), cex=1.5, bg=adjustcolor("dark green", 0.4), pch=22)
abline(0,1)
legend(-6,0,c("Quercus pubescens (O3HP)", "Quercus ilex (Puechabon)", "Pinus halepnsis (Font-Blanche)"), col=c(adjustcolor(4, 0.4), adjustcolor("dark green", 0.4),adjustcolor("orange", 0.4)), pch=16)
legend(-6,-1,c("predawn", "midday"), pch=c(1,0), bty="n")

