##################################################################################
# Script pour proposition de de fittage modèles une phase sur phéno budburst O3HP 
##################################################################################


library(lubridate)

###  ###  ### ### ### ### ###
rm(list=ls())
setwd("~/Dropbox/taf/pheno_OHP/one_phase_models_OHP")


source("FunctionPhenoCamb.r")


# Importation dates stade 7 (dates moyennes )

# Importation donnees temperature 
TTT= read.table('Temp_OHP.csv',sep=';',dec='.',header=T)
TTT$Date=as.character(TTT$Date)
TTT$Date=as.Date(TTT$Date,format="%Y-%m-%d")
io  = year(TTT$Date)>=2009
TTT =TTT[io,]

TTT$Year = year(TTT$Date)




# Importation data stades pheno 
ST_7  = read.table('stade_7.csv',header=T,sep=';')




# test general sans optimisation 
PredT0<-tapply(TTT[,"Valeur_Moyenne"], TTT[,"Year"],MGDD, dstart=1,Fcrit=600,Tbase=-5)


plot(2009:2017,PredT0,typ='l')
lines(ST_7,type='l',col='red')
lines(t(PredT0))




DstEx   = seq(0,60,5)            # gamme de valeur pour le parametre de début de comptage
TbEx     = seq(-10, 5, 1)    # gamme pour la valeur seuil de temperature 
FcritEx = seq(200, 1600, 20) # gamme de valeur pour le total 



length(DstEx)*length(TbEx)*length(FcritEx)



a <- proc.time()
vec<-rep(-9999,3)
Res<-NULL
for(i in 1: length(DstEx))
{
  print(i)
  vec[1]<-DstEx[i]
  for(j in 1:length(TbEx))
  {
    vec[2]<-TbEx[j]
    for(k in 1:length(FcritEx))
    {
      vec[3]<-FcritEx[k]
      PredT0<-tapply(TTT[,"Valeur_Moyenne"], TTT[,"Year"],MGDD, dstart=vec[1],Tbase=vec[2],Fcrit=vec[3])
     # print(PredT0)
      Recar<-mean((ST_7[,2]-PredT0)^2,na.rm=T)
      rmse=sqrt(Recar)
      #print(c(PredT0,rmse))
      res=c(vec, rmse,cor(ST_7$Date,PredT0)*cor(ST_7$Date,PredT0))
      Res<-rbind(Res, res)
      
      #print(cor(ST_7$Date,PredT0)*cor(ST_7$Date,PredT0))
    }
  }
  
}
print(proc.time()-a)
colnames(Res)<-c("Dstart", "Tbase", "Fcrit", "rmse")


plot(Res[,4])


BEST<-Res[Res[,4]==min(Res[,4]),]
#BEST<-Res[Res[,5]==max(Res[,5]),]

vec<-BEST[1,1:3]
PredT0<-tapply(TTT[,"Valeur_Moyenne"], TTT[,"Year"],MGDD, dstart=vec[1],Tbase=vec[2],Fcrit=vec[3])

plot(2009:2017,PredT0,typ='l',ylim=c(90,110))
lines(ST_7,type='l',col='red')
lines(t(PredT0))

cor(ST_7$Date,PredT0)*cor(ST_7$Date,PredT0)

# library("robustbase")
# data("coleman")


fit <- lmrob(Y ~ ., data=coleman)
