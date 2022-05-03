################################################################################################
#Liste des fonctions necessaires ? la simulation de l'arret et de la r?activaton cambiale
#BUT1 : Liste des fonctions ? appeller par le main (...)
################################################################################################


################################################################################################
#1/ Fonctiopn MGDD (Model Growing Degree Day): Modele Pheno Simple de for?age thermique ? 3 param?tres

#----------Liste des param?tres----------------------------
#dstart = date ? laquelle le cumul des temp?ratures commence
#Tbase = Temp?rature minimale audela de laquelle on commence ? sommer les temp?ratures 
#Frict = Somme de temp?ratures critiques permettant la r?activation cambiale
#----------Liste des variables----------------------------
#Temp = Temp?rature moyenne


MGDD<-function(temp, dstart, Fcrit, Tbase)
{
  AA = temp-Tbase
  AA[temp<Tbase]=0
  AA[0:dstart] <- 0
  
  TempCum<-cumsum(AA)
  DeltaTemp<-abs(TempCum-Fcrit)
  ddeb<-match(min(DeltaTemp),DeltaTemp)
  

  return(ddeb)
  
}

################################################################################################



rmse<-function(pred, obs)
{
  mean<-mean(sqrt((obs-est)^2))
}

cost <- function(obs,est,UNC=1, Growth1984=F) #RMSE of output (dafault value for UNC is 1 but can be modfied in the MAIN)
{
  
  ecartcar=(obs-est)^2
  if(is.data.frame(obs)==T) {
    RMSE<-sqrt(apply(ecartcar, 2, mean, na.rm=T))
  } else {RMSE<-sqrt(mean(na.omit(ecartcar)))} # ND: on prend la sqrt de la moyenne par le contraire
  misf1<-sum(RMSE)   
  misf2<-prod(RMSE)
  n <- length(est)
  
  #L <- exp(-misf)   #likelihood ?
  return(c(misf1, misf2))
}


