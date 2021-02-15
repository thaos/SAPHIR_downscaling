## Functions scripts
source("Comp_func_VM.R")
## PATHS
source("Setpath.R")



## ______________________________________________________________________________________________________________________
## **********************************************************************************************************************
##	Global varaibles
## **********************************************************************************************************************

#year <- c(2012:2016)  ## a mettre en commentaire lorsque tourne pour toutes les donnees
#yearlab <- ifelse(min(seqyear)==max(seqyear),min(seqyear),paste0(min(seqyear),"-",max(seqyear))) ##  a mettre en commentaire lorsque tourne pour toutes les donnees
#month <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October","November","December")  ## a mettre en commentaire lorsque tourne pour toutes les donnees
#month.num <- c("1","2", "3", "4", "5", "6", "7","8", "9", "10", "11", "12")  ## a mettre en commentaire lorsque tourne pour toutes les donnees
dx <- 5  # Maximum distance [km]
dt <- 30 # Maximum temporal separation [min]
#area = "Indian_Ocean"
#month.num=12
#year=2013

##---------------------------------------------------------------------------------------------------


prep_donnees_downscal<-function(area,year,month.num)
{
  
  ##	Set input filename

  # si l'aire d'étude est definie
  if(!is.null(area))
  {
    filename <-  paste0(bdd_data,"/CALIPSO_SAPHIR_CLOUDSAT_",year,"-",month.num,"_",dt,"_",dx,"_",area,"_","speed_RH_SR-OPAQ-PHASE_RF_allvar.dat")
  }
  
  # si l'aire d'étude n'est pas definie
  if(is.null(area))
  {
    filename <- paste0(bdd_data,"/CALIPSO_SAPHIR_CLOUDSAT_",year,"-",month.num,"_",dt,"_",dx,"_","speed_RH_SR-OPAQ-PHASE_RF_allvar.dat")
  }
  
  # Ouverture des fichiers
  if(!file.exists(filename))
  {
    next
  }
  match.CALIPSO.SAPHIR.CLOUDSAT <- read.table(filename,header=TRUE,sep=";", dec = ".")  # pour toutes les donnees
  
  # pour connaître le nombre de lignes initiales (donc de données CALIPSO)
  print(paste("nb initial obs",nrow(match.CALIPSO.SAPHIR.CLOUDSAT)))
  
  
  #### reorganisation du fichier match.CALIPSO.SAPHIR.CLOUDSAT
  #pour remettre les colonnes de RR dans le bon ordre : du sol vers l atmosphere
  match.CALIPSO.SAPHIR.CLOUDSAT<-match.CALIPSO.SAPHIR.CLOUDSAT[,c(1:178,284:179,285:286)]
  #pour convertir les heures, eviter les doublons et indicer les lignes
  match.CALIPSO.SAPHIR.CLOUDSAT$mlstdate.SAPHIR <- UTCtolocal(match.CALIPSO.SAPHIR.CLOUDSAT$date.SAPHIR,match.CALIPSO.SAPHIR.CLOUDSAT$time.SAPHIR,match.CALIPSO.SAPHIR.CLOUDSAT$lon.SAPHIR)
  match.CALIPSO.SAPHIR.CLOUDSAT$mlsttime.frac.SAPHIR <- hourtofrac(match.CALIPSO.SAPHIR.CLOUDSAT$mlstdate.SAPHIR)
  match.CALIPSO.SAPHIR.CLOUDSAT$mlstdate.CLOUDSAT <- UTCtolocal(match.CALIPSO.SAPHIR.CLOUDSAT$date.CLOUDSAT,match.CALIPSO.SAPHIR.CLOUDSAT$time.CLOUDSAT,match.CALIPSO.SAPHIR.CLOUDSAT$lon.CLOUDSAT)
  match.CALIPSO.SAPHIR.CLOUDSAT$mlsttime.frac.CLOUDSAT <- hourtofrac(match.CALIPSO.SAPHIR.CLOUDSAT$mlstdate.CLOUDSAT)
  match.CALIPSO.SAPHIR.CLOUDSAT <- match.CALIPSO.SAPHIR.CLOUDSAT[!duplicated(match.CALIPSO.SAPHIR.CLOUDSAT),]
  INDICE<-1: nrow(match.CALIPSO.SAPHIR.CLOUDSAT)
  match.CALIPSO.SAPHIR.CLOUDSAT<-cbind(INDICE,match.CALIPSO.SAPHIR.CLOUDSAT)
  
  ### Attribution d'un indice a chaque PIXEL SAPHIR!
  # creation d une colonne avec les variables permettant de differencier chaque pixel SAPHIR
  match.CALIPSO.SAPHIR.CLOUDSAT$pix_SAPHIR<- paste(match.CALIPSO.SAPHIR.CLOUDSAT$lat.SAPHIR, match.CALIPSO.SAPHIR.CLOUDSAT$lon.SAPHIR, match.CALIPSO.SAPHIR.CLOUDSAT$date.SAPHIR,match.CALIPSO.SAPHIR.CLOUDSAT$time.SAPHIR, sep="_")
  # attribution d un indice a chaque pixel SAPHIR 
  pix_temp<-unique(match.CALIPSO.SAPHIR.CLOUDSAT$pix_SAPHIR)
  tab_avec_ind_SAPH<-data.frame()
  for(pix in 1:length(pix_temp))
  {
    find_pix<-which(pix_temp[pix]==match.CALIPSO.SAPHIR.CLOUDSAT$pix_SAPHIR)
    selec_temp<-match.CALIPSO.SAPHIR.CLOUDSAT[find_pix,]
    selec_temp$IND_pix_SAPHIR<- pix
    tab_avec_ind_SAPH<-rbind(tab_avec_ind_SAPH, data.frame(selec_temp))
  }
  
  match.CALIPSO.SAPHIR.CLOUDSAT<-tab_avec_ind_SAPH
  
  
  
  ################### Controle des donnees ###################
  
  #### ne garder que les valeurs de jour pour les 3 SATELLITES (car pas de donnees CloudSat (RR) la nuit)
  
  # selection des donnees de jour
  index_NA<-which(rowSums(is.na(match.CALIPSO.SAPHIR.CLOUDSAT[,180:285]))!=106)   #### pour avoir les index des lignes CloudSat qui ne sont pas entierement en NA (lignes CloudSat entierement en NA= donnees de nuit) 
  data_day<-match.CALIPSO.SAPHIR.CLOUDSAT[index_NA,]                              #### pour garder juste les donnees de jours
  data_day<-data_day[order(data_day$INDICE,decreasing=F),]   #sort sur les indices au cas ou la selection aiet melangé les donnees
  
  # selection et controle des donnees de SAPHIR
  print("set SAPHIR variables")
  RH_L1 <- data_day[,c(grep("rh1",colnames(data_day)))]
  RH_L2 <- data_day[,c(grep("rh2",colnames(data_day)))]
  RH_L3 <- data_day[,c(grep("rh3",colnames(data_day)))]
  RH_L4 <- data_day[,c(grep("rh4",colnames(data_day)))]
  RH_L5 <- data_day[,c(grep("rh5",colnames(data_day)))]
  RH_L6 <- data_day[,c(grep("rh6",colnames(data_day)))]
  
  RH <- cbind(RH_L1,RH_L2,RH_L3,RH_L4,RH_L5,RH_L6)
  
  URH_L1 <- data_day[,c(grep("rh.unc1",colnames(data_day)))]
  URH_L2 <- data_day[,c(grep("rh.unc2",colnames(data_day)))]
  URH_L3 <- data_day[,c(grep("rh.unc3",colnames(data_day)))]
  URH_L4 <- data_day[,c(grep("rh.unc4",colnames(data_day)))]
  URH_L5 <- data_day[,c(grep("rh.unc5",colnames(data_day)))]
  URH_L6 <- data_day[,c(grep("rh.unc6",colnames(data_day)))]
  
  URH <- cbind(URH_L1,URH_L2,URH_L3,URH_L4,URH_L5,URH_L6)
  
  #SAPHIR<- match.CALIPSO.SAPHIR.CLOUDSAT[,132:137]
  complete_row_RH <- which(complete.cases(RH))  # trouver les lignes de RH completes 
  data_day<-data_day[complete_row_RH,]  # correspondance avec les données jours disponibles : selectionner les lignes de data_day correspondantes aux lignes de RH completes
  RH<-as.matrix(data_day[,132:137])
  sum(is.na(RH))
  range(RH)
  URH<-as.matrix(data_day[, 144:149])
  range(URH)
  sum(is.na(URH))
  
  
  ##	Set Extra variables
  
  coords.time <- as.POSIXct(paste(data_day[ , "date.CALIPSO"], data_day[ , "time.CALIPSO"], sep =" "),tz="UTC",format="%Y-%m-%d %H:%M:%S")
  DT <- data_day[,grep("dist.temp",colnames(data_day))]
  
  coords.sp <- data_day[ , c("lon.CALIPSO","lat.CALIPSO","lon.SAPHIR","lat.SAPHIR","lon.CLOUDSAT","lat.CLOUDSAT")]
  
  IND.SAPHIR <- data_day[ , "IND.SAPHIR.match"]
  IND_pix_SAPHIR<-data_day[ , "IND_pix_SAPHIR"]
  
  # selection et controle des donnees jour de CLOUDSAT
  print("set CLOUDSAT variables")
  RR2<-data_day[,180:284]
  # remplacement des NA de RR par -30
  RR2[is.na(RR2)] <- (-30)
  # remplacement des valeurs  inférieures à -30 par -30
  RR2[RR2<=(-30)] <- (-30)
  
  RR2_indices<-data_day[,1]
  RR2<-cbind(RR2_indices,RR2, data_day[,176:177])
  
  #moyenne des RR avec au moins 2 obs
  layer_RR<-c(1:25)
  
  RR<-RR2[,c(-1,-107:-109)]  ## agregation jusqu a 24 km (inclus), le km 25 ne comporte qu une couche de 240m, il est donc exclu du calcul ci apres
  
  RR_DF_moylayer<-data.frame()
  for(n in 1:(length(layer_RR)))
  {
    print(n)
    valt <- as.numeric(str_sub(colnames(RR),3))
    if(layer_RR[n]==1){ RR_mean<-RR[,(valt >= 1.2) & (valt <= 2)] }
    else
    {
      RR_mean<-RR[,(valt >= layer_RR[n]) & (valt <= layer_RR[n+1])] 
    }
    for(lgn in 1 :length(RR_mean))
    {
      if(sum(is.na(RR_mean[lgn,]))<=2){moy<-rowMeans(RR_mean, na.rm = TRUE)}
    }
    if(n==1){RR_DF_moylayer<-moy}    
    else
    {
      RR_DF_moylayer<-cbind(RR_DF_moylayer,data.frame(moy))
    }
  }
  
  names1<-paste0("RR",c(1.2,layer_RR[-1]))
  colnames(RR_DF_moylayer)<-names1
  RR_DF_moylayer<-RR_DF_moylayer[,-25]
  sum(is.na(RR_DF_moylayer))
  
  
  # selection et controle des donnees CALIPSO
  print("set CALIPSO variables")
  # Phases des nuages : voir table B1 Guzman et al., 2017, doi:10.1002/2016JD025946 
  # Data(time on rows, altitude on columns)
  #######!!!!!!!!!!!! regarder le netcdf regarder la variable float_instant_opaq!!!!!!!!!!!
  #phases_flags<-data_day[,87:126]
  PHASE_nuage <- data_day[,grep("PHASE",colnames(data_day))]
  #if(precip!=2) PRECIP <- data_day[,grep("Precip_flag",colnames(data_day))]
  PHASE_nuage[PHASE_nuage=="-9999"] <- NA													# Missing pixels
  PHASE_nuage[PHASE_nuage=="-888"] <- NA													# Surface pixels
  #PHASE[PHASE==0] #<- "CLEAR/no_cloud"
  #CLEAR_no_cloud<-sum(PHASE==0)
  #PHASE[PHASE==1] #<- "LIQ"
  #LIQ<-sum(PHASE==1)
  #PHASE[PHASE==2] #<- "ICE"
  #Ice<-sum(PHASE==2)
  #PHASE[PHASE==3] #<- "UNDEFINED"
  #UNDEFINED<-sum(PHASE==3)
  #PHASE[PHASE==4] #<- "FALSE LIQ"
  #FALSE_LIQ<-sum(PHASE==4)
  #PHASE[PHASE==5] #<- "FALSE ICE"
  #FALSE_ICE<-sum(PHASE==5)
  #PHASE[PHASE==6] #<- "Horizontally Oriented"
  #Horizontal_Or<-sum(PHASE==6)
  PHASE_nuage[PHASE_nuage==7] <- NA #"Unphysical value"
  #Unphysic_val<-sum(PHASE==7)
  #PHASE[PHASElab=="UNDEFINED" | PHASElab=="Horizontally Oriented" | PHASElab=="Unphysical value"] <- NA
  
  ## reperer les Fully Attenuated
  layer_Cal<-1:40
  CALIPSO_OPAQflag<-data_day[,47:86]
  
  PHASE_nuage2 <- as.matrix(PHASE_nuage)
  PHASE_nuage2[PHASE_nuage2 == 0 & as.matrix(CALIPSO_OPAQflag) %in% 7:10] <- 8 	    
  
  ## Transformation des SR en fonction des flags : OPAQ Mask for Each 480 m Layer (Instant_OPAQ Variable), voir Guzman, table B1 
  ## la transformation ne change pas les valeurs SR correspondant aux nuages elle permet juste de gérer les valeurs qui ne correspondent pas aux nuages
  print("transformation des flag de SR")
  CALIPSO_SR<-data_day[,7:46]
  
  SR<-data.frame()
  SR<-CALIPSO_SR
  SR[as.matrix(SR)& as.matrix(CALIPSO_OPAQflag)==0] <-NA
  SR[as.matrix(SR)& as.matrix(CALIPSO_OPAQflag)%in% 7:10]<-Inf  ### FULLY ATTENUATED
  
  SR[SR<1]<-0                         # s il y a des valeurs de SR negatives les codees en 0
  SR[SR>1 & SR<5] <-0                 # s il y a des valeurs de SR comprises entre 1.1 et 5 les codees en 0
  SR[SR>80 & SR<1000]<-0              # s il y a des valeurs de SR superieures a 80 (et inf a 1000 pour etre inferieur a l infini des FA)  
  
  
  ##	Reconstruction des SR avec la moyenne des plus proches voisins (si ce sont des valeurs de SR; si ce sont des FA alors restent en FA)
  # boucle pour reconstruire avec le plus proche voisin: mask sur les NA; puis moyenne de la premiere(ou jusqu a la 4eme) valeur/colonne precedente et de la premiere precedente
  # comme on a les FA sont en "infini", s il y a un FA dans les valeurs a moyenner, la moyenne sera Inf donc FA, sinon on aura la moyenne de SR avant et apres le NA 
  print("reconstruction des SR")
  for (dec in 1:4)
  {
    msk = is.na(SR)
    SRp1 = cbind(SR[,2:ncol(SR)],NA)
    SRm = SR[,1:(ncol(SR)-dec)]
    for (i in 1:dec) SRm = cbind(NA,SRm)
    SR[msk] = mapply(function(x,y){mean(c(x,y),na.rm=T)},SRp1[msk],SRm[msk])
  }
  
  # ajout des colonnes flag phase
  SR<-cbind(SR,PHASE_nuage2)
  
  # Pour avoir les memes orbites entre SR et RR apres avoir enleve les NA de SR (c-a-d, avoir les lignes de RR correspondantes aux mêmes lignes de SR)
  complete_row_SR <- which(complete.cases(SR))  ## avant d enlever les lignes avec les donnees manquantes, trouver les lignes de SR completes 
  RR_DF_moylayer2<-RR_DF_moylayer[complete_row_SR,]  ## selectionner les lignes de RR correspondantes aux lignes de SR completes
  
  # Pour avoir les memes orbites entre SR et RH/URH apres avoir enleve les NA de SR (c-a-d, avoir les lignes de RH correspondantes aux mêmes lignes de SR)
  RH<-RH[complete_row_SR,]  ## selectionner les lignes de RH correspondantes aux lignes de SR completes
  URH<-URH[complete_row_SR,]  ## selectionner les lignes de URH correspondantes aux lignes de SR completes
  
  
  print(paste("nb day obs",nrow(RR_DF_moylayer2)))
  
  # Pour finir d enlever les NA dans SR : en general, cas ou tout le profil est manquant
  SR<- na.omit(SR)
  sum(is.na(SR))
  
  
  ######### creation de la nouvelle table match.CALIPSO.SAPHIR.CLOUDSAT nettoyee a utiliser pour le script de downscalling
  coords.sp.CALIPSO <- data_day[ , c("lon.CALIPSO","lat.CALIPSO")]
  coords.time.CALIPSO <- as.POSIXct(paste(data_day[ , "date.CALIPSO"], data_day[ , "time.CALIPSO"], sep =" "),tz="UTC",format="%Y-%m-%d %H:%M:%S")
  coords.sp.SAPHIR <- data_day[ , c("lon.SAPHIR","lat.SAPHIR")]
  coords.time.SAPHIR <- as.POSIXct(paste(data_day[ , "date.SAPHIR"], data_day[ , "time.SAPHIR"], sep =" "),tz="UTC",format="%Y-%m-%d %H:%M:%S")
  coords.sp.CLOUDSAT <- data_day[ , c("lon.CLOUDSAT","lat.CLOUDSAT")]
  coords.time.CLOUDSAT <- as.POSIXct(paste(data_day[ , "date.CLOUDSAT"], data_day[ , "time.CLOUDSAT"], sep =" "),tz="UTC",format="%Y-%m-%d %H:%M:%S")
  matrix_coord<-data.frame(Indice=data_day[ , "INDICE"] ,time.CALIPSO=as.POSIXct(coords.time.CALIPSO),coords.sp.CALIPSO,time.SAPHIR=as.POSIXct(coords.time.SAPHIR),coords.sp.SAPHIR,time.CLOUDSAT=as.POSIXct(coords.time.CLOUDSAT),coords.sp.CLOUDSAT,IND.SAPHIR=IND.SAPHIR,IND_pix_SAPHIR=IND_pix_SAPHIR)
  matrix_coord<-matrix_coord[complete_row_SR,]
  
  match.CALIPSO.SAPHIR.CLOUDSAT2<-cbind(SR,RR_DF_moylayer2,RH,URH,matrix_coord)
  write.table(match.CALIPSO.SAPHIR.CLOUDSAT2,paste0(bdd_data,"/CALIPSO_SAPHIR_CLOUDSAT_",year,"-",month.num,"_",dt,"_",dx,"_",area,"_",
                                                    "for_downscal.dat"),col.names=TRUE,row.names=FALSE,append=TRUE,sep=";",quote=FALSE)
  
}#fin de la fonction


## *********************************************************************************************************************
##      Launcher
##	    area = Indian_Ocean, North_Atlantic
##          year = 2013 and 2016 
##          month.num = 7,8,12
## *********************************************************************************************************************
args = commandArgs(trailingOnly = TRUE)
if (length(args) == 3)
{
  area = args[1]
  year = args[2]
  month.num=args[3]
  prep_donnees_downscal(area,year,month.num)
}






























