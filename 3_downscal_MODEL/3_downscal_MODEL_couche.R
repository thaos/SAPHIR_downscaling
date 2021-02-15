source("/bdd/MT_WORKSPACE/SAPHIR_downscaling/Calibration-Data_vmichot/CALIPSO_SAPHIR_coloc_out_all_tropical_seas/allday/ALL_SAPHIR_DOWNSCALING_SCRIPTS/Comp_func_VM.R")

## PATHS
source("/bdd/MT_WORKSPACE/SAPHIR_downscaling/Calibration-Data_vmichot/CALIPSO_SAPHIR_coloc_out_all_tropical_seas/allday/ALL_SAPHIR_DOWNSCALING_SCRIPTS/Setpath.R")
library(plyr)


##Script MODEL = calcul du model qrf pour chaque couche d HR, pour tous les nuages avec CloudSat

## ______________________________________________________________________________________________________________________
## **********************************************************************************************************************
##	Global variables
## **********************************************************************************************************************
dx <- 5 																								# Maximum distance [km]
dt <- 30  																								# Maximum temporal separation [min]


#---------------------------------------------------------------------------------------------------
#	SAPHIR parameters
#---------------------------------------------------------------------------------------------------
nlayer <- 6																								# SAPHIR: There are 6 layers for relative humidity defined by their pressure boundaries as follows: 
#L1 = 100-200 hPa / L2 = 250-350 hPa / L3 = 400-600 hPa / L4 = 650-700 hPa / L5 = 750-800 hPa / L6= 850-950 hPa 
lablayer = c("100-200 hPa","250-350 hPa", "400-600 hPa","650-700 hPa","750-800 hPa", "850-950 hPa")

#---------------------------------------------------------------------------------------------------

## FONCTION de DOWNSCALING	
clustering_downscall<-function(area,year,month.num,layer)
{
  
  #---------------------------------------------------------------------------------------------------
  #	Set output directory
  #---------------------------------------------------------------------------------------------------
  filename.dir <- paste0("/bdd/MEGHA_TROPIQUES/MT_WORKSPACE/SAPHIR_downscaling/Calibration-Data_vmichot/CALIPSO_SAPHIR_coloc_out_all_tropical_seas/allday/fichier_par_mois_par_bassin")
  # dossier ou le modele sera enregistre
  output<- paste0(bdd_outputs,"/3_downscal_MODEL/")
  filename.dir.out <- paste0(output,"QRF_model_entire_timeseries/",year,"/")
  dir.create(file.path(filename.dir.out), showWarnings = FALSE,recursive=TRUE)
  #---------------------------------------------------------------------------------------------------
  #	Set input filename
  #---------------------------------------------------------------------------------------------------
     
  filename <- paste0( filename.dir,"/CALIPSO_SAPHIR_CLOUDSAT_",year,"-",month.num,"_",dt,"_",dx,"_",area,"_","for_learning.dat") # fichier de l echantillon d apprentissage
 
  
  library(base)
  if(!file.exists(filename))
  {
    next
  }


  #match.CALIPSO.SAPHIR.CLOUDSAT <- read.table(filename,header=TRUE,sep=";", dec = ".",nrow=50000)  # nombre d observation (CALIPSO) limite a 50000 pour limiter le temps d apprentissage
  match.CALIPSO.SAPHIR.CLOUDSAT <- read.table(filename,header=TRUE,sep=";", dec = ".")    # utilisation du fichier d'apprentissage entier 

  match.CALIPSO.SAPHIR.CLOUDSAT[match.CALIPSO.SAPHIR.CLOUDSAT==Inf]<-150   # transforme les FA (Inf) pour faire le downscaling


  print(paste("nb obs",nrow(match.CALIPSO.SAPHIR.CLOUDSAT)))

  
  ####### Preparation des donnees pour faire le downscaling
  ## table des RR
  RR<-match.CALIPSO.SAPHIR.CLOUDSAT[,grep("RR",colnames(match.CALIPSO.SAPHIR.CLOUDSAT))]
  RR=as.matrix(RR)
  
  ## table de l HR
  RH<-match.CALIPSO.SAPHIR.CLOUDSAT[,grep("rh",colnames(match.CALIPSO.SAPHIR.CLOUDSAT))]
  RH<-as.matrix(RH[,6:1])
 
  ## table de l URH 
  URH<-match.CALIPSO.SAPHIR.CLOUDSAT[,grep("rh.unc",colnames(match.CALIPSO.SAPHIR.CLOUDSAT))]
  URH<-as.matrix(URH)
       
  ## table des SR
  SR_temp<-as.matrix(match.CALIPSO.SAPHIR.CLOUDSAT[,grep("SR",colnames(match.CALIPSO.SAPHIR.CLOUDSAT))])
  SR_temp[SR_temp==Inf]<-150    ## transforme les FA pour faire le model
  z=as.matrix(SR_temp)
    
  ## table des phases
  SR_phase<-as.matrix(match.CALIPSO.SAPHIR.CLOUDSAT[,grep("PHASE",colnames(match.CALIPSO.SAPHIR.CLOUDSAT))])
  
  ind<-match.CALIPSO.SAPHIR.CLOUDSAT$IND_pix_SAPHIR
  lon_SAPHIR<-match.CALIPSO.SAPHIR.CLOUDSAT$lat.SAPHIR
  lat_SAPHIR<-match.CALIPSO.SAPHIR.CLOUDSAT$lon.SAPHIR
  lon_CALIPSO<-match.CALIPSO.SAPHIR.CLOUDSAT$lat.CALIPSO
  lat_CALIPSO<-match.CALIPSO.SAPHIR.CLOUDSAT$lon.CALIPSO
  lon_CLOUDSAT<-match.CALIPSO.SAPHIR.CLOUDSAT$lat.CLOUDSAT
  lat_CLOUDSAT<-match.CALIPSO.SAPHIR.CLOUDSAT$lon.CLOUDSAT
  M.downscal <- cbind(SR_temp,SR_phase,RR,RH,URH,ind) ## dans la fonction, qrf QRFiterative_CALIPSO_CLOUDSAT ne gardera que les 5 premiers km de RR
  M.downscal<-as.data.frame(M.downscal)
  M.downscal<- subset(M.downscal, M.downscal$rh5 < 95) # Select RH profiles where RH_L5 is less than 95% (error in the retrieval)
  
  M.downscal_temp<-M.downscal
  M.downscal_temp[,"freq"] <-  merge.with.order(M.downscal_temp, count(M.downscal_temp, "ind"),by = "ind", all=TRUE,  keep_order=1)[,"freq"]	### pour connaitre le nb de CALIPSO contenu a l origine dans le pixel SAPHIR
  M.downscal_temp[,"freq"] <- as.numeric(M.downscal_temp[,"freq"])
  ind_M.downscal <- which(M.downscal_temp$freq>25)  # recuperer les indices pour pouvoir ensuite retrouver les PHASE en numerique et faire les graphs
  
  ## Pour faire tourner la fonction qrf, passer les flag de phases de nuage en facteur
  PHASE <- M.downscal[, grep("PHASE", colnames(M.downscal))]
  PHASE[PHASE==0] <- "CLEAR"
  PHASE[PHASE==1] <- "LIQ"
  PHASE[PHASE==2] <- "ICE"
  PHASE[PHASE==3] <- "UNDEFINED"
  PHASE[PHASE==4] <- "FALSE LIQ"
  PHASE[PHASE==5] <- "FALSE ICE"
  PHASE[PHASE==6] <- "Horizontally Oriented"
  PHASE[PHASE==8] <- "FA"
  M.downscal[, grep("PHASE", colnames(M.downscal))] <- PHASE
    
  SR_factor<- M.downscal[, grep("PHASE", colnames(M.downscal))] 
  for(sr in 1:ncol(SR_factor))
  {
    SR_factor[, sr]<- factor(SR_factor[, sr], levels=c("CLEAR","LIQ","ICE","UNDEFINED","FALSE LIQ","FALSE ICE","Horizontally Oriented","FA"))
  }
  names_SR_PHASE<-colnames(M.downscal[, grep("PHASE", colnames(M.downscal))])
  colnames(SR_factor)<-names_SR_PHASE
  M.downscal[, grep("PHASE", colnames(M.downscal))] <- SR_factor
  
  ## la table x sera celle utiliser dans la fonction qrf
  x <- M.downscal
  x$rh.frac <-  x[,paste0("rh",layer)]/100 
  x$rh.unc.frac <-  x[,paste0("rh.unc",layer)]/100
    

  #################################################################################################################################################################################################################################  
  ##---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ####                                             DOWNSCALLING avec SR et RR
  ##---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #################################################################################################################################################################################################################################  
  
  print("partie downscalling")
  
  ## **********************************************************************************************************************
  ##	Which algorithm?
  ## **********************************************************************************************************************
  tol <- 0.0001    # tolerance
  nmaxiter <- 100  # nb maximum d iterations
  alg <- "Liu"
  
    print("run downscalling function")
    print(paste0("layer",layer))
 
 ## --------------------------------------------------------------------------------------------------------------------------------
 ##	QRF Iterative Fit
 ## --------------------------------------------------------------------------------------------------------------------------------

    qrf <- QRFiterative_CALIPSO_CLOUDSAT(data=x,tol,nmaxiter,alg)   #default: ntree=500 and mtry=predictors/3)

 #sauvegarde du model
 #saveRDS(qrf, file = paste0(filename.dir.out,"qrf_",area,"_",month.num,"_",year,"MODEL_",layer,"_50000.rds")) # pour l echantillon traiter avec 50000 obs CALIPSO (voir ligne 53)
 saveRDS(qrf, file = paste0(filename.dir.out,"qrf_",area,"_",month.num,"_",year,"MODEL_",layer,"_entire_learn_sample.rds"))  # pour l echantillon traiter avec toutes les obs CALIPSO (voir ligne 54)
 #qrf <- readRDS(file =paste0(filename.dir, "qrf_indian_07_2013_MODEL.rds"))

}



## *********************************************************************************************************************
##      Launcher
##	    area = Indian_Ocean, North_Atlantic
##          year = 2013 and 2016 
##          month.num = 7,8,12
## *********************************************************************************************************************
args = commandArgs(trailingOnly = TRUE)
if (length(args) > 0)
{
  area = args[1]
  year = args[2]
  month.num=args[3]
  layer=args[4]
  print(layer)
} else {
  area = "South_Pacific" #"Indian_Ocean"
  month.num=8 #7
  year=2013
}
  clustering_downscall(area,year,month.num,layer)




