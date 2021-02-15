


##Script MODEL = calcul du model qrf pour chaque couche d HR pour les nuages autre que glace, avec CloudSat


source("Comp_func_VM.R")

# ## PATHS
source("Setpath.R")
library(rlist)
library(ggpubr)
library(grid)
library(rlist )
library(plyr)


##	Global varaibles
## **********************************************************************************************************************
dx <- 5 																								# Maximum distance [km]
dt <- 30  																								# Maximum temporal separation [min]

#---------------------------------------------------------------------------------------------------
#	SAPHIR parameters
#---------------------------------------------------------------------------------------------------
nlayer <- 6																								# SAPHIR: There are 6 layers for relative humidity defined by their pressure boundaries as follows: 
#	L1 = 100-200 hPa / L2 = 250-350 hPa / L3 = 400-600 hPa / L4 = 650-700 hPa / L5 = 750-800 hPa / L6= 850-950 hPa 
lablayer = c("100-200 hPa","250-350 hPa", "400-600 hPa","650-700 hPa","750-800 hPa", "850-950 hPa")

#area="Indian_Ocean"
#year=2013
#month.num=7

#---------------------------------------------------------------------------------------------------

## FONCTION de DOWNSCALING sur l echantillon d apprentissage
clustering_downscall<-function(area,year,month.num,layer)
{
  
  #---------------------------------------------------------------------------------------------------
  #	Set output directory
  #---------------------------------------------------------------------------------------------------
  filename.dir <- paste0(bdd_outputs,"/fichier_par_mois_par_bassin")
  output<- paste0(bdd_data,"/fichier_learn_SAPHIR_entier/")
  filename.dir.out <- paste0(output,"model_LIQ_cloud_avec_CloudSat/QRF_model_entire_timeseries/",year,"/")
  dir.create(file.path(filename.dir.out), showWarnings = FALSE,recursive=TRUE)

  #---------------------------------------------------------------------------------------------------
  #	Set input filename
  #---------------------------------------------------------------------------------------------------
  filename <- paste0( filename.dir,"/CALIPSO_SAPHIR_CLOUDSAT_",year,"-",month.num,"_",dt,"_",dx,"_",area,"_","for_learning.dat")
    
  library(base)
  if(!file.exists(filename))
  {
    next
  }
    
  match.CALIPSO.SAPHIR.CLOUDSAT <- read.table(filename,header=TRUE,sep=";", dec = ".",nrow=50000)  
  match.CALIPSO.SAPHIR.CLOUDSAT[match.CALIPSO.SAPHIR.CLOUDSAT==Inf]<-150 # transforme les FA (Inf) pour faire le downscaling
  
  print(paste("nb obs",nrow(match.CALIPSO.SAPHIR.CLOUDSAT)))
  
  
  ## selection du type de nuage 
  indice_cloud<-apply(match.CALIPSO.SAPHIR.CLOUDSAT, 1, function(r) any(r == 1))
  new_data_2<-match.CALIPSO.SAPHIR.CLOUDSAT[indice_cloud,] 
  
  ####### Preparation des donnees pour faire le downscaling
  ## table des RR 
  RR<-new_data_2[,grep("RR",colnames(new_data_2))]
  RR=as.matrix(RR)

  ## table de l HR coarse 
  RH<-new_data_2[,grep("rh",colnames(new_data_2))]
  RH<-as.matrix(RH[,6:1])
  obs_SAPHIR<-unique(RH)
  nb_obs_SAPHIR<-nrow(obs_SAPHIR)
  
  ## table de l UHR coarse 
  URH<-new_data_2[,grep("rh.unc",colnames(new_data_2))]
  URH<-as.matrix(URH)
  
  ## table des SR 
  SR_temp<-as.matrix(new_data_2[,grep("SR",colnames(new_data_2))])
  z=as.matrix(SR_temp)
  
  ## phases nuage (SR)
  SR_phase<-as.matrix(new_data_2[,grep("PHASE",colnames(new_data_2))])
  
  ind<-new_data_2$IND_pix_SAPHIR
  lon_SAPHIR<-new_data_2$lat.SAPHIR
  lat_SAPHIR<-new_data_2$lon.SAPHIR
  lon_CALIPSO<-new_data_2$lat.CALIPSO
  lat_CALIPSO<-new_data_2$lon.CALIPSO
  lon_CLOUDSAT<-new_data_2$lat.CLOUDSAT
  lat_CLOUDSAT<-new_data_2$lon.CLOUDSAT
  M.downscal <- cbind(SR_temp,SR_phase,RR,RH,URH,ind) ## dans la fonction, qrf QRFiterative_CALIPSO_CLOUDSAT ne gardera que les 5 premiers km de RR
  M.downscal<-as.data.frame(M.downscal)
  M.downscal<- subset(M.downscal, M.downscal$rh5 < 95)		# Select RH profiles where RH_L5 is less than 95% (error in the retrieval)
  
  M.downscal_temp<-M.downscal
  M.downscal_temp[,"freq"] <-  merge.with.order(M.downscal_temp, count(M.downscal_temp, "ind"),by = "ind", all=TRUE,  keep_order=1)[,"freq"]	### pour connaitre le nb de CALIPSO contenu a l origine dans le pixel SAPHIR
  M.downscal_temp[,"freq"] <- as.numeric(M.downscal_temp[,"freq"])
  ind_M.downscal <- which(M.downscal_temp$freq>25)  # recuperer les indices pour pouvoir ensuite retrouver les PHASE en numerique et faire les graphs
  
  ## Pour faire tourner la fonction qrf, passer les flag de phases de nuage en facteur  PHASE <- M.downscal[, grep("PHASE", colnames(M.downscal))]
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

  ##	QRF Iterative Fit
  ## --------------------------------------------------------------------------------------------------------------------------------
 
  qrf <- QRFiterative_CALIPSO_CLOUDSAT(data=x,tol,nmaxiter,alg)   #default: ntree=500 and mtry=predictors/3)
  saveRDS(qrf, file = paste0(filename.dir.out,"qrf_",area,"_",month.num,"_",year,"MODEL_",layer,".rds"))


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




