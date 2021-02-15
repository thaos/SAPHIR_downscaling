


##Script MODEL = calcul du model qrf pour chaque couche d HR pour les nuages de glace, sans CloudSat


source("Comp_func_VM.R")

# ## PATHS
source("Setpath.R")
library(rlist)
library(ggpubr)
library(grid)
library(rlist )
library(plyr)



#####################
####### fonction QRF iter juste avec CALIPSO et SAPHIR
QRFiterative_CALIPSO_CLOUDSAT <- function( data,tol,nmaxiter,alg,verbose=FALSE){
  ## First iteration (sss=0)
  sss <- 1
  ## Coarse resolution data
  data[,"dep.coarse"] <-  data[,"rh.frac"]
  ## Number of fine resolution data per coarse resolution pixel
  data[,"freq"] <-  merge.with.order(data, count(data, vars="ind"),by = "ind", all=TRUE,  keep_order=1)[,"freq"]
  
  predictors <- c( paste0(colnames(data)[grep("SR",colnames(data))]), 
                   paste0(colnames(data)[grep("PHASE",colnames(data))]))
  print(names(data))
  print(predictors)
  ########### Compute QRF
  qrf <- quantregForest(data[, predictors], data$dep.coarse, keep.inbag=TRUE, keep.forest=TRUE, ntree=500) 
  str(qrf)
  ### prediction
  dep.fine <- predict(qrf, what=0.5)   
  dep.fine.unc<-predict(qrf, what=0.75,newdata=data)-predict(qrf, what=0.25)
  
  ### R2
  rsq<-(cor(dep.fine, data[,"dep.coarse"]))^2
 
  if(verbose){
    print(paste0("iter: ",sss))
    print(paste0("cond: NA"))	
    print(paste0("rsq: ", round(unique(data[,paste0("rsq.",sss)]),3)))
  }

  sss <- sss+1
  cond <- TRUE	
  while(cond & sss < nmaxiter){
    ## *****************************************************************************************************************************
    ## Mass Balance (the estimate of the high resolution data must match the value of the coarse resolution data within each pixel
    ## *****************************************************************************************************************************
    print(sss)
    # Compute mean
    dep.fine.mean <- merge.with.order(data,
                                      setNames(aggregate(dep.fine, by = data["ind"], mean), c("ind","dep.fine.mean")), 
                                      by = "ind", all=TRUE,  keep_order=1)[,"dep.fine.mean"]
    # Update (Liu et al., 2008)
    if(alg=="Liu")  dep.fine <- dep.fine + data[,"dep.coarse"] - dep.fine.mean
    
    ## *****************************************************************************************************************************
    ## QRF Fit
    ## *****************************************************************************************************************************
    qrf <- quantregForest(data[, predictors], dep.fine, keep.inbag=TRUE,ntree=500)  #### !!!! bien mettre dep.fine comme prédictant!!!
  
    dep.fine <- predict(qrf, what=0.5)
    dep.fine.unc<-predict(qrf, what=0.75,newdata=data)-predict(qrf, what=0.25)
    dep.fine[dep.fine<0] <- 0 
    dep.fine[dep.fine>100] <- 100
    rsq_old <- rsq
    rsq <- (cor(dep.fine, data[,"dep.coarse"]))^2
    
    if(alg=="Liu")
    {
      if(abs(rsq - rsq_old) < tol | (rsq - rsq_old) < 0) cond <- FALSE
    }	
   
    if(verbose)
    {
      print(paste0("iter: ",sss))
      print(paste0("cond: ",round(cond,3)))	
      print(paste0("rsq: ", round(unique(data[,paste0("rsq.",sss)]),3)))
    }
    sss <- sss+1
  }
  qrf$data <- data  
  qrf$rsq_sss<-rsq  # pour avoir le rsq de la derniere boucle !!! rsq pour chaque prediction!!!
  qrf$predict<-dep.fine # pour avoir les predivtions de la derniere boucle
  return(qrf)
}

## **********************************************************************************************************************
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

# area="Indian_Ocean"
# year=2013
# month.num=7
#---------------------------------------------------------------------------------------------------

## FONCTION de DOWNSCALING sur l echantillon d apprentissage
clustering_downscall<-function(area,year,month.num,layer)
{
  
  #---------------------------------------------------------------------------------------------------
  #	Set output directory
  #---------------------------------------------------------------------------------------------------
  filename.dir <- paste0(bdd_outputs,"/fichier_par_mois_par_bassin")
  output<- paste0(bdd_data,"/fichier_learn_SAPHIR_entier/")
  filename.dir.out <- paste0(output,"model_ICE_cloud_sans_CloudSat/QRF_model_entire_timeseries/",year,"/")
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
  match.CALIPSO.SAPHIR.CLOUDSAT[match.CALIPSO.SAPHIR.CLOUDSAT==Inf]<-150  # transforme les FA (Inf) pour faire le downscaling
  
  print(paste("nb obs",nrow(match.CALIPSO.SAPHIR.CLOUDSAT)))
  
  
  ## selection du type de nuage 
  indice_Ice<-apply(match.CALIPSO.SAPHIR.CLOUDSAT, 1, function(r) any(r == 2))
  new_data_2<-match.CALIPSO.SAPHIR.CLOUDSAT[indice_Ice,] 
  
  ####### Preparation des donnees pour faire le downscaling
  ## table de l HR 
  RH<-new_data_2[,grep("rh",colnames(new_data_2))]
  RH<-as.matrix(RH[,6:1])
  obs_SAPHIR<-unique(RH)
  nb_obs_SAPHIR<-nrow(obs_SAPHIR)
  
  ## table de l UHR coarse 
  URH<-new_data_2[,grep("rh.unc",colnames(new_data_2))]
  URH<-as.matrix(URH)
  
  ### table des SR 
  SR_temp<-as.matrix(new_data_2[,grep("SR",colnames(new_data_2))])
  z=as.matrix(SR_temp)
  
  ## table des phases nuage (SR)
  SR_phase<-as.matrix(new_data_2[,grep("PHASE",colnames(new_data_2))])
  
  ind<-new_data_2$IND_pix_SAPHIR
  lon_SAPHIR<-new_data_2$lat.SAPHIR
  lat_SAPHIR<-new_data_2$lon.SAPHIR
  lon_CALIPSO<-new_data_2$lat.CALIPSO
  lat_CALIPSO<-new_data_2$lon.CALIPSO
  lon_CLOUDSAT<-new_data_2$lat.CLOUDSAT
  lat_CLOUDSAT<-new_data_2$lon.CLOUDSAT
  M.downscal <- cbind(SR_temp,SR_phase,RH,URH,ind) ## dans la fonction, qrf QRFiterative_CALIPSO_CLOUDSAT ne gardera que les 5 premiers km de RR
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
    

 ################################################################################################################################################################################################################################  
 ##---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 ###                                             DOWNSCALLING avec SR et RR
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
  area = "Indian_Ocean" #"South_Pacific"
  month.num=7 #7
  year=2013
}
clustering_downscall(area,year,month.num,layer)




