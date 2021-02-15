
source("Comp_func_VM.R")

## PATHS
source("Setpath.R")

library(stringr)
library(ncdf4)
library(EnvStats)
library(rlist)
library(ncdf4)
library(viridis)
library(fields)
library("RColorBrewer")
library(rasterVis)
library(chron)
library(base)
library(ggplot2)
library(plyr)


## **********************************************************************************************************************
##	Global varaibles
## **********************************************************************************************************************
# year<-c(2012:2016)
# area<-"North_Atlantic" 	
#lablayer <- c("100:200hPa","250:350hPa", "400:600hPa","650:700hPa","750:800hPa", "850:950hPa")
# annee_RS<-c(2017, 2018, 2019)
# mois_RS<-c("01","02","03","04","05","06","07","08","09","10","11","12")
 
## ********************************************************************************************************************** 
## Comparaison aux données CNRM 
## **********************************************************************************************************************

## fonction pour lecture et selection des variables d interet 
# readRS_geo = function(filename)
# {
#   var.list = list()
#   ncid <- nc_open(filename)
#   var.list$lon = ncvar_get(ncid,"lon")
#   var.list$lat = ncvar_get(ncid,"lat")
#   var.list$pression = ncvar_get(ncid,"plev_t")  ## en hPa
#   var.list$time = as.POSIXct(3600*c(ncvar_get(ncid,"time")),origin="2000-01-01",tz="UTC")
#   var.list$temp_point_dew = ncvar_get(ncid,"td")   ### epaisseur de la couche observee
#   var.list$temperature = ncvar_get(ncid,"ta")  ### !!! T° en Kelvin
#   var.list$num_sta<-ncvar_get(ncid,"numer_sta")
#   nc_close(ncid)
#   return(var.list)
# }
 
 
## chemin des donnees
# path_RS<-'/bdd/MEGHA_TROPIQUES/MT_WORKSPACE/RS_cnrm/cnrm/tropics/commun/DATACOMMUN/BDM_obs/TEMPOMM'
# list_files_test<-list.files(path_RS,full.names = F,pattern = , recursive=TRUE) ##creation d'une liste contenant tous les fichiers      
 
 
# filename.dir <- paste0(bdd_outputs,"/RS_CNRM_pdf")
# dir.create(file.path(filename.dir), showWarnings = FALSE)
 
### pour sauvegarder les plots
# filename.plot <- paste0(filename.dir,"/plot/")
# dir.create(file.path(filename.plot), showWarnings = FALSE)  
 
##---------------------------------------------------------------------------------------------------
##	pdf RH RS CNRM
##---------------------------------------------------------------------------------------------------
 
#### Lecture des donnees
### pour faire des listes, par mois, des fichiers de RS du CNRM 
# files_date<-list()
# for(m in 1:length(mois_RS))
# {
#   pattern<-paste0("^tempomm_([0-9]){5}_20([0-9]){2}",mois_RS[m],"([0-9]){8}_([0-9]){2,3}_.*.nc$")
#   list_files<-list.files(path_RS,full.names = T,pattern = pattern, recursive=TRUE) ##creation d'une liste contenant tous les fichiers      
#   files_date<-list.append(files_date,list_files)
# }
 
 
##pour ouvrir les fichiers et en faire un dataframe. Donc on a un dataframe par mois (toutes annees confondues)
# files_of_interest <- list()
# for (l in 1:length(files_date)) {
#   nb_files <- length(files_date[[l]])
#   df_files <- data.frame()
#   for (f in 1:nb_files)
#   {
#     #print(files_date[[l]][f])
#     
#     
#     var.list.fileOI = tryCatch({x = readRS_geo(files_date[[l]][f])},error=function (x) {return(NULL)},finally=function (x) {return(x)})  ## si la fonction ne peut pas etre appliquee, pour pouvoir passer au fichier suivant
#     if (is.null(var.list.fileOI)) next
#     
#     #fileOI <- readRS_geo(files_date[[l]][f])
#     df_files <- rbind(df_files, data.frame(var.list.fileOI,f))
#   }
#   files_of_interest <- list.append(files_of_interest, df_files)
#   
# }

 
# saveRDS(files_of_interest, file = paste0(filename.dir,"files_of_interest_CNRM.rds"))
# files_of_interest <- readRDS(file =paste0(filename.dir, "files_of_interest_CNRM.rds"))
 
 
# RS_CNRM_mois<-list()
# #par date et numero de fichier:
# for(fi in 1:length(files_of_interest))
# {
   
#   file<-files_of_interest[[fi]]
#   print(nrow(file))
#   #date_file<-unique(str_sub(file$time,1,10))
#   num_file<-unique(file$f)
   
#   #df_num_file<-data.frame()
#   DF_mean_RH_RS<-data.frame()
   
#   for(nf in 1 :length(num_file))
#   {
#     subset_df<-subset(file, f == nf)
     
     ## ne PAS convertir la t° Kelvin en Celsius !
#     #options( "digits"=2, "scipen"=100)
#     #files_of_interest<-sort(files_of_interest, ord=pression,decreasing=TRUE)
#     TEMP_C<-subset_df$temperature
#     Temp_DEW<-subset_df$temp_point_dew
     
#     pression<-subset_df$pression
     
     
     ##calcul de l'HR _ Goff Gratch equation
#     log_TEMP_C<-(-7.90298*(373.16/TEMP_C-1)+ 5.02808 *log10(373.16/TEMP_C)- 1.3816*10^-7 * (10^(11.344*(1-TEMP_C/373.16))-1) + 8.1328*10^-3 * (10^(-3.49149 *(373.16/TEMP_C-1))-1) +log10(1013.246))
#     press_saturante_TEMP_C<-10^(log_TEMP_C)
     
#     log_Temp_DEW<-(-7.90298*(373.16/Temp_DEW-1)+ 5.02808 *log10(373.16/Temp_DEW)- 1.3816*10^-7 * (10^(11.344*(1-Temp_DEW/373.16))-1) + 8.1328*10^-3 * (10^(-3.49149 *(373.16/Temp_DEW-1))-1) +log10(1013.246))
#     press_saturante_Temp_DEW<-10^(log_Temp_DEW)
     
#     RH<- (press_saturante_Temp_DEW/press_saturante_TEMP_C)*100
     
#     df_RS<-cbind(RH,pression)
#     colnames(df_RS)<-c("RH","pression")
#     rownames(df_RS)<-pression
     
     #---------------------------------------------------------------------------------------------------
     #trouver les hPa correspondants aux couches de SAPHIR   ## a recalculer a chaque fois car hauteur de la couche en fonction de la journee de vol
     #---------------------------------------------------------------------------------------------------
     ## rassembler les couches de RS_CNRM, en faire la moyenne et calculer les pdf
     
#     #moyenne des couches Narval en fonction des couches de SAPHIR
#     Layer_SAPHIR<-list(100:200, 250:350, 400:600, 650:700, 750:800, 850:950)
     
#     DF_mean_RH_RS_temp<-data.frame()
#     for(lay in 1:(length(Layer_SAPHIR)))
#     {
#       #print(paste0("calcul moyenne couche",lay))
#       valt <- as.numeric(rownames(df_RS)) # on recupere les noms des lignes de RS
#       row_hPa_RS<-df_RS[(valt >= min(Layer_SAPHIR[[lay]], na.rm = TRUE)) & (valt <= max(Layer_SAPHIR[[lay]], na.rm = TRUE)),] ## selection des lignes de RS correspondantes aux bornes des couches SAPHIR
       
#       if(is.vector(row_hPa_RS)) {
#         mean_RH_RS<-row_hPa_RS[1]
#       } else{
#         if(all(is.na(row_hPa_RS[,1])) | sum(is.na(row_hPa_RS[,1])>1))
#         {
#           mean_RH_RS<-NA ## s il n y a que des NA ou plus de 1 valeurs en NA = la couche reste en NA; 
#         }else{
#           mean_RH_RS<-mean(row_hPa_RS[,1], na.rm = TRUE) #sinon calcul de la moyenne de la couche
#         }
#       }  
#       if(lay==1)
#       {
#         DF_mean_RH_RS_temp<-mean_RH_RS
#       } else{
#         DF_mean_RH_RS_temp<-cbind(DF_mean_RH_RS_temp,data.frame(mean_RH_RS))
#       }
#       
#     }
#     
#     DF_mean_RH_RS<-rbind(DF_mean_RH_RS,data.frame(DF_mean_RH_RS_temp))
#   }
#   colnames(DF_mean_RH_RS)<-lablayer
#   RS_CNRM_mois <- list.append(RS_CNRM_mois, DF_mean_RH_RS)
#   
# }
 
# saveRDS(RS_CNRM_mois, file = paste0(filename.dir,"RS_CNRM_mois.rds"))
# RS_CNRM_mois <- readRDS(file =paste0(filename.dir, "RS_CNRM_mois.rds"))
 
 
 ########################################################################################################"
 ##  NARVAL 
 ########################################################################################################
 
 ## donnees de NARVAL
# dir_narval<-'/bdd/MEGHA_TROPIQUES/MT_WORKSPACE/NARVAL_data/NARVAL/narval1_rh'
 
#	Set output directory
#---------------------------------------------------------------------------------------------------
# filename.dir <- paste0(bdd_outputs,"/NARVAL_pdf")
# dir.create(file.path(filename.dir), showWarnings = FALSE)## un dossier par year?
# 
# 
# ### pour sauvegarder les plots
# filename.plot <- paste0(filename.dir,"/plot/")
# dir.create(file.path(filename.plot), showWarnings = FALSE)  
# 
# 
# #---------------------------------------------------------------------------------------------------
# #	pdf RH NARVAL
# #---------------------------------------------------------------------------------------------------
# 
# #### Lecture des donnees NARVAL  : temps
# 
# ### !!!!!!!!!!!!!!!! Campagne 1
# 
# ### ouverture des fichiers Narval
# files_NARVAL<-list.files(path = dir_narval,full.names = F,pattern = "*.nc",recursive=T) ##creation d'une liste contenant tous les fichiers                  
# 
# 
# ## lecture et selection des variables Narval d interet 
# readNARVAL_geo = function(filename)
# {
#   var.list = list()
#   ncid <- nc_open(paste(dir_narval,filename,sep="/"))
#   var.list$lon = ncvar_get(ncid,"longitude")
#   var.list$lat = ncvar_get(ncid,"latitude")
#   var.list$z = c(ncvar_get(ncid,"range"))    ### z, altitude du vol, donc pour avoir les altitude de chaque obs
#   var.list$time = as.POSIXct(c(ncvar_get(ncid,"time")),origin="1970-01-01",tz="UTC")
#   var.list$RH = ncvar_get(ncid,"relative_humidity_liquid")
#   var.list$RH_resolution = ncvar_get(ncid,"range_resolution")   ### epaisseur de la couche observee
#   var.list$temperature = ncvar_get(ncid,"temperature")  ### !!! T° en Kelvin
#   nc_close(ncid)
#   
#   return(var.list)
# }
# ### difference entre "height above sea level", "range" et "range resolution"?
# 
# 
# ## stockage des variables
# 
# files_of_interest_N<-list()
# 
# for (f in 1 : length(files_NARVAL)){
#   print(f)
#   fileOI<-readNARVAL_geo (files_NARVAL[f])
#   print(files_NARVAL[f])
#   files_of_interest_N = list.append(files_of_interest_N,fileOI)
#   
# }
# 
# list_Narval_30_N_S<-list()
# for (i in 1: length(files_of_interest_N))
# {
#   print(paste("file num",files_NARVAL[i]))
#   annee<-str_sub(files_NARVAL[i],1,4)
#   mois<-str_sub(files_NARVAL[i],5,6)
#   jour<-str_sub(files_NARVAL[i],7,8)
#   date_file<-paste(annee, mois, jour, sep="_")
#   
#   
#   #alt_layer_Narval_12_10<-data.frame()
#   
#   # alt_layer<-as.data.frame(files_of_interest_N[[i]]$z) ## pour garder les donnees d altitude
#   # alt_layer_Narval_12_10<-rbind(alt_layer_Narval_12_10,data.frame(alt_layer))
#   
#   RH<-t(files_of_interest_N[[i]]$RH)  ## transposition des 1610 colonnes (mesures) en lignes, donc maintenant les colonnes correspondent a une altitude et les lignes a une coord X et Y 
#   RH[RH <0 | RH >90] <- NA
#   name_RH<-paste("RH", seq(1,ncol(RH),by=1),sep="")
#   colnames(RH)<-name_RH
#   RH<-RH[,ncol(RH):1]
#   
#   # RH_resolution<-t(files_of_interest_N[[i]]$RH_resolution)
#   alt_layer<-files_of_interest_N[[i]]$z
#   # alt_layer[alt_layer<0]<-(-alt_layer<0)
#   alt_layer[alt_layer<0]<-NA
#   
#   name_Narval<-c("lon","lat","time",colnames(RH))
#   #paste("Z", seq(1,ncol(alt_layer),by=1),sep=""))
#   Narval<-cbind(files_of_interest_N[[i]]$lon,files_of_interest_N[[i]]$lat,files_of_interest_N[[i]]$time,RH) ## fichier final sur lequel on travaille
#   colnames(Narval)<-name_Narval
#   
#   Narval<-as.data.frame(Narval)
#   Narval$time<-as.POSIXct(Narval$time, tz = "UTC",strptime("1970-01-01 00:00:00 00:00", "%Y-%m-%d %H:%M:%S"), optional = FALSE)
#   # rm(i,alt_layer,RH,name_Narval)
#   
#   ##supprimer les lignes avec seulement des NA
#   for(obs in 1: nrow(Narval))
#   {
#     if(all(is.na( Narval[obs,4:length(Narval)])))
#     {
#       Narval<-Narval[-obs,] 
#     }
#   }
#   
#   ## recuper les indices des lignes a enlever pour les enlever aussi dans RH pour ensuite faire les plots
#   
#   ##### selectionner seulement les traces de vol entre 30°N et 30°S
#   Narval_30_N_S<-subset(Narval,Narval[,2]<=30 |Narval[,2]<=(-30)) ## autre option
#   
#   ### plot pour visualiser les valeurs de RH de NARVAL
#   png(file=paste(filename.plot,"NARVAL_RH_layer",i,".png",sep=""), width=1200, height=500, res=200)         
#   #pdf(paste0(filename.plot,"NARVAL_RH_",date_file,".pdf"),width=20,heigh=15)
#   RH_rot <- apply(Narval_30_N_S, 2, rev)
#   RH_flat <- c(RH_rot) ## transformation matrice -> vecteur
#   grid<- expand.grid(X=Narval_30_N_S$time,Y=alt_layer)
#   Z <- RH_flat
#   p <- levelplot(Z~X*Y, grid,
#                  par.settings=viridisTheme(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),
#                  xlab=list("time", cex=1),
#                  ylab=list("altitude (m)", cex=1),
#                  scale=list(x=list(alternating=1,rot=90,format="%d-%m-%y\n%H:%M:%S",cex=1),
#                             y=list(cex=1)))
#   
#   p
#   
#   dev.off()
#   
#   
#   #---------------------------------------------------------------------------------------------------
#   #trouver les hPa correspondants aux altitudes   ## a recalculer a chaque fois car hauteur de la couche en fonction de la journee de vol
#   #---------------------------------------------------------------------------------------------------
#   alt_Narval<-alt_layer
#   #layer_Narval<-unique(RH_resolution)
#   altitude_hPa<-c()
#   ## calcul conversion de l altitude en hPa
#   for(alti in 1: length(alt_Narval)){
#     hPa_f_altitude<-1013*(1-((0.0065*alt_Narval[alti])/288.15))^5.255  ## calcul avec pression normalisée (donc sans faire varier la T° avec l altitude)
#     hPa_f_altitude<-round(hPa_f_altitude,0)
#     altitude_hPa<-c(altitude_hPa,hPa_f_altitude)
#     
#   }
#   
#   ## changement des noms de colonnes de RH en hPa
#   Narval_alti_hPa<-Narval_30_N_S
#   names_Narval_30_N_S<-colnames(Narval_30_N_S[,1:3])
#   colnames(Narval_alti_hPa)<-c(names_Narval_30_N_S,altitude_hPa)
#   
#   ## Apres avoir trouver les hPa de SAPHIR correspondant aux altitudes de Narval
#   ## rassembler les couches de Narval, en faire la moyenne et calculer les pdf
#   
#   #selection des colonnes d HR
#   col_RH_NARVAL<-Narval_alti_hPa[,4:length(Narval_alti_hPa)]
#   
#   #moyenne des couches Narval en fonction des couches de SAPHIR
#   nlayer <- 6	# SAPHIR: There are 6 layers for relative humidity defined by their pressure boundaries as follows: 
#   Layer_SAPHIR<-list(100:200, 250:350, 400:600, 650:700, 750:800, 850:950)
#   
#   DF_mean_RH_NARVAL<-data.frame()
#   for(lay in 1:(length(Layer_SAPHIR))){
#     print(paste0("calcul moyenne couche",lay))
#     valt <- as.numeric(colnames(col_RH_NARVAL)) # on recupere les noms des colonnes de RH NARVAL
#     col_mean_RH_NARVAL<-col_RH_NARVAL[,(valt >= min(Layer_SAPHIR[[lay]], na.rm = TRUE)) & (valt <= max(Layer_SAPHIR[[lay]], na.rm = TRUE))] ## selection des colonnes Narval correspondantes aux bornes des couches SAPHIR
#     
#     for (l in 1:nrow(col_mean_RH_NARVAL)){
#       if(all(is.na(col_mean_RH_NARVAL[l,])) | sum(is.na(col_mean_RH_NARVAL[l,]))>=30){mean_RH_NARVAL<-NA}## s il n y a que des NA ou plus de 30 valeurs en NA = la couche reste en NA; 
#       else{
#         mean_RH_NARVAL<-rowMeans(col_mean_RH_NARVAL, na.rm = TRUE) #sinon calcul de la moyenne de la couche
#       }
#     }   
#     if(lay==1){DF_mean_RH_NARVAL<-mean_RH_NARVAL}    
#     else{
#       DF_mean_RH_NARVAL<-cbind(DF_mean_RH_NARVAL,data.frame(mean_RH_NARVAL))
#     }
#   }
#   
#   colnames(DF_mean_RH_NARVAL)<-lablayer
#   
#   
#   list_Narval_30_N_S<- list.append(list_Narval_30_N_S,DF_mean_RH_NARVAL)
#   
# }
# 
# ##rassemblement des df des differents jours
# RH_NARVAL_dec<- data.frame()
# for(N in 1:length(list_Narval_30_N_S))
# {
#   Narval_30_N_S<-list_Narval_30_N_S[[N]]
#   RH_NARVAL_dec<- rbind(RH_NARVAL_dec, data.frame(Narval_30_N_S))
# }
# 
# list_Narval_30_N_S_names<-colnames(list_Narval_30_N_S[[1]])
# colnames(RH_NARVAL_dec)<-list_Narval_30_N_S_names
# 
# saveRDS(RH_NARVAL_dec, file = paste0(filename.dir,"RH_NARVAL_dec.rds"))
# RH_NARVAL_dec <- readRDS(file =paste0(filename.dir, "RH_NARVAL_dec.rds"))
# 
# 
# 
# ### attention au pas de temps qui correspondent aussi au lat/lon, donc on va vers des regions plus/moins humides   
# 
# 
# ### !!!!!!!!!!!!!!!! Campagne 2
# dir_narval2<-'/bdd/MEGHA_TROPIQUES/MT_WORKSPACE/NARVAL_data/NARVAL/narval2_rh'
# 
# ## lecture et selection des variables Narval d interet 
# readNARVAL_geo2 = function(filename)
# {
#   var.list = list()
#   ncid <- nc_open(paste(dir_narval2,filename,sep="/"))
#   var.list$lon = ncvar_get(ncid,"longitude")
#   var.list$lat = ncvar_get(ncid,"latitude")
#   var.list$z = c(ncvar_get(ncid,"range"))    ### z, altitude du vol, donc pour avoir les altitude de chaque obs
#   var.list$time = as.POSIXct(c(ncvar_get(ncid,"time")),origin="1970-01-01",tz="UTC")
#   var.list$RH = ncvar_get(ncid,"relative_humidity_liquid")
#   var.list$RH_resolution = ncvar_get(ncid,"range_resolution")   ### epaisseur de la couche observee
#   var.list$temperature = ncvar_get(ncid,"temperature")  ### !!! T° en Kelvin
#   nc_close(ncid)
#   
#   return(var.list)
# }
# 
# ### ouverture des fichiers Narval
# files_NARVAL2<-list.files(path = dir_narval2,full.names = F,pattern = "*.nc",recursive=T) ##creation d'une liste contenant tous les fichiers                  
# 
# 
# ## stockage des variables
# 
# files_of_interest_N_2<-list()
# 
# for (f in 1 : length(files_NARVAL2)){
#   print(f)
#   fileOI<-readNARVAL_geo2 (files_NARVAL2[f])
#   print(files_NARVAL2[f])
#   files_of_interest_N_2 = list.append(files_of_interest_N_2,fileOI)
#   
# }
# 
# list_Narval_30_N_S_2<-list()
# for (i in 1: length(files_of_interest_N_2))
# {
#   print(paste("file num",files_NARVAL2[i]))
#   annee<-str_sub(files_NARVAL2[i],1,4)
#   mois<-str_sub(files_NARVAL2[i],5,6)
#   jour<-str_sub(files_NARVAL2[i],7,8)
#   date_file<-paste(annee, mois, jour, sep="_")
#   
#   
#   #alt_layer_Narval_12_10<-data.frame()
#   
#   # alt_layer<-as.data.frame(files_of_interest_N_2[[i]]$z) ## pour garder les donnees d altitude
#   # alt_layer_Narval_12_10<-rbind(alt_layer_Narval_12_10,data.frame(alt_layer))
#   
#   RH<-t(files_of_interest_N_2[[i]]$RH)  ## transposition des 1610 colonnes (mesures) en lignes, donc maintenant les colonnes correspondent a une altitude et les lignes a une coord X et Y 
#   RH[RH <0 | RH >90] <- NA
#   name_RH<-paste("RH", seq(1,ncol(RH),by=1),sep="")
#   colnames(RH)<-name_RH
#   RH<-RH[,ncol(RH):1]
#   
#   # RH_resolution<-t(files_of_interest_N_2[[i]]$RH_resolution)
#   alt_layer<-files_of_interest_N_2[[i]]$z
#   # alt_layer[alt_layer<0]<-(-alt_layer<0)
#   alt_layer[alt_layer<0]<-NA
#   
#   name_Narval<-c("lon","lat","time",colnames(RH))
#   #paste("Z", seq(1,ncol(alt_layer),by=1),sep=""))
#   Narval<-cbind(files_of_interest_N_2[[i]]$lon,files_of_interest_N_2[[i]]$lat,files_of_interest_N_2[[i]]$time,RH) ## fichier final sur lequel on travaille
#   colnames(Narval)<-name_Narval
#   
#   Narval<-as.data.frame(Narval)
#   Narval$time<-as.POSIXct(Narval$time, tz = "UTC",strptime("1970-01-01 00:00:00 00:00", "%Y-%m-%d %H:%M:%S"), optional = FALSE)
#   # rm(i,alt_layer,RH,name_Narval)
#   
#   ##supprimer les lignes avec seulement des NA
#   for(obs in 1: nrow(Narval))
#   {
#     if(all(is.na( Narval[obs,4:length(Narval)])))
#     {
#       Narval<-Narval[-obs,] 
#     }
#   }
#   
#   ## recuper les indices des lignes a enlever pour les enlever aussi dans RH pour ensuite faire les plots
#   
#   ##### selectionner seulement les traces de vol entre 30°N et 30°S
#   Narval_30_N_S<-subset(Narval,Narval[,2]<=30 |Narval[,2]<=(-30)) ## autre option
#   
#   ### plot pour visualiser les valeurs de RH de NARVAL
#   png(file=paste(filename.plot,"NARVAL_RH_layer_august",i,".png",sep=""), width=1200, height=500, res=200)         
#   #pdf(paste0(filename.plot,"NARVAL_RH_",date_file,".pdf"),width=20,heigh=15)
#   RH_rot <- apply(Narval_30_N_S, 2, rev)
#   RH_flat <- c(RH_rot) ## transformation matrice -> vecteur
#   grid<- expand.grid(X=Narval_30_N_S$time,Y=alt_layer)
#   Z <- RH_flat
#   p <- levelplot(Z~X*Y, grid,
#                  par.settings=viridisTheme(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),
#                  xlab=list("time", cex=1),
#                  ylab=list("altitude (m)", cex=1),
#                  scale=list(x=list(alternating=1,rot=90,format="%d-%m-%y\n%H:%M:%S",cex=1),
#                             y=list(cex=1)))
#   
#   p
#   
#   dev.off()
#   
#   #}
#   #---------------------------------------------------------------------------------------------------
#   #trouver les hPa correspondants aux altitudes   ## a recalculer a chaque fois car hauteur de la couche en fonction de la journee de vol
#   #---------------------------------------------------------------------------------------------------
#   alt_Narval<-alt_layer
#   #layer_Narval<-unique(RH_resolution)
#   altitude_hPa<-c()
#   ## calcul conversion de l altitude en hPa
#   for(alti in 1: length(alt_Narval)){
#     hPa_f_altitude<-1013*(1-((0.0065*alt_Narval[alti])/288.15))^5.255  ## calcul avec pression normalisée (donc sans faire varier la T° avec l altitude)
#     hPa_f_altitude<-round(hPa_f_altitude,0)
#     altitude_hPa<-c(altitude_hPa,hPa_f_altitude)
#     
#   }
#   
#   ## changement des noms de colonnes de RH en hPa
#   Narval_alti_hPa<-Narval_30_N_S
#   names_Narval_30_N_S<-colnames(Narval_30_N_S[,1:3])
#   colnames(Narval_alti_hPa)<-c(names_Narval_30_N_S,altitude_hPa)
#   
#   ## Apres avoir trouver les hPa de SAPHIR correspondant aux altitudes de Narval
#   ## rassembler les couches de Narval, en faire la moyenne et calculer les pdf
#   
#   #selection des colonnes d HR
#   col_RH_NARVAL<-Narval_alti_hPa[,4:length(Narval_alti_hPa)]
#   
#   #moyenne des couches Narval en fonction des couches de SAPHIR
#   nlayer <- 6	# SAPHIR: There are 6 layers for relative humidity defined by their pressure boundaries as follows: 
#   Layer_SAPHIR<-list(100:200, 250:350, 400:600, 650:700, 750:800, 850:950)
#   
#   DF_mean_RH_NARVAL<-data.frame()
#   for(lay in 1:(length(Layer_SAPHIR))){
#     print(paste0("calcul moyenne couche",lay))
#     valt <- as.numeric(colnames(col_RH_NARVAL)) # on recupere les noms des colonnes de RH NARVAL
#     col_mean_RH_NARVAL<-col_RH_NARVAL[,(valt >= min(Layer_SAPHIR[[lay]], na.rm = TRUE)) & (valt <= max(Layer_SAPHIR[[lay]], na.rm = TRUE))] ## selection des colonnes Narval correspondantes aux bornes des couches SAPHIR
#     
#     for (l in 1:nrow(col_mean_RH_NARVAL)){
#       if(all(is.na(col_mean_RH_NARVAL[l,])) | sum(is.na(col_mean_RH_NARVAL[l,]))>=30){mean_RH_NARVAL<-NA}## s il n y a que des NA ou plus de 30 valeurs en NA = la couche reste en NA; 
#       else{
#         mean_RH_NARVAL<-rowMeans(col_mean_RH_NARVAL, na.rm = TRUE) #sinon calcul de la moyenne de la couche
#       }
#     }   
#     if(lay==1){DF_mean_RH_NARVAL<-mean_RH_NARVAL}    
#     else{
#       DF_mean_RH_NARVAL<-cbind(DF_mean_RH_NARVAL,data.frame(mean_RH_NARVAL))
#     }
#   }
#   
#   colnames(DF_mean_RH_NARVAL)<-lablayer
#   
#   
#   list_Narval_30_N_S_2<- list.append(list_Narval_30_N_S_2,DF_mean_RH_NARVAL)
#   
# }
# 
# ##rassemblement des df des differents jours
# RH_NARVAL_aout<- data.frame()
# for(N in 1:length(list_Narval_30_N_S_2))
# {
#   Narval_30_N_S<-list_Narval_30_N_S_2[[N]]
#   RH_NARVAL_aout<- rbind(RH_NARVAL_aout, data.frame(Narval_30_N_S))
# }
# 
# list_Narval_30_N_S_names<-colnames(list_Narval_30_N_S_2[[1]])
# colnames(RH_NARVAL_aout)<-list_Narval_30_N_S_names
# 
# saveRDS(RH_NARVAL_aout, file = paste0(filename.dir,"RH_NARVAL_aout.rds"))
# RH_NARVAL_aout <- readRDS(file =paste0(filename.dir, "RH_NARVAL_aout.rds"))

############################################################################
## PDF CINDY DYNAMOS
############################################################################

list_bdd<-"/bdd/MT_WORKSPACE/CALVAL/RS/CDMT/L3"
patternCDYNAMO<-"profile_rh_1hPa$"
list_files_CDYNAMO<-list.files(list_bdd,full.names = T,pattern = patternCDYNAMO, recursive=TRUE) ##creation d'une liste contenant tous les fichiers      

filename.dir <- paste0(bdd_outputs,"/CDYNAMO")
dir.create(file.path(filename.dir), showWarnings = FALSE)## un dossier par year?


### pour sauvegarder les plots
# filename.plot <- paste0(filename.dir,"/plot/")
# dir.create(file.path(filename.plot), showWarnings = FALSE)  
# 
# 
# #mois_CDYNAMO<-vector("data.frame",12)
# mois_CDYNAMO<-setNames(replicate(12,data.frame()),mois_RS)
# df_CDYNAMO<-data.frame()
# 
# for (fil in 1:length(list_files_CDYNAMO))   
#   #for (fil in 1:12)
# {
#   print(fil)
#   file_name<-list_files_CDYNAMO[fil]        ## recupere le nom du fichier 
#   date<-str_split(file_name,"/")[[1]][9]
#   
#   mois<- substr(date,5,6)
#   
#   # m<-as.numeric(mois_CDYNAMO)
#   read<- read.table(list_files_CDYNAMO[[fil]],header=FALSE,sep=" ", dec = ".")
#   df_temp<-data.frame(read)
#   df_temp<-cbind(df_temp,fil)
#   mois_CDYNAMO[[mois]]<-rbind(mois_CDYNAMO[[mois]],df_temp)
# }
# 
# 
 Layer_SAPHIR<-list(100:200, 250:350, 400:600, 650:700, 750:800, 850:950)
# ##moyenne par fichier par niveau de pression 
# DF_RH_CDYN<-list()
# for(mCD in 1:(length(mois_CDYNAMO)))    ### pour chaque mois de CDYNAMO
# {
#   if (length(mois_CDYNAMO[[mCD]])>0)   ## s il y a quelque chose dans le mois
#   {
#     print(mCD)
#     fichier<-mois_CDYNAMO[mCD]
#     cb<-cbind(fichier[[1]][1],fichier[[1]][2],fichier[[1]][3])	
#     #cb[,3]<-as.numeric(cb[,3])
#     fichier2<-na.omit(cb)
#     fichier2$V2[fichier2$V2<0]=NA
#     fichier2$V2[fichier2$V2>100]=NA
#     fichier2<-na.omit(fichier2)
#     order_f<-fichier2[order(fichier2[,3]),]
#     #test<-as.matrix(order_f$V2)
#     #image.plot(test)	
#     
#     num_file<-unique(order_f$fil)	    ### moyenne de la couche pour chaque fichier du mois
#     DF_mean_RH_CDYNAMO_fichier<-array(0, c(length(num_file),length(Layer_SAPHIR)))
#     k<-1
#     for(n in num_file)   ## pour chaque couche
#     {
#       subset_df<-subset(order_f, fil == n)
#       
#       for(lay in 1:(length(Layer_SAPHIR)))
#       {
#         selec_hPa<-subset_df[(subset_df[,1] >= min(Layer_SAPHIR[[lay]], na.rm = TRUE)) & (subset_df[,1] <= max(Layer_SAPHIR[[lay]], na.rm = TRUE)),] ## selection des lignes de RS correspondantes aux bornes des couches SAPHIR
#         mean_RH_CDYNAMO_temp<-mean(selec_hPa[,2], na.rm=TRUE)
#         DF_mean_RH_CDYNAMO_fichier[k,lay]<-mean_RH_CDYNAMO_temp
#         print(c(k,lay,mean_RH_CDYNAMO_temp))
#       }
#       
#       k<-k+1
#     }
#     
#     
#     
#     DF_RH_CDYN<-list.append(DF_RH_CDYN,DF_mean_RH_CDYNAMO_fichier)
#   }
#   if (length(mois_CDYNAMO[[mCD]])==0)   ## s il n'y a rien dans le mois
#   {
#     no_data<-paste0(mCD," no data")
#     DF_RH_CDYN<-list.append(DF_RH_CDYN,no_data)
#   }
# }
# 
# 
# saveRDS(DF_RH_CDYN, file = paste0(filename.dir,"CDYNAMO_mois.rds"))
DF_RH_CDYN <- readRDS(file =paste0(filename.dir, "CDYNAMO_mois.rds"))





############################################################################
###pdf RH SAPHIR
############################################################################
#year<-c(2012:2016)
year<-2013
month.num=7
mois<-c(1:12)
area<-"Indian_Ocean" 	
Type<-c("T1","T2","T3","T4","T5","T6")

## donnees de SAPHIR

###!!!!!! attention changer date 2015!!!!!!!
#file.dir_SAPHIR <- "/bdd/MEGHA_TROPIQUES/MT_WORKSPACE/SAPHIR_downscaling/Calibration-Data_vmichot/CALIPSO_SAPHIR_coloc_out_all_tropical_seas/allday/BClustering_2015/QRF_prediction/"
#filename <- "/bdd/MEGHA_TROPIQUES/MT_WORKSPACE/SAPHIR_downscaling/Calibration-Data_vmichot/CALIPSO_SAPHIR_coloc_out_all_tropical_seas/allday/fichier_learn_SAPHIR_partiel/QRF_prediction/T0/North_Atlantic_2013_7/QRF_pred_T0_North_Atlantic_7_2013.dat"
#filename <- "/bdd/MEGHA_TROPIQUES/MT_WORKSPACE/SAPHIR_downscaling/Calibration-Data_vmichot/CALIPSO_SAPHIR_coloc_out_all_tropical_seas/allday/fichier_learn_SAPHIR_partiel/QRF_prediction/T0/Indian_Ocean_2013_7/QRF_pred_T0_Indian_Ocean_7_2013.dat"
#RH_SAPHIR_file <- read.table(filename,header=TRUE,sep=";", dec = ".")  # pour toutes les donnees

file.dir_SAPHIR <- paste0(bdd_outputs,"/fichier_learn_SAPHIR_entier/")
filename.dir.out <- paste0(file.dir_SAPHIR,"QRF_prediction/T0/",area,"_",year,"_",month.num,"/")## pour resultats bases sur donnees apprentissage SAPHIR indice partiel
RH_SAPHIR_file<- readRDS(file = paste0(filename.dir.out,"xx_plot_",area,month.num,"_",year,".rds"))

print(paste("nb obs",nrow(RH_SAPHIR_file)))


#for (file in 1:ncol(RH_SAPHIR_file)){}
## pour SAPHIR coarse 
IND_pix_SAPHIR<-RH_SAPHIR_file[,c(grep("IND_pix_SAPHIR",colnames(RH_SAPHIR_file)))]
RH_L1 <- RH_SAPHIR_file[,c(grep("rh1",colnames(RH_SAPHIR_file)))]
RH_L2 <- RH_SAPHIR_file[,c(grep("rh2",colnames(RH_SAPHIR_file)))]
RH_L3 <- RH_SAPHIR_file[,c(grep("rh3",colnames(RH_SAPHIR_file)))]
RH_L4 <- RH_SAPHIR_file[,c(grep("rh4",colnames(RH_SAPHIR_file)))]
RH_L5 <- RH_SAPHIR_file[,c(grep("rh5",colnames(RH_SAPHIR_file)))]
RH_L6 <- RH_SAPHIR_file[,c(grep("rh6",colnames(RH_SAPHIR_file)))]

RH_SAPHIR<- cbind(IND_pix_SAPHIR,RH_L1,RH_L2,RH_L3,RH_L4,RH_L5,RH_L6)
RH_SAPHIR<- as.data.frame(RH_SAPHIR)
names_ind<-unique(RH_SAPHIR$IND_pix_SAPHIR)
pix_unique<-data.frame()
for(nID in 1:length(names_ind))
    {
      num_pix<-which(names_ind[nID]==RH_SAPHIR$IND_pix_SAPHIR)
      selec_pix<-unique(RH_SAPHIR[num_pix,-1])
      pix_unique<-rbind(pix_unique, data.frame(selec_pix))
     }


## pour SAPHIR fine, median
RH_fine_L1 <- RH_SAPHIR_file[,c(grep("rh.fine1",colnames(RH_SAPHIR_file)))]
RH_fine_L2 <- RH_SAPHIR_file[,c(grep("rh.fine2",colnames(RH_SAPHIR_file)))]
RH_fine_L3 <- RH_SAPHIR_file[,c(grep("rh.fine3",colnames(RH_SAPHIR_file)))]
RH_fine_L4 <- RH_SAPHIR_file[,c(grep("rh.fine4",colnames(RH_SAPHIR_file)))]
RH_fine_L5 <- RH_SAPHIR_file[,c(grep("rh.fine5",colnames(RH_SAPHIR_file)))]
RH_fine_L6 <- RH_SAPHIR_file[,c(grep("rh.fine6",colnames(RH_SAPHIR_file)))]

RH_SAPHIR_fine<- cbind(RH_fine_L1,RH_fine_L2,RH_fine_L3,RH_fine_L4,RH_fine_L5,RH_fine_L6)

## pour SAPHIR fine, 1000 predictions
RH_fine_L1_quant <- RH_SAPHIR_file[,c(grep("Lay1_Q.?.",colnames(RH_SAPHIR_file)))]
RH_fine_L2_quant <- RH_SAPHIR_file[,c(grep("Lay2_Q.?.",colnames(RH_SAPHIR_file)))]
RH_fine_L3_quant <- RH_SAPHIR_file[,c(grep("Lay3_Q.?.",colnames(RH_SAPHIR_file)))]
RH_fine_L4_quant <- RH_SAPHIR_file[,c(grep("Lay4_Q.?.",colnames(RH_SAPHIR_file)))]
RH_fine_L5_quant <- RH_SAPHIR_file[,c(grep("Lay5_Q.?.",colnames(RH_SAPHIR_file)))]
RH_fine_L6_quant <- RH_SAPHIR_file[,c(grep("Lay6_Q.?.",colnames(RH_SAPHIR_file)))]

RH_SAPHIR_fine_quant<- cbind(RH_fine_L1_quant,RH_fine_L2_quant,RH_fine_L3_quant,RH_fine_L4_quant,RH_fine_L5_quant,RH_fine_L6_quant)
############################################################################################################################################
 
 filename.plot<-"/bdd/MEGHA_TROPIQUES/MT_WORKSPACE/SAPHIR_downscaling/Calibration-Data_vmichot/CALIPSO_SAPHIR_coloc_out_all_tropical_seas/allday/fichier_learn_SAPHIR_entier/QRF_prediction/PDF_CDF"
 dir.create(file.path(filename.plot), showWarnings = FALSE, recursive=TRUE)

###### Calcul de la densite et plot
 
# ##plot pour les mois de 8 et 12 avec SAPHIR et prediction
# #for (rs in c(8,12))
# #{
# #DF_SAPHIR<-RH_SAPHIR[,rs]
# png(paste0(filename.plot,"/PDF_SAPHIR_O_Indien_7",".png"), width = 1500, height = 1000, units = "px",res=200)
# par(mfrow=c(2,3))
# par(mar=c(4.5,6.5,4,1))
# for(couche in 1: ncol(RH_SAPHIR))
# {
#   lay_name<-lablayer[couche]
#   if(!all(is.na(RH_SAPHIR[,couche])))
#   {
#     dens_SAPHIR<-density(RH_SAPHIR[,couche], na.rm=TRUE,from=0, to=100, cut=TRUE) ##vérifier les fenetres de convolution de density
#     dens_RH_fine<-density(RH_SAPHIR_fine[,couche], na.rm=TRUE,from=0, to=100, cut=TRUE) ##vérifier les fenetres de convolution de density; arg from et to pour etre entre 0 et 100
#     min_plot<-min(c(min(dens_SAPHIR$y),min(dens_RH_fine$y)))
#     max_plot<-max(c(max(dens_SAPHIR$y),max(dens_RH_fine$y)))
#     plot(dens_SAPHIR, bty="n", ylim=c(min_plot,max_plot),col="blue", xlim=c(0,100),lwd=2,xlab="RH in %", main= paste("Distribution of RH at ",lay_name),cex.main=1, cex= 1.4, cex.axis=1.4, cex.lab=1.4, ylab="Density")
#     lines(dens_RH_fine,col="red")
#   }
# }
# dev.off()
# #}
# 
# ##plot pour les mois de 1 à 7 et 9 à 11 avec SAPHIR et RS CNRM     
# 
# for (rs in c(1:7,9:11))
# {
#   DF_mean_RH_RS<-RS_CNRM_mois[[rs]]
#   
#   #pdf(paste0(filename.plot,"/plots_RS_CNRM_PDF_",rs,".pdf"),width=20,heigh=15)
#   par(mfrow=c(2,3))
#   par(mar=c(4.5,6.5,4,1))
#   for(couche in 1: ncol(DF_mean_RH_RS))
#   {
#     lay_name<-colnames(DF_mean_RH_RS[couche])
#     if(!all(is.na(DF_mean_RH_RS[couche])))
#     {
#       dens<-density(DF_mean_RH_RS[[couche]], na.rm=TRUE,from=0, to=100, cut=TRUE) ##vérifier les fenetres de convolution de density
#       dens_RH_SAPH<-density(RH_SAPHIR[[couche]], na.rm=TRUE,from=0, to=100, cut=TRUE) ##vérifier les fenetres de convolution de density; arg from et to pour etre entre 0 et 100
#       dens_RH_fine<-density(RH_SAPHIR_fine[[couche]], na.rm=TRUE,from=0, to=100, cut=TRUE) ##vérifier les fenetres de convolution de density; arg from et to pour etre entre 0 et 100
#       min_plot<-min(c(min(dens_RS$y),min(dens_RH_SAPH$y),min(dens_RH_fine$y)))
#       max_plot<-max(c(max(dens_RS$y),max(dens_RH_SAPH$y),max(dens_RH_fine$y)))
#       plot(dens_RS, bty="n", ylim=c(min_plot,max_plot), xlim=c(0,100),lwd=2,xlab="RH in %", main= paste("Distribution of RH at ",lay_name, "for ",rs),cex.main=1.5, cex= 1, cex.axis=1.5, cex.lab=1.7, ylab="Density")
#       lines(dens_RH_SAPH,col="red")
#       lines(dens_RH_fine,col="blue")
#     }
#   }
#   dev.off()
# } 
# 
# 
# 
# 
# ##############################PDF
# ##plot pour le mois d'aout avec NARVAL, SAPHIR et RS CNRM     
# DF_mean_RH_RS<-RS_CNRM_mois[[8]]
# #pdf(paste0(filename.plot,"plots_RS_CNRM_PDF_",rs,".pdf"),width=20,heigh=15)
# #setEPS()
# #postscript(paste0(filename.plot,"/PDF_all_prod_8",".eps"), horizontal = FALSE, onefile = FALSE, paper = "special")
# png(paste0(filename.plot,"/PDF_all_prod_8",".png"), width = 1500, height = 1000, units = "px",res=200)
# par(mfrow=c(2,3))
# par(mar=c(4.5,6.5,4,1))
# for(couche in 1: ncol(DF_mean_RH_RS))
# {
#   lay_name<-colnames(DF_mean_RH_RS[couche])
#   if(!all(is.na(DF_mean_RH_RS[couche])) & !all(is.na(RH_NARVAL_aout[couche])))
#   {
#     dens_RS<-density(DF_mean_RH_RS[[couche]], na.rm=TRUE,from=0, to=100, cut=TRUE) ##vérifier les fenetres de convolution de density
#     densNarval_aout<-density(RH_NARVAL_aout[[couche]], na.rm=TRUE,from=0, to=100, cut=TRUE) ##vérifier les fenetres de convolution de density
#     dens_RH_SAPH<-density(RH_SAPHIR[,couche], na.rm=TRUE,from=0, to=100, cut=TRUE) ##vérifier les fenetres de convolution de density; arg from et to pour etre entre 0 et 100
#     dens_RH_fine<-density(RH_SAPHIR_fine[,couche], na.rm=TRUE,from=0, to=100, cut=TRUE) ##vérifier les fenetres de convolution de density; arg from et to pour etre entre 0 et 100
#     min_plot<-min(c(min(dens_RS$y),min(densNarval_aout$y),min(dens_RH_SAPH$y),min(dens_RH_fine$y)))
#     max_plot<-max(c(max(dens_RS$y),max(densNarval_aout$y),max(dens_RH_SAPH$y),max(dens_RH_fine$y)))
#     plot(dens_RS, bty="n", ylim=c(min_plot,max_plot),col="black",xlim=c(0,100),lwd=2,xlab="RH in %", main= paste("RH Distribution,",lay_name),cex.main=1.4, cex= 1.4, cex.axis=1.4, cex.lab=1.4, ylab="Density")
#     lines(densNarval_aout,col="green")
#     lines(dens_RH_SAPH,col="blue")
#     lines(dens_RH_fine,col="red")
#     # legend(0.5,3.5,legend=c("NARVAL", "RS CNRM", "SAPHIR", "RH PREDICTION"),  #http://www.sthda.com/french/wiki/ajouter-une-legende-aux-graphiques-avec-le-logiciel-r-comment-prendre-le-controle
#     # col=c("black","green","blue","red"), lty=1, cex=1.2)
#   }
# }
# dev.off()
# 
# 
# ###############################   ECDF   Atlantique ############################################################      
# ##plot pour le mois d'aout avec NARVAL, SAPHIR et RS CNRM     
# DF_mean_RH_RS<-RS_CNRM_mois[[8]]
# #pdf(paste0(filename.plot,"plots_RS_CNRM_PDF_",rs,".pdf"),width=20,heigh=15)
# #setEPS()
# #postscript(paste0(filename.plot,"PDF_all_prod_8",".eps"), horizontal = FALSE, onefile = FALSE, paper = "special")
# png(paste0(filename.plot,"/PDF_all_prod_8",".png"), width = 1500, height = 1000, units = "px",res=200)
# par(mfrow=c(2,3))
# par(mar=c(4.5,6.5,4,1))
# for(couche in 1: ncol(DF_mean_RH_RS))
# {
#   lay_name<-colnames(DF_mean_RH_RS[couche])
#   if(!all(is.na(DF_mean_RH_RS[couche])) & !all(is.na(RH_NARVAL_aout[couche])))
#   {
#     ecdf_RS<-ecdf(DF_mean_RH_RS[[couche]]) 
#     ecdfNarval_aout<-ecdf(RH_NARVAL_aout[[couche]]) 
#     ecdf_RH_SAPH<-ecdf(RH_SAPHIR[,couche]) 
#     ecdf_RH_fine<-ecdf(RH_SAPHIR_fine[,couche])   
#     plot(ecdf_RS, col="black",pch=19,xlim=c(0,100),xlab="RH in %", main= paste("RH Distribution,",lay_name),cex.main=1, cex= 0.2, cex.axis=1.1, cex.lab=1.1, ylab="Cumulative probability")
#     plot(ecdfNarval_aout,do.point=T,add=TRUE,col="green",cex=0.2)
#     plot(ecdf_RH_SAPH,add=TRUE,do.point=T,col="blue", cex=0.2)
#     plot(ecdf_RH_fine,add=TRUE,do.point=T,col="red", cex=0.2)
#     # legend(0.5,3.5,legend=c("NARVAL", "RS CNRM", "SAPHIR", "RH PREDICTION"),  #http://www.sthda.com/french/wiki/ajouter-une-legende-aux-graphiques-avec-le-logiciel-r-comment-prendre-le-controle
#     # col=c("black","green","blue","red"), lty=1, cex=1.2)
#   }
# }
# dev.off()
# 
###############################   ECDF   Ocean indien ############################################################      
##plot pour le mois de juillet avec Cindy dynamo, SAPHIR coarse et SAPHIR fine
CDYNAMO<-DF_RH_CDYN[[7]]
dim(CDYNAMO)
colnames(CDYNAMO)<-Layer_SAPHIR
png(paste0(filename.plot,"/CDF_IndianO_CDYNAMO_7",".png"), width = 1500, height = 1000, units = "px",res=200)
par(mfrow=c(2,3))
par(mar=c(4.5,6.5,4,1))

for(couche in 1: ncol(CDYNAMO))
{
  names_CDYNAMO<-colnames(CDYNAMO)
  lay_name<-names_CDYNAMO[couche]
  #if(!all(is.na(CDYNAMO[couche])))
  #{
  ecdf_CDYNAMO<-ecdf(CDYNAMO[,couche]) 
  ecdf_RH_SAPH<-ecdf(pix_unique[,couche]) 
  ecdf_RH_fine<-ecdf(RH_SAPHIR_fine[,couche])   
  ecdf_RH_fine_quant<-list()
  for(ECDF_q in 1:1000)
  {
    quant<-RH_SAPHIR_fine_quant[,grep(paste0("Lay",couche,"_Q"),colnames(RH_SAPHIR_fine_quant))]
    quant<-quant[,ECDF_q]
    ecdf_quant<-ecdf(quant) 
    ecdf_RH_fine_quant<-list.append(ecdf_RH_fine_quant,ecdf_quant)
  }
  plot(ecdf_RH_fine_quant[[1]],lwd =0.1, col="grey",xlim=c(0,100),xlab="RH in %", main= paste("RH Distribution,",lay_name),cex.main=1, cex= 0.4, cex.axis=1.1, cex.lab=1.1, ylab="Cumulative probability")
  for(plot_q in 2:1000)
    #for(plot_q in c(2,100,500,1000))
  {
    plot(ecdf_RH_fine_quant[[plot_q]],add=TRUE,col="grey", cex=0.2,lwd =0.1)
  }
  plot(ecdf_CDYNAMO,lwd =1,add=TRUE,col="green", cex=0.4)
  plot(ecdf_RH_SAPH,lwd =1,add=TRUE,col="blue", cex=0.4)
  plot(ecdf_RH_fine,lwd =1,add=TRUE,col="red", cex=0.4)
  # legend(0.5,3.5,legend=c("NARVAL", "RS CNRM", "SAPHIR", "RH PREDICTION"),  #http://www.sthda.com/french/wiki/ajouter-une-legende-aux-graphiques-avec-le-logiciel-r-comment-prendre-le-controle
  # col=c("black","green","blue","red"), lty=1, cex=1.2)
  #}
}
dev.off()

## pour faire la legende
par(mar=c(3.5,6.5,4,1))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
plot_colors <- c("green","blue", "red", "grey")
legend("top",inset = 0,legend = c("CINDY DYNAMO","SAPHIR","Prediction (median)","Predictions (1000 quantiles)"),col=plot_colors, lwd=4, cex=1, horiz = TRUE)


# 
# 
# ##plot pour le mois de decembre avec NARVAL, SAPHIR et RS CNRM     
# 
# DF_mean_RH_RS<-RS_CNRM_mois[[12]]
# #pdf(paste0(filename.plot,"plots_RS_CNRM_PDF_",rs,".pdf"),width=20,heigh=15)
# setEPS()
# postscript(paste0(filename.plot,"PDF_all_prod_12",".eps"), horizontal = FALSE, onefile = FALSE, paper = "special")
# par(mfrow=c(2,3))
# par(mar=c(4.5,6.5,4,1))
# for(couche in 1: ncol(DF_mean_RH_RS))
# {
#   lay_name<-colnames(DF_mean_RH_RS[couche])
#   if(!all(is.na(DF_mean_RH_RS[couche])) & !all(is.na(RH_NARVAL_dec[couche])))
#   {
#     dens_RS<-density(DF_mean_RH_RS[[couche]], na.rm=TRUE,from=0, to=100, cut=TRUE) ##vérifier les fenetres de convolution de density
#     densNarval_dec<-density(RH_NARVAL_dec[[couche]], na.rm=TRUE,from=0, to=100, cut=TRUE) ##vérifier les fenetres de convolution de density
#     dens_RH_SAPH<-density(RH_SAPHIR[,couche], na.rm=TRUE,from=0, to=100, cut=TRUE) ##vérifier les fenetres de convolution de density; arg from et to pour etre entre 0 et 100
#     dens_RH_fine<-density(RH_SAPHIR_fine[,couche], na.rm=TRUE,from=0, to=100, cut=TRUE) ##vérifier les fenetres de convolution de density; arg from et to pour etre entre 0 et 100
#     min_plot<-min(c(min(dens_RS$y),min(densNarval_dec$y),min(dens_RH_SAPH$y),min(dens_RH_fine$y)))
#     max_plot<-max(c(max(dens_RS$y),max(densNarval_dec$y),max(dens_RH_SAPH$y),max(dens_RH_fine$y)))
#     plot(dens_RS, bty="n", ylim=c(min_plot,max_plot),xlim=c(0,100),lwd=2,xlab="RH in %", main= paste("RH Distribution,",lay_name),cex.main=1.05, cex= 1.2, cex.axis=1.2, cex.lab=1.2, ylab="Density")
#     lines(densNarval_dec,col="green")
#     lines(dens_RH_SAPH,col="blue")
#     lines(dens_RH_fine,col="red")
#     legend(0.5,3.5,legend=c("NARVAL", "RS CNRM", "SAPHIR", "RH PREDICTION"),  #http://www.sthda.com/french/wiki/ajouter-une-legende-aux-graphiques-avec-le-logiciel-r-comment-prendre-le-controle
#            col=c("black","green","blue","red"), lty=1, cex=1.2)
#   }
# }
# dev.off()
# 
