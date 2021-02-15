# ## PATHS
source("/bdd/MT_WORKSPACE/SAPHIR_downscaling/Calibration-Data_vmichot/CALIPSO_SAPHIR_coloc_out_all_tropical_seas/allday/ALL_SAPHIR_DOWNSCALING_SCRIPTS/Setpath.R")
library(rlist)
library(plyr)
## script pour separer les donnees d apprentissage de celles de predictions
## ici donnees d apprentissage avec les Indices de SAPHIR (indice par pixel cree dans le script 1_prep_donnees_downscal.R)


## **********************************************************************************************************************
##	Global varaibles
## **********************************************************************************************************************
dx <- 5 																								# Maximum distance [km]
dt <- 30  																								# Maximum temporal separation [min]

area="Indian_Ocean"
year=2013
month.num=07


## fonction pour separer les echantillons
#split_files<-function(area,year,month.num)
#{
filename <-  paste0(bdd_data,"/CALIPSO_SAPHIR_CLOUDSAT_",year,"-",month.num,"_",dt,"_",dx,"_",area,"_","for_downscal.dat")

# library(base)
# if(!file.exists(filename))
# {
#   next
#   print(c(file," file does't exist"))
# }

match.CALIPSO.SAPHIR.CLOUDSAT<- read.table(filename,header=TRUE,sep=";", dec = ".",nrow=1000)  # pour toutes les donnees

#print(c(area,year,month.num))

# 
# #verification que les pixels SAPHIR ne contiennent bien qu une valeur d HR
# ID<-unique(match.CALIPSO.SAPHIR.CLOUDSAT$IND_pix_SAPHIR)
# df_px<-data.frame()
# for(x in 1:length(ID))
# {
#   find_px<-which(ID[x]==match.CALIPSO.SAPHIR.CLOUDSAT$IND_pix_SAPHIR)
#   selec<-match.CALIPSO.SAPHIR.CLOUDSAT[find_px,]
#   rh_px<-unique(selec[,grep("rh",colnames(selec))][1:6])
#   nom_px<-unique(selec[,grep("IND_pix_SAPHIR",colnames(selec))])
#   if(nrow(rh_px)>1){
#     " pix >1 valeur"
#   }
#   df_px<-rbind(df_px, data.frame(rh_px,nom_px))  
# }    
# 
#pix_duplic2<-sum(duplicated(df_px$nom_pix))

  ###################################################################################################
  #### Classification par phase de nuages pour l ensemble du fichier

  ##Selection des phases
   PHASE_nuage <- match.CALIPSO.SAPHIR.CLOUDSAT[,grep("PHASE",colnames(match.CALIPSO.SAPHIR.CLOUDSAT))]
   PHASE_nuage <-as.matrix(PHASE_nuage)
   PHASE_nuage[PHASE_nuage==0] <- "CLEAR"
   PHASE_nuage[PHASE_nuage==1] <- "LIQ"
   PHASE_nuage[PHASE_nuage==2] <- "ICE"
   PHASE_nuage[PHASE_nuage==3] <- "UNDEF"
   PHASE_nuage[PHASE_nuage==4] <- "F_LIQ"
   PHASE_nuage[PHASE_nuage==5] <- "F_ICE"
   PHASE_nuage[PHASE_nuage==6] <- "Horiz"
   PHASE_nuage[PHASE_nuage==8] <- "FA"
   Profil_RH <- match.CALIPSO.SAPHIR.CLOUDSAT[,grep("rh",colnames(match.CALIPSO.SAPHIR.CLOUDSAT))][, 1:6]
   ##faire la liste de tous les profils du fichier PHASE_nuage
   list_profil_nuage<-character(nrow(PHASE_nuage))
   for (i in 1:nrow(PHASE_nuage))
   {
     profil_nuage_i<-PHASE_nuage[i,]
     Phases_profil <- unique(profil_nuage_i)
     Order_phases <- sort(Phases_profil)
     list_profil_nuage[i] <- paste(Order_phases, collapse = ";")
   }

   Profil_RH_molten <- data.frame(
    profile = rep(list_profil_nuage, 6),
    level = rep(1:6, rep(nrow(Profil_RH), 6)),
    rh = unlist(Profil_RH),
    stringsAsFactors = FALSE
   )
   
  pdf("/bdd/MT_WORKSPACE/SAPHIR_downscaling/Calibration-Data_vmichot/CALIPSO_SAPHIR_coloc_out_all_tropical_seas/allday/ALL_SAPHIR_DOWNSCALING_SCRIPTS/2_split_learning_and_prediction_by_ocean/rh_distbyprof.pdf")
  for(profile in unique(list_profil_nuage))
   {
     df <- Profil_RH_molten[Profil_RH_molten$profile == profile, ]
     boxplot(rh ~ profile + level, data = df, main = paste("n =", nrow(df)/6))
   
   }
dev.off()
  