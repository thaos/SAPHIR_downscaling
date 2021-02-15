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

match.CALIPSO.SAPHIR.CLOUDSAT<- read.table(filename,header=TRUE,sep=";", dec = ".")  # pour toutes les donnees

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
 
   ##faire la liste de tous les profils du fichier PHASE_nuage
   list_profil<-character(nrow(PHASE_nuage))
   for (i in 1:nrow(PHASE_nuage))
   {
     profil_i<-PHASE_nuage[i,]
     Phases_profil <- unique(profil_i)
     Order_phases <- sort(Phases_profil)
     list_profil[i] <- paste(Order_phases, collapse = ";")
   }
   profil_freq <- count(list_profil)
   profil_prop <- profil_freq
   profil_prop$freq <-as.numeric(profil_prop$freq)
   profil_prop$pourcent <-round((profil_freq$freq *100)/ nrow(PHASE_nuage),2)
   #profil_prop$freq <-profil_prop$freq / nrow(PHASE_nuage)
   profil_prop<-profil_prop[order(profil_prop[,3],decreasing=F), ]
   write.table(profil_prop, file = paste0(bdd_data,"/ALL_SAPHIR_DOWNSCALING_SCRIPTS/2_split_learning_and_prediction_by_ocean/profils_prop_FICHIERentier.csv"), dec=".",col.names=T,row.names=F,append = FALSE, sep = " ")

  ## graph 
  par(mar=c(14,5,2,2))
  barplot(profil_prop$pourcent, ylab="% de chaque profil",names.arg= profil_prop$x,cex.names=0.8,las=2,ylim=c(0,max(profil_prop$pourcent)))


  ###################################################################################################

  ###### echantillon d apprentissage
  ## determiner les pixels SAPHIR qui serviront a l apprentissage
  names_ind<-unique(match.CALIPSO.SAPHIR.CLOUDSAT$IND_pix_SAPHIR)
  sample_apprent<- sample(names_ind,(length(names_ind)*80)/100)
  sample_apprent<- sort(sample_apprent)


  ##creation d un vecteur avec les indices des lignes correspondantes au sample
  same_ind<-c()
  for(nID in 1:length(sample_apprent))
  {
    same_ind<-c(same_ind,which(sample_apprent[nID]==match.CALIPSO.SAPHIR.CLOUDSAT$IND_pix_SAPHIR))
  }

  ### selection d un echantillon aleatoire
  data_sample<-match.CALIPSO.SAPHIR.CLOUDSAT[same_ind,]

  # nombre de lignes, donc de pixels CALIPSO, de l echantillon d apprentissage
  nb_pix_CALIPSO<-nrow(data_sample)
  print(paste0("nombre de pixels CALIPSO dans l'apprentissage ", nb_pix_CALIPSO))

  # nombre de pixels SAPHIR dans l echantillon d apprentissage
  nb_pixel_SAPHIR<-length(sample_apprent)
  print(paste0("nombre de pixels SAPHIR dans l'apprentissage ", nb_pixel_SAPHIR))

  #double verification : les pixels SAPHIR ne contiennent qu une valeur de HR
  ID2<-unique(data_sample$IND_pix_SAPHIR)
  df_px2<-data.frame()
  for(x in 1:length(ID2))
  {
    find_px2<-which(ID2[x]==data_sample$IND_pix_SAPHIR)
    selec2<-data_sample[find_px2,]
    rh_px2<-unique(selec2[,grep("rh",colnames(selec2))][1:6])
    nom_px2<-unique(selec2[,grep("IND_pix_SAPHIR",colnames(selec2))])
    if(nrow(rh_px2)>1){
      " pix >1 valeur"
    }
    df_px2<-rbind(df_px2, data.frame(rh_px2,nom_px2))
  }

  pix_duplic3<-sum(duplicated(df_px2$nom_px2))

  #### Classification par phase de nuages pour l echantillon d apprentissage
  ##Selection des phases
  PHASE_nuage_app <- data_sample[,grep("PHASE",colnames(data_sample))]
  PHASE_nuage_app <-as.matrix(PHASE_nuage_app)
  PHASE_nuage_app [PHASE_nuage_app ==0] <- "CLEAR"
  PHASE_nuage_app [PHASE_nuage_app ==1] <- "LIQ"
  PHASE_nuage_app [PHASE_nuage_app ==2] <- "ICE"
  PHASE_nuage_app [PHASE_nuage_app ==3] <- "UNDEF"
  PHASE_nuage_app [PHASE_nuage_app ==4] <- "F_LIQ"
  PHASE_nuage_app [PHASE_nuage_app ==5] <- "F_ICE"
  PHASE_nuage_app [PHASE_nuage_app ==6] <- "Horiz"
  PHASE_nuage_app [PHASE_nuage_app ==8] <- "FA"
 
  ##faire la liste de tous les profils de l echantillon d apprentissage PHASE_nuage
  list_profil_app<-character(nrow(PHASE_nuage_app))
  for (l in 1:nrow(PHASE_nuage_app))
  {
    profil_l_app<-PHASE_nuage_app[l,]
    Phases_profil_app <- unique(profil_l_app)
    Order_phases_app <- sort(Phases_profil_app)
    list_profil_app[l] <- paste(Order_phases_app, collapse = ";")
  }
  profil_freq_app <- count(list_profil_app)
  profil_prop_app <- profil_freq_app
  profil_prop_app$freq <-as.numeric(profil_prop_app$freq)
  profil_prop_app$pourcent <-round((profil_freq_app$freq *100)/ nrow(PHASE_nuage_app),2)
 # profil_prop_app$freq <-profil_prop_app$freq / nrow(PHASE_nuage_app)
  profil_prop_app<-profil_prop_app[order(profil_prop_app[,3],decreasing=F), ]
  write.table(profil_prop_app, file = paste0(bdd_data,"/ALL_SAPHIR_DOWNSCALING_SCRIPTS/2_split_learning_and_prediction_by_ocean/profils_prop_APPRENTISSAGE.csv"), dec=".",col.names=T,row.names=F,append = FALSE, sep = " ")

  ## graph 
  par(mar=c(14,5,2,2))
  barplot(profil_prop_app$pourcent, ylab="fréquence de chaque profil",names.arg= profil_prop_app$x,cex.names=0.8,las=2)



  ###### echantillon de test
  ### reste du fichier pour les predictions/tests
  other_data<-match.CALIPSO.SAPHIR.CLOUDSAT[-same_ind,]

  #### Classification par phase de nuages pour l echantillon de test
  ##Selection des phases
  PHASE_nuage_test <- other_data[,grep("PHASE",colnames(other_data))]
  PHASE_nuage_test <-as.matrix(PHASE_nuage_test)
  PHASE_nuage_test [PHASE_nuage_test ==0] <- "CLEAR"
  PHASE_nuage_test [PHASE_nuage_test ==1] <- "LIQ"
  PHASE_nuage_test [PHASE_nuage_test ==2] <- "ICE"
  PHASE_nuage_test [PHASE_nuage_test ==3] <- "UNDEF"
  PHASE_nuage_test [PHASE_nuage_test ==4] <- "F_LIQ"
  PHASE_nuage_test [PHASE_nuage_test ==5] <- "F_ICE"
  PHASE_nuage_test [PHASE_nuage_test ==6] <- "Horiz"
  PHASE_nuage_test [PHASE_nuage_test ==8] <- "FA"

  ##faire la liste de tous les profils de l echantillon d apprentissage PHASE_nuage
  list_profil_test<-character(nrow(PHASE_nuage_test))
  for (l in 1:nrow(PHASE_nuage_test))
  {
    profil_l_test<-PHASE_nuage_test[l,]
    Phases_profil_test <- unique(profil_l_test)
    Order_phases_test <- sort(Phases_profil_test)
    list_profil_test[l] <- paste(Order_phases_test, collapse = ";")
  }
  profil_freq_test <- count(list_profil_test)
  profil_prop_test <- profil_freq_test
  profil_prop_test$freq <-as.numeric(profil_prop_test$freq)
  profil_prop_test$pourcent <-round((profil_freq_test$freq *100)/ nrow(PHASE_nuage_test),2)
  # profil_prop_test$freq <-profil_prop_test$freq / nrow(PHASE_nuage_test)
  profil_prop_test<-profil_prop_test[order(profil_prop_test[,3],decreasing=F), ]
  write.table(profil_prop_test, file = paste0(bdd_data,"/ALL_SAPHIR_DOWNSCALING_SCRIPTS/2_split_learning_and_prediction_by_ocean/profils_prop_TEST.csv"), dec=".",col.names=T,row.names=F,append = FALSE, sep = " ")
  ## graph 
  par(mar=c(14,5,2,2))
  barplot(profil_prop_test$pourcent, ylab="fréquence de chaque profil",names.arg= profil_prop_test$x,cex.names=0.8,las=2)
  
#   
#   ################# PROPORTION des Phases de nuage dans chaque echantillon
#   ## Occurrence par type de nuage dans la base d apprentissage
#   print("Occurrence par type de nuage")
#   
#   PHASE_nuage <- data_sample[,grep("PHASE",colnames(data_sample))]
#   No_cloud<-sum(PHASE_nuage==0)
#   LIQ<-sum(PHASE_nuage==1)
#   Ice<-sum(PHASE_nuage==2)
#   UNDEFINED<-sum(PHASE_nuage==3)
#   FALSE_LIQ<-sum(PHASE_nuage==4)
#   FALSE_ICE<-sum(PHASE_nuage==5)
#   Horizontal_Or<-sum(PHASE_nuage==6)
#   FA<-sum(PHASE_nuage==8)
#   
#   print(c(paste("BASE d'APPRENTISSAGE: ","CLEAR_no_cloud ",No_cloud,"; LIQ ",LIQ,"; Ice ",Ice,"; UNDEFINED ",UNDEFINED,"; FALSE_LIQ ",FALSE_LIQ,"; FALSE_ICE ",FALSE_ICE,
#                 "; Horizontal_Or ",Horizontal_Or,"; FA",FA)))
#   
#   
#   ## Occurrence par type de nuage dans la base d apprentissage
#   print("Occurrence par type de nuage")
#   
#   PHASE_nuage_test <- other_data[,grep("PHASE",colnames(other_data))]
#   No_cloud_test<-sum(PHASE_nuage_test==0)
#   LIQ_test<-sum(PHASE_nuage_test==1)
#   Ice_test<-sum(PHASE_nuage_test==2)
#   UNDEFINED_test<-sum(PHASE_nuage_test==3)
#   FALSE_LIQ_test<-sum(PHASE_nuage_test==4)
#   FALSE_ICE_test<-sum(PHASE_nuage_test==5)
#   Horizontal_Or_test<-sum(PHASE_nuage_test==6)
#   FA_test<-sum(PHASE_nuage_test==8)
#   
#   print(c(paste("BASE de TEST: ","CLEAR_no_cloud ",No_cloud_test,"; LIQ ",LIQ_test,"; Ice ",Ice_test,"; UNDEFINED ",UNDEFINED_test,"; FALSE_LIQ ",FALSE_LIQ_test,"; FALSE_ICE ",FALSE_ICE_test,
#                 "; Horizontal_Or ",Horizontal_Or_test,"; FA",FA_test)))
#   
#   ## Proportion de nuage par rapport a la base d apprentissage
#   nb_obs_matrice<-nrow(PHASE_nuage)*40 # 40 niveau pour CALIPSO
#   prop_No_cloud_learn<-round((No_cloud*100)/nb_obs_matrice,1)
#   prop_LIQ_learn<-round((LIQ*100)/nb_obs_matrice,1)
#   prop_Ice_learn<-round((Ice*100)/nb_obs_matrice,1)
#   prop_UNDEFINED_learn<-round((UNDEFINED*100)/nb_obs_matrice,1)
#   prop_FALSE_LIQ_learn<-round((FALSE_LIQ*100)/nb_obs_matrice,1)
#   prop_FALSE_ICE_learn<-round((FALSE_ICE*100)/nb_obs_matrice,1)
#   prop_Horizontal_Or_learn<-round((Horizontal_Or*100)/nb_obs_matrice,1)
#   prop_FA_learn<-round((FA*100)/nb_obs_matrice,1)
#   prop_FALSE_LIQ_learn<-round((FALSE_LIQ*100)/nb_obs_matrice,1)
#   
#   ## Proportion de nuage par rapport a la base de test
#   nb_obs_matrice<-nrow(PHASE_nuage_test)*40 # 40 niveau pour CALIPSO
#   prop_No_cloud_test<-round((No_cloud_test*100)/nb_obs_matrice,1)
#   prop_LIQ_test<-round((LIQ_test*100)/nb_obs_matrice,1)
#   prop_Ice_test<-round((Ice_test*100)/nb_obs_matrice,1)
#   prop_UNDEFINED_test<-round((UNDEFINED_test*100)/nb_obs_matrice,1)
#   prop_FALSE_LIQ_test<-round((FALSE_LIQ_test*100)/nb_obs_matrice,1)
#   prop_FALSE_ICE_test<-round((FALSE_ICE_test*100)/nb_obs_matrice,1)
#   prop_Horizontal_Or_test<-round((Horizontal_Or_test*100)/nb_obs_matrice,1)
#   prop_FA_test<-round((FA_test*100)/nb_obs_matrice,1)
#   prop_FALSE_LIQ_test<-round((FALSE_LIQ_test*100)/nb_obs_matrice,1)
#   
#   ## graph pie chart base d apprentissage
#   lbls<-c(paste("No_cloud",prop_No_cloud_learn),paste("LIQ",prop_LIQ_learn),paste("Ice",prop_Ice_learn),paste("UNDEFINED",prop_UNDEFINED_learn), paste("FALSE_LIQ",prop_FALSE_LIQ_learn),
#           paste("FALSE_ICE",prop_FALSE_ICE_learn),paste("Horizontal_Or",prop_Horizontal_Or_learn),paste("FA",prop_FA_learn),paste("FALSE_LIQ",prop_FALSE_LIQ_learn))
#   slices_apprentissage<- c(prop_No_cloud_learn,prop_LIQ_learn,prop_Ice_learn,prop_UNDEFINED_learn,prop_FALSE_LIQ_learn,prop_FALSE_ICE_learn,prop_Horizontal_Or_learn,prop_FA_learn,prop_FALSE_LIQ_learn)
#   pie(slices_apprentissage,labels=lbls,col=rainbow(length(lbls)))
#   
#   ## graph pie chart base de test
#   lbls<-c(paste("No_cloud",prop_No_cloud_test),paste("LIQ",prop_LIQ_test),paste("Ice",prop_Ice_test),paste("UNDEFINED",prop_UNDEFINED_test), paste("FALSE_LIQ",prop_FALSE_LIQ_test),
#           paste("FALSE_ICE",prop_FALSE_ICE_test),paste("Horizontal_Or",prop_Horizontal_Or_test),paste("FA",prop_FA_test),paste("FALSE_LIQ",prop_FALSE_LIQ_test))
#   slices_apprentissage_test<- c(prop_No_cloud_test,prop_LIQ_test,prop_Ice_test,prop_UNDEFINED_test,prop_FALSE_LIQ_test,prop_FALSE_ICE_test,prop_Horizontal_Or_test,prop_FA_test,prop_FALSE_LIQ_test,prop_FALSE_LIQ_test)
#   pie(slices_apprentissage_test,labels=lbls,col=rainbow(length(lbls)))
#   
#   
#   ## ecriture des fichiers
#   filename.dir <- paste0(bdd_outputs,"/fichier_par_mois_par_bassin/")
#   dir.create(file.path(filename.dir), showWarnings = FALSE, recursive=TRUE)
#   
#   write.table(other_data,paste0(filename.dir,"/CALIPSO_SAPHIR_CLOUDSAT_",year,"-",month.num,"_",dt,"_",dx,"_",area,"_",
#                                 "for_prediction.dat"),col.names=TRUE,row.names=FALSE,append=FALSE,sep=";",quote=FALSE) 		###donnees a utiliser ensuite dans les predictions qrf
#   
#   write.table(data_sample,paste0(filename.dir,"/CALIPSO_SAPHIR_CLOUDSAT_",year,"-",month.num,"_",dt,"_",dx,"_",area,"_",
#                                  "for_learning.dat"),col.names=TRUE,row.names=FALSE,append=FALSE,sep=";",quote=FALSE) 		###donnees a utiliser ensuite dans l apprentissage qrf
#   
#   return (data_sample)
# }   
# 
# ## *********************************************************************************************************************
# ##      Launcher
# ##	    area = Indian_Ocean, North_Atlantic
# ##          year = 2013 and 2016 
# ##          month.num = 7,8,12
# ## *********************************************************************************************************************
# args = commandArgs(trailingOnly = TRUE)
# if (length(args) == 3)
# {
#   area = args[1]
#   year = args[2]
#   month.num=args[3]
#   split_files(area, year, month.num)
# }
# 
# 
# 
# 
