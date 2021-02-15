

##Script downscal = dowscaling a partir des models qrf crees dans les scripts 3_downscal_MODEL_*.R

source("/bdd/MT_WORKSPACE/SAPHIR_downscaling/Calibration-Data_vmichot/CALIPSO_SAPHIR_coloc_out_all_tropical_seas/allday/ALL_SAPHIR_DOWNSCALING_SCRIPTS/Comp_func_VM.R")

## PATHS
source("/bdd/MT_WORKSPACE/SAPHIR_downscaling/Calibration-Data_vmichot/CALIPSO_SAPHIR_coloc_out_all_tropical_seas/allday/ALL_SAPHIR_DOWNSCALING_SCRIPTS/Setpath.R")
library(rlist)

#---------------------------------------------------------------------------------------------------
#	SAPHIR parameters
#---------------------------------------------------------------------------------------------------
nlayer <- 6																								# SAPHIR: There are 6 layers for relative humidity defined by their pressure boundaries as follows: 
lablayer = c("100-200 hPa","250-350 hPa", "400-600 hPa","650-700 hPa","750-800 hPa", "850-950 hPa")

#---------------------------------------------------------------------------------------------------
#	CALIPSO parameters
#---------------------------------------------------------------------------------------------------
resprofile <- 0.48		
alt <- seq(0.24,18.96,by=resprofile)
#---------------------------------------------------------------------------------------------------

## FONCTION de DOWNSCALING sur l echantillon de test
downscall_TYPE_0<-function(area,year,month.num)
{
  
  print("start function")
  #---------------------------------------------------------------------------------------------------
  #	Set output directory
  #---------------------------------------------------------------------------------------------------
  filename.dir <- paste0(bdd_outputs,"/4_downscal_test/")
  
  #Sauvegarde des fichiers
  filename.dir.out <- paste0(filename.dir,"QRF_prediction/",area,"_",year,"_",month.num,"/")
  dir.create(file.path(filename.dir.out), showWarnings = FALSE, recursive=TRUE)
  
  
  #---------------------------------------------------------------------------------------------------
  #	Set input filename
  #---------------------------------------------------------------------------------------------------
 # modeles QRF pour chaque couche d HR
  filename.dir_QRF<-"/bdd/MEGHA_TROPIQUES/MT_WORKSPACE/SAPHIR_downscaling/Calibration-Data_vmichot/CALIPSO_SAPHIR_coloc_out_all_tropical_seas/allday/ALL_SAPHIR_DOWNSCALING_SCRIPTS/3_downscal_MODEL/QRF_model_entire_timeseries/2013"
  pattern_QRF<-paste0("qrf_Indian_Ocean_7_2013MODEL_.?._entire_learn_sample.rds")  ##ici le .?. represente le num de la couche
  list_QRF<-list.files(filename.dir_QRF,full.names = T,pattern = pattern_QRF, recursive=TRUE) ##creation d'une liste contenant tous les fichiers      


 # echantillon de prediction d humidite relative
 filepred <- paste0("/bdd/MEGHA_TROPIQUES/MT_WORKSPACE/SAPHIR_downscaling/Calibration-Data_vmichot/CALIPSO_SAPHIR_coloc_out_all_tropical_seas/allday/fichier_par_mois_par_bassin/CALIPSO_SAPHIR_CLOUDSAT_",year,"-",month.num,"_30_5_",area,"_for_prediction.dat")
    
  library(base)
  if(!file.exists(filepred))
  {
    next
  }
  
  match.CALIPSO.SAPHIR.CLOUDSAT <- read.table(filepred,header=TRUE,sep=";", dec = ".")
  match.CALIPSO.SAPHIR.CLOUDSAT[match.CALIPSO.SAPHIR.CLOUDSAT==Inf]<-150 # transforme les FA (Inf) pour faire le downscaling
  
  new_data<-match.CALIPSO.SAPHIR.CLOUDSAT
  new_data<- subset(new_data, new_data$rh5 < 95) # Select RH profiles where RH_L5 is less than 95% (error in the retrieval)
  print(paste0("nb obs a traiter ", nrow(new_data)))
  
  ## Noms des plots
  plotlab <- paste0("plot_entire_series_PRED_T0_",area,"_",month.num,"_",year)
  plot_part_serie <- paste0("plot_part_serie_PRED_T0_",area,"_",month.num,"_",year)
  plotimportance<-paste0("plot_importance_PRED_T0_",area,"_",month.num,"_",year)
  plot_predict_obs<-paste0("plot_predict_obs_PRED_T0_",area,"_",month.num,"_",year)
  plot_predict_obs_multiR2<-paste0("plot_predict_obs_PRED_multiR2_T0_",area,"_",month.num,"_",year)
  plot_CRPS<-paste0("plot_CRPS_PRED_T0_",area,"_",month.num,"_",year)
  
  filename.plot <- paste0(filename.dir.out,plotlab)
  filename.plot_part <- paste0(filename.dir.out,plot_part_serie)
  filename.plot_imp <- paste0(filename.dir.out,plotimportance)
  filename.plot_predict_obs<-paste0(filename.dir.out,plot_predict_obs)
  filename.plot_predict_obs_multiR2<-paste0(filename.dir.out,plot_predict_obs_multiR2)
  filename.plot_CRPS<-paste0(filename.dir.out,plot_CRPS)
  filename.dat <- paste0(filename.dir.out,"QRF_pred_T0_",area,"_",month.num,"_",year,".dat")
  
  
  ####### Preparation des donnees pour faire le downscaling
  ### definition des variables servant a la prediction
  new_predictors<-new_data[,1:85]
  
  ## Pour faire tourner la fonction de prediction, passer les flag de phases de nuage en facteur PHASE 
  PHASE <- new_predictors[,grep("PHASE",colnames(new_predictors))]
  PHASE[PHASE==0] <- "CLEAR"
  PHASE[PHASE==1] <- "LIQ"
  PHASE[PHASE==2] <- "ICE"
  PHASE[PHASE==3] <- "UNDEFINED"
  PHASE[PHASE==4] <- "FALSE LIQ"
  PHASE[PHASE==5] <- "FALSE ICE"
  PHASE[PHASE==6] <- "Horizontally Oriented"
  PHASE[PHASE==8] <- "FA"
  for(sr in 1:ncol(PHASE))
  {
    PHASE[, sr]<- factor(PHASE[, sr], levels=c("CLEAR","LIQ","ICE","UNDEFINED","FALSE LIQ","FALSE ICE","Horizontally Oriented","FA"))
  }
  new_predictors[,grep("PHASE",colnames(new_predictors))] <- PHASE

  
  nom_quantiles<-paste0("Q",seq(1,1000,1))
  
   
  ##############################################################################
  #### DOWNSCALLING

  xx <- data.frame()
  rsq<-list()
  rsq_median<-list()
  dfRMSE<-list()
  BIAIS<-list()
  for(q in 1 : length (list_QRF)){
    
    #ouvrir le fichier qrf
    model<-readRDS(list_QRF[q])
      
    ##climatologie de rh coarse
    data_rh<-(new_data[,grep(paste0("rh",q),colnames(new_data))])/100  ##donnees SAPHIR coarse resolution
    rh_clim.quant <- quantile(data_rh, probs=seq(5,95)/100)
    rh_clim.mat <- t(matrix(rep(rh_clim.quant,nrow(new_data)), ncol = nrow(new_data)))  
   
    ## prediction pour la mediane
    rh.fine<-predict(model, what=0.5,newdata=new_predictors)
    rh.fine[rh.fine<0]<-0
    rh.fine[rh.fine>1.00]<-1
    
    ## prediction de l ecart interquartile
    dep.fine.unc<-predict(model, what=0.75,newdata=new_predictors)-predict(model, what=0.25,newdata=new_predictors)
    dep.fine.unc<-dep.fine.unc*100    

    ## prediction pour les 1000 quantiles par emprunte SAPHIR
    names_ind<-unique(new_data$IND_pix_SAPHIR)
    rh.fine_quant<-data.frame()
    for(ni in 1:length(names_ind))
       {
          # definir le pixel SAPHIR basse resolution
          pix_SAPHIR<-which(names_ind[ni]==new_data$IND_pix_SAPHIR)
          predictors_pix<-new_predictors[pix_SAPHIR,]

          ## permutation des quantiles
         #permute_Quant<-permute(quantiles)
          quantiles<-runif(1000)

          ##
          rh.fine_quant_temp<-predict(model, what=quantiles,newdata=predictors_pix)
          colnames(rh.fine_quant_temp)<-nom_quantiles
          rh.fine_quant_temp[rh.fine_quant_temp<0]<-0
          rh.fine_quant_temp[rh.fine_quant_temp>1.00]<-1
          rh.fine_quant<-rbind(rh.fine_quant,data.frame(rh.fine_quant_temp))
          #colnames(rh.fine_quant)<-nom_quantiles

    }
    colnames(rh.fine_quant)<-nom_quantiles
    
    ### R2 : moyenne des predictions pour pouvoir comparer a la resolution originale
    ## determiner les pixels 
    noms_col<-c(colnames(new_data),nom_quantiles,paste0("rh_fine",q))
    df_data_temp<-cbind(new_data,rh.fine_quant,rh.fine)
    colnames(df_data_temp)<-noms_col
    
    ##trouver les indices des lignes correspondantes au pixel + calculer la moyenne
    moyennes_pred<-data.frame()
    noms_col<-c("IND_pix_SAPHIR",paste0("rh",1:6),paste0(nom_quantiles,"L",q),paste0("mean_rh_fine",q))
    for(nID in 1:length(names_ind))
    {
      num_pix<-which(names_ind[nID]==df_data_temp$IND_pix_SAPHIR)
      df_pix_pred<-df_data_temp[num_pix,]
      Quant_du_pixel<-df_pix_pred[,grep("Q",colnames(df_pix_pred))]
      rh_coarse<-unique(df_pix_pred[,grep("rh",colnames(df_pix_pred))][1:6])
      moy_pred_pix<-apply(Quant_du_pixel,2,mean)
      moy_pred_pix<-moy_pred_pix*100
      moy_pred_median<-mean(df_pix_pred[,paste0("rh_fine",q)])
      moy_pred_median<-moy_pred_median*100
      moyennes_pred<-rbind(moyennes_pred, data.frame( names_ind[nID], rh_coarse,t(moy_pred_pix), moy_pred_median))

    } 
    colnames(moyennes_pred)<-noms_col
    
    
    ## calcul rsq de la mediane
    rh_pix<-moyennes_pred[,grep(paste0("rh",q),colnames(moyennes_pred))]
    rh_pred_median<-moyennes_pred[,grep(paste0("mean_rh_fine",q),colnames(moyennes_pred))]
    rsq_med<-cor(rh_pred_median,rh_pix)^2
    rsq_median<-list.append(rsq_median, rsq_med)
    
    ## calcul du RMSE de chaque couche predite avec la médiane
    RMSE<-sqrt(mean((rh_pix-rh_pred_median)^2, na.rm = TRUE))                    ## relatif par rapport au nombre de jours dans la serie
    dfRMSE<-list.append(dfRMSE)

    ## calcul du biais de chaque couche predite avec la médiane
    BIAIS_perc<-100*(sum(rh_pred_median-rh_pix)/sum(rh_pix))  
    BIAIS<-list.append(BIAIS_perc)

    
    rsq_du_quant<-list()
    ##calcul rsq en fonction des quantiles
    for(p in 1:1000)
    {
      rh_pix<-moyennes_pred[,grep(paste0("rh",q),colnames(moyennes_pred))]
      rh_pred<-moyennes_pred[,grep("Q",colnames(moyennes_pred))]
      rh_pred_p<-rh_pred[,p]
      rsq_p<-cor(rh_pred_p,rh_pix)^2
      rsq_du_quant <-list.append(rsq_du_quant, rsq_p)
    }
    
    rsq<-list.append(rsq,rsq_du_quant)

    
    ### Compute CRPS
    rh_f_quant <- quantile(rh.fine, probs=seq(5,95)/100)
    rh_fine_mat <- t(matrix(rep(rh_f_quant,nrow(new_data)), ncol = nrow(new_data)))  
    CRPS <-FairCrps(rh_fine_mat,data_rh) # CRPS de la prevision par rapport a la climatologie; plus rapide en calcul que Taillardat
    
    rh.fine_quant<-rh.fine_quant*100
    rh.fine<-rh.fine*100
    
    
    # df_rh_fine<-cbind(rh.fine,dep.fine.unc,CRPS)
    # colnames(df_rh_fine) <- c(paste0("rh.fine",q),paste0("dep.fine.unc",q),paste0("CRPS",q))
    #df_rh_fine<-cbind(rh.fine_quant,dep.fine.unc)
    df_rh_fine<-cbind(rh.fine_quant,dep.fine.unc,rh.fine,CRPS)
    colnames(df_rh_fine) <- c(paste0("Lay",q,"_",nom_quantiles),paste0("dep.fine.unc",q),paste0("rh.fine",q),paste0("CRPS",q))
    
    if(q==1)
    {
      xx<-df_rh_fine
    }else{    
      xx <- cbind(xx,data.frame(df_rh_fine))   
    }
    ##### PLOTS
    ##plot variable importance
    yscale.RR<-seq(1,5,by=1)
    lab_axe3<-c("SR1","PHASE1","SR2","PHASE2","SR3","PHASE3","SR4","PHASE4","SR5","PHASE5","SR6","PHASE6","SR7","PHASE7",   
                "SR8","PHASE8","SR9","PHASE9","SR10","PHASE1","SR11","PHASE11","SR12","PHASE12","SR13","PHASE13","SR14","PHASE14",   
                "SR15","PHASE15","SR16","PHASE16","SR17","PHASE17","SR18","PHASE18","SR19","PHASE19","SR20","PHASE20","SR21","PHASE21",   
                "SR22","PHASE22","SR23","PHASE23","SR24","PHASE24","SR25","PHASE25","SR26","PHASE26","SR27","PHASE27","SR28","PHASE28",   
                "SR29","PHASE29","SR30","PHASE30","SR31","PHASE31","SR32","PHASE32","SR33","PHASE33","SR34","PHASE34","SR35","PHASE35",   
                "SR36","PHASE36","SR37","PHASE37","SR38","PHASE38","SR39","PHASE39","SR40","PHASE40","RR1.2","RR2","RR3","RR4","RR5")
    par(mar=c(4,4,4,2),oma=c(3,1,0,2))
    
    png(paste0(filename.plot_imp,"_lay",q,".png"), width = 1500, height = 1000, units = "px",res=200)
    ##pour enlever l axe x du barplot
    import = t(importance(model, type =2))
    import<-import[,c("SR1","PHASE1","SR2","PHASE2","SR3","PHASE3","SR4","PHASE4","SR5","PHASE5","SR6","PHASE6","SR7","PHASE7",   
                      "SR8","PHASE8","SR9","PHASE9","SR10","PHASE1","SR11","PHASE11","SR12","PHASE12","SR13","PHASE13","SR14","PHASE14",   
                      "SR15","PHASE15","SR16","PHASE16","SR17","PHASE17","SR18","PHASE18","SR19","PHASE19","SR20","PHASE20","SR21","PHASE21",   
                      "SR22","PHASE22","SR23","PHASE23","SR24","PHASE24","SR25","PHASE25","SR26","PHASE26","SR27","PHASE27","SR28","PHASE28",   
                      "SR29","PHASE29","SR30","PHASE30","SR31","PHASE31","SR32","PHASE32","SR33","PHASE33","SR34","PHASE34","SR35","PHASE35",   
                      "SR36","PHASE36","SR37","PHASE37","SR38","PHASE38","SR39","PHASE39","SR40","PHASE40","RR1.2","RR2","RR3","RR4","RR5")]
    
    rownames(import) = NULL
    colnames(import) = NULL
    color<-c(rep(c("blue","green"),40),rep("red",5))			  
    graphics::barplot(import, names.arg = lab_axe3, ylab='Node Purity',col=color,las=2,ylim=c(0,1.25*max(import)),cex.axis=0.8, cex.names=0.50, main=lablayer[q])
    #barplot(t(importance(qrf)[,1]), xlab='',ylab='',las=2)
    dev.off()
    
  }
  
  saveRDS(rsq, file = paste0(filename.dir.out,"R2_1000draw_ALL_clouds_with_CloudSat_",area,month.num,"_",year,".rds"))
  saveRDS(rsq_median, file = paste0(filename.dir.out,"R2median_ALL_clouds_with_CloudSat_",area,month.num,"_",year,".rds"))
 
  
  xx<-cbind(new_data,xx)
  xx[,"freq"] <-  merge.with.order(xx, count(xx, "IND_pix_SAPHIR"),by = "IND_pix_SAPHIR", all=TRUE,  keep_order=1)[,"freq"]	### pour connaitre le nb de CALIPSO contenu a l origine dans le pixel SAPHIR
  xx$irow <- seq.int(nrow(xx))
  xx.plot<-xx
  xx.plot <- ddply(xx.plot, .(IND_pix_SAPHIR), transform, rh.diff_L1=mean(abs(rh1-rh.fine1)), ### correspond au mass balance de la couche 1
                   rh.diff_L2=mean(abs(rh2-rh.fine2)), 
                   rh.diff_L3=mean(abs(rh3-rh.fine3)), 
                   rh.diff_L4=mean(abs(rh4-rh.fine4)), 
                   rh.diff_L5=mean(abs(rh5-rh.fine5)), 
                   rh.diff_L6=mean(abs(rh6-rh.fine6)))   
  xx.plot <- xx.plot[order(xx.plot$irow), ]  ### pour trier le tableau par les ID "irow"

  saveRDS(xx.plot, file = paste0(filename.dir.out,"xx_plot_",area,month.num,"_",year,".rds"))

  
  #  shapiro<-list()
  #  for(sh in 1:6)
  #  {
  #  rh_temp<-xx[,grep(paste0("rh",sh),colnames(xx))]
  #  shapiro[[sh]]<-shapiro.test(rh_temp)
  #  }
  #print(shapiro)
  
  #  wilcoxon<-list()
  #  for(sh in 1:6)
  #  {
  #    rh_temp<-xx[,grep(paste0("rh",sh),colnames(xx))]
  #    da<-new_data[,grep(paste0("rh",sh),colnames(new_data))]
  #    wilcoxon[[sh]]<-wilcox.test(rh_temp, da,conf.int = TRUE)
  #  }
  
  #print(wilcoxon)
  
  
  # saveRDS(xx, file="/bdd/MEGHA_TROPIQUES/MT_WORKSPACE/SAPHIR_downscaling/Calibration-Data_vmichot/intermediar_results_R/ocean_indien/7_2013/7_2013_OIndienSR_downscal_entire_TS.RDS")
  # xx<-readRDS("/home/michot/Bureau/LATMOS/SCRIPTS/Pour_moi/xx_serie_entiere.RDS")
  
  #***************************************************************************************************
  #	Save File
  #***************************************************************************************************
  
  write.table(xx, filename.dat, sep=";",col.names=TRUE, row.names=FALSE, quote=FALSE, append= F)
  
  #***************************************************************************************************
  #	Plots
  #***************************************************************************************************
  print("plot downscalling")
  
   
}


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
  downscall_TYPE_0(area,year,month.num)
  
} else {
  area = "North_Atlantic"
  month.num=7
  year=2013
}




