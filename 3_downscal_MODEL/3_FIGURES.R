########
## script pour faire les figures de R2 et de l echantillon d apprentissage

## PATHS
source("/bdd/MT_WORKSPACE/SAPHIR_downscaling/Calibration-Data_vmichot/CALIPSO_SAPHIR_coloc_out_all_tropical_seas/allday/ALL_SAPHIR_DOWNSCALING_SCRIPTS/Setpath.R")
library(rlist)
library(graphics)
library(cowplot)
library(ggplot2)

##SAPHIR Parameters
lablayer = c("100-200 hPa","250-350 hPa", "400-600 hPa","650-700 hPa","750-800 hPa", "850-950 hPa")
couches<-1:6

##Global variables
year<-2013
cloud_type_sans_CL<- "model_ICE_cloud_avec_CloudSat"
cloud_type_avec_CL<- "model_ICE_cloud_sans_CloudSat"

## dossier de sortie
output<- paste0(bdd_outputs,"/3_downscal_MODEL/")

filename.dir.out <- paste0(output,"model_all_cloud_avec_CloudSat/QRF_prediction/",year,"/") #!!! a changer en fonction du type de nuage etudie !!!
dir.create(file.path(filename.dir.out), showWarnings = FALSE,recursive=TRUE)

## dossier des modeles QRF
filename.dir_QRF<-paste0(output,"QRF_model_entire_timeseries/",year,"/")
 
############### RECUPERER le MODEL QRF AVEC OU SANS CLOUDSAT
## fichiers du model qrf avec CALIPSO et CloudSat pour les nuages de glace (a changer en fonction du type de nuage etudie)
pattern_QRF<-paste0("qrf_Indian_Ocean_7_2013MODEL_.?._entire_learn_sample.rds")  ##ici le .?. represente le num de la couche
list_QRF<-list.files(filename.dir_QRF,full.names = T,pattern = pattern_QRF, recursive=TRUE) ##creation d'une liste contenant tous les fichiers      

## fichiers du model qrf avec CALIPSO sans CloudSat (a changer en fonction du type de nuage etudie)
pattern_QRF_no_CL<-paste0("qrf_Indian_Ocean_7_2013MODEL_.?._NoCL_entire_learn_sample.rds")  ##ici le .?. represente le num de la couche
list_QRF_no_CL<-list.files(filename.dir_QRF,full.names = T,pattern = pattern_QRF_no_CL, recursive=TRUE) ##creation d'une liste contenant tous les fichiers      


##################################################################################################################
#### calcul R2 avec CloudSat
##################################################################################################################

## recuperer les donnees de RH coarse et predites dans le modele  
rh<-data.frame()
for (qr in 1 : length (list_QRF))
{
    #ouvrir le fichier qrf
    model<-readRDS(list_QRF[qr])

    rh.fine_t<-model$predict*100
    rh.coarse_t<-model$data[,grep(paste0("rh",qr),colnames(model$data))]
    if(qr==1)
    {
      rh<-cbind(rh.fine_t,rh.coarse_t)
    }else{    
      rh <- cbind(rh,data.frame(rh.fine_t,rh.coarse_t))
    }
} 

rh<-cbind(rh, "ind"=model$data[,grep("ind",colnames(model$data))])
nom_rh<-c("rh_fine1","rh1","rh_fine2","rh2","rh_fine3","rh3","rh_fine4","rh4","rh_fine5","rh5","rh_fine6","rh6","ind")
colnames(rh)<-nom_rh

## moyenne des predictions d HR pour pouvoir comparer a la resolution originale de SAPHIR
df_data_temp<-rh
    
##trouver les indices des lignes correspondantes au pixel + calculer la moyenne
moyennes_pred<-data.frame()
moy_pred_median<-data.frame()
noms_col<-c("IND_pix_SAPHIR",paste0("rh",1:6),paste0("mean_rh_fine",1:6))
names_ind<-unique(rh$ind)

for(nID in 1:length(names_ind))
{
  num_pix<-which(names_ind[nID]==df_data_temp$ind)
  df_pix_pred<-df_data_temp[num_pix,]
  rh_coarse<-df_pix_pred[,grep("rh",colnames(df_pix_pred))]
  rh_coarse<-unique(rh_coarse[,-grep("rh_fine",colnames(rh))])
  for(mo in 1:6)
  {
    moy_pred_temp<-mean(df_pix_pred[,paste0("rh_fine",mo)])
    if(mo==1)
    {
      moy_pred_median<-cbind(moy_pred_temp)
    }else{    
	   moy_pred_median<- cbind(moy_pred_median,data.frame(moy_pred_temp))
	 }
    }
    moyennes_pred<-rbind(moyennes_pred, data.frame(names_ind[nID], rh_coarse,moy_pred_median))

} 
colnames(moyennes_pred)<-noms_col

 
## calcul rsq de la mediane dans l'apprentissage
rsq<-list()
for(la in 1:6)
{
  rh_pix<-moyennes_pred[,grep(paste0("rh",la),colnames(moyennes_pred))]
  rh_pred_median<-moyennes_pred[,grep(paste0("mean_rh_fine",la),colnames(moyennes_pred))]
  rsq_med<-cor(rh_pred_median,rh_pix)^2
  rsq<-list.append(rsq, rsq_med)
}


##################################################################################################################
#### calcul R2 pour Ice Cloud sans CloudSat
##################################################################################################################

## recuperer les donnees de RH coarse et predites dans le modele  
rh_no_CL<-data.frame()
for (qr in 1 : length (list_QRF_no_CL))
{
    #ouvrir le fichier qrf
    model_no_CL<-readRDS(list_QRF_no_CL[qr])

    rh.fine_t<-model_no_CL$predict*100
    rh.coarse_t<-model_no_CL$data[,grep(paste0("rh",qr),colnames(model_no_CL$data))]
    if(qr==1)
    {
      rh_no_CL<-cbind(rh.fine_t,rh.coarse_t)
    }else{    
      rh_no_CL <- cbind(rh_no_CL,data.frame(rh.fine_t,rh.coarse_t))
    }
} 

rh_no_CL<-cbind(rh_no_CL, "ind"=model_no_CL$data[,grep("ind",colnames(model_no_CL$data))])
nom_rh<-c("rh_fine1","rh1","rh_fine2","rh2","rh_fine3","rh3","rh_fine4","rh4","rh_fine5","rh5","rh_fine6","rh6","ind")
colnames(rh_no_CL)<-nom_rh


## moyenne des predictions d HR pour pouvoir comparer a la resolution originale de SAPHIR
df_data_temp_no_CL<-rh_no_CL
    
##trouver les indices des lignes correspondantes au pixel + calculer la moyenne
moyennes_pred_no_CL<-data.frame()
moy_pred_median_no_CL<-data.frame()
noms_col<-c("IND_pix_SAPHIR",paste0("rh",1:6),paste0("mean_rh_fine",1:6))
names_ind_no_CL<-unique(rh_no_CL$ind)

for(nID in 1:length(names_ind_no_CL))
{
  num_pix<-which(names_ind_no_CL[nID]==df_data_temp_no_CL$ind)
  df_pix_pred<-df_data_temp_no_CL[num_pix,]
  rh_coarse<-df_pix_pred[,grep("rh",colnames(df_pix_pred))]
  rh_coarse<-unique(rh_coarse[,-grep("rh_fine",colnames(rh_no_CL))])
  for(mo in 1:6)
  {
    moy_pred_temp<-mean(df_pix_pred[,paste0("rh_fine",mo)])
    if(mo==1)
    {
      moy_pred_median_no_CL<-cbind(moy_pred_temp)
    }else{    
	   moy_pred_median_no_CL<- cbind(moy_pred_median_no_CL,data.frame(moy_pred_temp))
	 }
    }
    moyennes_pred_no_CL<-rbind(moyennes_pred_no_CL, data.frame(names_ind_no_CL[nID], rh_coarse,moy_pred_median_no_CL))

} 
colnames(moyennes_pred_no_CL)<-noms_col
 
## calcul rsq de la mediane dans l'apprentissage
rsq_no_CL<-list()
for(la in 1:6)
{
  rh_pix<-moyennes_pred_no_CL[,grep(paste0("rh",la),colnames(moyennes_pred_no_CL))]
  rh_pred_median<-moyennes_pred_no_CL[,grep(paste0("mean_rh_fine",la),colnames(moyennes_pred_no_CL))]
  rsq_med<-cor(rh_pred_median,rh_pix)^2
  rsq_no_CL<-list.append(rsq_no_CL, rsq_med)
}


#######################   PLOTS #######################
###### SACTTER PLOT
par(mar=c(0,1,1,1))   
png(paste0(filename.dir.out,"scatterplot_APPRENTISSAGE.png"), width = 1500, height = 1000, units = "px",res=200)
h2d_list<-list()
for(plot in 1: length(couches))
{
   dens.plot<- data.frame(rh[,grep(paste0("rh",plot),colnames(rh))],(rh[,grep(paste0("rh_fine",plot),colnames(rh))]))
   nom_col<-c("coarse","fine")
   colnames(dens.plot)<-nom_col
   nb_obs<-nrow(dens.plot)
   rsq_L<-rsq[plot]
   rsq_L<-as.character(rsq_L)
   rsq_L<-substr(rsq_L,1,4)
   r2_without_Cl<-rsq_no_CL[plot]
   r2_without_Cl<-as.character(r2_without_Cl)
   r2_without_Cl<-substr(r2_without_Cl,1,4)
   L<-lablayer[plot]
    
   h2d<- ggplot(dens.plot, aes(x=coarse, y=fine))+theme_bw()+geom_abline(intercept= 0,slope=1,colour="black")+ geom_hex(bins = 40,colour="gray", size=0.4) +      
         scale_fill_gradient(low =  "lightblue", high = "#FC4E07")+
         ggtitle(L) +
         xlab("RH(%)")+ylab("predicted RH(%)")+ xlim(c(0,100))+ ylim(c(0,100))+geom_text(x=80,y=5,label=paste0("with CloudSat, R2=",rsq_L),size=6,family = "Times New Roman", font_face=2)+geom_text(x=78,y=15,label=paste0("without CloudSat, R2=",r2_without_Cl),size=6, family = "Times New Roman", font_face=2)+
         theme(axis.text.x = element_text(size=15, face="bold"),
               axis.text.y = element_text(size=15,face="bold"),
               axis.title=element_text(size=15),legend.position=c(0.1,0.8), legend.box = "horizontal",
               legend.text = element_text(size=15),legend.title=element_text(size=15),plot.title = element_text(size=15, face="bold"))
   h2d_list<-list.append(h2d_list, h2d)
    
}              
grid.arrange(grobs = h2d_list, ncol=3, nrow=2)
dev.off()
png(paste0(filename.dir.out,"scatterplot_APPRENTISSAGE_LEGEND.png"), width = 1500, height = 1000, units = "px",res=200)
grid.arrange(LEGEND,ncol=1, nrow=1)
dev.off()


##### Distribution des données 
lablayer = c( "850-950 hPa", "750-800 hPa","650-700 hPa","400-600 hPa","250-350 hPa","100-200 hPa")

##RH
minRH<- 0  #min(RH)
maxRH<-100 #max(RH)
png(paste0(filename.dir.out,"distri_RH_Ice_cloud.png"),width = 2048, height = 1536)
par(mfrow=c(3,2))
par(mar=c(4.5,6.5,4,1))
#layout(n=13)
for(cou in 6:1)
{
  lay_name<-lablayer[cou]
  RH_dens<-density(RH[,cou], na.rm=TRUE,from=0, to=100, cut=TRUE)
  min_plot<-minRH
  max_plot<-maxRH
  plot(RH_dens, bty="n", col="blue",lwd=3,xlab="", cex.axis=2.5, cex.lab=2.5, ylab="",main=paste0(lablayer[cou],"hPa"))
}
dev.off()


## RR
png(paste0(filename.dir.out,"distri_RR_Ice_cloud.png"),width = 2048, height = 1536)
minRR<-min(RR)
maxRR<-max(RR)
par(mfrow=c(3,2))
par(mar=c(4.5,6.5,4,1))
#layout(n=13)
for(cou in 6:1)
{
  lay_name<-lablayer[cou]
  #dens juillet
  RR_dens<-density(RR[,cou], na.rm=TRUE, cut=TRUE) ##vérifier les fenetres de convolution de density; arg from et to pour etre entre 0 et 100
  min_plot<-minRR
  max_plot<-maxRR  
  plot(RR_dens, bty="n", col="darkgreen",lwd=3,xlab="", cex.axis=2.5, cex.lab=2.5, ylab="",xlim=c(minRR,maxRR),main=paste0(lablayer[cou],"hPa"))
}
dev.off()


## SR
png(paste0(filename.dir.out,"distri_SR_Ice_cloud.png"),width = 2048, height = 1536)
par(mfrow=c(3,2))
par(mar=c(4.5,6.5,4,1))
#layout(n=13)
for(couche in 6:1)
{
  lay_name<-lablayer[cou]
  #dens juillet
  SR_temp_dens<-density(SR_temp[,cou], na.rm=TRUE, cut=TRUE) ##vérifier les fenetres de convolution de density; arg from et to pour etre entre 0 et 100
  plot(SR_temp_dens, bty="n", col="red", lwd=3,xlab="", cex.axis=2.5, cex.lab=2.5, ylab="",main=paste0(lablayer[cou],"hPa"))
}
dev.off()

## PHASE
png(paste0(filename.dir.out,"distri_Phase_Ice_cloud.png"),width = 2048, height = 1536)
par(mfrow=c(3,2))
par(mar=c(4.5,6.5,4,1))
#layout(n=13)
for(couche in 6:1)
{
  lay_name<-lablayer[cou]
  #dens juillet
  SR_phase_dens<-density(SR_phase[,couche], na.rm=TRUE,cut=TRUE) ##vérifier les fenetres de convolution de density; arg from et to pour etre entre 0 et 100
  plot(SR_phase_dens, bty="n", col="darkred", lwd=3,xlab="", cex.axis=2.5, cex.lab=2.5, ylab="",main=paste0(lablayer[cou],"hPa"))
}
dev.off()



