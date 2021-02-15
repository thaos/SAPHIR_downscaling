
source("Comp_func_VM.R")

## PATHS
source("Setpath.R")
library(rlist )
library(ggpubr)
library(grid)


## **********************************************************************************************************************
##	Global varaibles
## **********************************************************************************************************************
#area="Indian_Ocean"
#month.num=7
#year=2013

#---------------------------------------------------------------------------------------------------
#	CALIPSO parameters
#---------------------------------------------------------------------------------------------------
resprofile <- 0.48		
alt <- seq(0.24,16.08,by=resprofile)

#---------------------------------------------------------------------------------------------------
#	SAPHIR parameters
#---------------------------------------------------------------------------------------------------
nlayer <- 6																								# SAPHIR: There are 6 layers for relative humidity defined by their pressure boundaries as follows: 
lablayer = c("100-200 hPa","250-350 hPa", "400-600 hPa","650-700 hPa","750-800 hPa", "850-950 hPa")


### directory folder
filename.dir <- paste0(bdd_outputs,"/fichier_learn_SAPHIR_entier/")

#---------------------------------------------------------------------------------------------------
#	Set directory ICE_cloud_sans_CloudSat
#---------------------------------------------------------------------------------------------------
dir.ICE_cloud_without_Cloudsat <- paste0(filename.dir,"model_ICE_cloud_sans_CloudSat/QRF_prediction/",year,"/",area,"/")## pour resultats bases sur donnees apprentissage SAPHIR indice partiel
dir.create(file.path(dir.ICE_cloud_without_Cloudsat), showWarnings = FALSE, recursive=TRUE)
## open file  
ICE_cloud_without_Cloudsat<-readRDS(file = paste0(dir.ICE_cloud_without_Cloudsat,"xx_plot.rds"))
rsq_1000_ICE_cloud_without_Cloudsat<-readRDS(file = paste0(dir.ICE_cloud_without_Cloudsat,"R2_1000draw_other_clouds_without_CloudSat.rds"))
rsq_median_ICE_cloud_without_Cloudsat<-readRDS(file = paste0(dir.ICE_cloud_without_Cloudsat,"R2median_other_clouds_without_CloudSat.rds"))

 
#---------------------------------------------------------------------------------------------------
#	Set directory ICE_cloud_avec_CloudSat
#---------------------------------------------------------------------------------------------------
dir.ICE_cloud_with_Cloudsat <- paste0(filename.dir,"model_ICE_cloud_avec_CloudSat/QRF_prediction/",year,"/",area,"/")## pour resultats bases sur donnees apprentissage SAPHIR indice partiel
dir.create(file.path(dir.ICE_cloud_with_Cloudsat), showWarnings = FALSE, recursive=TRUE)
## open file  
ICE_cloud_with_Cloudsat<-readRDS(file = paste0(dir.ICE_cloud_with_Cloudsat,"xx_plot.rds"))
rsq_1000_ICE_cloud_with_Cloudsat<-readRDS(file = paste0(dir.ICE_cloud_with_Cloudsat,"R2_1000draw_other_clouds_without_CloudSat.rds"))
rsq_median_ICE_cloud_with_Cloudsat<-readRDS(file = paste0(dir.ICE_cloud_with_Cloudsat,"R2median_other_clouds_without_CloudSat.rds"))

## plot names
plot_variable_Ice <- paste0("plot_SR_PHASES_SR_RH_",area,"_",month.num,"_",year)
plot_RH_Ice <- paste0("plot_coarse_fine_RH_",area,"_",month.num,"_",year)
plot_predict_obs_Ice<-paste0("plot_predict_obs_PRED_T0_",area,"_",month.num,"_",year)
plot_CRPS_Ice<-paste0("plot_CRPS_PRED_T0_",area,"_",month.num,"_",year)
  
## set the filename of figure for other_cloud_with_Cloudsat
filename.plot_Ice <- paste0(dir.ICE_cloud_with_Cloudsat,plot_variable_Ice)
filename.plot_RH_Ice <- paste0(dir.ICE_cloud_with_Cloudsat,plot_RH_Ice)
filename.plot_predict_obs_Ice<-paste0(dir.ICE_cloud_with_Cloudsat,plot_predict_obs_Ice)
filename.plot_CRPS_Ice<-paste0(dir.ICE_cloud_with_Cloudsat,plot_CRPS_Ice)
  
#---------------------------------------------------------------------------------------------------
#	Set directory LIQ_cloud_sans_CloudSat
#---------------------------------------------------------------------------------------------------
dir.other_cloud_without_Cloudsat <- paste0(filename.dir,"model_LIQ_cloud_sans_CloudSat/QRF_prediction/",year,"/",area,"/")## pour resultats bases sur donnees apprentissage SAPHIR indice partiel
dir.create(file.path(dir.other_cloud_without_Cloudsat), showWarnings = FALSE, recursive=TRUE)
## open file  
other_cloud_without_Cloudsat<-readRDS(file = paste0(dir.other_cloud_without_Cloudsat,"xx_plot.rds"))
rsq_1000_other_cloud_without_Cloudsat<-readRDS(file = paste0(dir.other_cloud_without_Cloudsat,"R2_1000draw_other_clouds_without_CloudSat.rds"))
rsq_median_other_cloud_without_Cloudsat<-readRDS(file = paste0(dir.other_cloud_without_Cloudsat,"R2median_other_clouds_without_CloudSat.rds"))


#---------------------------------------------------------------------------------------------------
#	Set directory LIQ_cloud_avec_CloudSat
#---------------------------------------------------------------------------------------------------
dir.other_cloud_with_Cloudsat <- paste0(filename.dir,"model_LIQ_cloud_avec_CloudSat/QRF_prediction/",year,"/",area,"/")## pour resultats bases sur donnees apprentissage SAPHIR indice partiel
dir.create(file.path(dir.other_cloud_with_Cloudsat), showWarnings = FALSE, recursive=TRUE)
## open file  
other_cloud_with_Cloudsat<-readRDS(file = paste0(dir.other_cloud_with_Cloudsat,"xx_plot.rds"))
rsq_1000_other_cloud_with_Cloudsat<-readRDS(file = paste0(dir.other_cloud_with_Cloudsat,"R2_1000draw_other_clouds_without_CloudSat.rds"))
rsq_median_other_cloud_with_Cloudsat<-readRDS(file = paste0(dir.other_cloud_with_Cloudsat,"R2median_other_clouds_without_CloudSat.rds"))

## plot names
plotlab_Liq <- paste0("plot_entire_series_PRED_T0_",area,"_",month.num,"_",year)
plot_part_serie_Liq<- paste0("plot_part_serie_PRED_T0_",area,"_",month.num,"_",year)
plot_predict_obs_Liq<-paste0("plot_predict_obs_PRED_T0_",area,"_",month.num,"_",year)
plot_CRPS_Liq<-paste0("plot_CRPS_PRED_T0_",area,"_",month.num,"_",year)
  
## set the filename of figure for other_cloud_with_Cloudsat
filename.plot_Liq <- paste0(dir.other_cloud_with_Cloudsat,plotlab_Liq)
filename.plot_predict_obs_Liq<-paste0(dir.other_cloud_with_Cloudsat,plot_predict_obs_Liq)
filename.plot_CRPS_Liq<-paste0(dir.other_cloud_with_Cloudsat,plot_CRPS_Liq)
filename.dat_Liq <- paste0(dir.other_cloud_with_Cloudsat,"QRF_pred_T0_",area,"_",month.num,"_",year,".dat")
  

#---------------------------------------------------------------------------------------------------
#	Set directory ALL_cloud_sans_CloudSat
#---------------------------------------------------------------------------------------------------
dir.all_cloud_without_Cloudsat <- paste0(filename.dir,"QRF_prediction_NO_Cloudsat/T0/",area,"_",year,"_",month.num,"/")## pour resultats bases sur donnees apprentissage SAPHIR indice partiel
dir.create(file.path(dir.all_cloud_without_Cloudsat), showWarnings = FALSE, recursive=TRUE)
## open file  
all_cloud_without_Cloudsat<-readRDS(file = paste0(dir.all_cloud_without_Cloudsat,"xx_plot_",area,month.num,"_",year,".rds"))
rsq_1000_all_cloud_without_Cloudsat<-readRDS(file = paste0(dir.all_cloud_without_Cloudsat,"R2_1000draw_ALL_clouds_without_CloudSat_",area,month.num,"_",year,".rds"))
rsq_median_all_cloud_without_Cloudsat<-readRDS(file = paste0(dir.all_cloud_without_Cloudsat,"R2median_ALL_clouds_without_CloudSat_",area,month.num,"_",year,".rds"))


#---------------------------------------------------------------------------------------------------
#	Set directory ALL_cloud_avec_CloudSat
#---------------------------------------------------------------------------------------------------
dir.all_cloud_with_Cloudsat <- paste0(filename.dir,"QRF_prediction/T0/",area,"_",year,"_",month.num,"/")## pour resultats bases sur donnees apprentissage SAPHIR indice partiel
dir.create(file.path(dir.all_cloud_with_Cloudsat), showWarnings = FALSE, recursive=TRUE)
## open file  
all_cloud_with_Cloudsat<-readRDS(file = paste0(dir.all_cloud_with_Cloudsat,"xx_plot_Indian_Ocean7_2013.rds"))
rsq_1000_all_cloud_with_Cloudsat<-readRDS(file = paste0(dir.all_cloud_with_Cloudsat,"R2_1000draw_ALL_clouds_with_CloudSat.rds"))
rsq_median_all_cloud_with_Cloudsat<-readRDS(file = paste0(dir.all_cloud_with_Cloudsat,"R2median_ALL_clouds_with_CloudSat.rds"))


##########################################################################################################
###	P SCATTER PLOT & CRPS
##########################################################################################################

################ ICE cloud AVEC CloudSat

par(mar=c(0,1,1,1))   #
png(paste0(filename.plot_predict_obs_Ice,".png"), width = 1500, height = 1000, units = "px",res=200)
couches<-1:6
h2d_list1<-list()
for(plot in 1: length(couches))
{
  dens.plot<- data.frame(ICE_cloud_with_Cloudsat[,grep(paste0("rh",plot),colnames(ICE_cloud_with_Cloudsat))],(ICE_cloud_with_Cloudsat[,grep(paste0("rh.fine",plot),colnames(ICE_cloud_with_Cloudsat))]))
  nom_col<-c("coarse","fine")
  colnames(dens.plot)<-nom_col
  rsq_L<-rsq_median_ICE_cloud_with_Cloudsat[plot]
  rsq_L<-as.character(rsq_L)
  rsq_L<-substr(rsq_L,1,4)
  r2_without_Cl<-rsq_median_other_cloud_without_Cloudsat[plot]
  r2_without_Cl<-as.character(r2_without_Cl)
  r2_without_Cl<-substr(r2_without_Cl,1,4)
  nb_obs<-nrow(dens.plot)
  L<-lablayer[plot]
	   
 h2d<- ggplot(dens.plot, aes(x=coarse, y=fine))+theme_bw()+geom_abline(intercept= 0,slope=1,colour="black")+ geom_hex(bins = 40,colour="gray", size=0.4) +      
      scale_fill_gradient(low =  "lightblue", high = "#FC4E07")+
      ggtitle(L) +
      xlab("RH(%)")+ylab("predicted RH(%)")+ xlim(c(0,100))+ ylim(c(0,100))+geom_text(x=80,y=5,label=paste0("with CloudSat, R2=",rsq_L),size=6,family = "Times New Roman", font_face=2)+geom_text(x=78,y=15,label=paste0("without CloudSat, R2=",r2_without_Cl),size=6, family = "Times New Roman", font_face=2)+
      theme(axis.text.x = element_text(size=15, face="bold"),
            axis.text.y = element_text(size=15,face="bold"),
           axis.title=element_text(size=15),legend.position=c(0.1,0.8), legend.box = "horizontal",
            legend.text = element_text(size=15),legend.title=element_text(size=15),plot.title = element_text(size=15, face="bold"))
    h2d_list1<-list.append(h2d_list1, h2d)
           # legend.box.spacing = unit(1.2, "cm"),legend.key.size = unit(1.2, "lines"),legend.box.background = element_rect(colour = "gray"))
    
}              
grid.arrange(grobs = h2d_list1,ncol=3, nrow=2)
dev.off()

  
## Plot CRPS density pour rh_frac
png(paste0(filename.plot_CRPS_Ice,".png"), width = 1500, height = 1000, units = "px",res=200)
par(mfrow=c(2,3))
par(mar=c(4.5,6.5,4,1))
  
couches<-1:6
for(plot in 1: length(couches))
{
  crps<- ICE_cloud_with_Cloudsat[,grep(paste0("CRPS",plot),colnames(ICE_cloud_with_Cloudsat))]
  dens_plot_CRPS <- density(crps)
  plot(dens_plot_CRPS,main=paste0(lablayer[plot]),xlab="CRPS",cex.main=1.5, cex= 1, cex.axis=1.5, cex.lab=1.7 )
}
dev.off()


################ LIQUID cloud AVEC CloudSat

par(mar=c(0,1,1,1))   
png(paste0(filename.plot_predict_obs_Liq,".png"), width = 1500, height = 1000, units = "px",res=200)
couches<-1:6
h2d_list2<-list()
for(plot in 1: length(couches))
{
  dens.plot<- data.frame(other_cloud_with_Cloudsat[,grep(paste0("rh",plot),colnames(other_cloud_with_Cloudsat))],(other_cloud_with_Cloudsat[,grep(paste0("rh.fine",plot),colnames(other_cloud_with_Cloudsat))]))
  nom_col<-c("coarse","fine")
  colnames(dens.plot)<-nom_col
  rsq_L<-rsq_median_other_cloud_with_Cloudsat[plot]
  rsq_L<-as.character(rsq_L)
  rsq_L<-substr(rsq_L,1,4)
  r2_without_Cl<-rsq_median_other_cloud_without_Cloudsat[plot]
  r2_without_Cl<-as.character(r2_without_Cl)
  r2_without_Cl<-substr(r2_without_Cl,1,4)
  nb_obs<-nrow(dens.plot)
  L<-lablayer[plot]
	   
 h2d<- ggplot(dens.plot, aes(x=coarse, y=fine))+theme_bw()+geom_abline(intercept= 0,slope=1,colour="black")+ geom_hex(bins = 40,colour="gray", size=0.4) +      
      scale_fill_gradient(low =  "lightblue", high = "#FC4E07")+
      ggtitle(L) +
      xlab("RH(%)")+ylab("predicted RH(%)")+ xlim(c(0,100))+ ylim(c(0,100))+geom_text(x=80,y=5,label=paste0("with CloudSat, R2=",rsq_L),size=6,family = "Times New Roman", font_face=2)+geom_text(x=78,y=15,label=paste0("without CloudSat, R2=",r2_without_Cl),size=6, family = "Times New Roman", font_face=2)+
      theme(axis.text.x = element_text(size=15, face="bold"),
            axis.text.y = element_text(size=15,face="bold"),
           axis.title=element_text(size=15),legend.position=c(0.1,0.8), legend.box = "horizontal",
            legend.text = element_text(size=15),legend.title=element_text(size=15),plot.title = element_text(size=15, face="bold"))

   h2d_list2<-list.append(h2d_list2,h2d)    
}              
grid.arrange(grobs = h2d_list2,ncol=3, nrow=2)
dev.off()


## Plot CRPS density pour rh_frac
png(paste0(filename.plot_CRPS_Liq,".png"), width = 1500, height = 1000, units = "px",res=200)
par(mfrow=c(2,3))
par(mar=c(4.5,6.5,4,1))

couches<-1:6
for(plot in 1: length(couches))
{
  crps<- other_cloud_with_Cloudsat[,grep(paste0("CRPS",plot),colnames(other_cloud_with_Cloudsat))]
  dens_plot_CRPS <- density(crps)
  plot(dens_plot_CRPS,main=paste0(lablayer[plot]),xlab="CRPS",cex.main=1.5, cex= 1, cex.axis=1.5, cex.lab=1.7 )
}
dev.off()



##########################################################################################################
###	PLOTS SR, PHASES, RR, RH coarse & fine, par troncons de la serie pour mieux visualiser les donnees
##########################################################################################################
 
################ ICE cloud SANS & AVEC CloudSat  
xx.plot<-ICE_cloud_with_Cloudsat
names_ind<-unique(xx.plot$IND_pix_SAPHIR)
length_ind<-c()
for(ID in 1:length(names_ind))
{
  length_ind<-c(length_ind,length(which(names_ind[ID]==xx.plot$IND_pix_SAPHIR)))
}
  
sum_ind_sup_10<-sum(length_ind>=10)
ind_sup_10<-sum_ind_sup_10/3
  
min_ind<-23  ##pour visualiser seulement les pixels de SAPHIR avec le plus grand nombre de points CALIPSO et CloudSAt
max_ind<-30
xx.plot2<-data.frame()
for(nID in 1:length(names_ind))
{
  same_ind<-which(names_ind[nID]==xx.plot$IND_pix_SAPHIR)
  if(length(same_ind)>=(min_ind) & length(same_ind)<=(max_ind))
  {
    lines_ind<-xx.plot[same_ind,]
    xx.plot2<-rbind(xx.plot2,data.frame(nID,lines_ind))
  }
}
  
partition<-split(xx.plot2,xx.plot2$IND_pix_SAPHIR)

#### pour selectionner les premiers elements
parti_list<-partition[1:20]
partition_part<-data.frame()
ICE_cloud_without_Cloudsat_plot<-data.frame()

for(par in 1:length(parti_list))
{
  df_temp_parti<-parti_list[[par]]
  partition_part<-rbind(partition_part, data.frame(df_temp_parti))
  indice<-unique(df_temp_parti$IND_pix_SAPHIR)
  same_indice<-which(indice==ICE_cloud_without_Cloudsat$IND_pix_SAPHIR)
  ICE_cloud_without_Cloudsat_temp<-ICE_cloud_without_Cloudsat[same_indice,]
  ICE_cloud_without_Cloudsat_plot<-rbind(ICE_cloud_without_Cloudsat_plot,data.frame(ICE_cloud_without_Cloudsat_temp))
}


saveRDS(partition_part, file = paste0(dir.ICE_cloud_without_Cloudsat,"ICE_cloud_with_Cloudsat_for_plot.rds"))
partition_part<-readRDS(file = paste0(dir.ICE_cloud_without_Cloudsat,"ICE_cloud_with_Cloudsat_for_plot.rds"))

saveRDS(ICE_cloud_without_Cloudsat_plot, file = paste0(dir.ICE_cloud_without_Cloudsat,"ICE_cloud_without_Cloudsat_for_plot.rds"))
ICE_cloud_without_Cloudsat_plot<-readRDS(file = paste0(dir.ICE_cloud_without_Cloudsat,"ICE_cloud_without_Cloudsat_for_plot.rds"))

  
#### pour faire les lignes de separation delimitant les pixels SAPHIR
names_ind2<-unique(partition_part$IND_pix_SAPHIR)
length_ind2<-c()
for(ID in 1:length(names_ind2))
{
  length_ind2<-c(length_ind2,length(which(names_ind2[ID]==partition_part$IND_pix_SAPHIR)))
}

coord_abline<-c()
for(li2 in 1:length(length_ind2))
{
  if(li2==1){coord_abline<-c(length_ind2[1]+0.5)}
  else{
	coord<-coord_abline[li2-1]+length_ind2[li2]
	coord_abline<-c(coord_abline,coord)
  }
}


##data.frame for plot 
SR.plot_Ice <- as.matrix(partition_part[,grep("SR",colnames(partition_part))])
SR.plot_Ice <- SR.plot_Ice[,2:34]
SR_phase.plot_Ice <- as.matrix(partition_part[,grep("PHASE",colnames(partition_part))])
SR_phase.plot_Ice<-SR_phase.plot_Ice[,2:34]
RR.plot_Ice<-as.matrix(partition_part[,grep("RR",colnames(partition_part))])
RR.plot_Ice<-cbind(RR.plot_Ice[,2],RR.plot_Ice[,1:16])
RR.plot_Ice[,1]<-(-30)
RR.plot_5km_Ice<-as.matrix(RR.plot_Ice[,1:6])
RH_plot_Ice<- as.matrix(partition_part[,grep("rh",colnames(partition_part))])
RH_plot_Ice <-RH_plot_Ice[,6:1]
RH_plot_Ice<- cbind(RH_plot_Ice[,1],RH_plot_Ice[,1],RH_plot_Ice[,1],RH_plot_Ice[,2],RH_plot_Ice[,1],RH_plot_Ice[,3], RH_plot_Ice[,1],RH_plot_Ice[,4],RH_plot_Ice[,4],RH_plot_Ice[,4],RH_plot_Ice[,4],RH_plot_Ice[,4],RH_plot_Ice[,4],
		    RH_plot_Ice[,1],RH_plot_Ice[,1],RH_plot_Ice[,5],RH_plot_Ice[,5],RH_plot_Ice[,5],RH_plot_Ice[,5],RH_plot_Ice[,5],RH_plot_Ice[,1],RH_plot_Ice[,6],RH_plot_Ice[,6],RH_plot_Ice[,6],RH_plot_Ice[,6],RH_plot_Ice[,6],RH_plot_Ice[,6],RH_plot_Ice[,6],RH_plot_Ice[,6])
RH_plot_Ice[,c(3,5,7,14,15,21)]<-(-10)

### RH pour ICE clouds SANS CloudSat
URH.plot_I_no_Cl <- as.matrix(ICE_cloud_without_Cloudsat_plot[,grep("rh.unc",colnames(ICE_cloud_without_Cloudsat_plot))])
URH.plot_I_no_Cl <-URH.plot_I_no_Cl[,6:1]
URH.plot_I_no_Cl<- cbind(URH.plot_I_no_Cl[,1],URH.plot_I_no_Cl[,1],URH.plot_I_no_Cl[,1],URH.plot_I_no_Cl[,2],URH.plot_I_no_Cl[,1],URH.plot_I_no_Cl[,3], URH.plot_I_no_Cl[,1],URH.plot_I_no_Cl[,4],URH.plot_I_no_Cl[,4],URH.plot_I_no_Cl[,4],URH.plot_I_no_Cl[,4],URH.plot_I_no_Cl[,4],URH.plot_I_no_Cl[,4],
                         URH.plot_I_no_Cl[,1],URH.plot_I_no_Cl[,1],URH.plot_I_no_Cl[,5],URH.plot_I_no_Cl[,5],URH.plot_I_no_Cl[,5],URH.plot_I_no_Cl[,5],URH.plot_I_no_Cl[,5],URH.plot_I_no_Cl[,1],URH.plot_I_no_Cl[,6],URH.plot_I_no_Cl[,6],URH.plot_I_no_Cl[,6],URH.plot_I_no_Cl[,6],URH.plot_I_no_Cl[,6],URH.plot_I_no_Cl[,6],URH.plot_I_no_Cl[,6],URH.plot_I_no_Cl[,6])
URH.plot_I_no_Cl[,c(3,5,7,14,15,21)]<-(-10)

RH.fine.plot_I_no_Cl <- as.matrix(ICE_cloud_without_Cloudsat_plot[,grep("rh.fine",colnames(ICE_cloud_without_Cloudsat_plot))])
RH.fine.plot_I_no_Cl <-RH.fine.plot_I_no_Cl[,6:1]
RH.fine.plot_I_no_Cl<- cbind(RH.fine.plot_I_no_Cl[,1],RH.fine.plot_I_no_Cl[,1],RH.fine.plot_I_no_Cl[,1],RH.fine.plot_I_no_Cl[,2],RH.fine.plot_I_no_Cl[,1],RH.fine.plot_I_no_Cl[,3], RH.fine.plot_I_no_Cl[,1],RH.fine.plot_I_no_Cl[,4],RH.fine.plot_I_no_Cl[,4],RH.fine.plot_I_no_Cl[,4],RH.fine.plot_I_no_Cl[,4],RH.fine.plot_I_no_Cl[,4],RH.fine.plot_I_no_Cl[,4],
                             RH.fine.plot_I_no_Cl[,1],RH.fine.plot_I_no_Cl[,1],RH.fine.plot_I_no_Cl[,5],RH.fine.plot_I_no_Cl[,5],RH.fine.plot_I_no_Cl[,5],RH.fine.plot_I_no_Cl[,5],RH.fine.plot_I_no_Cl[,5],RH.fine.plot_I_no_Cl[,1],RH.fine.plot_I_no_Cl[,6],RH.fine.plot_I_no_Cl[,6],RH.fine.plot_I_no_Cl[,6],RH.fine.plot_I_no_Cl[,6],RH.fine.plot_I_no_Cl[,6],RH.fine.plot_I_no_Cl[,6],RH.fine.plot_I_no_Cl[,6],RH.fine.plot_I_no_Cl[,6])
RH.fine.plot_I_no_Cl[,c(3,5,7,14,15,21)]<-(-10)

URH.fine.plot_I_no_Cl <- as.matrix(ICE_cloud_without_Cloudsat_plot[,grep("dep.fine.unc",colnames(ICE_cloud_without_Cloudsat_plot))])
URH.fine.plot_I_no_Cl <- URH.fine.plot_I_no_Cl [,6:1]
URH.fine.plot_I_no_Cl<- cbind(URH.fine.plot_I_no_Cl[,1],URH.fine.plot_I_no_Cl[,1],URH.fine.plot_I_no_Cl[,1],URH.fine.plot_I_no_Cl[,2],URH.fine.plot_I_no_Cl[,1],URH.fine.plot_I_no_Cl[,3], URH.fine.plot_I_no_Cl[,1],URH.fine.plot_I_no_Cl[,4],URH.fine.plot_I_no_Cl[,4],URH.fine.plot_I_no_Cl[,4],URH.fine.plot_I_no_Cl[,4],URH.fine.plot_I_no_Cl[,4],URH.fine.plot_I_no_Cl[,4],
                              URH.fine.plot_I_no_Cl[,1],URH.fine.plot_I_no_Cl[,1],URH.fine.plot_I_no_Cl[,5],URH.fine.plot_I_no_Cl[,5],URH.fine.plot_I_no_Cl[,5],URH.fine.plot_I_no_Cl[,5],URH.fine.plot_I_no_Cl[,5],URH.fine.plot_I_no_Cl[,1],URH.fine.plot_I_no_Cl[,6],URH.fine.plot_I_no_Cl[,6],URH.fine.plot_I_no_Cl[,6],URH.fine.plot_I_no_Cl[,6],URH.fine.plot_I_no_Cl[,6],URH.fine.plot_I_no_Cl[,6],URH.fine.plot_I_no_Cl[,6],URH.fine.plot_I_no_Cl[,6])
URH.fine.plot_I_no_Cl[,c(3,5,7,14,15,21)]<-(-10)

RH.diff.plot_I_no_Cl <- as.matrix(ICE_cloud_without_Cloudsat_plot[,grep("rh.diff_L",colnames(ICE_cloud_without_Cloudsat_plot))])
RH.diff.plot_I_no_Cl <-RH.diff.plot_I_no_Cl[,6:1]
RH.diff.plot_I_no_Cl<- cbind(RH.diff.plot_I_no_Cl[,1],RH.diff.plot_I_no_Cl[,1],RH.diff.plot_I_no_Cl[,1],RH.diff.plot_I_no_Cl[,2],RH.diff.plot_I_no_Cl[,1],RH.diff.plot_I_no_Cl[,3], RH.diff.plot_I_no_Cl[,1],RH.diff.plot_I_no_Cl[,4],RH.diff.plot_I_no_Cl[,4],RH.diff.plot_I_no_Cl[,4],RH.diff.plot_I_no_Cl[,4],RH.diff.plot_I_no_Cl[,4],RH.diff.plot_I_no_Cl[,4],
                             RH.diff.plot_I_no_Cl[,1],RH.diff.plot_I_no_Cl[,1],RH.diff.plot_I_no_Cl[,5],RH.diff.plot_I_no_Cl[,5],RH.diff.plot_I_no_Cl[,5],RH.diff.plot_I_no_Cl[,5],RH.diff.plot_I_no_Cl[,5],RH.diff.plot_I_no_Cl[,1],RH.diff.plot_I_no_Cl[,6],RH.diff.plot_I_no_Cl[,6],RH.diff.plot_I_no_Cl[,6],RH.diff.plot_I_no_Cl[,6],RH.diff.plot_I_no_Cl[,6],RH.diff.plot_I_no_Cl[,6],RH.diff.plot_I_no_Cl[,6],RH.diff.plot_I_no_Cl[,6])
RH.diff.plot_I_no_Cl[,c(3,5,7,14,15,21)]<-(-10)

### RH pour ICE clouds AVEC CloudSat
URH.plot_I_with_Cl  <- as.matrix(partition_part[,grep("rh.unc",colnames(partition_part))])
URH.plot_I_with_Cl  <-URH.plot_I_with_Cl [,6:1]
URH.plot_I_with_Cl<- cbind(URH.plot_I_with_Cl[,1],URH.plot_I_with_Cl[,1],URH.plot_I_with_Cl[,1],URH.plot_I_with_Cl[,2],URH.plot_I_with_Cl[,1],URH.plot_I_with_Cl[,3], URH.plot_I_with_Cl[,1],URH.plot_I_with_Cl[,4],URH.plot_I_with_Cl[,4],URH.plot_I_with_Cl[,4],URH.plot_I_with_Cl[,4],URH.plot_I_with_Cl[,4],URH.plot_I_with_Cl[,4],
                           URH.plot_I_with_Cl[,1],URH.plot_I_with_Cl[,1],URH.plot_I_with_Cl[,5],URH.plot_I_with_Cl[,5],URH.plot_I_with_Cl[,5],URH.plot_I_with_Cl[,5],URH.plot_I_with_Cl[,5],URH.plot_I_with_Cl[,1],URH.plot_I_with_Cl[,6],URH.plot_I_with_Cl[,6],URH.plot_I_with_Cl[,6],URH.plot_I_with_Cl[,6],URH.plot_I_with_Cl[,6],URH.plot_I_with_Cl[,6],URH.plot_I_with_Cl[,6],URH.plot_I_with_Cl[,6])
URH.plot_I_with_Cl[,c(3,5,7,14,15,21)]<-(-10)

RH.fine.plot_I_with_Cl  <- as.matrix(partition_part[,grep("rh.fine",colnames(partition_part))])
RH.fine.plot_I_with_Cl  <-RH.fine.plot_I_with_Cl [,6:1]
RH.fine.plot_I_with_Cl<- cbind(RH.fine.plot_I_with_Cl[,1],RH.fine.plot_I_with_Cl[,1],RH.fine.plot_I_with_Cl[,1],RH.fine.plot_I_with_Cl[,2],RH.fine.plot_I_with_Cl[,1],RH.fine.plot_I_with_Cl[,3], RH.fine.plot_I_with_Cl[,1],RH.fine.plot_I_with_Cl[,4],RH.fine.plot_I_with_Cl[,4],RH.fine.plot_I_with_Cl[,4],RH.fine.plot_I_with_Cl[,4],RH.fine.plot_I_with_Cl[,4],RH.fine.plot_I_with_Cl[,4],
                               RH.fine.plot_I_with_Cl[,1],RH.fine.plot_I_with_Cl[,1],RH.fine.plot_I_with_Cl[,5],RH.fine.plot_I_with_Cl[,5],RH.fine.plot_I_with_Cl[,5],RH.fine.plot_I_with_Cl[,5],RH.fine.plot_I_with_Cl[,5],RH.fine.plot_I_with_Cl[,1],RH.fine.plot_I_with_Cl[,6],RH.fine.plot_I_with_Cl[,6],RH.fine.plot_I_with_Cl[,6],RH.fine.plot_I_with_Cl[,6],RH.fine.plot_I_with_Cl[,6],RH.fine.plot_I_with_Cl[,6],RH.fine.plot_I_with_Cl[,6],RH.fine.plot_I_with_Cl[,6])
RH.fine.plot_I_with_Cl[,c(3,5,7,14,15,21)]<-(-10)

URH.fine.plot_I_with_Cl  <- as.matrix(partition_part[,grep("dep.fine.unc",colnames(partition_part))])
URH.fine.plot_I_with_Cl  <- URH.fine.plot_I_with_Cl  [,6:1]
URH.fine.plot_I_with_Cl<- cbind(URH.fine.plot_I_with_Cl[,1],URH.fine.plot_I_with_Cl[,1],URH.fine.plot_I_with_Cl[,1],URH.fine.plot_I_with_Cl[,2],URH.fine.plot_I_with_Cl[,1],URH.fine.plot_I_with_Cl[,3], URH.fine.plot_I_with_Cl[,1],URH.fine.plot_I_with_Cl[,4],URH.fine.plot_I_with_Cl[,4],URH.fine.plot_I_with_Cl[,4],URH.fine.plot_I_with_Cl[,4],URH.fine.plot_I_with_Cl[,4],URH.fine.plot_I_with_Cl[,4],
                                URH.fine.plot_I_with_Cl[,1],URH.fine.plot_I_with_Cl[,1],URH.fine.plot_I_with_Cl[,5],URH.fine.plot_I_with_Cl[,5],URH.fine.plot_I_with_Cl[,5],URH.fine.plot_I_with_Cl[,5],URH.fine.plot_I_with_Cl[,5],URH.fine.plot_I_with_Cl[,1],URH.fine.plot_I_with_Cl[,6],URH.fine.plot_I_with_Cl[,6],URH.fine.plot_I_with_Cl[,6],URH.fine.plot_I_with_Cl[,6],URH.fine.plot_I_with_Cl[,6],URH.fine.plot_I_with_Cl[,6],URH.fine.plot_I_with_Cl[,6],URH.fine.plot_I_with_Cl[,6])
URH.fine.plot_I_with_Cl[,c(3,5,7,14,15,21)]<-(-10)

RH.diff.plot_I_with_Cl  <- as.matrix(partition_part[,grep("rh.diff_L",colnames(partition_part))])
RH.diff.plot_I_with_Cl  <-RH.diff.plot_I_with_Cl [,6:1]
RH.diff.plot_I_with_Cl <- cbind(RH.diff.plot_I_with_Cl [,1],RH.diff.plot_I_with_Cl [,1],RH.diff.plot_I_with_Cl [,1],RH.diff.plot_I_with_Cl [,2],RH.diff.plot_I_with_Cl [,1],RH.diff.plot_I_with_Cl [,3], RH.diff.plot_I_with_Cl [,1],RH.diff.plot_I_with_Cl [,4],RH.diff.plot_I_with_Cl [,4],RH.diff.plot_I_with_Cl [,4],RH.diff.plot_I_with_Cl [,4],RH.diff.plot_I_with_Cl [,4],RH.diff.plot_I_with_Cl [,4],
                                RH.diff.plot_I_with_Cl [,1],RH.diff.plot_I_with_Cl [,1],RH.diff.plot_I_with_Cl [,5],RH.diff.plot_I_with_Cl [,5],RH.diff.plot_I_with_Cl [,5],RH.diff.plot_I_with_Cl [,5],RH.diff.plot_I_with_Cl [,5],RH.diff.plot_I_with_Cl [,1],RH.diff.plot_I_with_Cl [,6],RH.diff.plot_I_with_Cl [,6],RH.diff.plot_I_with_Cl [,6],RH.diff.plot_I_with_Cl [,6],RH.diff.plot_I_with_Cl [,6],RH.diff.plot_I_with_Cl [,6],RH.diff.plot_I_with_Cl [,6],RH.diff.plot_I_with_Cl [,6])
RH.diff.plot_I_with_Cl [,c(3,5,7,14,15,21)]<-(-10)

## plot parameters  
########### !!! premiere couche de l HR a 950 hPa = 500m d altitude
##axes SR
xscale<-1:nrow(partition_part)
labs_yscale_SR<-c(0.5,'',1.5,'',2.5,'',3.5,'',4.5,'',5.5,'',6.5,'',7.5,'',8.5,'',9.5,'',10.5,'',11.5,'',12.5,'',13.5,'',14.5,'',15.5,'',16.5)
yscale.SR<-seq(0.5,16,by=resprofile)
##axes RR     
yscale.RR<-c(0.5,1.2,seq(2, 16,1))
yscale.RR<-seq(0.5,17,1)
labs_yscale_RR<-seq(0.5,17,1)
yscale.RR_5km <-seq(0.5,6,1)
yscale.RH <- seq(0.5,6,1)
##axes des RH
labels_km<-c("0.6", "","1.5","1.9","2.5","","3","3.6","","4.2","","","","","7.2","","","8.1","","","","10.4","","11.8","","","","","","","15.8")
labels_hPa<-c("950", "","850","800","750","","700","650","","600","","","","","400","","","350","","","","250","","200","","","","","","","100")
##conversion de la pression en km
lablay<-c(950,850,800,750,700,650,600,400,350,250,200,100)
axe4_RH<-c()
for(press in 1: length(lablay))
{
  km<-(1-exp(log(lablay[press]/1013.25)/5.255))*(300/0.0065) ## 300K pour tropiques, moyennes latitudes: 288.15K
  km<-round(km,0)/1000
  axe4_RH<-c(axe4_RH,km)
}
axe4_RH<-round(axe4_RH,1)
## breaks et palette RR
breaks_RR <- seq(floor(min(RR.plot_Ice,na.rm = TRUE)), ceiling(max(RR.plot_Ice,na.rm=TRUE)), length=max(xx.plot[,grep("RR",colnames(xx.plot))]))
breaks_RR<- round(breaks_RR, 0)
breaks_RR <- unique(breaks_RR)
pal.RR<-colorRampPalette(t(c("blue","lightblue","green","yellow", "orange","darkorange","red","darkred")))(length(breaks_RR)-1)
##breaks et palette SR
breaks_SR<-c(seq(0,80,by=5),150)
pal.SR<-colorRampPalette(t(c("lightblue","blue","darkblue","darkred","red","darkorange", "orange","yellow","lightyellow","darkgrey")))(length(breaks_SR)-1)
## breaks PHASES
breaks_PHASE<-c(0:6,8)
nom_phase<-c("No cloud", "Ice","Liquid","Undefined","False liquid","False ice", "Horizontal","Fully Attenuated")
palette_PHASE<-colorRampPalette(t(c("lightblue","blue","darkred","red","darkorange", "orange","yellow","darkgrey")))(length(breaks_PHASE)-1)
#lablayer = c( "850-950", "750-800","650-700","400-600","250-350","100-200")
print("plot entire series")
##breaks RH 
breaks_RH<-seq(0,100,by=10)
pal.RH=rev(plasma(10)) 
##breaks URH 
min_break_URH<-0
max_break_URH<-max(max(URH.fine.plot_I_no_Cl),max(URH.fine.plot_I_with_Cl))
breaks_URH<-seq(min_break_URH,max_break_URH,by=1)
pal.URH<-colorRampPalette(t(c("white","blue","black")))(length(breaks_URH)-1)
##breaks Mass Balance
min_break_MassB<-0
max_break_MassB<-max(max(RH.diff.plot_I_no_Cl),max(RH.diff.plot_I_with_Cl))
breaks_URH<-seq(min_break_URH,max_break_URH,by=1)
pal.MASS_BAL<-colorRampPalette(t(c("white","red","darkred")))(length(breaks_URH)-1)
pal.MASS_BAL<-colorRampPalette(t(c("white","red","darkred","black")))(length(breaks_URH)-1)
zlim <- c(0, 100) 
ibreaks <- breaks_URH >= zlim[1] & breaks_URH <= zlim[2]
ipalette <- breaks_URH >= zlim[1] & breaks_URH < zlim[2]
yscale.RH <- seq(1,nlayer)


####  plots SR, phases, RR, RH coarse resolution
png(paste0(filename.plot_Ice,"_partition.png"), width = 1500, height = 1000, units = "px",res=200)
par(oma=c(2,1,2,2),mar=c(1,4,4,4)) ## https://informatique-mia.inrae.fr/r4ciam/sites/default/files/download/tutoriels/FenetreGraphique.pdf
par(mfrow=c(2,2))
# SR
image.plot(x=xscale, y=yscale.SR,z=SR.plot_Ice,col= pal.SR,ylim=c(min(yscale.SR),max(yscale.SR)),breaks=breaks_SR,lab.breaks = breaks_SR,axes=FALSE,ylab='',xlab='',horizontal=TRUE,legend.mar=3,legend.lab = "Scattering Ratio", legend.cex=1.3)
axis(2, at = yscale.SR,labels = labs_yscale_SR,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
abline(v =coord_abline,col="black" )
text(10,15,"a)",col='black',cex=2)
#mtext("a",3,line=1,col='black',cex=2)
#RR
image.plot(x=1:nrow(RR.plot_Ice), y=1:ncol(RR.plot_Ice),z=RR.plot_Ice,ylim=c(min(yscale.RR),max(yscale.RR)),col= pal.RR,breaks=breaks_RR,lab.breaks = breaks_RR,axes=FALSE,ylab='',xlab='', horizontal=TRUE,legend.mar=3,legend.lab = "Radar Reflectivity", legend.cex=1.3)
axis(2, at = yscale.RR,labels = labs_yscale_RR,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
abline(v =coord_abline,col="black" )
abline(h =5,col="black" )
text(10,15.5,"c)",col='black',cex=2)
# phases
image.plot(x=xscale, y=yscale.SR,z=SR_phase.plot_Ice,ylim=c(min(yscale.SR),max(yscale.SR)),breaks = breaks_PHASE, lab.breaks = nom_phase, col= palette_PHASE,axes=FALSE,ylab='',xlab='', horizontal=TRUE,legend.mar=3,legend.lab = "Cloud phase flag", legend.cex=1.3)
axis(2, at = yscale.SR,labels = labs_yscale_SR,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
abline(v =coord_abline,col="black" )
text(10,15,"b)",col='black',cex=2)
# RH
image.plot(x=1:nrow(RH_plot_Ice), y=1:ncol(RH_plot_Ice),z=RH_plot_Ice,col= pal.RH,breaks=breaks_RH,lab.breaks = breaks_RH, axes=FALSE,ylab='',xlab='',horizontal=TRUE,legend.mar=3, legend.lab = "Relative Humidity (%)", legend.cex=1.3)
axis(4, at = 1:31,labels = labels_hPa,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
#axis(2,at=log10(c(0.5,1.5,2,2.5,3,3.5,4,7,8,10.5,12,16)),labels = c(0.5,1.5,2,2.5,3,3.5,4,7,8,10.5,12,16),las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
abline(v =coord_abline,col="black" )
text(10,28,"d)",col='black',cex=2)
dev.off()
#add.log.axis(make.labs=TRUE, logs=axe4_RH, side=2)



## plot RH coarse and fine resolution (with and without CloudSat) and URH
print("plot graph partition RH coarse")
png(paste0(filename.plot_RH_Ice,"_partition.png"), width = 1500, height = 1000, units = "px",res=200)
par(oma=c(2,1,2,2),mar=c(1,4,4,4)) 
par(mfrow=c(3,2))
#plot de l HR coarse
image(x=1:nrow(RH_plot_Ice), y=1:ncol(RH_plot_Ice),z=RH_plot_Ice,col= pal.RH,breaks=breaks_RH,lab.breaks = breaks_RH, axes=FALSE,ylab='',xlab='', main='SAPHIR coarse resolution',cex.main=1.5)
axis(4, at = 1:31,labels = labels_hPa,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
#axis(2,at=1:31,labels = labels_km,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
abline(v =coord_abline,col="black" )
plot.new()
#plot de l HR fine sans CloudSat
image(x=1:nrow(RH.fine.plot_I_no_Cl), y=1:ncol(RH.fine.plot_I_no_Cl),z=RH.fine.plot_I_no_Cl,col= pal.RH,breaks=breaks_RH,lab.breaks = breaks_RH, axes=FALSE,ylab='',xlab='', main= "Predicted from SAPHIR, without CloudSat",cex.main=1.5)
abline(v =coord_abline,col="black" )
axis(4, at = 1:31,labels = labels_hPa,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
#axis(2,at=1:31,labels = labels_km,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
#plot de l UHR fine sans CloudSat
image(x=1:nrow(URH.fine.plot_I_no_Cl), y=1:ncol(URH.fine.plot_I_no_Cl),z=URH.fine.plot_I_no_Cl,col= pal.URH,breaks=breaks_URH,lab.breaks = breaks_URH,axes=FALSE,ylab='',xlab='', main= "Predicted Relative Humidity uncertainty(%), fine resolution",cex.main=1.5)
axis(4, at = 1:31,labels = labels_hPa,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
#axis(2,at=1:31,labels = labels_km,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
abline(v =coord_abline,col="black" )
#plot de l HR fine avec CloudSat
image.plot(x=1:nrow(RH.fine.plot_I_with_Cl), y=1:ncol(RH.fine.plot_I_with_Cl),z=RH.fine.plot_I_with_Cl,col= pal.RH,breaks=breaks_RH,lab.breaks = breaks_RH, axes=FALSE,ylab='',xlab='', main= "Predicted from SAPHIR, with CloudSat",cex.main=1.5,horizontal=TRUE,legend.mar=3,legend.lab = "Relative Humidity (%)")
abline(v =coord_abline,col="black" )
axis(4, at = 1:31,labels = labels_hPa,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
#axis(2,at=1:31,labels = labels_km,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
#plot de l UHR fine avec CloudSat
image.plot(x=1:nrow(URH.fine.plot_I_with_Cl), y=1:ncol(URH.fine.plot_I_with_Cl),z=URH.fine.plot_I_with_Cl,col= pal.URH,breaks=breaks_URH,lab.breaks = breaks_URH,axes=FALSE,ylab='',xlab='', main= "Predicted Relative Humidity uncertainty(%), fine resolution",cex.main=1.5,horizontal=TRUE,legend.mar=3)
abline(v =coord_abline,col="black" )
axis(4, at = 1:31,labels = labels_hPa,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
#axis(2,at=1:31,labels = labels_km,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
dev.off()


## plot Mass balance
par(oma=c(2,1,2,2),mar=c(1,4,4,4)) 
par(mfrow=c(2,2))
#mass balance sans CloudSat
#v<-expression(paste0("|RH(SAPHIR)-(RH(predicted))"["pixel"], "|(%)"))  
image(x=1:nrow(RH.diff.plot_I_no_Cl), y=1:ncol(RH.diff.plot_I_no_Cl),z=RH.diff.plot_I_no_Cl,col= pal.MASS_BAL,breaks=breaks_URH,lab.breaks = breaks_URH,axes=FALSE,ylab='',xlab='', main= "Mass balance",cex.main=1.5)
abline(v =coord_abline,col="black" )
#axis(1, at = seq(1,nrow(xx.plot),nrow(xx.plot)/10), labels = x_labels[seq(1,nrow(xx.plot),nrow(xx.plot)/10)],las = 0, srt = 45, xpd = TRUE, cex.axis =0.9)
axis(4, at = 1:31,labels = labels_hPa,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
#axis(2,at=1:31,labels = labels_km,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
plot.new()

#mass balance avec CloudSat
#v<-expression(paste0("|RH(SAPHIR)-(RH(predicted))"["pixel"], "|(%)"))  
image.plot(x=1:nrow(RH.diff.plot_I_with_Cl), y=1:ncol(RH.diff.plot_I_with_Cl),z=RH.diff.plot_I_with_Cl,col= pal.MASS_BAL,breaks=breaks_URH,lab.breaks = breaks_URH,axes=FALSE,ylab='',xlab='', main= "Mass balance",cex.main=1.5,horizontal=TRUE,legend.mar=3)
abline(v =coord_abline,col="black" )
#axis(1, at = seq(1,nrow(xx.plot),nrow(xx.plot)/10), labels = x_labels[seq(1,nrow(xx.plot),nrow(xx.plot)/10)],las = 0, srt = 45, xpd = TRUE, cex.axis =0.9)
axis(4, at = 1:31,labels = labels_hPa,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
#axis(2,at=1:31,labels = labels_km,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
plot.new()


######################################

rm(xx.plot,xx.plot2,partition_part)

######################################

################ LIQ cloud SANS & AVEC CloudSat  

xx.plot<-other_cloud_with_Cloudsat
names_ind<-unique(xx.plot$IND_pix_SAPHIR)
length_ind<-c()
for(ID in 1:length(names_ind))
{
  length_ind<-c(length_ind,length(which(names_ind[ID]==xx.plot$IND_pix_SAPHIR)))
}
  
sum_ind_sup_10<-sum(length_ind>=10)
ind_sup_10<-sum_ind_sup_10/3
  
min_ind<-23				##pour visualiser seulement les pixels de SAPHIR avec le plus grand nombre de points CALIPSO et CloudSAt
max_ind<-30
xx.plot2<-data.frame()
for(nID in 1:length(names_ind))
{
  same_ind<-which(names_ind[nID]==xx.plot$IND_pix_SAPHIR)
  if(length(same_ind)>=(min_ind) & length(same_ind)<=(max_ind))
  {
    lines_ind<-xx.plot[same_ind,]
    xx.plot2<-rbind(xx.plot2,data.frame(nID,lines_ind))
  }
}
  
partition<-split(xx.plot2,xx.plot2$IND_pix_SAPHIR)

#### pour selectionner les premiers elements
parti_list<-partition[c(2:10,12,14:16,20,26)]
partition_part<-data.frame()
Liq_cloud_without_Cloudsat_plot<-data.frame()
for(par in 1:length(parti_list))
{
  df_temp_parti<-parti_list[[par]]
  partition_part<-rbind(partition_part, data.frame(df_temp_parti))
  indice<-unique(df_temp_parti$IND_pix_SAPHIR)
  same_indice<-which(indice==other_cloud_without_Cloudsat$IND_pix_SAPHIR)
  Liq_cloud_without_Cloudsat_temp<-other_cloud_without_Cloudsat[same_indice,]
  Liq_cloud_without_Cloudsat_plot<-rbind(Liq_cloud_without_Cloudsat_plot,data.frame(Liq_cloud_without_Cloudsat_temp))
}


saveRDS(partition_part, file = paste0(dir.other_cloud_with_Cloudsat,"ICE_cloud_with_Cloudsat_for_plot.rds"))
partition_part<-readRDS(file = paste0(dir.other_cloud_with_Cloudsat,"ICE_cloud_with_Cloudsat_for_plot.rds"))

saveRDS(Liq_cloud_without_Cloudsat_plot, file = paste0(dir.other_cloud_without_Cloudsat,"ICE_cloud_without_Cloudsat_for_plot.rds"))
Liq_cloud_without_Cloudsat_plot<-readRDS(file = paste0(dir.other_cloud_without_Cloudsat,"ICE_cloud_without_Cloudsat_for_plot.rds"))



#### pour faire les lignes de separation delimitant les pixels SAPHIR
names_ind2<-unique(partition_part$IND_pix_SAPHIR)
length_ind2<-c()
for(ID in 1:length(names_ind2))
{
  length_ind2<-c(length_ind2,length(which(names_ind2[ID]==partition_part$IND_pix_SAPHIR)))
}

coord_abline<-c()
for(li2 in 1:length(length_ind2))
{
  if(li2==1){coord_abline<-c(length_ind2[1]+0.5)}
  else{
	coord<-coord_abline[li2-1]+length_ind2[li2]
	coord_abline<-c(coord_abline,coord)
  }
}


##data.frame for plot 
SR.plot_Liq <- as.matrix(partition_part[,grep("SR",colnames(partition_part))])
SR.plot_Liq <- SR.plot_Liq[,2:34]
SR_phase.plot_Liq <- as.matrix(partition_part[,grep("PHASE",colnames(partition_part))])
SR_phase.plot_Liq<-SR_phase.plot_Liq[,2:34]
RR.plot_Liq<-as.matrix(partition_part[,grep("RR",colnames(partition_part))])
RR.plot_Liq<-cbind(RR.plot_Liq[,2],RR.plot_Liq[,1:16])
RR.plot_Liq[,1]<-(-30)
RR.plot_5km_Liq<-as.matrix(RR.plot_Liq[,1:6])
RH_plot_Liq<- as.matrix(partition_part[,grep("rh",colnames(partition_part))])
RH_plot_Liq <-RH_plot_Liq[,6:1]
RH_plot_Liq<- cbind(RH_plot_Liq[,1],RH_plot_Liq[,1],RH_plot_Liq[,1],RH_plot_Liq[,2],RH_plot_Liq[,1],RH_plot_Liq[,3], RH_plot_Liq[,1],RH_plot_Liq[,4],RH_plot_Liq[,4],RH_plot_Liq[,4],RH_plot_Liq[,4],RH_plot_Liq[,4],RH_plot_Liq[,4],
                    RH_plot_Liq[,1],RH_plot_Liq[,1],RH_plot_Liq[,5],RH_plot_Liq[,5],RH_plot_Liq[,5],RH_plot_Liq[,5],RH_plot_Liq[,5],RH_plot_Liq[,1],RH_plot_Liq[,6],RH_plot_Liq[,6],RH_plot_Liq[,6],RH_plot_Liq[,6],RH_plot_Liq[,6],RH_plot_Liq[,6],RH_plot_Liq[,6],RH_plot_Liq[,6])
RH_plot_Liq[,c(3,5,7,14,15,21)]<-(-10)


### RH pour LIQ clouds SANS CloudSat
URH.plot_L_no_Cl <- as.matrix(Liq_cloud_without_Cloudsat_plot[,grep("rh.unc",colnames(Liq_cloud_without_Cloudsat_plot))])
URH.plot_L_no_Cl <-URH.plot_L_no_Cl[,6:1]
URH.plot_L_no_Cl<- cbind(URH.plot_L_no_Cl[,1],URH.plot_L_no_Cl[,1],URH.plot_L_no_Cl[,1],URH.plot_L_no_Cl[,2],URH.plot_L_no_Cl[,1],URH.plot_L_no_Cl[,3], URH.plot_L_no_Cl[,1],URH.plot_L_no_Cl[,4],URH.plot_L_no_Cl[,4],URH.plot_L_no_Cl[,4],URH.plot_L_no_Cl[,4],URH.plot_L_no_Cl[,4],URH.plot_L_no_Cl[,4],
                         URH.plot_L_no_Cl[,1],URH.plot_L_no_Cl[,1],URH.plot_L_no_Cl[,5],URH.plot_L_no_Cl[,5],URH.plot_L_no_Cl[,5],URH.plot_L_no_Cl[,5],URH.plot_L_no_Cl[,5],URH.plot_L_no_Cl[,1],URH.plot_L_no_Cl[,6],URH.plot_L_no_Cl[,6],URH.plot_L_no_Cl[,6],URH.plot_L_no_Cl[,6],URH.plot_L_no_Cl[,6],URH.plot_L_no_Cl[,6],URH.plot_L_no_Cl[,6],URH.plot_L_no_Cl[,6])
URH.plot_L_no_Cl[,c(3,5,7,14,15,21)]<-(-10)

RH.fine.plot_L_no_Cl <- as.matrix(Liq_cloud_without_Cloudsat_plot[,grep("rh.fine",colnames(Liq_cloud_without_Cloudsat_plot))])
RH.fine.plot_L_no_Cl <-RH.fine.plot_L_no_Cl[,6:1]
RH.fine.plot_L_no_Cl<- cbind(RH.fine.plot_L_no_Cl[,1],RH.fine.plot_L_no_Cl[,1],RH.fine.plot_L_no_Cl[,1],RH.fine.plot_L_no_Cl[,2],RH.fine.plot_L_no_Cl[,1],RH.fine.plot_L_no_Cl[,3], RH.fine.plot_L_no_Cl[,1],RH.fine.plot_L_no_Cl[,4],RH.fine.plot_L_no_Cl[,4],RH.fine.plot_L_no_Cl[,4],RH.fine.plot_L_no_Cl[,4],RH.fine.plot_L_no_Cl[,4],RH.fine.plot_L_no_Cl[,4],
                             RH.fine.plot_L_no_Cl[,1],RH.fine.plot_L_no_Cl[,1],RH.fine.plot_L_no_Cl[,5],RH.fine.plot_L_no_Cl[,5],RH.fine.plot_L_no_Cl[,5],RH.fine.plot_L_no_Cl[,5],RH.fine.plot_L_no_Cl[,5],RH.fine.plot_L_no_Cl[,1],RH.fine.plot_L_no_Cl[,6],RH.fine.plot_L_no_Cl[,6],RH.fine.plot_L_no_Cl[,6],RH.fine.plot_L_no_Cl[,6],RH.fine.plot_L_no_Cl[,6],RH.fine.plot_L_no_Cl[,6],RH.fine.plot_L_no_Cl[,6],RH.fine.plot_L_no_Cl[,6])
RH.fine.plot_L_no_Cl[,c(3,5,7,14,15,21)]<-(-10)

URH.fine.plot_L_no_Cl <- as.matrix(Liq_cloud_without_Cloudsat_plot[,grep("dep.fine.unc",colnames(Liq_cloud_without_Cloudsat_plot))])
URH.fine.plot_L_no_Cl <- URH.fine.plot_L_no_Cl [,6:1]
URH.fine.plot_L_no_Cl<- cbind(URH.fine.plot_L_no_Cl[,1],URH.fine.plot_L_no_Cl[,1],URH.fine.plot_L_no_Cl[,1],URH.fine.plot_L_no_Cl[,2],URH.fine.plot_L_no_Cl[,1],URH.fine.plot_L_no_Cl[,3], URH.fine.plot_L_no_Cl[,1],URH.fine.plot_L_no_Cl[,4],URH.fine.plot_L_no_Cl[,4],URH.fine.plot_L_no_Cl[,4],URH.fine.plot_L_no_Cl[,4],URH.fine.plot_L_no_Cl[,4],URH.fine.plot_L_no_Cl[,4],
                              URH.fine.plot_L_no_Cl[,1],URH.fine.plot_L_no_Cl[,1],URH.fine.plot_L_no_Cl[,5],URH.fine.plot_L_no_Cl[,5],URH.fine.plot_L_no_Cl[,5],URH.fine.plot_L_no_Cl[,5],URH.fine.plot_L_no_Cl[,5],URH.fine.plot_L_no_Cl[,1],URH.fine.plot_L_no_Cl[,6],URH.fine.plot_L_no_Cl[,6],URH.fine.plot_L_no_Cl[,6],URH.fine.plot_L_no_Cl[,6],URH.fine.plot_L_no_Cl[,6],URH.fine.plot_L_no_Cl[,6],URH.fine.plot_L_no_Cl[,6],URH.fine.plot_L_no_Cl[,6])
URH.fine.plot_L_no_Cl[,c(3,5,7,14,15,21)]<-(-10)

RH.diff.plot_L_no_Cl <- as.matrix(Liq_cloud_without_Cloudsat_plot[,grep("rh.diff_L",colnames(Liq_cloud_without_Cloudsat_plot))])
RH.diff.plot_L_no_Cl <-RH.diff.plot_L_no_Cl[,6:1]
RH.diff.plot_L_no_Cl<- cbind(RH.diff.plot_L_no_Cl[,1],RH.diff.plot_L_no_Cl[,1],RH.diff.plot_L_no_Cl[,1],RH.diff.plot_L_no_Cl[,2],RH.diff.plot_L_no_Cl[,1],RH.diff.plot_L_no_Cl[,3], RH.diff.plot_L_no_Cl[,1],RH.diff.plot_L_no_Cl[,4],RH.diff.plot_L_no_Cl[,4],RH.diff.plot_L_no_Cl[,4],RH.diff.plot_L_no_Cl[,4],RH.diff.plot_L_no_Cl[,4],RH.diff.plot_L_no_Cl[,4],
                             RH.diff.plot_L_no_Cl[,1],RH.diff.plot_L_no_Cl[,1],RH.diff.plot_L_no_Cl[,5],RH.diff.plot_L_no_Cl[,5],RH.diff.plot_L_no_Cl[,5],RH.diff.plot_L_no_Cl[,5],RH.diff.plot_L_no_Cl[,5],RH.diff.plot_L_no_Cl[,1],RH.diff.plot_L_no_Cl[,6],RH.diff.plot_L_no_Cl[,6],RH.diff.plot_L_no_Cl[,6],RH.diff.plot_L_no_Cl[,6],RH.diff.plot_L_no_Cl[,6],RH.diff.plot_L_no_Cl[,6],RH.diff.plot_L_no_Cl[,6],RH.diff.plot_L_no_Cl[,6])
RH.diff.plot_L_no_Cl[,c(3,5,7,14,15,21)]<-(-10)

### RH pour LIQ clouds AVEC CloudSat
URH.plot_L_with_Cl  <- as.matrix(partition_part[,grep("rh.unc",colnames(partition_part))])
URH.plot_L_with_Cl  <-URH.plot_L_with_Cl [,6:1]
URH.plot_L_with_Cl<- cbind(URH.plot_L_with_Cl[,1],URH.plot_L_with_Cl[,1],URH.plot_L_with_Cl[,1],URH.plot_L_with_Cl[,2],URH.plot_L_with_Cl[,1],URH.plot_L_with_Cl[,3], URH.plot_L_with_Cl[,1],URH.plot_L_with_Cl[,4],URH.plot_L_with_Cl[,4],URH.plot_L_with_Cl[,4],URH.plot_L_with_Cl[,4],URH.plot_L_with_Cl[,4],URH.plot_L_with_Cl[,4],
                           URH.plot_L_with_Cl[,1],URH.plot_L_with_Cl[,1],URH.plot_L_with_Cl[,5],URH.plot_L_with_Cl[,5],URH.plot_L_with_Cl[,5],URH.plot_L_with_Cl[,5],URH.plot_L_with_Cl[,5],URH.plot_L_with_Cl[,1],URH.plot_L_with_Cl[,6],URH.plot_L_with_Cl[,6],URH.plot_L_with_Cl[,6],URH.plot_L_with_Cl[,6],URH.plot_L_with_Cl[,6],URH.plot_L_with_Cl[,6],URH.plot_L_with_Cl[,6],URH.plot_L_with_Cl[,6])
URH.plot_L_with_Cl[,c(3,5,7,14,15,21)]<-(-10)

RH.fine.plot_L_with_Cl  <- as.matrix(partition_part[,grep("rh.fine",colnames(partition_part))])
RH.fine.plot_L_with_Cl  <-RH.fine.plot_L_with_Cl [,6:1]
RH.fine.plot_L_with_Cl<- cbind(RH.fine.plot_L_with_Cl[,1],RH.fine.plot_L_with_Cl[,1],RH.fine.plot_L_with_Cl[,1],RH.fine.plot_L_with_Cl[,2],RH.fine.plot_L_with_Cl[,1],RH.fine.plot_L_with_Cl[,3], RH.fine.plot_L_with_Cl[,1],RH.fine.plot_L_with_Cl[,4],RH.fine.plot_L_with_Cl[,4],RH.fine.plot_L_with_Cl[,4],RH.fine.plot_L_with_Cl[,4],RH.fine.plot_L_with_Cl[,4],RH.fine.plot_L_with_Cl[,4],
                               RH.fine.plot_L_with_Cl[,1],RH.fine.plot_L_with_Cl[,1],RH.fine.plot_L_with_Cl[,5],RH.fine.plot_L_with_Cl[,5],RH.fine.plot_L_with_Cl[,5],RH.fine.plot_L_with_Cl[,5],RH.fine.plot_L_with_Cl[,5],RH.fine.plot_L_with_Cl[,1],RH.fine.plot_L_with_Cl[,6],RH.fine.plot_L_with_Cl[,6],RH.fine.plot_L_with_Cl[,6],RH.fine.plot_L_with_Cl[,6],RH.fine.plot_L_with_Cl[,6],RH.fine.plot_L_with_Cl[,6],RH.fine.plot_L_with_Cl[,6],RH.fine.plot_L_with_Cl[,6])
RH.fine.plot_L_with_Cl[,c(3,5,7,14,15,21)]<-(-10)

URH.fine.plot_L_with_Cl  <- as.matrix(partition_part[,grep("dep.fine.unc",colnames(partition_part))])
URH.fine.plot_L_with_Cl  <- URH.fine.plot_L_with_Cl  [,6:1]
URH.fine.plot_L_with_Cl<- cbind(URH.fine.plot_L_with_Cl[,1],URH.fine.plot_L_with_Cl[,1],URH.fine.plot_L_with_Cl[,1],URH.fine.plot_L_with_Cl[,2],URH.fine.plot_L_with_Cl[,1],URH.fine.plot_L_with_Cl[,3], URH.fine.plot_L_with_Cl[,1],URH.fine.plot_L_with_Cl[,4],URH.fine.plot_L_with_Cl[,4],URH.fine.plot_L_with_Cl[,4],URH.fine.plot_L_with_Cl[,4],URH.fine.plot_L_with_Cl[,4],URH.fine.plot_L_with_Cl[,4],
                                URH.fine.plot_L_with_Cl[,1],URH.fine.plot_L_with_Cl[,1],URH.fine.plot_L_with_Cl[,5],URH.fine.plot_L_with_Cl[,5],URH.fine.plot_L_with_Cl[,5],URH.fine.plot_L_with_Cl[,5],URH.fine.plot_L_with_Cl[,5],URH.fine.plot_L_with_Cl[,1],URH.fine.plot_L_with_Cl[,6],URH.fine.plot_L_with_Cl[,6],URH.fine.plot_L_with_Cl[,6],URH.fine.plot_L_with_Cl[,6],URH.fine.plot_L_with_Cl[,6],URH.fine.plot_L_with_Cl[,6],URH.fine.plot_L_with_Cl[,6],URH.fine.plot_L_with_Cl[,6])
URH.fine.plot_L_with_Cl[,c(3,5,7,14,15,21)]<-(-10)

RH.diff.plot_L_with_Cl  <- as.matrix(partition_part[,grep("rh.diff_L",colnames(partition_part))])
RH.diff.plot_L_with_Cl  <-RH.diff.plot_L_with_Cl [,6:1]
RH.diff.plot_L_with_Cl <- cbind(RH.diff.plot_L_with_Cl [,1],RH.diff.plot_L_with_Cl [,1],RH.diff.plot_L_with_Cl [,1],RH.diff.plot_L_with_Cl [,2],RH.diff.plot_L_with_Cl [,1],RH.diff.plot_L_with_Cl [,3], RH.diff.plot_L_with_Cl [,1],RH.diff.plot_L_with_Cl [,4],RH.diff.plot_L_with_Cl [,4],RH.diff.plot_L_with_Cl [,4],RH.diff.plot_L_with_Cl [,4],RH.diff.plot_L_with_Cl [,4],RH.diff.plot_L_with_Cl [,4],
                                RH.diff.plot_L_with_Cl [,1],RH.diff.plot_L_with_Cl [,1],RH.diff.plot_L_with_Cl [,5],RH.diff.plot_L_with_Cl [,5],RH.diff.plot_L_with_Cl [,5],RH.diff.plot_L_with_Cl [,5],RH.diff.plot_L_with_Cl [,5],RH.diff.plot_L_with_Cl [,1],RH.diff.plot_L_with_Cl [,6],RH.diff.plot_L_with_Cl [,6],RH.diff.plot_L_with_Cl [,6],RH.diff.plot_L_with_Cl [,6],RH.diff.plot_L_with_Cl [,6],RH.diff.plot_L_with_Cl [,6],RH.diff.plot_L_with_Cl [,6],RH.diff.plot_L_with_Cl [,6])
RH.diff.plot_L_with_Cl [,c(3,5,7,14,15,21)]<-(-10)
 
## plot parameters  
########### !!! premiere couche de l HR a 950 hPa = 500m d altitude
##axes SR
xscale<-1:nrow(partition_part)
labs_yscale_SR<-c(0.5,'',1.5,'',2.5,'',3.5,'',4.5,'',5.5,'',6.5,'',7.5,'',8.5,'',9.5,'',10.5,'',11.5,'',12.5,'',13.5,'',14.5,'',15.5,'',16.5)
yscale.SR<-seq(0.5,16,by=resprofile)
##axes RR     
yscale.RR<-c(0.5,1.2,seq(2, 16,1))
yscale.RR<-seq(0.5,17,1)
labs_yscale_RR<-seq(0.5,17,1)
yscale.RR_5km <-seq(0.5,6,1)
yscale.RH <- seq(0.5,6,1)
##axes des RH
labels_km<-c("0.6", "","1.5","1.9","2.5","","3","3.6","","4.2","","","","","7.2","","","8.1","","","","10.4","","11.8","","","","","","","15.8")
labels_hPa<-c("950", "","850","800","750","","700","650","","600","","","","","400","","","350","","","","250","","200","","","","","","","100")
##conversion de la pression en km
lablay<-c(950,850,800,750,700,650,600,400,350,250,200,100)
axe4_RH<-c()
for(press in 1: length(lablay))
{
  km<-(1-exp(log(lablay[press]/1013.25)/5.255))*(300/0.0065) ## 300K pour tropiques, moyennes latitudes: 288.15K
  km<-round(km,0)/1000
  axe4_RH<-c(axe4_RH,km)
}
axe4_RH<-round(axe4_RH,1)
## breaks et palette RR
breaks_RR <- seq(floor(min(RR.plot_Liq,na.rm = TRUE)), ceiling(max(RR.plot_Liq,na.rm=TRUE)), length=max(xx.plot[,grep("RR",colnames(xx.plot))]))
breaks_RR<- round(breaks_RR, 0)
breaks_RR <- unique(breaks_RR)
pal.RR<-colorRampPalette(t(c("blue","lightblue","green","yellow", "orange","darkorange","red","darkred")))(length(breaks_RR)-1)
##breaks et palette SR
breaks_SR<-c(seq(0,80,by=5),150)
pal.SR<-colorRampPalette(t(c("lightblue","blue","darkblue","darkred","red","darkorange", "orange","yellow","lightyellow","darkgrey")))(length(breaks_SR)-1)
## breaks PHASES
breaks_PHASE<-c(0:6,8)
nom_phase<-c("No cloud", "Ice","Liquid","Undefined","False liquid","False ice", "Horizontal","Fully Attenuated")
palette_PHASE<-colorRampPalette(t(c("lightblue","blue","darkred","red","darkorange", "orange","yellow","darkgrey")))(length(breaks_PHASE)-1)
#lablayer = c( "850-950", "750-800","650-700","400-600","250-350","100-200")
print("plot entire series")
##breaks RH 
breaks_RH<-seq(0,100,by=10)
pal.RH=rev(plasma(10)) 
##breaks URH 
min_break_URH<-0
max_break_URH<-max(max(URH.fine.plot_L_with_Cl),max(URH.fine.plot_L_with_Cl))
breaks_URH<-seq(min_break_URH,max_break_URH,by=1)
pal.URH<-colorRampPalette(t(c("white","blue","black")))(length(breaks_URH)-1)
##breaks Mass Balance
min_break_MassB<-0
max_break_MassB<-max(max(RH.diff.plot_L_no_Cl),max(RH.diff.plot_L_with_Cl))
breaks_URH<-seq(min_break_URH,max_break_URH,by=1)
pal.MASS_BAL<-colorRampPalette(t(c("white","red","darkred")))(length(breaks_URH)-1)
pal.MASS_BAL<-colorRampPalette(t(c("white","red","darkred","black")))(length(breaks_URH)-1)
zlim <- c(0, 100) 
ibreaks <- breaks_URH >= zlim[1] & breaks_URH <= zlim[2]
ipalette <- breaks_URH >= zlim[1] & breaks_URH < zlim[2]
yscale.RH <- seq(1,nlayer)
## breaks PHASES
breaks_PHASE<-c(0:6,8)
nom_phase<-c("No cloud", "Ice","Liquid","Undefined","False liquid","False ice", "Horizontal","Fully Attenuated")
palette_PHASE<-colorRampPalette(t(c("lightblue","blue","darkred","red","darkorange", "orange","yellow","darkgrey")))(length(breaks_PHASE)-1)
lablayer = c( "850-950", "750-800","650-700","400-600","250-350","100-200")
print("plot entire series")


##plot SR, phases, RR, RH coarse resolution
#png(paste0(filename.plot,"_RH_partition.png"), width = 1500, height = 1000, units = "px",res=200)
par(oma=c(2,1,2,2),mar=c(1,4,4,4)) ## https://informatique-mia.inrae.fr/r4ciam/sites/default/files/download/tutoriels/FenetreGraphique.pdf
par(mfrow=c(2,2))
# SR
image.plot(x=xscale, y=yscale.SR,z=SR.plot_Liq,col= pal.SR,ylim=c(min(yscale.SR),max(yscale.SR)),breaks=breaks_SR,lab.breaks = breaks_SR,axes=FALSE,ylab='',xlab='',horizontal=TRUE,legend.mar=4,legend.lab = "Scattering Ratio",legend.cex=1.3)
axis(2, at = yscale.SR,labels = labs_yscale_SR,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
abline(v =coord_abline,col="black" )
text(10,15,"a)",col='black',cex=2)
#mtext("a",3,line=1,col='black',cex=2)
#RR
image.plot(x=1:nrow(RR.plot_Liq), y=1:ncol(RR.plot_Liq),z=RR.plot_Liq,ylim=c(min(yscale.RR),max(yscale.RR)),col= pal.RR,breaks=breaks_RR,lab.breaks = breaks_RR,axes=FALSE,ylab='',xlab='', horizontal=TRUE,legend.mar=3,legend.lab = "Radar Reflectivity", legend.cex=1.3)
axis(2, at = yscale.RR,labels = labs_yscale_RR,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
abline(v =coord_abline,col="black" )
abline(h =5,col="black" )
text(10,15.5,"c)",col='black',cex=2)
# phases
image.plot(x=xscale, y=yscale.SR,z=SR_phase.plot_Liq,ylim=c(min(yscale.SR),max(yscale.SR)),breaks = breaks_PHASE, lab.breaks = nom_phase, col= palette_PHASE,axes=FALSE,ylab='',xlab='', horizontal=TRUE,legend.mar=3,legend.lab = "Cloud phase flag", legend.cex=1.3)
axis(2, at = yscale.SR,labels = labs_yscale_SR,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
abline(v =coord_abline,col="black" )
text(10,15,"b)",col='black',cex=2)
# RH
image.plot(x=1:nrow(RH_plot_Liq), y=1:ncol(RH_plot_Liq),z=RH_plot_Liq,col= pal.RH,breaks=breaks_RH,lab.breaks = breaks_RH, axes=FALSE,ylab='',xlab='',horizontal=TRUE,legend.mar=3, legend.lab = "Relative Humidity (%)", legend.cex=1.3)
axis(4, at = 1:31,labels = labels_hPa,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
#axis(2,at=log10(c(0.5,1.5,2,2.5,3,3.5,4,7,8,10.5,12,16)),labels = c(0.5,1.5,2,2.5,3,3.5,4,7,8,10.5,12,16),las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
abline(v =coord_abline,col="black" )
text(10,28,"d)",col='black',cex=2)
dev.off()

## plot RH coarse and fine resolution (with and without CloudSat), Mass balance and URH
print("plot graph partition RH coarse")
#png(paste0(filename.plot,"_RH_partition.png"), width = 1500, height = 1000, units = "px",res=200)
par(oma=c(2,1,2,2),mar=c(1,4,4,4)) 
par(mfrow=c(3,2))
#plot de l HR coarse
image(x=1:nrow(RH_plot_Liq), y=1:ncol(RH_plot_Liq),z=RH_plot_Liq,col= pal.RH,breaks=breaks_RH,lab.breaks = breaks_RH, axes=FALSE,ylab='',xlab='', main='SAPHIR coarse resolution',cex.main=1.5)
#axis(4, at = 1:31,labels = labels_hPa,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
#axis(2,at=1:31,labels = labels_km,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
abline(v =coord_abline,col="black" )
plot.new()
#plot de l HR fine sans CloudSat
image(x=1:nrow(RH.fine.plot_L_no_Cl), y=1:ncol(RH.fine.plot_L_no_Cl),z=RH.fine.plot_L_no_Cl,col= pal.RH,breaks=breaks_RH,lab.breaks = breaks_RH, axes=FALSE,ylab='',xlab='', main= "Predicted from SAPHIR, without CloudSat",cex.main=1.5)
abline(v =coord_abline,col="black" )
#axis(4, at = 1:31,labels = labels_hPa,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
#axis(2,at=1:31,labels = labels_km,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
#plot de l UHR fine sans CloudSat
image(x=1:nrow(URH.fine.plot_L_no_Cl), y=1:ncol(URH.fine.plot_L_no_Cl),z=URH.fine.plot_L_no_Cl,col= pal.URH,breaks=breaks_URH,lab.breaks = breaks_URH,axes=FALSE,ylab='',xlab='', main= "Predicted Relative Humidity uncertainty(%), fine resolution",cex.main=1.5)
#axis(4, at = 1:31,labels = labels_hPa,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
#axis(2,at=1:31,labels = labels_km,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
abline(v =coord_abline,col="black" )
#plot de l HR fine avec CloudSat
image.plot(x=1:nrow(RH.fine.plot_L_with_Cl), y=1:ncol(RH.fine.plot_L_with_Cl),z=RH.fine.plot_L_with_Cl,col= pal.RH,breaks=breaks_RH,lab.breaks = breaks_RH, axes=FALSE,ylab='',xlab='', main= "Predicted from SAPHIR, with CloudSat",cex.main=1.5,horizontal=TRUE,legend.mar=3,legend.lab = "Relative Humidity (%)")
abline(v =coord_abline,col="black" )
#axis(4, at = 1:31,labels = labels_hPa,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
#axis(2,at=1:31,labels = labels_km,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
#plot de l UHR fine avec CloudSat
image.plot(x=1:nrow(URH.fine.plot_L_with_Cl), y=1:ncol(URH.fine.plot_L_with_Cl),z=URH.fine.plot_L_with_Cl,col= pal.URH,breaks=breaks_URH,lab.breaks = breaks_URH,axes=FALSE,ylab='',xlab='', main= "Predicted Relative Humidity uncertainty(%), fine resolution",cex.main=1.5,horizontal=TRUE,legend.mar=3)
abline(v =coord_abline,col="black" )
#axis(4, at = 1:31,labels = labels_hPa,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
#axis(2,at=1:31,labels = labels_km,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)

dev.off()

## plot mass balance
par(oma=c(2,1,2,2),mar=c(1,4,4,4)) 
par(mfrow=c(2,2))
#mass balance sans CloudSat
#v<-expression(paste0("|RH(SAPHIR)-(RH(predicted))"["pixel"], "|(%)"))  
image(x=1:nrow(RH.diff.plot_L_no_Cl), y=1:ncol(RH.diff.plot_L_no_Cl),z=RH.diff.plot_L_no_Cl,col= pal.MASS_BAL,breaks=breaks_URH,lab.breaks = breaks_URH,axes=FALSE,ylab='',xlab='', main= "Mass balance",cex.main=1.5)
abline(v =coord_abline,col="black" )
#axis(1, at = seq(1,nrow(xx.plot),nrow(xx.plot)/10), labels = x_labels[seq(1,nrow(xx.plot),nrow(xx.plot)/10)],las = 0, srt = 45, xpd = TRUE, cex.axis =0.9)
axis(4, at = 1:31,labels = labels_hPa,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
#axis(2,at=1:31,labels = labels_km,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
plot.new()

#mass balance avec CloudSat
#v<-expression(paste0("|RH(SAPHIR)-(RH(predicted))"["pixel"], "|(%)"))  
image.plot(x=1:nrow(RH.diff.plot_L_with_Cl), y=1:ncol(RH.diff.plot_L_with_Cl),z=RH.diff.plot_L_with_Cl,col= pal.MASS_BAL,breaks=breaks_URH,lab.breaks = breaks_URH,axes=FALSE,ylab='',xlab='', main= "Mass balance",cex.main=1.5,horizontal=TRUE,legend.mar=3,legend.lab = "absolute difference between coarse and predicted RH")
abline(v =coord_abline,col="black" )
#axis(1, at = seq(1,nrow(xx.plot),nrow(xx.plot)/10), labels = x_labels[seq(1,nrow(xx.plot),nrow(xx.plot)/10)],las = 0, srt = 45, xpd = TRUE, cex.axis =0.9)
axis(4, at = 1:31,labels = labels_hPa,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
plot.new()

rm(c(xx.plot,xx.plot2,partition_part))

######################################



################ ALL cloud SANS & AVEC CloudSat  
xx.plot<-all_cloud_with_Cloudsat
names_ind<-unique(xx.plot$IND_pix_SAPHIR)
length_ind<-c()
for(ID in 1:length(names_ind))
{
  length_ind<-c(length_ind,length(which(names_ind[ID]==xx.plot$IND_pix_SAPHIR)))
}
  
sum_ind_sup_10<-sum(length_ind>=10)
ind_sup_10<-sum_ind_sup_10/3
  
min_ind<-23				##pour visualiser seulement les pixels de SAPHIR avec le plus grand nombre de points CALIPSO et CloudSAt
max_ind<-30
xx.plot2<-data.frame()
for(nID in 1:length(names_ind))
{
  same_ind<-which(names_ind[nID]==xx.plot$IND_pix_SAPHIR)
  if(length(same_ind)>=(min_ind) & length(same_ind)<=(max_ind))
  {
    lines_ind<-xx.plot[same_ind,]
    if(lines_ind[,2:41]>0)
    {  
      xx.plot2<-rbind(xx.plot2,data.frame(nID,lines_ind))
    }
  }
}
  
partition<-split(xx.plot2,xx.plot2$IND_pix_SAPHIR)

#### pour selectionner les premiers elements
parti_list<-partition[c(100:120)]
partition_part<-data.frame()
for(par in 1:length(parti_list))
{
  df_temp_parti<-parti_list[[par]]
  partition_part<-rbind(partition_part, data.frame(df_temp_parti))
  }


saveRDS(partition_part, file = paste0(dir.all_cloud_with_Cloudsat,"ALL_cloud_with_Cloudsat_for_plot.rds"))
partition_part<-readRDS(file = paste0(dir.all_cloud_with_Cloudsat,"ALL_cloud_with_Cloudsat_for_plot.rds"))



#### pour faire les lignes de separation delimitant les pixels SAPHIR
names_ind2<-unique(partition_part$IND_pix_SAPHIR)
length_ind2<-c()
for(ID in 1:length(names_ind2))
{
  length_ind2<-c(length_ind2,length(which(names_ind2[ID]==partition_part$IND_pix_SAPHIR)))
}

coord_abline<-c()
for(li2 in 1:length(length_ind2))
{
  if(li2==1){coord_abline<-c(length_ind2[1]+0.5)}
  else{
	coord<-coord_abline[li2-1]+length_ind2[li2]
	coord_abline<-c(coord_abline,coord)
  }
}


##data.frame for plot 
SR.plot_ALL <- as.matrix(partition_part[,grep("SR",colnames(partition_part))])
SR.plot_ALL <- SR.plot_ALL[,2:34]
SR_phase.plot_ALL <- as.matrix(partition_part[,grep("PHASE",colnames(partition_part))])
SR_phase.plot_ALL<-SR_phase.plot_ALL[,2:34]
RR.plot_ALL<-as.matrix(partition_part[,grep("RR",colnames(partition_part))])
RR.plot_ALL<-cbind(RR.plot_ALL[,2],RR.plot_ALL[,1:16])
RR.plot_ALL[,1]<-(-30)
RR.plot_5km_ALL<-as.matrix(RR.plot_ALL[,1:6])
RH_plot_ALL<- as.matrix(partition_part[,grep("rh",colnames(partition_part))])
RH_plot_ALL <-RH_plot_ALL[,6:1]
RH_plot_ALL<- cbind(RH_plot_ALL[,1],RH_plot_ALL[,1],RH_plot_ALL[,1],RH_plot_ALL[,2],RH_plot_ALL[,1],RH_plot_ALL[,3], RH_plot_ALL[,1],RH_plot_ALL[,4],RH_plot_ALL[,4],RH_plot_ALL[,4],RH_plot_ALL[,4],RH_plot_ALL[,4],RH_plot_ALL[,4],
                    RH_plot_ALL[,1],RH_plot_ALL[,1],RH_plot_ALL[,5],RH_plot_ALL[,5],RH_plot_ALL[,5],RH_plot_ALL[,5],RH_plot_ALL[,5],RH_plot_ALL[,1],RH_plot_ALL[,6],RH_plot_ALL[,6],RH_plot_ALL[,6],RH_plot_ALL[,6],RH_plot_ALL[,6],RH_plot_ALL[,6],RH_plot_ALL[,6],RH_plot_ALL[,6])
RH_plot_ALL[,c(3,5,7,14,15,21)]<-(-10)


### RH pour ALL clouds SANS CloudSat
URH.plot_A_no_Cl <- as.matrix(Liq_cloud_without_Cloudsat_plot[,grep("rh.unc",colnames(Liq_cloud_without_Cloudsat_plot))])
URH.plot_A_no_Cl <-URH.plot_A_no_Cl[,6:1]
URH.plot_A_no_Cl<- cbind(URH.plot_A_no_Cl[,1],URH.plot_A_no_Cl[,1],URH.plot_A_no_Cl[,1],URH.plot_A_no_Cl[,2],URH.plot_A_no_Cl[,1],URH.plot_A_no_Cl[,3], URH.plot_A_no_Cl[,1],URH.plot_A_no_Cl[,4],URH.plot_A_no_Cl[,4],URH.plot_A_no_Cl[,4],URH.plot_A_no_Cl[,4],URH.plot_A_no_Cl[,4],URH.plot_A_no_Cl[,4],
                         URH.plot_A_no_Cl[,1],URH.plot_A_no_Cl[,1],URH.plot_A_no_Cl[,5],URH.plot_A_no_Cl[,5],URH.plot_A_no_Cl[,5],URH.plot_A_no_Cl[,5],URH.plot_A_no_Cl[,5],URH.plot_A_no_Cl[,1],URH.plot_A_no_Cl[,6],URH.plot_A_no_Cl[,6],URH.plot_A_no_Cl[,6],URH.plot_A_no_Cl[,6],URH.plot_A_no_Cl[,6],URH.plot_A_no_Cl[,6],URH.plot_A_no_Cl[,6],URH.plot_A_no_Cl[,6])
URH.plot_A_no_Cl[,c(3,5,7,14,15,21)]<-(-10)

RH.fine.plot_A_no_Cl <- as.matrix(Liq_cloud_without_Cloudsat_plot[,grep("rh.fine",colnames(Liq_cloud_without_Cloudsat_plot))])
RH.fine.plot_A_no_Cl <-RH.fine.plot_A_no_Cl[,6:1]
RH.fine.plot_A_no_Cl<- cbind(RH.fine.plot_A_no_Cl[,1],RH.fine.plot_A_no_Cl[,1],RH.fine.plot_A_no_Cl[,1],RH.fine.plot_A_no_Cl[,2],RH.fine.plot_A_no_Cl[,1],RH.fine.plot_A_no_Cl[,3], RH.fine.plot_A_no_Cl[,1],RH.fine.plot_A_no_Cl[,4],RH.fine.plot_A_no_Cl[,4],RH.fine.plot_A_no_Cl[,4],RH.fine.plot_A_no_Cl[,4],RH.fine.plot_A_no_Cl[,4],RH.fine.plot_A_no_Cl[,4],
                             RH.fine.plot_A_no_Cl[,1],RH.fine.plot_A_no_Cl[,1],RH.fine.plot_A_no_Cl[,5],RH.fine.plot_A_no_Cl[,5],RH.fine.plot_A_no_Cl[,5],RH.fine.plot_A_no_Cl[,5],RH.fine.plot_A_no_Cl[,5],RH.fine.plot_A_no_Cl[,1],RH.fine.plot_A_no_Cl[,6],RH.fine.plot_A_no_Cl[,6],RH.fine.plot_A_no_Cl[,6],RH.fine.plot_A_no_Cl[,6],RH.fine.plot_A_no_Cl[,6],RH.fine.plot_A_no_Cl[,6],RH.fine.plot_A_no_Cl[,6],RH.fine.plot_A_no_Cl[,6])
RH.fine.plot_A_no_Cl[,c(3,5,7,14,15,21)]<-(-10)

URH.fine.plot_A_no_Cl <- as.matrix(Liq_cloud_without_Cloudsat_plot[,grep("dep.fine.unc",colnames(Liq_cloud_without_Cloudsat_plot))])
URH.fine.plot_A_no_Cl <- URH.fine.plot_A_no_Cl [,6:1]
URH.fine.plot_A_no_Cl<- cbind(URH.fine.plot_A_no_Cl[,1],URH.fine.plot_A_no_Cl[,1],URH.fine.plot_A_no_Cl[,1],URH.fine.plot_A_no_Cl[,2],URH.fine.plot_A_no_Cl[,1],URH.fine.plot_A_no_Cl[,3], URH.fine.plot_A_no_Cl[,1],URH.fine.plot_A_no_Cl[,4],URH.fine.plot_A_no_Cl[,4],URH.fine.plot_A_no_Cl[,4],URH.fine.plot_A_no_Cl[,4],URH.fine.plot_A_no_Cl[,4],URH.fine.plot_A_no_Cl[,4],
                              URH.fine.plot_A_no_Cl[,1],URH.fine.plot_A_no_Cl[,1],URH.fine.plot_A_no_Cl[,5],URH.fine.plot_A_no_Cl[,5],URH.fine.plot_A_no_Cl[,5],URH.fine.plot_A_no_Cl[,5],URH.fine.plot_A_no_Cl[,5],URH.fine.plot_A_no_Cl[,1],URH.fine.plot_A_no_Cl[,6],URH.fine.plot_A_no_Cl[,6],URH.fine.plot_A_no_Cl[,6],URH.fine.plot_A_no_Cl[,6],URH.fine.plot_A_no_Cl[,6],URH.fine.plot_A_no_Cl[,6],URH.fine.plot_A_no_Cl[,6],URH.fine.plot_A_no_Cl[,6])
URH.fine.plot_A_no_Cl[,c(3,5,7,14,15,21)]<-(-10)

RH.diff.plot_A_no_Cl <- as.matrix(Liq_cloud_without_Cloudsat_plot[,grep("rh.diff_L",colnames(Liq_cloud_without_Cloudsat_plot))])
RH.diff.plot_A_no_Cl <-RH.diff.plot_A_no_Cl[,6:1]
RH.diff.plot_A_no_Cl<- cbind(RH.diff.plot_A_no_Cl[,1],RH.diff.plot_A_no_Cl[,1],RH.diff.plot_A_no_Cl[,1],RH.diff.plot_A_no_Cl[,2],RH.diff.plot_A_no_Cl[,1],RH.diff.plot_A_no_Cl[,3], RH.diff.plot_A_no_Cl[,1],RH.diff.plot_A_no_Cl[,4],RH.diff.plot_A_no_Cl[,4],RH.diff.plot_A_no_Cl[,4],RH.diff.plot_A_no_Cl[,4],RH.diff.plot_A_no_Cl[,4],RH.diff.plot_A_no_Cl[,4],
                             RH.diff.plot_A_no_Cl[,1],RH.diff.plot_A_no_Cl[,1],RH.diff.plot_A_no_Cl[,5],RH.diff.plot_A_no_Cl[,5],RH.diff.plot_A_no_Cl[,5],RH.diff.plot_A_no_Cl[,5],RH.diff.plot_A_no_Cl[,5],RH.diff.plot_A_no_Cl[,1],RH.diff.plot_A_no_Cl[,6],RH.diff.plot_A_no_Cl[,6],RH.diff.plot_A_no_Cl[,6],RH.diff.plot_A_no_Cl[,6],RH.diff.plot_A_no_Cl[,6],RH.diff.plot_A_no_Cl[,6],RH.diff.plot_A_no_Cl[,6],RH.diff.plot_A_no_Cl[,6])
RH.diff.plot_A_no_Cl[,c(3,5,7,14,15,21)]<-(-10)

### RH pour ALL clouds AVEC CloudSat
URH.plot_A_with_Cl  <- as.matrix(partition_part[,grep("rh.unc",colnames(partition_part))])
URH.plot_A_with_Cl  <-URH.plot_A_with_Cl [,6:1]
URH.plot_A_with_Cl<- cbind(URH.plot_A_with_Cl[,1],URH.plot_A_with_Cl[,1],URH.plot_A_with_Cl[,1],URH.plot_A_with_Cl[,2],URH.plot_A_with_Cl[,1],URH.plot_A_with_Cl[,3], URH.plot_A_with_Cl[,1],URH.plot_A_with_Cl[,4],URH.plot_A_with_Cl[,4],URH.plot_A_with_Cl[,4],URH.plot_A_with_Cl[,4],URH.plot_A_with_Cl[,4],URH.plot_A_with_Cl[,4],
                           URH.plot_A_with_Cl[,1],URH.plot_A_with_Cl[,1],URH.plot_A_with_Cl[,5],URH.plot_A_with_Cl[,5],URH.plot_A_with_Cl[,5],URH.plot_A_with_Cl[,5],URH.plot_A_with_Cl[,5],URH.plot_A_with_Cl[,1],URH.plot_A_with_Cl[,6],URH.plot_A_with_Cl[,6],URH.plot_A_with_Cl[,6],URH.plot_A_with_Cl[,6],URH.plot_A_with_Cl[,6],URH.plot_A_with_Cl[,6],URH.plot_A_with_Cl[,6],URH.plot_A_with_Cl[,6])
URH.plot_A_with_Cl[,c(3,5,7,14,15,21)]<-(-10)

RH.fine.plot_A_with_Cl  <- as.matrix(partition_part[,grep("rh.fine",colnames(partition_part))])
RH.fine.plot_A_with_Cl  <-RH.fine.plot_A_with_Cl [,6:1]
RH.fine.plot_A_with_Cl<- cbind(RH.fine.plot_A_with_Cl[,1],RH.fine.plot_A_with_Cl[,1],RH.fine.plot_A_with_Cl[,1],RH.fine.plot_A_with_Cl[,2],RH.fine.plot_A_with_Cl[,1],RH.fine.plot_A_with_Cl[,3], RH.fine.plot_A_with_Cl[,1],RH.fine.plot_A_with_Cl[,4],RH.fine.plot_A_with_Cl[,4],RH.fine.plot_A_with_Cl[,4],RH.fine.plot_A_with_Cl[,4],RH.fine.plot_A_with_Cl[,4],RH.fine.plot_A_with_Cl[,4],
                               RH.fine.plot_A_with_Cl[,1],RH.fine.plot_A_with_Cl[,1],RH.fine.plot_A_with_Cl[,5],RH.fine.plot_A_with_Cl[,5],RH.fine.plot_A_with_Cl[,5],RH.fine.plot_A_with_Cl[,5],RH.fine.plot_A_with_Cl[,5],RH.fine.plot_A_with_Cl[,1],RH.fine.plot_A_with_Cl[,6],RH.fine.plot_A_with_Cl[,6],RH.fine.plot_A_with_Cl[,6],RH.fine.plot_A_with_Cl[,6],RH.fine.plot_A_with_Cl[,6],RH.fine.plot_A_with_Cl[,6],RH.fine.plot_A_with_Cl[,6],RH.fine.plot_A_with_Cl[,6])
RH.fine.plot_A_with_Cl[,c(3,5,7,14,15,21)]<-(-10)

URH.fine.plot_A_with_Cl  <- as.matrix(partition_part[,grep("dep.fine.unc",colnames(partition_part))])
URH.fine.plot_A_with_Cl  <- URH.fine.plot_A_with_Cl  [,6:1]
URH.fine.plot_A_with_Cl<- cbind(URH.fine.plot_A_with_Cl[,1],URH.fine.plot_A_with_Cl[,1],URH.fine.plot_A_with_Cl[,1],URH.fine.plot_A_with_Cl[,2],URH.fine.plot_A_with_Cl[,1],URH.fine.plot_A_with_Cl[,3], URH.fine.plot_A_with_Cl[,1],URH.fine.plot_A_with_Cl[,4],URH.fine.plot_A_with_Cl[,4],URH.fine.plot_A_with_Cl[,4],URH.fine.plot_A_with_Cl[,4],URH.fine.plot_A_with_Cl[,4],URH.fine.plot_A_with_Cl[,4],
                                URH.fine.plot_A_with_Cl[,1],URH.fine.plot_A_with_Cl[,1],URH.fine.plot_A_with_Cl[,5],URH.fine.plot_A_with_Cl[,5],URH.fine.plot_A_with_Cl[,5],URH.fine.plot_A_with_Cl[,5],URH.fine.plot_A_with_Cl[,5],URH.fine.plot_A_with_Cl[,1],URH.fine.plot_A_with_Cl[,6],URH.fine.plot_A_with_Cl[,6],URH.fine.plot_A_with_Cl[,6],URH.fine.plot_A_with_Cl[,6],URH.fine.plot_A_with_Cl[,6],URH.fine.plot_A_with_Cl[,6],URH.fine.plot_A_with_Cl[,6],URH.fine.plot_A_with_Cl[,6])
URH.fine.plot_A_with_Cl[,c(3,5,7,14,15,21)]<-(-10)

RH.diff.plot_A_with_Cl  <- as.matrix(partition_part[,grep("rh.diff_L",colnames(partition_part))])
RH.diff.plot_A_with_Cl  <-RH.diff.plot_A_with_Cl [,6:1]
RH.diff.plot_A_with_Cl <- cbind(RH.diff.plot_A_with_Cl [,1],RH.diff.plot_A_with_Cl [,1],RH.diff.plot_A_with_Cl [,1],RH.diff.plot_A_with_Cl [,2],RH.diff.plot_A_with_Cl [,1],RH.diff.plot_A_with_Cl [,3], RH.diff.plot_A_with_Cl [,1],RH.diff.plot_A_with_Cl [,4],RH.diff.plot_A_with_Cl [,4],RH.diff.plot_A_with_Cl [,4],RH.diff.plot_A_with_Cl [,4],RH.diff.plot_A_with_Cl [,4],RH.diff.plot_A_with_Cl [,4],
                                RH.diff.plot_A_with_Cl [,1],RH.diff.plot_A_with_Cl [,1],RH.diff.plot_A_with_Cl [,5],RH.diff.plot_A_with_Cl [,5],RH.diff.plot_A_with_Cl [,5],RH.diff.plot_A_with_Cl [,5],RH.diff.plot_A_with_Cl [,5],RH.diff.plot_A_with_Cl [,1],RH.diff.plot_A_with_Cl [,6],RH.diff.plot_A_with_Cl [,6],RH.diff.plot_A_with_Cl [,6],RH.diff.plot_A_with_Cl [,6],RH.diff.plot_A_with_Cl [,6],RH.diff.plot_A_with_Cl [,6],RH.diff.plot_A_with_Cl [,6],RH.diff.plot_A_with_Cl [,6])
RH.diff.plot_A_with_Cl [,c(3,5,7,14,15,21)]<-(-10)

## plot parameters  
########### !!! premiere couche de l HR a 950 hPa = 500m d altitude
##axes SR
xscale<-1:nrow(partition_part)
labs_yscale_SR<-c(0.5,'',1.5,'',2.5,'',3.5,'',4.5,'',5.5,'',6.5,'',7.5,'',8.5,'',9.5,'',10.5,'',11.5,'',12.5,'',13.5,'',14.5,'',15.5,'',16.5)
yscale.SR<-seq(0.5,16,by=resprofile)
##axes RR     
yscale.RR<-c(0.5,1.2,seq(2, 16,1))
yscale.RR<-seq(0.5,17,1)
labs_yscale_RR<-seq(0.5,17,1)
yscale.RR_5km <-seq(0.5,6,1)
yscale.RH <- seq(0.5,6,1)
##axes des RH
labels_km<-c("0.6", "","1.5","1.9","2.5","","3","3.6","","4.2","","","","","7.2","","","8.1","","","","10.4","","11.8","","","","","","","15.8")
labels_hPa<-c("950", "","850","800","750","","700","650","","600","","","","","400","","","350","","","","250","","200","","","","","","","100")
##conversion de la pression en km
lablay<-c(950,850,800,750,700,650,600,400,350,250,200,100)
axe4_RH<-c()
for(press in 1: length(lablay))
{
  km<-(1-exp(log(lablay[press]/1013.25)/5.255))*(300/0.0065) ## 300K pour tropiques, moyennes latitudes: 288.15K
  km<-round(km,0)/1000
  axe4_RH<-c(axe4_RH,km)
}
axe4_RH<-round(axe4_RH,1)
## breaks et palette RR
breaks_RR <- seq(floor(min(RR.plot_ALL,na.rm = TRUE)), ceiling(max(RR.plot_ALL,na.rm=TRUE)), length=max(xx.plot[,grep("RR",colnames(xx.plot))]))
breaks_RR<- round(breaks_RR, 0)
breaks_RR <- unique(breaks_RR)
pal.RR<-colorRampPalette(t(c("blue","lightblue","green","yellow", "orange","darkorange","red","darkred")))(length(breaks_RR)-1)
##breaks et palette SR
breaks_SR<-c(seq(0,80,by=5),150)
pal.SR<-colorRampPalette(t(c("lightblue","blue","darkblue","darkred","red","darkorange", "orange","yellow","lightyellow","darkgrey")))(length(breaks_SR)-1)
## breaks PHASES
breaks_PHASE<-c(0:6,8)
nom_phase<-c("No cloud", "Ice","Liquid","Undefined","False liquid","False ice", "Horizontal","Fully Attenuated")
palette_PHASE<-colorRampPalette(t(c("lightblue","blue","darkred","red","darkorange", "orange","yellow","darkgrey")))(length(breaks_PHASE)-1)
#lablayer = c( "850-950", "750-800","650-700","400-600","250-350","100-200")
print("plot entire series")
##breaks RH 
breaks_RH<-seq(0,100,by=10)
pal.RH=rev(plasma(10)) 
##breaks URH 
min_break_URH<-0
max_break_URH<-max(max(URH.fine.plot_A_with_Cl),max(URH.fine.plot_A_with_Cl))
breaks_URH<-seq(min_break_URH,max_break_URH,by=1)
pal.URH<-colorRampPalette(t(c("white","blue","black")))(length(breaks_URH)-1)
##breaks Mass Balance
min_break_MassB<-0
max_break_MassB<-max(max(RH.diff.plot_A_with_Cl),max(RH.diff.plot_A_with_Cl))
breaks_URH<-seq(min_break_URH,max_break_URH,by=1)
pal.MASS_BAL<-colorRampPalette(t(c("white","red","darkred")))(length(breaks_URH)-1)
pal.MASS_BAL<-colorRampPalette(t(c("white","red","darkred","black")))(length(breaks_URH)-1)
zlim <- c(0, 100) 
ibreaks <- breaks_URH >= zlim[1] & breaks_URH <= zlim[2]
ipalette <- breaks_URH >= zlim[1] & breaks_URH < zlim[2]
yscale.RH <- seq(1,nlayer)
## breaks PHASES
breaks_PHASE<-c(0:6,8)
nom_phase<-c("No cloud", "Ice","Liquid","Undefined","False liquid","False ice", "Horizontal","Fully Attenuated")
palette_PHASE<-colorRampPalette(t(c("lightblue","blue","darkred","red","darkorange", "orange","yellow","darkgrey")))(length(breaks_PHASE)-1)
lablayer = c( "850-950", "750-800","650-700","400-600","250-350","100-200")
print("plot entire series")


##plot SR, phases, RR, RH coarse resolution
#png(paste0(filename.plot,"_RH_partition.png"), width = 1500, height = 1000, units = "px",res=200)
par(oma=c(2,1,2,2),mar=c(1,4,4,4)) ## https://informatique-mia.inrae.fr/r4ciam/sites/default/files/download/tutoriels/FenetreGraphique.pdf
par(mfrow=c(2,2))
# SR
image.plot(x=xscale, y=yscale.SR,z=SR.plot_ALL,col= pal.SR,ylim=c(min(yscale.SR),max(yscale.SR)),breaks=breaks_SR,lab.breaks = breaks_SR,axes=FALSE,ylab='',xlab='',horizontal=TRUE,legend.mar=4,legend.lab = "Scattering Ratio",legend.cex=1.3)
axis(2, at = yscale.SR,labels = labs_yscale_SR,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
abline(v =coord_abline,col="black" )
text(10,15,"a)",col='black',cex=2)
#mtext("a",3,line=1,col='black',cex=2)
#RR
image.plot(x=1:nrow(RR.plot_ALL), y=1:ncol(RR.plot_ALL),z=RR.plot_ALL,ylim=c(min(yscale.RR),max(yscale.RR)),col= pal.RR,breaks=breaks_RR,lab.breaks = breaks_RR,axes=FALSE,ylab='',xlab='', horizontal=TRUE,legend.mar=3,legend.lab = "Radar Reflectivity", legend.cex=1.3)
axis(2, at = yscale.RR,labels = labs_yscale_RR,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
abline(v =coord_abline,col="black" )
abline(h =5,col="black" )
text(10,15.5,"c)",col='black',cex=2)
# phases
image.plot(x=xscale, y=yscale.SR,z=SR_phase.plot_ALL,ylim=c(min(yscale.SR),max(yscale.SR)),breaks = breaks_PHASE, lab.breaks = nom_phase, col= palette_PHASE,axes=FALSE,ylab='',xlab='', horizontal=TRUE,legend.mar=3,legend.lab = "Cloud phase flag", legend.cex=1.3)
axis(2, at = yscale.SR,labels = labs_yscale_SR,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
abline(v =coord_abline,col="black" )
text(10,15,"b)",col='black',cex=2)
# RH
image.plot(x=1:nrow(RH_plot_ALL), y=1:ncol(RH_plot_ALL),z=RH_plot_ALL,col= pal.RH,breaks=breaks_RH,lab.breaks = breaks_RH, axes=FALSE,ylab='',xlab='',horizontal=TRUE,legend.mar=3, legend.lab = "Relative Humidity (%)", legend.cex=1.3)
axis(4, at = 1:31,labels = labels_hPa,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
#axis(2,at=log10(c(0.5,1.5,2,2.5,3,3.5,4,7,8,10.5,12,16)),labels = c(0.5,1.5,2,2.5,3,3.5,4,7,8,10.5,12,16),las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
abline(v =coord_abline,col="black" )
text(10,28,"d)",col='black',cex=2)
dev.off()

## plot RH coarse and fine resolution (with CloudSat), Mass balance and URH
print("plot graph partition RH coarse")
#png(paste0(filename.plot,"_RH_partition.png"), width = 1500, height = 1000, units = "px",res=200)
par(oma=c(2,1,2,2),mar=c(1,4,4,4)) 
par(mfrow=c(2,2))
#plot de l HR coarse
image(x=1:nrow(RH_plot_ALL), y=1:ncol(RH_plot_ALL),z=RH_plot_ALL,col= pal.RH,breaks=breaks_RH,lab.breaks = breaks_RH, axes=FALSE,ylab='',xlab='', main='SAPHIR coarse resolution',cex.main=1.5)
axis(4, at = 1:31,labels = labels_hPa,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
#axis(2,at=1:31,labels = labels_km,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
abline(v =coord_abline,col="black" )
plot.new()
#plot de l HR fine avec CloudSat
image(x=1:nrow(RH.fine.plot_A_with_Cl), y=1:ncol(RH.fine.plot_A_with_Cl),z=RH.fine.plot_A_with_Cl,col= pal.RH,breaks=breaks_RH,lab.breaks = breaks_RH, axes=FALSE,ylab='',xlab='', main= "Predicted from SAPHIR, without CloudSat",cex.main=1.5)
abline(v =coord_abline,col="black" )
axis(4, at = 1:31,labels = labels_hPa,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
#axis(2,at=1:31,labels = labels_km,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
#plot de l UHR fine sans CloudSat
image(x=1:nrow(URH.fine.plot_A_with_Cl), y=1:ncol(URH.fine.plot_A_with_Cl),z=URH.fine.plot_A_with_Cl,col= pal.URH,breaks=breaks_URH,lab.breaks = breaks_URH,axes=FALSE,ylab='',xlab='', main= "Predicted Relative Humidity uncertainty(%), fine resolution",cex.main=1.5)
axis(4, at = 1:31,labels = labels_hPa,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
#axis(2,at=1:31,labels = labels_km,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)
abline(v =coord_abline,col="black" )

dev.off()

## plot mass balance
par(oma=c(2,1,2,2),mar=c(1,4,4,4)) 
#mass balance avec CloudSat
#v<-expression(paste0("|RH(SAPHIR)-(RH(predicted))"["pixel"], "|(%)"))  
image.plot(x=1:nrow(RH.diff.plot_A_with_Cl), y=1:ncol(RH.diff.plot_A_with_Cl),z=RH.diff.plot_A_with_Cl,col= pal.MASS_BAL,breaks=breaks_URH,lab.breaks = breaks_URH,axes=FALSE,ylab='',xlab='', main= "Mass balance",cex.main=1.5,horizontal=TRUE,legend.mar=3,legend.lab = "absolute difference between coarse and predicted RH")
abline(v =coord_abline,col="black" )
#axis(1, at = seq(1,nrow(xx.plot),nrow(xx.plot)/10), labels = x_labels[seq(1,nrow(xx.plot),nrow(xx.plot)/10)],las = 0, srt = 45, xpd = TRUE, cex.axis =0.9)
axis(4, at = 1:31,labels = labels_hPa,las=1,srt = 45, cex.axis =1.5, las = 2, xpd = TRUE)





