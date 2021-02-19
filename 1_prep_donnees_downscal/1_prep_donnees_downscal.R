# PATHS
# source("../Setpath.R")

  # Phases des nuages : voir table B1 Guzman et al., 2017, doi:10.1002/2016JD025946 
  # 0 = CLEAR
  # 1 = LIQ
  # 2 = ICE
  # 3 = UNDEFINED
  # 4 = FALSE_LIQ
  # 5 = FALSE_ICE
  # 6 = HORIZONTALLY ORIENTED
  # 7 = UNPHYSICAL VALUE
  # 8 = FULLY ATTENUATED added by vmichot
classify_profile <- function(phases) {
  # classes <- c("ICE", "LIQUID", "MIX", "LIQ_UNDEF", "ICE_UNDEF", "MIX_UNDEF", "CLEAR", "CLEAR_UNDEF", "FALSE_ICELIQ")
  if (any(phases %in% 4:5)) {
    return("FALSE_ICELIQ")
  } else if (all(phases == 0)) {
    return("CLEAR")
  } else if (any(phases %in% c(3, 6, 7))) {
    if (any(phases == 1) & any(phases == 2)) {
      return("MIX_UNDEF")
    } else if (any(phases == 1)) {
      return("LIQ_UNDEF")
    } else if (any(phases == 2)) {
      return("ICE_UNDEF")
    } else {
      return("CLEAR_UNDEF")
    }
  } else if (any(phases == 1) & any(phases == 2)) {
    return("MIX")
  } else if (any(phases == 1)) {
    return("LIQ")
  } else if (any(phases == 2)) {
    return("ICE")
  } else {
    return(NA)
  }
}



# dx: Maximum distance [km]
# dt: Maximum temporal separation [min]
prep_donnees_downscal <- function(area, year, month.num, dx = 5, dt = 30)
{
  
  # Set input filename
  bdd_data <- file.path("/bdd/MEGHA_TROPIQUES/MT_WORKSPACE/SAPHIR_downscaling/Calibration-Data_vmichot/CALIPSO_SAPHIR_coloc_out_all_tropical_seas/allday")
  if ( !is.null(area) ) {
    filename <-  paste0(bdd_data,"/CALIPSO_SAPHIR_CLOUDSAT_",year,"-",month.num,"_",dt,"_",dx,"_",area,"_","speed_RH_SR-OPAQ-PHASE_RF_allvar.dat")
  } else {
    filename <- paste0(bdd_data,"/CALIPSO_SAPHIR_CLOUDSAT_",year,"-",month.num,"_",dt,"_",dx,"_","speed_RH_SR-OPAQ-PHASE_RF_allvar.dat")
  }
  
  # Ouverture des fichiers
  stopifnot(file.exists(filename))
  match_df <- read.table(filename,header=TRUE,sep=";", dec = ".")  
  
  # pour connaître le nombre de lignes initiales (donc de données CALIPSO)
  print(paste("nb initial obs", nrow(match_df)))
  
  
  # --- Reorganisation du fichier match_df ---

  # pour remettre les colonnes de RR dans le bon ordre : du sol vers l atmosphere
  i_RR <- grep("RR", colnames(match_df))
  valt <- as.numeric(gsub("RR", "", colnames(match_df)[i_RR]))
  match_df[, i_RR]<- match_df[, i_RR[order(valt, decreasing = TRUE)]]
  
  # Remove duplicated
  match_df <- match_df[!duplicated(match_df),]
  
  # --- Attribution d'un indentifiant a chaque PIXEL SAPHIR! ---

  # creation d une colonne avec les variables permettant de differencier chaque pixel SAPHIR
  pix_SAPHIR <- paste(
    match_df$lat.SAPHIR,
    match_df$lon.SAPHIR,
    match_df$date.SAPHIR, 
    match_df$time.SAPHIR,
    sep="_"
  )

  # attribution d un indice a chaque pixel SAPHIR 
  match_df <- cbind(
    IND_pix_SAPHIR = as.numeric(factor(pix_SAPHIR, levels = unique(pix_SAPHIR))),
    match_df
  )
  rm(pix_SAPHIR)
  
  
  ################### Controle des donnees ###################
  i_RR <- grep("RR", colnames(match_df))
  i_RH <- grep("^rh[1-6]$", colnames(match_df))
  i_URH <- grep("^rh.unc[1-6]$", colnames(match_df))
  i_PHASE <- grep("PHASE", colnames(match_df))
  i_OPAQ <- grep("OPAQ", colnames(match_df))
  i_SR <- grep("SR", colnames(match_df))
  
  # selection et controle des donnees de SAPHIR
  print("set SAPHIR variables")
  
 # trouver les lignes de RH completes 
  complete_row_RH <- complete.cases(match_df[, i_RH]) 
  match_df <- match_df[complete_row_RH,]
  
  
  # selection et controle des donnees jour de CLOUDSAT
  print("set CLOUDSAT variables")

  # ne garder que les valeurs de jour pour les 3 SATELLITES (car pas de donnees CloudSat (RR) la nuit)
  # lignes CloudSat entierement en NA = donnees de nuit)
  i_NA <- (rowMeans(is.na(match_df[, i_RR])) == 1)
  match_df <- match_df[!i_NA,]
  
  
  # remplacement des NA de RR par -30
  # remplacement des valeurs  inférieures à -30 par -30
  RR <- as.matrix(match_df[, i_RR])
  i_minus30 <- (is.na(RR) | RR <= -30)
  RR[i_minus30] <- -30

  ## agregation jusqu a 24 km (inclus), le km 25 ne comporte qu une couche de 240m, il est donc exclu du calcul ci apres
  average_bylayer <- function(RR, valt, layers){
    RR_bylayer <- matrix(
      NA,
      nrow = nrow(RR),
      ncol = length(layers) - 1
    )
    for (ilayer in seq.int(length(layers) - 1)) {
      RR_l <- RR[, (valt >= layer_RR[ilayer]) & (valt <= layer_RR[ilayer + 1])] 
      RR_bylayer[, ilayer] <- rowMeans(RR_l)
    }
    return(RR_bylayer)
  }
  valt <- as.numeric(
    substring(colnames(RR), 3, nchar(colnames(RR)))
  )
  layer_RR <- c(1.2, 2:25)
  RR_bylayer <- average_bylayer(RR, valt, layer_RR)
  colnames(RR_bylayer) <- paste0("RR", head(layer_RR, -1))
  
  
  # selection et controle des donnees CALIPSO
  print("set CALIPSO variables")
  # Phases des nuages : voir table B1 Guzman et al., 2017, doi:10.1002/2016JD025946 
  # 0 = CLEAR
  # 1 = LIQ
  # 2 = ICE
  # 3 = UNDEFINED
  # 4 = FALSE_LIQ
  # 5 = FALSE_ICE
  # 6 = HORIZONTALLY ORIENTED
  # 7 = UNPHYSICAL VALUE
  # !!! regarder le netcdf regarder la variable float_instant_opaq!!!
  PHASE <- as.matrix(match_df[, i_PHASE])
  # Missing pixels
  PHASE[PHASE == "-9999"] <- NA
  # Surface pixels
  PHASE[PHASE == "-888"] <- NA
  
  # Reperer les Fully Attenuated -> PHASE = 8
  OPAQ <- as.matrix(match_df[, i_OPAQ])
  PHASE[PHASE == 0 & OPAQ %in% (7:10)] <- 8
  PROFILE_TYPE <- apply(PHASE, 1, classify_profile)

  # Transformation des SR en fonction des flags : OPAQ Mask for Each 480 m Layer (Instant_OPAQ Variable), voir Guzman, table B1 
  # la transformation ne change pas les valeurs SR correspondant aux nuages elle permet juste de gérer les valeurs qui ne correspondent pas aux nuages
  print("transformation des flag de SR")

  SR <- as.matrix(match_df[, i_SR])
  SR[OPAQ == 0] <- NA
  
  # FULLY ATTENUATED
  SR[OPAQ %in% 7:10] <- Inf
  
  SR[SR < 1] <- 0
  SR[SR > 1 & SR < 5] <- 0
  # valeurs de SR superieures a 80 (et inf a 1000 pour etre inferieur a l infini des FA)  
  SR[SR > 80 & SR < 1000] <- 0 
  
  
  # Reconstruction des SR avec la moyenne des plus proches voisins (si ce sont des valeurs de SR; si ce sont des FA alors restent en FA)
  # Moyenne de la 4eme valeur precedente à la premiere valeur precedente
  # Comme les FA sont codés "infini", s'il y a un FA dans les valeurs a moyenner, la moyenne sera Inf donc FA
  print("reconstruction des SR")
  i_NA = which(is.na(SR),  arr.ind = TRUE)
  i_start <- pmax.int(1, i_NA[, 2] - 4)
  i_end <- pmax.int(1, i_NA[, 2] - 1)
  for(irow in seq.int(nrow(i_NA))){
    SR[i_NA[irow, , drop = FALSE]] <- mean(
      SR[i_NA[irow, 1], seq(i_start[irow], i_end[irow])],
      na.rm = TRUE
    )
  }
  
  # ajout des colonnes flag phase
  SR <- cbind(
    "PROFILE_TYPE" = PROFILE_TYPE,
     SR,
     PHASE
  )

  complete_row_SR <- complete.cases(SR)
  SR <- SR[complete_row_SR, ]
  RR_bylayer <- RR_bylayer[complete_row_SR, ] 
  
  match_df <- match_df[complete_row_SR, ]
  
  
  ######### creation de la nouvelle table match_df nettoyee a utiliser pour le script de downscalling
  coords.time.CALIPSO <- as.POSIXct(
    paste(match_df[ , "date.CALIPSO"], match_df[ , "time.CALIPSO"]),
    tz = "UTC",
    format = "%Y-%m-%d %H:%M:%S"
  )
  coords.time.SAPHIR <- as.POSIXct(
    paste(match_df[ , "date.SAPHIR"], match_df[ , "time.SAPHIR"]),
    tz = "UTC",
    format = "%Y-%m-%d %H:%M:%S"
  )
  coords.time.CLOUDSAT <- as.POSIXct(
    paste(match_df[ , "date.CLOUDSAT"], match_df[ , "time.CLOUDSAT"]),
    tz = "UTC", 
    format = "%Y-%m-%d %H:%M:%S"
  )
 
  processed_df <- cbind(
    SR,
    RR_bylayer,
    match_df[, i_RH],
    match_df[, i_URH],
    time.CALIPSO = coords.time.CALIPSO,
    match_df[ , c("lon.CALIPSO","lat.CALIPSO")],
    time.SAPHIR = coords.time.SAPHIR,
    match_df[ , c("lon.SAPHIR","lat.SAPHIR")], 
    time.CLOUDSAT = coords.time.CLOUDSAT,
    match_df[ , c("lon.CLOUDSAT","lat.CLOUDSAT")],
    IND.SAPHIR = match_df[ , "IND.SAPHIR.match"],
    IND_pix_SAPHIR = match_df[ , "IND_pix_SAPHIR"]
  )
  
  write.table(
    processed_df,
    file = paste0(
      # bdd_data,
      ".",
      "/CALIPSO_SAPHIR_CLOUDSAT_",
      year, "-", month.num, "_",
      dt, "_", dx, "_", area,"_",
      "for_downscal.dat"
    ),
    col.names = TRUE, 
    row.names = FALSE,
    append = FALSE,
    sep = ";",
    quote = FALSE 
  )
  
}
# debug(prep_donnees_downscal)
prep_donnees_downscal("Indian_Ocean", "2013", "7")


## *********************************************************************************************************************
##      Launcher
##	    area = Indian_Ocean, North_Atlantic
##          year = 2013 and 2016 
##          month.num = 7,8,12
## *********************************************************************************************************************
# args = commandArgs(trailingOnly = TRUE)
# if (length(args) == 3)
# {
#   area = args[1]
#   year = args[2]
#   month.num=args[3]
#   prep_donnees_downscal(area,year,month.num)
# }

