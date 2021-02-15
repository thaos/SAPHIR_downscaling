packages <- c("reshape2", 
              "fields", 
              "maps", 
              "scales", 
              "grDevices", 
              "extrafont", 
              "ggplot2", 
              "reshape", 
              "RColorBrewer", 
              "gstat", 
              "sp", 
              "spacetime", 
              "raster", 
              "rgdal", 
              "session", 
              "gridExtra", 
              "geosphere", 
              "lattice", 
              "devtools", 
              "raster", 
              "data.table", 
              "maptools", 
              "tidyr", 
              "gtools",
              "plyr",
              "googleVis",
              "mgcv",
              "randomForest",
              "quantregForest",
              "cluster",
              "SpecsVerification",
              "data.table",
              "stringr",
              "fields",
              "factoextra",
              "cluster",
              "devtools",
              "randomForest",
              "quantregForest"
)


#download.file('https://github.com/bhaskarvk/colormap/colormap_0.1.4.tar.gz', 
#              f <- tempfile())
#unzip(f, exdir=tempdir())
#file.copy(file.path(tempdir(), '.RData'), 'colormap.RData')
#load('colormap.RData')


load.packages.custom <- function(x){
  for( i in x ){
    #  Require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      tryCatch({
        install.packages(i , dependencies = TRUE, repos='http://cran.us.r-project.org')
        require( i , character.only = TRUE )
      },
      error = function(e){
        e <<- e
        cat("ERROR: ", e$message, "\nin ")
        print(e$call)},
      warning = function(e){
        e <<- e
        cat("Warning: ", e$message, "\nin ")
        print(e$call)}
      )
    }
  }
}
#  Then try/install packages...
load.packages.custom(packages)

detachAllPackages <- function(){
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}



resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
}

string2number <- function(x){
  x <- as.factor(x)
  levels(x) <- 1:length(levels(x))
  x <- as.numeric(x)
}

degtokm <- function(x,y){
  lon0 <- mean(range(x))
  lat0 <- mean(range(y))
  
  rx <- 6371 * acos(sin(lat0 *pi/180)^2 + cos(lat0*pi/180)^2 * cos((lon0+.5)*pi/180 - (lon0-.5)*pi/180))
  ry <- 6371 * acos(sin((lat0 -.5)*pi/180) *  sin((lat0+.5)*pi/180) + cos((lat0-.5)*pi/180) * cos((lat0+.5)*pi/180))
  
  x.km <-(x-lon0)*rx
  y.km <-(y-lat0)*ry
  
  df <- data.frame(cbind(lon.km=x.km,lat.km=y.km))
  return(df)
}

round.POSIXct <- function(x, units = c("secs","mins", "5 mins", "10 mins", "15 mins", "quarter hours", "30 mins", "half hours", "hours")){ 
  if(is.numeric(units)) units <- as.character(units) 
  units <- match.arg(units) 
  r <- switch(units, 
              "secs" = 1, 
              "mins" = 60, 
              "5 mins" = 60*5, 
              "10 mins" = 60*10, 
              "15 mins"=, "quarter hours" = 60*15, 
              "30 mins"=, "half hours" = 60*30, 
              "hours" = 60*60 
  ) 
  H <- as.integer(format(x, "%H")) 
  M <- as.integer(format(x, "%M")) 
  S <- as.integer(format(x, "%S")) 
  D <- format(x, "%Y-%m-%d") 
  secs <- 3600*H + 60*M + S 
  as.POSIXct(round(secs/r)*r, origin=D) 
} 

hrs <- function(u) {
  x <- u * 3600
  return(x)
}

hourtodecimal <- function(date) {
  time <- strftime(round.POSIXct(time, units="secs"), format="%H:%M:%S",tz="UTC")
  time <- as.numeric(unlist(strsplit(time, ":")))
  time.dec  <-  time[1]*60+time[2]+time[3]/60
  return(time.dec)
}

hourtofrac <- function(date){
  time <- strftime(round.POSIXct(date, units="30 min"), format="%H:%M",tz="UTC")
  time.frac <- sapply(strsplit(time,":"),function(x){
    x <- as.numeric(x)
    x[1]+x[2]/60
  }
  )
  time.frac <- ifelse(time.frac==0.0,23.5,time.frac)
  return(time.frac)
}

UTCtolocal <- function(date,UTCtime,lon){
  date <- as.POSIXct(paste(date,UTCtime),format="%Y-%m-%d %H:%M:%S",tz="UTC")
  offset <- lon/360*24
  mlstdate <- as.POSIXct(format(date + hrs(offset)),tz="local")
  return(mlstdate)
}

distancekm  <- function(latmin,lonmin,latmax,lonmax){
  R <- 6371 #R is the Earth radius (6371 km)
  phi1 = (latmin * pi) / 180
  phi2 = (latmax * pi) / 180
  Dphi = (latmax-latmin) * pi / 180
  Dlambda = (lonmax-lonmin) * pi / 180
  a = sin(Dphi/2) * sin(Dphi/2) +
    cos(phi1) * cos(phi2) *
    sin(Dlambda/2) * sin(Dlambda/2)
  c = 2 * atan2(sqrt(a), sqrt(1-a))
  d = R * c
  
  return(d)
}

make_circles <- function(centers, radius, nPoints = 100){
  # centers: the data frame of centers with ID
  # radius: radius measured in kilometer
  #
  meanLat <- mean(centers$lat)
  # length per lon changes with latitude, so need correction
  radiusLon <- radius /111 / cos(meanLat/57.3) 
  radiusLat <- radius / 111
  circleDF <- data.frame(ID = rep(centers$ID, each = nPoints))
  angle <- seq(0,2*pi,length.out = nPoints)
  
  circleDF$lon <- unlist(lapply(centers$lon, function(x) x + radiusLon * cos(angle)))
  # 	circleDF$lat <- unlist(lapply(centers$lat, function(x) x + Lat * sin(angle)))
  return(circleDF)
}

draw_circle <- function(lon,lat, radius, col,nPoints = 100,new=TRUE,xlab=xlab,ylab=ylab,title=title){
  
  meanLat <- mean(lat)
  # length per lon changes with latitude, so need correction
  radiusLon <- radius /111 / cos(meanLat/57.3) 
  radiusLat <- radius / 111
  angle <- seq(0,2*pi,length.out = nPoints)
  
  vlon <- unlist(lapply(lon, function(x) x + radiusLon * cos(angle)))
  vlat <- unlist(lapply(lat, function(x) x + radiusLat * sin(angle)))
  
  if(new) plot(vlon,vlat,type="l",xlab=xlab,ylab=ylab,main=title)
  polygon(vlon,vlat,border=col,col=col)
}

floor_time <- function(x, k = 1, unit = c("second", "minute", "hour", "day", 
                                          "week", "month", "year")) {
  require(lubridate)
  
  nmax <- NULL
  
  switch(unit, second = {nmax <- 60},
         minute = {nmax <- 60},
         hour = {nmax <- 24})
  
  cuts <- seq(from = 0, to = nmax - 1, by = k)
  new <- switch(unit, 
                second = update(x, seconds = cuts[findInterval(second(x), cuts)]), 
                minute = update(x, minutes = cuts[findInterval(minute(x), cuts)], 
                                seconds = 0), 
                hour = update(x, hours = cuts[findInterval(hour(x), cuts)], 
                              minutes = 0, seconds = 0), 
                day = update(x, hours = 0, minutes = 0, seconds = 0), 
                week = update(x, wdays = 1, hours = 0, minutes = 0, seconds = 0), 
                month = update(x, mdays = 1, hours = 0, minutes = 0, seconds = 0), 
                year = update(x, ydays = 1, hours = 0, minutes = 0, seconds = 0))
  
  new
}

dist.sp.temp <- function(df,alphaC){
  dist.sp.norm.min <- min(df$dist.sp,na.rm=TRUE)
  dist.sp.norm.max <- max(df$dist.sp,na.rm=TRUE)
  dist.sp.norm.max.min <- max(df$dist.sp,na.rm=TRUE) - min(df$dist.sp,na.rm=TRUE)
  
  df$dist.temp <- abs(as.numeric(df$dist.temp))
  dist.temp.norm.min <- min(df$dist.temp,na.rm=TRUE)
  dist.temp.norm.max <- max(df$dist.temp,na.rm=TRUE)
  dist.temp.norm.max.min <- max(df$dist.temp,na.rm=TRUE) - min(df$dist.temp)
  
  #******************************************************************** SPATIAL ONLY *****************************************************************
  
  if(!is.null(alphaC)){
    
    if(alphaC==0){
      df$dist.sp.temp.norm <- df$dist.sp
      # Choose the 'closest' match
      df <- df[which(df$dist.sp.temp.norm==min(df$dist.sp.temp.norm,na.rm=TRUE)),]
    }
    
    #****************************************************** SPATIO - TEMPORAL (Min - max normalization) *************************************************
    
    if(alphaC>0){
      
      # SPATIAL ONLY
      if(dist.sp.norm.min != dist.sp.norm.max & dist.temp.norm.min == dist.temp.norm.max) df$dist.sp.temp.norm <- df$dist.sp
      
      # TEMPORAL ONLY
      if(dist.sp.norm.min == dist.sp.norm.max & dist.temp.norm.min != dist.temp.norm.max) df$dist.sp.temp.norm <- df$dist.temp
      
      # SPATIO-TEMPORAL
      if(dist.sp.norm.min != dist.sp.norm.max & dist.temp.norm.min != dist.temp.norm.max){
        #Min - max normalization (take into account different ranges) + alphaC
        df$dist.sp.temp.norm <- sqrt((1-alphaC) * ((df$dist.sp-dist.sp.norm.min)/(dist.sp.norm.max.min))^2 + alphaC * ((df$dist.temp-dist.temp.norm.min)/(dist.temp.norm.max.min))^2)
      }
      # Choose the 'closest' match
      df <- df[which(df$dist.sp.temp.norm==min(df$dist.sp.temp.norm,na.rm=TRUE)),]
    }
  }
  
  #******************************************************************** SPATIO - TEMPORAL (Speed) *****************************************************
  
  if(is.null(alphaC)){
    
    df$dist.sp.temp.norm <- ifelse(df$dist.sp == 0 & df$dist.temp == 0, -Inf, NA)
    df$dist.sp.temp.norm <- ifelse(df$dist.sp != 0 & df$dist.temp != 0 | df$dist.sp == 0 & df$dist.temp != 0 | df$dist.sp != 0 & df$dist.temp == 0, df$dist.sp/df$dist.temp,df$dist.sp.temp.norm)
    
    # Choose the 'closest' match
    df <- df[which(df$dist.sp.temp.norm==min(df$dist.sp.temp.norm,na.rm=TRUE,na.rm=TRUE)),]
  }
  return(df)
}


find_support.SAPHIR <- function(df,radius,alphaC){
  if(nrow(df)==1){
    df$dist.temp <- abs(as.numeric(df$dist.temp))
    IND.SAPHIR.match <- df[,"IND.SAPHIR"]
    df$dist.sp.temp.norm <- NA
  }
  
  
  if(nrow(df)>1){
    
    dist.sp.norm.min <- min(df$dist.sp,na.rm=TRUE)
    dist.sp.norm.max <- max(df$dist.sp,na.rm=TRUE)
    dist.sp.norm.max.min <- max(df$dist.sp,na.rm=TRUE) - min(df$dist.sp,na.rm=TRUE)
    
    df$dist.temp <- abs(as.numeric(df$dist.temp))
    dist.temp.norm.min <- min(df$dist.temp,na.rm=TRUE)
    dist.temp.norm.max <- max(df$dist.temp,na.rm=TRUE)
    dist.temp.norm.max.min <- max(df$dist.temp,na.rm=TRUE) - min(df$dist.temp,na.rm=TRUE)
    
    #******************************************************************** SPATIAL ONLY *****************************************************************
    
    if(!is.null(alphaC)){
      
      if(alphaC==0){
        df$dist.sp.temp.norm <- df$dist.sp
        # Choose the 'closest' match
        IND.SAPHIR.match <- df[which(df$dist.sp.temp.norm==min(df$dist.sp.temp.norm,na.rm=TRUE)),"IND.SAPHIR"]
      }
      
      #****************************************************** SPATIO - TEMPORAL (Min - max normalization) *************************************************
      
      if(alphaC>0){
        # SPATIAL ONLY
        if(dist.sp.norm.min != dist.sp.norm.max & dist.temp.norm.min == dist.temp.norm.max) df$dist.sp.temp.norm <- df$dist.sp
        
        # TEMPORAL ONLY
        if(dist.sp.norm.min == dist.sp.norm.max & dist.temp.norm.min != dist.temp.norm.max) df$dist.sp.temp.norm <- df$dist.temp
        
        # SPATIO-TEMPORAL
        if(dist.sp.norm.min != dist.sp.norm.max & dist.temp.norm.min != dist.temp.norm.max){
          #Min - max normalization (take into account different ranges) + alphaC
          df$dist.sp.temp.norm <- sqrt((1-alphaC) * ((df$dist.sp-dist.sp.norm.min)/(dist.sp.norm.max.min))^2 + alphaC * ((df$dist.temp-dist.temp.norm.min)/(dist.temp.norm.max.min))^2)
        }
        # Choose the 'closest' match
        IND.SAPHIR.match <- df[which(df$dist.sp.temp.norm==min(df$dist.sp.temp.norm,na.rm=TRUE)),"IND.SAPHIR"]
      }
    }
    
    #******************************************************************** SPATIO - TEMPORAL (Speed) *****************************************************
    
    if(is.null(alphaC)){
      
      df$dist.sp.temp.norm <- ifelse(df$dist.sp == 0 & df$dist.temp == 0, -Inf, NA)
      df$dist.sp.temp.norm <- ifelse(df$dist.sp != 0 & df$dist.temp != 0 | df$dist.sp == 0 & df$dist.temp != 0 | df$dist.sp != 0 & df$dist.temp == 0, df$dist.sp/df$dist.temp,df$dist.sp.temp.norm)
      # Choose the 'closest' match
      IND.SAPHIR.match <- df[which(df$dist.sp.temp.norm==min(df$dist.sp.temp.norm,na.rm=TRUE)),"IND.SAPHIR"]
    }
    
  }
  
  df$IND.SAPHIR.match <- ifelse(df$IND.SAPHIR==IND.SAPHIR.match, IND.SAPHIR.match, NA)
  df$IND.SAPHIR.match <- ifelse(df$dist.sp <= radius, df$IND.SAPHIR.match, NA)
  
  return(df)
}

merge.with.order <- function(x,y, ..., sort = T, keep_order){
  # this function works just like merge, only that it adds the option to return the merged data.frame ordered by x (1) or by y (2)
  add.id.column.to.data <- function(DATA)
  {
    data.frame(DATA, id... = seq_len(nrow(DATA)))
  }
  # add.id.column.to.data(data.frame(x = rnorm(5), x2 = rnorm(5)))
  order.by.id...and.remove.it <- function(DATA)
  {
    # gets in a data.frame with the "id..." column.  Orders by it and returns it
    if(!any(colnames(DATA)=="id...")) stop("The function order.by.id...and.remove.it only works with data.frame objects which includes the 'id...' order column") 
    ss_r <- order(DATA$id...)
    ss_c <- colnames(DATA) != "id..."
    DATA[ss_r, ss_c]
  }
  
  # tmp <- function(x) x==1; 1	# why we must check what to do if it is missing or not...
  # tmp()
  
  if(!missing(keep_order))
  {
    if(keep_order == 1) return(order.by.id...and.remove.it(merge(x=add.id.column.to.data(x),y=y,..., sort = FALSE)))
    if(keep_order == 2) return(order.by.id...and.remove.it(merge(x=x,y=add.id.column.to.data(y),..., sort = FALSE)))
    # if you didn't get "return" by now - issue a warning.
    warning("The function merge.with.order only accepts NULL/1/2 values for the keep_order variable")
  } else {return(merge(x=x,y=y,..., sort = sort))}
}

line.perp.point <- function(line,point){
  slope <- -1/(line$coefficients[2])
  intercept <- point[2] - slope * point[1]
  df <- array(c(slope=slope,intercept=intercept))
  return(df)
}

distfromline <- function(line.lon,line.lat,pnts.lon,pnts.lat,units='km',projections=FALSE){
  line <- cbind(line.lon,line.lat)
  pnts <- cbind(pnts.lon,pnts.lat)
  d = dist2Line(pnts, line,distfun=distHaversine)
  colnames(d) <- c("distance","lon.proj","lat.proj")
  if(units =='km') d[,"distance"] = d[,"distance"]/1000
  if(!projections) d = d[,"distance"]	
  return(d)
}

average.bins <- function(df,bin.scale,bin.num){
  bin.med <- findInterval(bin.scale,seq(0,max(bin.scale),by=bin.num))  ##  trouve les couches dont l alt est est inf a alt.boundary (2km)
  colnames(df) <- paste0(bin.med)
  df <- sapply(split.default(df, names(df)), function(x) rowMeans(x,na.rm = TRUE))
  df <- df[,order(as.numeric(colnames(df)))]
  return(df)
}

average.bins.matrix <- function(M,M.colnames,scale.low,scale.up,scale,scale.boundary,resbin.low,resbin.up){
  M.orig <- M
  M[,paste0(M.colnames,scale.low)] <- average.bins(M[,grep(M.colnames,colnames(M))][scale<scale.boundary],alt[scale<scale.boundary],resbin.low)
  M[,paste0(M.colnames,scale.up)] <- average.bins(M[,grep(M.colnames,colnames(M))][scale>=scale.boundary],alt[scale>=scale.boundary],resbin.up)
  M <- M[,(ncol(M.orig)+1):ncol(M)]
  return(M)
}

sd.bins <- function(df,bin.scale,bin.num){
  bin.med <- findInterval(bin.scale,seq(0,max(bin.scale),by=bin.num))
  colnames(df) <- paste0(bin.med)
  df <- sapply(split.default(df, names(df)), function(x) apply(x,1, sd, na.rm = TRUE))
  df <- df[,order(as.numeric(colnames(df)))]
  return(df)
}

sd.bins.matrix <- function(M,M.colnames,scale.low,scale.up,scale,scale.boundary,resbin.low,resbin.up){
  M.orig <- M
  M[,paste0(M.colnames,scale.low)] <- sd.bins(M[,grep(M.colnames,colnames(M))][scale<scale.boundary],alt[scale<scale.boundary],resbin.low)
  M[,paste0(M.colnames,scale.up)] <- sd.bins(M[,grep(M.colnames,colnames(M))][scale>=scale.boundary],alt[scale>=scale.boundary],resbin.up)
  M <- M[,(ncol(M.orig)+1):ncol(M)]
  return(M)
}

SR.pca <- function(M){
  M.scale <- scale(M)
  
  ## Calculating PCs
  pca.dat = prcomp(~.,data.frame(M.scale),na.action=na.omit,scale=FALSE,center=FALSE)
  
  ## Compute summaries
  modes <- pca.dat$rotation
  weights <- pca.dat$x
  perc.var <- summary(pca.dat)$importance[2,]  ### contribution de chaque composante
  
  ## Set number of PCs retained (all: perc.use=max(cumsum(perc.var))
  perc.use <- 0.9
  pc.use <- which(cumsum(perc.var)>=perc.use)[1]
  
  ## Reconstruct the original data retaining only the pricipal PCs (non-missing values only)
  M.rec <- weights[,1:pc.use] %*% t(modes[,1:pc.use])
  
  ## Project into PC space and Reconstruct the original data retaining only the pricipal PCs (both missing and non-missing if these are present)
  if(omitpar==FALSE){
    M.proj <- M.scale 
    colnames(M.proj) <-  rownames(modes)
    M.proj[is.na(M.proj)] <- 0
    M.proj[M.proj =="NaN"] <- 0
    M.proj <- M.proj %*% modes
    
    M.rec <- M.proj[,1:pc.use] %*% t(modes[,1:pc.use])
    M.rec[is.na(M)] <- NA
  }
  
  ## Rescale
  M.rec <- t(t(M.rec %*% diag(attr(M.scale, 'scaled:scale'))) + attr(M.scale, 'scaled:center'))
  #	M.rec_test_2<-t(t(M.rec_test %*% diag(attr(test, 'scaled:scale'))) + attr(test, 'scaled:center'))
  ## Reset areosols to original value
  M.rec[M<SR.threshold] <- M[M<SR.threshold]
  
  ## Trim Reconstructed data for unphysical (negative) values
  M.rec[M.rec<0] <- 0
  
  return(M.rec)
}

cumul_zeros <- function(x)  {
  x <- !x
  rl <- rle(x)
  len <- rl$lengths
  v <- rl$values
  cumLen <- cumsum(len)
  z <- x
  # replace the 0 at the end of each zero-block in z by the 
  # negative of the length of the preceding 1-block....
  iDrops <- c(0, diff(v)) < 0
  z[ cumLen[ iDrops ] ] <- -len[ c(iDrops[-1],FALSE) ]
  # ... to ensure that the cumsum below does the right thing.
  # We zap the cumsum with x so only the cumsums for the 1-blocks survive:
  x*cumsum(z)
}

find_cons <- function(x,ncons.min){
  runs <- rle(x>0)
  runs.cons <- which(runs$values == FALSE & runs$lengths >= ncons.min)
  runs.lengths.cumsum = cumsum(runs$lengths)
  ends <- as.numeric(runs.lengths.cumsum[runs.cons])
  starts <- as.numeric(ends-runs$lengths[runs.cons]+1)
  
  end_start <- as.data.frame(cbind(starts,ends))
  end_start$width <- end_start$ends-end_start$starts+1
  return(end_start)
}

assign_cloudtype <- function(df,listtype){
  df$cloudtype.alt <- NA
  df$cloudtype.width <- NA
  indlist <- which(names(listtype)==unique(df$rownum))
  if(length(indlist)>0){
    dftype <- listtype[[indlist]]
    for(l in 1:nrow(dftype)){
      indrows <- seq(dftype$starts[l],dftype$ends[l])
      
      couldtype.alt.unique <- unique(cloudtype.alt[indrows])
      df$cloudtype.alt[indrows] <- ifelse(length(couldtype.alt.unique)==1, couldtype.alt.unique, paste(couldtype.alt.unique,collapse="-"))
      
      df$cloudtype.width[indrows] <- dftype$width[l]
    }
  }	
  return(df)
}

cloudmask.class <- function(H,P,ncons.min){
  
  # ******************************************************** Assign cloud type: Height ********************************************************* 
  
  # Find number of consecutive bins (H=Height)
  H.cons <- H
  H.cons[H.cons<SR.threshold] <- NA
  H.cons <- as.data.frame(t(apply(H.cons,1,function(x) na.approx(x,maxgap=2,na.rm=FALSE))))
  H.cons[is.na(H.cons)] <- 0
  H.cumul <- apply(H.cons,1,function(x) list(cumul_zeros(x)))
  H.cumul <- lapply(H.cumul, unlist)
  H.cumul.cons <- lapply(H.cumul,function(x) find_cons(x,ncons.min))
  names(H.cumul.cons) <- seq(1:nrow(H))
  H.cumul.cons <- H.cumul.cons[sapply(H.cumul.cons, function(i) nrow(i) > 0)]
  
  # Assign cloud type: Height and Depth
  H <- as.data.frame(H)
  colnames(H) <- paste0("SR",colnames(H))
  H$rownum <- seq(1,nrow(H))
  
  H <- gather(data = H, key = "SR.var", value = "SR.val", grep("SR",colnames(H)))
  H <- H[order(H$rownum),]
  H.rownum.list <- split(H,list(H$rownum),drop=TRUE)
  H.rownum.list <- lapply(H.rownum.list, function(x) assign_cloudtype(x,H.cumul.cons))
  
  H.rownum.list <- lapply(H.rownum.list, function(x) cbind(x, cloudtype.widthr=ifelse(x$cloudtype.width*resprofile<=1, "LESS THAN 1 km", ifelse(x$cloudtype.width*resprofile<=2," 1 - 2 km", ifelse(x$cloudtype.width*resprofile>2 & x$cloudtype.width*resprofile<=4, " 2 - 4 km", "MORE THAN 4 km")))))
  H.rownum.list <- lapply(H.rownum.list, function(x) cbind(x, cloudtype.alt.widthr=factor(paste0(x$cloudtype.alt," LEVEL CLOUD", "; ", x$cloudtype.widthr))))
  
  H.rownum.list <- lapply(H.rownum.list, 
                          function(x){
                            x <- x[!is.na(x$cloudtype.alt),]
                            if(nrow(x)==0) x.out <- NA
                            if(nrow(x)>0){
                              cloudtype.alt.string <- unique(unlist(strsplit(x$cloudtype.alt,"-")))
                              x.out <- paste(cloudtype.alt.string,collapse="-")
                            }
                            return(x.out)
                          }
  )
  
  cluster.height.dat <- do.call(rbind,H.rownum.list)
  cluster.height.dat <- ifelse(is.na(cluster.height.dat),"CLEAR",cluster.height.dat)
  
  # ********************************************************* Assign cloud type: Phase ********************************************************* 
  P <- as.data.frame(P)
  P$rownum <- seq(1,nrow(P))
  
  P <- gather(data = P, key = "PHASE.var", value = "PHASE.val", grep("PHASE",colnames(P)))
  P <- P[order(P$rownum),]
  P.rownum.list <- split(P,list(P$rownum),drop=TRUE)
  
  P.rownum.list <- lapply(P.rownum.list, 
                          function(x){
                            x <- x[!is.na(x$PHASE.val),]
                            if(nrow(x)==0) x.out <- NA
                            if(nrow(x)>0){
                              PHASE.val.string <- unique(unlist(strsplit(x$PHASE.val,"-")))
                              PHASE.val.string <- sort(PHASE.val.string) # Only if the vertical order does not matter
                              x.out <- paste(PHASE.val.string,collapse="-")
                            }
                            return(x.out)
                          }
  )
  
  cluster.phase.dat <- do.call(rbind,P.rownum.list)
  
  # ********************************************************************************************************************************************
  
  cluster.dat <- cbind(cluster.height.dat,cluster.phase.dat)
  colnames(cluster.dat) <- c("HEIGHT","PHASE")
  return(cluster.dat)
  
}

CVgam.custom <- function (formula, family, data, nfold = nrow(data),method = "GCV.Cp", cvparts = NULL, gamma = 1, seed = 29){
  if (is.null(cvparts)) {
    set.seed(seed)
    if(nfold<nrow(data)) cvparts <- sample(1:nfold, nrow(data), replace = TRUE)		# This is n-fold validation (or leave-p-out cross validation)
    if(nfold==nrow(data)) cvparts <- sample(1:nfold, nrow(data), replace = FALSE)	# This is leave-one-out cross validation
    if(nfold>nrow(data)){
      print("More folds than data!")
      break
    }
  }
  folds <- unique(cvparts)
  hat <- numeric(nrow(data))
  gm <- gam(formula, family, data = data, method = method, gamma = gamma)
  scale.gam <- summary(gm)$scale
  for (i in folds) {
    trainrows <- cvparts != i
    testrows <- cvparts == i
    
    data.train <- data[trainrows,]
    tryCatch(cv.gm <- gam(formula, family, data = data.train, method = method, gamma = gamma, select=TRUE), error=function(e){})
    
    data.test <- data[testrows,]
    if(!is.null(cv.gm)){	
      hat[testrows] <- predict(cv.gm, newdata = data.test, type = "response")
      res <- residuals(cv.gm)
    }
    if(is.null(cv.gm)){
      hat[testrows] <- predict(gm, newdata = data.test, type = "response")
      res <- residuals(gm)
    }
  }
  y <- eval(formula[[2]], envir = as.data.frame(data))
  res <- y - hat
  invisible(list(fitted = hat, resid = res))
}

CVgam.mrf.custom <- function (formula, family, data, nfold = nrow(data), method = "GCV.Cp", cvparts = NULL, gamma = 1, seed = 29){
  if (is.null(cvparts)) {
    set.seed(seed)
    if(nfold<nrow(data)) cvparts <- sample(1:nfold, nrow(data), replace = TRUE)		# This is n-fold validation (or leave-p-out cross validation)
    if(nfold==nrow(data)) cvparts <- sample(1:nfold, nrow(data), replace = FALSE)	# This is leave-one-out cross validation
    if(nfold>nrow(data)){
      print("More folds than data!")
      break
    }
  }
  folds <- unique(cvparts)
  hat <- numeric(nrow(data))
  scale.gam <- summary(gam(formula, family, data = data, method = method, gamma = gamma))$scale
  for (i in folds) {
    tmp <- data
    tmp$wt.train <- ifelse(cvparts != i, 1, 0)
    cv.gm <- gam(formula, family, data = tmp, method = method, gamma = gamma, weights = wt.train, select=TRUE)
    testrows <- cvparts == i
    tmp$wt.test <- ifelse(cvparts == i, 1, 0)
    hat[testrows] <- predict(cv.gm, newdata = tmp, weights = wt.test, type = "response")[testrows]
    
    res <- residuals(cv.gm)
  }
  y <- eval(formula[[2]], envir = as.data.frame(data))
  res <- y - hat
  invisible(list(fitted = hat, resid = res))
}

CVrf.custom <- function (x,y, nfold = length(y), cvparts = NULL, seed = 29){
  if (is.null(cvparts)) {
    set.seed(seed)
    if(nfold<length(y)) cvparts <- sample(1:nfold, length(y), replace = TRUE)		# This is n-fold validation (or leave-p-out cross validation)
    if(nfold==length(y)) cvparts <- sample(1:nfold, length(y), replace = FALSE)		# This is leave-one-out cross validation
    if(nfold>length(y)){
      print("More folds than data!")
      break
    }
  }
  folds <- unique(cvparts)
  khat <- hat <- numeric(length(y))
  for (i in folds) {
    trainrows <- cvparts != i
    testrows <- cvparts == i
    
    x.train <- x[trainrows,]
    y.train <- y[trainrows]
    elev.rf <- randomForest(x.train,y.train, importance=TRUE)	
    
    x.test <- x[testrows,]
    y.test <- y[testrows]
    hat[testrows] <- predict(elev.rf, newdata = data.frame(cbind(x.test,y.test)), type = "response")
  }
  res <- y - hat
  cvscale <- sum(res^2)/length(res)
  invisible(list(fitted = hat, resid = res))
}

CVqrf.custom <- function (x,y, nfold = length(y), cvparts = NULL, seed = 29, quant=seq(5,95)/100){
  if (is.null(cvparts)) {
    set.seed(seed)
    if(nfold<length(y)) cvparts <- sample(1:nfold, length(y), replace = TRUE)		# This is n-fold validation (or leave-p-out cross validation)
    if(nfold==length(y)) cvparts <- sample(1:nfold, length(y), replace = FALSE)		# This is leave-one-out cross validation
    if(nfold>length(y)){
      print("More folds than data!")
      break
    }
  }
  folds <- unique(cvparts)
  khat <- hat <- array(dim=c(length(y), length(quant)))
  for (i in folds) {
    trainrows <- cvparts != i
    testrows <- cvparts == i
    
    x.train <- x[trainrows,]
    y.train <- y[trainrows]
    elev.qrf <- quantregForest(x.train,y.train, keep.inbag=TRUE)	
    
    x.test <- x[testrows,]
    y.test <- y[testrows]
    hat[testrows,] <- predict(elev.qrf, newdata = data.frame(cbind(x.test,y.test)), what=quant)
  }
  res <- y - hat
  cvscale <- sum(res^2)/length(res)
  invisible(list(fitted = hat, resid = res))
}

rmse <- function(res){
  res <- sqrt(sum(res^2)/length(res))
  return(res)
}

mse <- function(res){
  res <- sum(res^2)/length(res)
  return(res)
}

match.list.ind <- function(a,b){
  g <- rep(seq_along(a), sapply(a, length))
  aa <- unlist(a)
  au <- unique(aa)
  af <- factor(aa, levels=au)
  gg <- split(g, af)
  gg[match(b, au)]
  
  return(gg)
}

## Neighbourhood structure info for MRF
mrf.nb <- function(data){
  nb.ind <- lapply(split(data,data$ind, drop=TRUE), "[", c("ind"))
  nb.ind <- Map(cbind, nb.ind, id = lapply(nb.ind, rownames))
  nb.ind <- lapply(nb.ind,function(x) as.vector(x$id))
  nb <- as.list(data$id)
  nb <- match.list.ind(nb.ind,nb)
  nb <- lapply(nb,function(x) nb.ind[[x[[length(x)]]]])
  nb <- nb[order(as.integer(names(nb)))]
  nb <- lapply(nb,function(x) as.integer(x))
  return(nb)
}

## Penalty matrix info for MRF
mrf.penalty.matrix <- function(data,nb){
  x <- as.factor(data$id)	
  k <- as.factor(levels(x))
  a.name <- names(nb)
  np <- length(x)
  S <- matrix(0,np,np)
  rownames(S) <- colnames(S) <- factor(k)
  for (i in 1:np) {
    ind <- nb[[i]]
    lind <- length(ind)
    S[a.name[i],a.name[i]] <- lind
    if (lind>0) for (j in 1:lind){
      if(S[a.name[i],a.name[ind[j]]]==0) S[a.name[i],a.name[ind[j]]] <- -1
    }
  }
  return(S)	
  
}

cluster.geo <- function(lon, lat, d){
  
  # convert data to a SpatialPointsDataFrame object
  xy <- SpatialPointsDataFrame(
    matrix(c(lon,lat), ncol=2), data.frame(ID=seq(1:length(lon))),
    proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  
  # use the distm function to generate a geodesic distance matrix in meters
  mdist <- distm(xy)
  
  # cluster all points using a hierarchical clustering approach
  hc <- hclust(as.dist(mdist), method="complete")
  
  # define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
  xy$clust <- cutree(hc, h=d)
  
  return(xy)
  
}

find_UTM_zone <- function(lon) {
  ## Function to get the UTM zone for a given longitude
  (floor((lon + 180)/6) %% 60) + 1
}

find_UTM_hemisphere <- function(lat) {
  ifelse(lat > 0, "north", "south")
}

UTMToLongLat <- function(x_utm,y_utm, zone_utm, hemisphere_utm, units_utm){
  
  ## Find most common zone and hemisphere
  all_zone <- count(zone_utm)
  all_hemisphere <- count(hemisphere_utm)
  zone_utm <- all_zone[which(all_zone$freq == max(all_zone$freq)),1]
  hemisphere_utm <- as.character(all_hemisphere[which(all_hemisphere$freq == max(all_hemisphere$freq)),1])
  
  utmcoor<-SpatialPoints(cbind(x_utm,y_utm), proj4string=CRS(paste0("+proj=utm +zone=", zone_utm," +ellps=WGS84"," +", hemisphere_utm," +units=", units_utm)[1L]))
  res <- spTransform(utmcoor,CRS("+proj=longlat"))
  res <- as.data.frame(res)
  colnames(res) <- c("lon","lat")
  return(res)
}

LongLatToUTM <- function(lon, lat, units = 'm') {
  df <- data.frame(
    ID = seq_along(lon), 
    x = lon, 
    y = lat
  )
  sp::coordinates(df) <- c("x", "y")
  
  ## Get zone and hemisphere for all the points in the data frame
  df$hemisphere <- find_UTM_hemisphere(lat)
  df$zone <- find_UTM_zone(lon)
  
  ## Convert to UTM
  sp::proj4string(df) <- sp::CRS("+init=epsg:4326") 
  df.list <- split(df,list(df$zone,df$hemisphere),drop=TRUE)
  res <- lapply(df.list,function(x) spTransform(x, CRS(paste0("+proj=utm +zone=", unique(x$zone)," +ellps=WGS84"," +", unique(x$hemisphere)," +units=", units)[1L])))
  
  ## Convert to data frame
  res <- lapply(res, function(x) as.data.frame(x))
  res <- do.call(rbind, res)
  df <- as.data.frame(df)
  colnames(df) <- c("ID","lon","lat","hemisphere","zone")
  res <- merge(df, res, by=c("ID","hemisphere","zone"))
  res <- res[order(res$ID),]
  
  return(res)
}

SR2ranges <- function(SR, SR.int, crit){
  for(i in 1:(length(SR.int)-1)){  ### pour chaque seuil de SR (SR.int)
    if(crit=="lower") SR[SR  >= SR.int[i] & SR < SR.int[i+1]] <- SR.int[i]  ## si le critère est lower, attribuer aux valeurs de SR compris entre SR.int[i] et SR[i+1] la valeur de SR[i] 
  }
  i = length(SR.int)    # c a d un SR a 80
  SR[SR  > SR.int[i]] <- SR.int[i]   ## pour toutes les valeurs dont le SR est superieur a 80, attribuer 80...?????
  SR[SR > SR.unc.threshold & SR < SR.threshold] <- NA		# Uncertain pixels	; SR.unc.threshold est a 1.2, donc toutes les valeurs de SR 
  # comprises entre les 1.2 et 5 (SR.threshold) sont transformees en NA
  return(SR)
}

rps <- function(predictions, observed){
  
  # predictions: matrix with the predictions. It should be laid out so that each row is one prediction, laid out in the proper order, where each element is a probability and each row sum to 1
  # observed: numeric vector that indicates the outcome that was actually observed.
  
  ncat <- ncol(predictions)
  npred <- nrow(predictions)
  
  rps <- numeric(npred)
  
  for (rr in 1:npred){
    obsvec <- rep(0, ncat)
    obsvec[observed[rr]] <- 1
    cumulative <- 0
    for (i in 1:ncat){
      cumulative <- cumulative + (sum(predictions[rr,1:i]) - sum(obsvec[1:i]))^2
    }
    rps[rr] <- (1/(ncat-1))*cumulative
  }
  return(rps)
}

MAE <- function(y_pred, y_true) {
  MAE <- mean(abs(y_true - y_pred))
  return(MAE)
}

crps.Gneiting <- function(obs, pred, ...){
  
  ## Tilmann Gneiting's crps code, assumes pred is either a vector of length
  ## 2 (mu, sig) or a matrix of mu and sig if each forcast is different
  
  if(is.null( dim(pred)) & length(pred)==2){
    mu <- pred[1];
    sigma <- pred[2]
  }else{
    mu <- as.numeric( pred[,1] )
    sigma <- as.numeric( pred[,2]) 
  }
  
  z <- (obs-mu)/sigma ## center and scale
  
  crps <- sigma * (z*(2*pnorm(z,0,1)-1) + 2*dnorm(z,0,1) - 1/sqrt(pi))
  ign <-  0.5*log(2*pi*sigma^2) + (obs - mu)^2/(2*sigma^2)
  pit <- pnorm(obs, mu,sigma )
  
  return(list(crps = crps, CRPS = mean(crps), ign = ign, IGN = mean(ign), pit = pit) )
  
}

pseudoR2 <- function(res,y){
  tmp <- 1-sum(res^2)/sum((y-mean(y))^2)
  return(tmp)
}

crps.Taillardat <- function(obs, pred){
  
  ## Taillardat et al.(2016) assumes (Eq. 7) pred is a matrix representing the ensemble forcast
  
  if(class(pred)=="numeric"){
    nobs <- length(obs)
    
    crps <- array()
    for(i in 1:nobs){
      crps.t1 <- abs(pred[i]-obs[i])
      crps[i] <- as.numeric(crps.t1)
    }
    return(crps)
  }	
  
  if(class(pred)=="matrix"){		
    nobs <- nrow(pred)
    nens <- ncol(pred)
    
    crps <- array()
    for(i in 1:nobs){
      
      crps.t1 <- sum(abs(pred[i,]-obs[i])) 
      crps.t2 <- 0
      for(k in 1:nens){
        for(j in 1:nens) crps.t2 <- crps.t2 + abs(pred[i,k]-pred[i,j])
      } 
      
      crps[i] <- as.numeric(1/nens * crps.t1 - 1/(2*nens*(nens-1)) * crps.t2)
    }
    return(crps)
  }
}


####### QRF iterative only for SAPHIR-CALIPSO 
QRFiterative <- function(data,tol,nmaxiter,alg,verbose=FALSE){
  
  ## --------------------------------------------------------------------------------------------------------------------------------
  ## First iteration (s=0)
  ## --------------------------------------------------------------------------------------------------------------------------------
  
  s <- 1
  
  ## Coarse resolution data
  data[,"dep.coarse"] <-  data[,"rh.frac"]
  
  ## Number of fine resolution data per coarse resolution pixel
  data[,"freq"] <-  merge.with.order(data, count(data, "ind"),by = "ind", all=TRUE,  keep_order=1)[,"freq"]	
  
  ## *****************************************************************************************************************************
  ## QRF - Fit
  ## *****************************************************************************************************************************
  
  qrf <- quantregForest(data[grep("SR",colnames(data))],data$dep.coarse,keep.inbag=TRUE)
  
  data[,paste0("dep.fine.",s)] <- predict(qrf, what=0.5)
  data[,paste0("dep.fine.unc.",s)] <- predict(qrf, what=0.75)-predict(qrf, what=0.5)
  tmp.resid <- data[,paste0("dep.fine.",s)]-data[,"dep.coarse"]
  data[,paste0("rsq.",s)] <- pseudoR2(tmp.resid, data[,"dep.coarse"])
  
  if(verbose){
    print(paste0("iter: ",s))
    print(paste0("cond: NA"))	
    print(paste0("rsq: ", round(unique(data[,paste0("rsq.",s)]),3)))
  }
  
  ## --------------------------------------------------------------------------------------------------------------------------------
  ## s>0
  ## --------------------------------------------------------------------------------------------------------------------------------
  
  s <- s+1
  cond <- TRUE	
  while(cond & s < nmaxiter){
    ## *****************************************************************************************************************************
    ## Mass Balance (the estimate of the high resolution data must match the value of the coarse resolution data within each pixel
    ## *****************************************************************************************************************************
    
    # Compute mean
    data[,paste0("dep.fine.mean.",s-1)] <- merge.with.order(data, 
                                                            setNames(aggregate(formula(paste0("dep.fine.",s-1,"~ind")), data, mean),c("ind","dep.fine.mean")), 
                                                            by = "ind", all=TRUE,  keep_order=1)[,"dep.fine.mean"]
    # Update (Malone et al., 2012)
    if(alg=="Malone") data[,paste0("dep.fine.",s)] <- data[,paste0("dep.fine.",s-1)] * data[,"dep.coarse"]/data[,paste0("dep.fine.mean.",s-1)]
    
    # Update (Liu et al., 2008)
    if(alg=="Liu")  data[,paste0("dep.fine.",s)] <- data[,paste0("dep.fine.",s-1)] + data[,"dep.coarse"] - data[,paste0("dep.fine.mean.",s-1)]
    
    ## *****************************************************************************************************************************
    ## QRF Fit
    ## *****************************************************************************************************************************
    qrf <- quantregForest(data[grep("SR",colnames(data))],data$dep.coarse,keep.inbag=TRUE)
    name_col<-paste("dep.fine.",s)
    data[,] <- predict(qrf, what=0.5)
    data[,paste0("dep.fine.unc.",s)] <- predict(qrf, what=0.75)-predict(qrf, what=0.5)
    tmp.resid <- data[,paste0("dep.fine.",s)]-data[,"dep.coarse"]
    data[,paste0("rsq.",s)] <- pseudoR2(tmp.resid, data[,"dep.coarse"])
    
    if(alg=="Malone"){
      if(sum(abs(data[,paste0("dep.fine.",s)] - data[,paste0("dep.fine.",s-1)]))/nrow(data) < tol) cond <- FALSE
    }
    if(alg=="Liu"){
      if(unique(abs(data[,paste0("rsq.",s)]-data[,paste0("rsq.",s-1)])) < tol | unique(data[,paste0("rsq.",s)]-data[,paste0("rsq.",s-1)]) < 0) cond <- FALSE
    }	
    
    if(verbose){
      print(paste0("iter: ",s))
      print(paste0("cond: ",round(cond,3)))	
      print(paste0("rsq: ", round(unique(data[,paste0("rsq.",s)]),3)))
    }
    s <- s+1
  }
  
  data <- data[,1:(ncol(data)-3)] 
  data$iter <- sss-2
  
  return(data)
}










####### QRF iterative pour CALIPSO et CLOUDSAT
QRFiterative_CALIPSO_CLOUDSAT <- function( data,tol,nmaxiter,alg,verbose=FALSE){
  #remplacer argument "nindice_sample" par "indice_sample"? 
  
  ## --------------------------------------------------------------------------------------------------------------------------------
  ## First iteration (sss=0)
  ## --------------------------------------------------------------------------------------------------------------------------------
  
  sss <- 1
  
  ## Coarse resolution data
  data[,"dep.coarse"] <-  data[,"rh.frac"]
  
  
  ## Number of fine resolution data per coarse resolution pixel
  data[,"freq"] <-  merge.with.order(data, count(data, vars="ind"),by = "ind", all=TRUE,  keep_order=1)[,"freq"]	## à quoi sert? utile si le fait aussi dans le script de downscalling?
  
  ## *****************************************************************************************************************************
  ## QRF - Fit
  ## *****************************************************************************************************************************
  
  predictors <- c( paste0(colnames(data)[grep("SR",colnames(data))]), 
                   paste0(colnames(data)[grep("PHASE",colnames(data))]),
                   paste0("RR", c(1.2, 2:5)))
  print(names(data))
  print(predictors)
  
  #form <- as.formula(paste0("rh.frac ~ ",paste(predictors, collapse= "+")))  ###???
  
  
  ##########################
  ########### Compute QRF
  ###temps de process
  #print(Sys.time (qrf<-quantregForest(data_sample,data$dep.coarse[indice_sample],keep.inbag=TRUE, keep.forest=TRUE)))
  qrf <- quantregForest(data[, predictors], data$dep.coarse, keep.inbag=TRUE, keep.forest=TRUE, ntree=500) ## fait le modele sur l echantillon d apprentissage
  str(qrf)
  
  ### pour plotter la foret
  #tree<-getTree(qrf,k=1,labelVar=TRUE)
  #devtools::install_github('skinner927/reprtree')
  # library(reprtree)
  # qrf$terms<-""
  # attr(qrf$terms, "dataClasses") = unlist(sapply(data, class))
  # reprtree:::plot.getTree(qrf)
  # 
  #library(plotmo)  #https://www.r-pkg.org/pkg/plotmo
  #plotmo(qrf)   ## pour visualiser les erreurs d estimation lorsque la foret est agrandie
  #https://www.rdocumentation.org/packages/quantregForest/versions/1.3-7/topics/predict.quantregForest
  #https://cran.r-project.org/web/packages/ModelMap/vignettes/Vquantile.pdf
  #https://living-sun.com/fr/r/747859-how-can-i-get-the-probability-density-function-from-a-regression-random-forest-r-random-forest-probability-density.html
  
  ### prediction
  #data[,paste0("dep.fine.",sss)] <- predict(qrf, what=0.5)
  dep.fine <- predict(qrf, what=0.5)   ### fait la prediction sur l ensemble des donnees
  dep.fine.unc<-predict(qrf, what=0.75,newdata=data)-predict(qrf, what=0.25)
  
  
  ### R2
  rsq<-(cor(dep.fine, data[,"dep.coarse"]))^2
  #tmp.resid <- dep.fine - data[,"dep.coarse"] ## calcul des residus et du R2 sur l echantillon test par rapport aux donnees a basses resolution
  #rsq <- pseudoR2(tmp.resid, data[,"dep.coarse"])
  ### ecart interquartile => uncertitude
  #data[,paste0("dep.fine.unc.",sss)] <- predict(qrf, what=0.75)-predict(qrf, what=0.5)     
  
  if(verbose){
    print(paste0("iter: ",sss))
    print(paste0("cond: NA"))	
    print(paste0("rsq: ", round(unique(data[,paste0("rsq.",sss)]),3)))
  }
  
  ## --------------------------------------------------------------------------------------------------------------------------------
  ## sss>0
  ## --------------------------------------------------------------------------------------------------------------------------------
  
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
    
    ##### !!! garder les memes indices!!!
    qrf <- quantregForest(data[, predictors], dep.fine, keep.inbag=TRUE,ntree=500)  #### !!!! bien mettre dep.fine comme prédictant!!!
    
    dep.fine <- predict(qrf, what=0.5)
    dep.fine.unc<-predict(qrf, what=0.75,newdata=data)-predict(qrf, what=0.25)
    
    dep.fine[dep.fine<0] <- 0      ### lors de l iteration possible production de valeurs negatives ou superieures a 100, donc on seuille les valeurs a chaque iteration
    dep.fine[dep.fine>100] <- 100
    
    #tmp.resid <- dep.fine - data[,"dep.coarse"]
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
  
  qrf$data <- data  #pour recalculer la climatologie dans le code
  qrf$rsq_sss<-rsq  # pour avoir le rsq de la derniere boucle !!! rsq pour chaque prediction!!!
  qrf$predict<-dep.fine # pour avoir les predivtions de la derniere boucle
  return(qrf)
}



















GAMgpiterative <- function(data,tol,nmaxiter,alg,verbose=FALSE){
  
  gm <- list()
  
  ## --------------------------------------------------------------------------------------------------------------------------------
  ## First iteration (s=0)
  ## --------------------------------------------------------------------------------------------------------------------------------
  
  s <- 1
  
  ## Coarse resolution data
  data[,"dep.coarse"] <-  data[,"rh.frac"]
  
  ## Number of fine resolution data per coarse resolution pixel
  data[,"freq"] <-  merge.with.order(data, count(data, "ind"),by = "ind", all=TRUE,  keep_order=1)[,"freq"]	
  
  ## *****************************************************************************************************************************
  ## GAM Fit
  ## *****************************************************************************************************************************
  
  predictors <- paste0("s(", colnames(data)[grep("SR",colnames(data))],", bs='cr')")
  
  ## Define correlation structure
  nknots.sp <- 20 	
  range <- 10 * 10^3  # Range of the correlation (m)
  model <- 2 			# Exponential: exp((dist/range)^k)
  power <- 1 			# Power (k)
  predictors.sp <- paste0("s(x,y, bs='gp', k=",nknots.sp,", m=c(",2,",",range,",",power,"))")
  
  predictors <- c(predictors,predictors.sp)
  form <- as.formula(paste0("rh.frac ~ ",paste(predictors, collapse= "+")))
  fam <- betar(link="logit")
  gm[[s]] <- gam(formula=form,family=fam, data=data, method="GCV.Cp")
  
  data[,paste0("dep.fine.",s)] <- predict(gm[[s]], type = "response")
  tmp.resid <- data[,paste0("dep.fine.",s)]-data[,"dep.coarse"]
  data[,paste0("rsq.",s)] <- pseudoR2(tmp.resid, data[,"dep.coarse"])
  
  if(verbose){
    print(paste0("iter: ",s))
    print(paste0("cond: NA"))	
    print(paste0("rsq: ", round(unique(data[,paste0("rsq.",s)]),3)))
  }
  
  ## --------------------------------------------------------------------------------------------------------------------------------
  ## s>0
  ## --------------------------------------------------------------------------------------------------------------------------------
  
  s <- s+1
  cond <- TRUE	
  while(cond & s < nmaxiter){
    ## *****************************************************************************************************************************
    ## Mass Balance (the estimate of the high resolution data must match the value of the coarse resolution data within each pixel
    ## *****************************************************************************************************************************
    
    # Compute mean
    data[,paste0("dep.fine.mean.",s-1)] <- merge.with.order(data, 
                                                            setNames(aggregate(formula(paste0("dep.fine.",s-1,"~ind")), data, mean),c("ind","dep.fine.mean")), 
                                                            by = "ind", all=TRUE,  keep_order=1)[,"dep.fine.mean"]
    # Update (Malone et al., 2012)
    if(alg=="Malone") data[,paste0("dep.fine.",s)] <- data[,paste0("dep.fine.",s-1)] * data[,"dep.coarse"]/data[,paste0("dep.fine.mean.",s-1)]
    
    # Update (Liu et al., 2008)
    if(alg=="Liu")  data[,paste0("dep.fine.",s)] <- data[,paste0("dep.fine.",s-1)] + data[,"dep.coarse"] - data[,paste0("dep.fine.mean.",s-1)]
    
    ## *****************************************************************************************************************************
    ## GAM Fit
    ## *****************************************************************************************************************************
    
    predictors <- paste0("s(", colnames(data)[grep("SR",colnames(data))],", bs='cr')")
    
    ## Define correlation structure
    nknots.sp <- 20 	
    range <- 10 * 10000 # Range of the correlation (m)
    model <- 2 			# Exponential: exp((dist/range)^k)
    power <- 1 			# Power (k)
    predictors.sp <- paste0("s(x,y, bs='gp', k=",nknots.sp,", m=c(",2,",",range,",",power,"))")
    
    predictors <- c(predictors,predictors.sp)
    form <- as.formula(paste0("rh.frac ~ ",paste(predictors, collapse= "+")))
    fam <- betar(link="logit")
    gm[[s]] <- gam(formula=form,family=fam, data=data, method="GCV.Cp")
    
    data[,paste0("dep.fine.",s)] <- predict(gm[[s]], type = "response")
    tmp.resid <- data[,paste0("dep.fine.",s)]-data[,"dep.coarse"]
    data[,paste0("rsq.",s)] <- pseudoR2(tmp.resid, data[,"dep.coarse"])
    
    if(alg=="Malone"){
      if(sum(abs(data[,paste0("dep.fine.",s)] - data[,paste0("dep.fine.",s-1)]))/nrow(data) < tol) cond <- FALSE
    }
    if(alg=="Liu"){
      if(unique(abs(data[,paste0("rsq.",s)]-data[,paste0("rsq.",s-1)])) < tol | unique(data[,paste0("rsq.",s)]-data[,paste0("rsq.",s-1)]) < 0) cond <- FALSE
    }	
    
    if(verbose){
      print(paste0("iter: ",s))
      print(paste0("cond: ",round(cond,3)))	
      print(paste0("rsq: ", round(unique(data[,paste0("rsq.",s)]),3)))
    }
    s <- s+1
    print(s)
    
  }
  
  data <- data[,1:(ncol(data)-3)] 
  data$iter <- s-2
  
  out <- list()
  out[[1]] <- data 
  out[[2]] <- gm[[s-2]]
  
  
  return(out)
}

