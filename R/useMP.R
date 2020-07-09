# attempt at cleanup of the useMP function


surveyArea = "/home/marvin/casestudies/uas_flight_wolfskaute/fp/initial_planning.plan"

useMP = function(surveyArea, uavViewDir = 0, followSurfaceRes = 1, followSurface = TRUE)




t <- jsonlite::fromJSON(surveyArea)
listPos <- grep("command", t$mission$items$TransectStyleComplexItem$Items)
tmp<- t$mission$items$TransectStyleComplexItem$Items[listPos][[1]]
#length(tmp$params[[60]])
#tmp$params[[1]][5:6]
  coord<-tmp[tmp["command"]==16, ]
  #coord$params
  df_coordinates<-t(as.data.frame(rlist::list.cbind(coord[,"params",])))[,5:6]
  # t$mission$items$TransectStyleComplexItem$VisualTransectPoints
  tracks<- ceiling(nrow(coord)/4)
  trackDistance <- t$mission$items$TransectStyleComplexItem$CameraCalc$AdjustedFootprintFrontal[listPos]
  crossDistance   <- t$mission$items$TransectStyleComplexItem$CameraCalc$AdjustedFootprintSide[listPos]
  totalTrackdistance <- trackDistance
  fliAltRatio     <- 1 - t$mission$items$TransectStyleComplexItem$CameraCalc$SideOverlap[listPos]/100
  flightAltitude  <- t$mission$items$TransectStyleComplexItem$CameraCalc$DistanceToSurface[listPos]
  maxSpeed        <- t$mission$cruiseSpeed
  launchLat       <- t$mission$plannedHomePosition[1]
  launchLon       <- t$mission$plannedHomePosition[2]
  updir           <- t$mission$items$angle[listPos]
  if (updir <= 180) {
    downdir <- updir + 180
    }else if (updir>180) {
      downdir<- updir -180}
  
  
  
  crossdir        <- geosphere::bearing(c(df_coordinates[2,][2],df_coordinates[2,][1] ),c(df_coordinates[3,][2],df_coordinates[3,][1] ),a = 6378137,f = 1 / 298.257223563)
  missionArea     <- t$mission$items$polygon[listPos]
  # calculate heading from launch position to mission start position
  launch2startHeading <- geosphere::bearing(c(launchLon, launchLat),c(df_coordinates[1,][2],df_coordinates[1,][1] ),a = 6378137,f = 1 / 298.257223563)
  groundResolution<-t$mission$items$TransectStyleComplexItem$CameraCalc$ImageDensity[listPos]
  
  # set cumulative flightlength to zero
  flightLength <- 0
  
  flightParams = c(flightPlanMode = flightPlanMode,
                   flightAltitude = flightAltitude,
                   overlap = 1- fliAltRatio ,
                   uavViewDir = uavViewDir,
                   followSurfaceRes = followSurfaceRes)
  
  p <- makeFlightParam( c(missionArea[[1]][1],missionArea[[1]][5],
                          missionArea[[1]][2],missionArea[[1]][6] ,
                          missionArea[[1]][3],missionArea[[1]][7] ,
                          launchLat, launchLon),
                        flightParams, followSurface)
  mode<-p$flightPlanMode
  
  
  # set universal view direction of the uav
  if (abs(as.numeric(flightParams["uavViewDir"])) == 0) {
    uavViewDir <- updir
  }else {
    uavViewDir <- abs(as.numeric(flightParams["uavViewDir"]))
  }    
  ## calculate survey area
  # create an sp polygon object of the mission area
  # your data (removed crs column)
  tarea <- data.table::data.table(
    longitude= as.data.frame(t$mission$items$polygon[listPos][1])[,2],
    latitude=as.data.frame(t$mission$items$polygon[listPos][1])[,1])
  tarea = sf::st_as_sf(tarea, coords = c("longitude", "latitude"), 
                       crs = 4326)
  tarea<- sf::st_bbox(tarea)
  taskArea<-sf::st_as_sfc(sf::st_bbox(tarea))
  taskAreaUTM <- sf::st_transform(taskArea, 4326)
  # reproject it to UTM
  #taskAreaUTM <- sp::spTransform(taskArea, sp::CRS(paste("+proj=utm +zone=",long2UTMzone(p$lon1)," ellps=WGS84",sep = '')))
  # calculate area
  surveyAreaUTM <- sf::st_area(taskAreaUTM)
  #########################################
  #########################
  # initialize jiDF and mav
  mavDF <- data.frame()
  #set initial heading
  heading <- updir
  # define output line var
  lns <- list()
  lns <- launch2flightalt(p, lns, uavViewDir, launch2startHeading)
  # assign starting point
  pos <- c(df_coordinates[1,][2],df_coordinates[1,][1])
  
  footprint <- calcCamFoot(pos[1], pos[2], uavViewDir, trackDistance, flightAltitude, 0, 0,factor)
  footprint<-  sp::spTransform(footprint,sp::CRS("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  landscape<-abs(abs(footprint@bbox[1]-footprint@bbox[3])*overlap-abs(footprint@bbox[1]-footprint@bbox[3]))
  portrait<- abs(abs(footprint@bbox[2]-footprint@bbox[4])*overlap-abs(footprint@bbox[2]-footprint@bbox[4]))
  
  
  # calculates the footprint of the first position and returns a SpatialPolygonsDataFrame
  if (picFootprint)  camera <- calcCamFoot(pos[1], pos[2], uavViewDir, trackDistance, flightAltitude, 0, 0)
  else  camera = "NULL"
  
  ## creates the export control parameter set of the first position
  if (uavType == "pixhawk") {
    lns[length(lns) + 1] <-  makeUavPointMAV(lat = pos[2],lon = pos[1], head = uavViewDir, group = 99 )
  }
  # push pos to old pos
  pOld <- pos
  
  ## set counter and params for mode = "track" mode
  if (mode == "track") {
    if (uavType == "pixhawk") {
      lns[length(lns) + 1] <- makeUavPointMAV(lat = pos[2],lon = pos[1],head = uavViewDir,group = 99)
    }
    trackDistance <- len
    multiply <- 1
  }
  
  ## set counter and params for mode = "waypoints"
  else if (mode == "waypoints") {
    if (uavType == "pixhawk") {
      lns[length(lns) + 1] <- makeUavPointMAV(lat = pos[2],lon = pos[1],head = uavViewDir,group = 99)
    }
  }
  
  ## set counter and params for mode = "terrainTrack"
  else if (mode == "terrainTrack") group = 99
  df_coord<-as.data.frame(df_coordinates)
  names(df_coord)<-c("lat","lon")
  for (j in seq(1:(nrow(df_coord)-1))) {
    df_coord$heading[j] <- geosphere::bearing(c(df_coord$lon[j],df_coord$lat[j] ), c(df_coord$lon[j + 1],df_coord$lat[j + 1]),a = 6378137,f = 1 / 298.257223563)
    df_coord$len[j] <- geosphere::distGeo(c(df_coord$lon[j],df_coord$lat[j] ), c(df_coord$lon[j + 1],df_coord$lat[j + 1]),a = 6378137,f = 1 / 298.257223563)
    df_coord$multiply <- floor(df_coord$len / followSurfaceRes)
  }
  ## now start calculating the waypoints according to the resolution
  cat("calculating waypoints...\n")
  pb <- pb <- utils::txtProgressBar(max = tracks, style = 3)
  # then do for the rest  forward and backward
  for (j in seq(1:(nrow(df_coord)-1))) {
    pOld<- c(df_coord$lon[j],df_coord$lat[j])
    for (i in seq(1:df_coord$multiply[j])) {
      if (mode == "waypoints" || mode == "terrainTrack") {
        if (i >= df_coord$multiply[j]) {group <- 99}
        else      {group <- 1}}
      else {i <- 2}
      # calc next coordinate
      
      pos <- calcNextPos(pOld[1], pOld[2], df_coord$heading[j], followSurfaceRes)
      
      pOld <- pos
      
      flightLength <- flightLength + followSurfaceRes
      
      if (mode == "track") {
        group <- 99
      }
      
      lns[length(lns) + 1] <- makeUavPointMAV(lat = pos[2], lon = pos[1], head = uavViewDir, group = group)
      
    }
    
    
    utils::setTxtProgressBar(pb, j)
  }
  close(pb)    