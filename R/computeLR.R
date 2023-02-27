#' Compute Loss Rate
#'
#' Compute Loss Rate based on Fire Intensity FLI and Distance from Fire Edge DFE, according to the formula
#' LR = LRfactor * FLI^(LRexp)
#' By default, LRfactor = 4.94 and LRexp = 0.34
#' For indirect exposure, LR is multiplied by the Normalized Loss Rate NLR
#' NLR = NLRfactor*Ln(DFE) + NLRcost
#' By default, NLRfacotr = -0.21 and NLRcost = 1.37
#' If FLI is greater that 6800 kW/m, then LR=100
#' If fire edge is further than maxDist, then LR=0
#' @param fi Fire Intensity and position of fires
#' @param flame_arrival_time Flame arrival time
#' @param buildings Buildings' grid
#' @param maxDist Maximal distance from fire edge
#' @param LRfactor First parameter of LR formula
#' @param LRexp Second parameter of LR formula
#' @param NLRfactor First parameter of NLR formula
#' @param NLRcost Second parameter of NLR formula
#' @return Square of impacted by fire and summarizing table
#' @examples 
#' grid <- createBuildingGrid <- function(buildings, 100, "conservative")
#' @export
#' 

computeLR <- function(fi,
                      flame_arrival_time = NULL,
                      buildings,
                      maxDist = 500,
                      LRfactor = 4.94,
                      LRexp = 0.34,
                      NLRfactor = -0.21,
                      NLRcost = 1.37)
{
  require(sf)
  require(raster)
  require(ggplot2)
  require(ggnewscale)
  require(beepr)
  require(data.table)
  
  # LRfactor = 4.94
  # LRexp = 0.34
  # NLRfactor = -0.21
  # NLRcost = 1.37
  
  buildingsPos <- buildings[buildings$Sum_Count > 0, ]
  totalBuildings <- sum(buildingsPos$Sum_Count)
  
  if (is(fi, "RasterLayer")) {
    print("Convert fi raster to vector")
    fi <- rasterToPoints(fi, spatial = T)
    fi <- st_as_sf(fi)
  }
  if (is(fi, "SpatialPolygonsDataFrame")) {
    fi <- st_as_sf(fi)
  }
  if (!is(fi, "sf")) {
    stop("Wrong format for fi")
  }
  
  colnames(fi)[1] <- "fi"
  
  if (is.null(flame_arrival_time)) {
    fire_sp <- fi
    fire_sp$flame.arrival.time <- 161
  } else{
    if (is(flame_arrival_time, "RasterLayer")) {
      flame_arrival_time <-
        rasterToPoints(flame_arrival_time, spatial = T)
      flame_arrival_time <- st_as_sf(flame_arrival_time)
    }
    if (is(flame_arrival_time, "SpatialPolygonsDataFrame")) {
      flame_arrival_time <- st_as_sf(flame_arrival_time)
    }
    if (!is(fi, "sf")) {
      stop("Wrong format for flame_arrival_time")
    }
    
    fire_sp <- st_join(fi, flame_arrival_time)
  }
  
  fire_sp <- fire_sp[fire_sp$fi > 0, ]
  
  if (nrow(fire_sp) == 0) {
    exposedBuildings <- 0
    lostBuildings <- 0
    averageLossRate <- 0
    averageCommunityLossRate <- 0
    
    out <-
      data.frame(
        totalBuildings,
        exposedBuildings,
        lostBuildings,
        averageLossRate,
        averageCommunityLossRate
      )
    
    final <- data.frame(NULL)
    return(list(final = final, out = out))
  }
  
  fire_sp <- st_transform(fire_sp, st_crs(buildings))
  
  buffer <- st_union(fire_sp) %>% 
    st_convex_hull() %>% 
    st_buffer(2*maxDist) %>% 
    st_transform(st_crs(buildings))
  
  buildingsPos <- suppressWarnings(st_intersection(buildingsPos, buffer))
  buildings <- suppressWarnings(st_intersection(buildings, buffer))
  
  fire_sp <- st_as_sf(buildings$geometry) %>% 
    st_join(fire_sp, join = st_contains, left = F)
  
  if (nrow(fire_sp) == 0) {
    exposedBuildings <- 0
    lostBuildings <- 0
    averageLossRate <- 0
    averageCommunityLossRate <- 0
    
    out <-
      data.frame(
        totalBuildings,
        exposedBuildings,
        lostBuildings,
        averageLossRate,
        averageCommunityLossRate
      )
    
    final <- data.frame(NULL)
    
    return(list(final = final, out = out))
  }
  
  
  fire_sp$flame.arrival.time <-
    (fire_sp$flame.arrival.time - 161) / 0.04166666666
  
  buildingsPosCentroid <- suppressWarnings(st_centroid(buildingsPos))

  distFire <- st_distance(buildingsPosCentroid, fire_sp) #Distance Centroid to Border
  # distFire <- st_distance(buildingsPos, fire_sp) #Distance Border to Border
  
  units(distFire) <- NULL
  distFire <- round(distFire, 2)
  
  # Compute all LRs for fires in the (maxDist)m radius, then select the highest LR
  # Select highest FI in the 500m radius
  
  neighbors <- which(distFire < maxDist, arr.ind=T)
  
  if (nrow(neighbors)==0) {
    exposedBuildings <- 0
    lostBuildings <- 0
    averageLossRate <- 0
    averageCommunityLossRate <- 0
    
    out <-
      data.frame(
        totalBuildings,
        exposedBuildings,
        lostBuildings,
        averageLossRate,
        averageCommunityLossRate
      )
    
    final <- data.frame(NULL)
    
    return(list(final = final, out = out))
  }
  
  if(nrow(neighbors) > 1){
    neighbors <- neighbors[order(neighbors[, "row"]),]
  }
  
  
  exposed <- unique(neighbors[, "row"])
  final <- buildingsPos[exposed,]
  
  closeFire <- unique(neighbors[, "col"])
  fire_sp <- fire_sp[closeFire,]
  
  eq1 <- LRfactor * fire_sp$fi ^ (LRexp)
  eq1[eq1 > 100] <- 100
  
  eq1 <- cbind(col = closeFire,
               fi = fire_sp$fi,
               fat = fire_sp$flame.arrival.time,
               eq1)
  
  dist <- distFire[neighbors]
  
  eq2 <- NLRfactor * log(dist) + NLRcost
  eq2[is.infinite(eq2)] <- 1
  eq2 <- cbind(neighbors, eq2)
  
  neighbors <- cbind(neighbors, dist)
  neighbors <- merge(neighbors, eq1)
  neighbors <- merge(neighbors, eq2)
  
  neighbors$LR <- neighbors[, "eq1"] * neighbors[, "eq2"]
  
  neighbors <- as.data.table(neighbors)
  neighbors <- neighbors[, -1]
  neighbors <- neighbors[neighbors[, .I[which.max(LR)], by=row]$V1]
  
  neighbors <- neighbors[order(neighbors[, "row"]),]
  
  neighbors$LRbin <- ifelse(neighbors$LR == 0, 0,
                            ifelse(neighbors$LR <= 20, "1-20",
                                   ifelse(
                                     neighbors$LR <= 40,
                                     "21-40",
                                     ifelse(
                                       neighbors$LR <= 60,
                                       "41-60",
                                       ifelse(neighbors$LR <= 80, "61-80", "81-100")
                                     )
                                   )))
  
  final <- cbind(final, neighbors[,-1])
  
  exposedBuildings <- sum(final$Sum_Count)
  lostBuildings <- round(sum(final$Sum_Count * final$LR / 100))
  averageLossRate <- round(lostBuildings / exposedBuildings * 100, 2)
  if (is.nan(averageLossRate)) {
    averageLossRate <- 0
  }
  averageCommunityLossRate <-
    round(lostBuildings / totalBuildings * 100, 2)
  
  out <-
    data.frame(
      totalBuildings,
      exposedBuildings,
      lostBuildings,
      averageLossRate,
      averageCommunityLossRate
    )

  return(list(final = final, out = out))
  
}