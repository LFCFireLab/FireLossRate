#' Summary Statistics for Loss Rate
#'
#' Compute summary statistics of Loss Rate object produced with computeLR function
#' @param final Loss Rate object produced with computeLR function
#' @param damagedThreshold Loss Rate threshold for considering a structure damaged, defauilt is 50
#' @return sf object with average LR, median LR, min LR, max LR, times exposed and times damaged for each impacted Pixel With Structure
#' @examples 
#' summary <- summaryStatisticsLR(final)
#' @export
#' 
summaryStatisticsLR <- function(final, damagedThreshold = 50){
  require(sf)
  require(dplyr)
  
  if(damagedThreshold < 0){
    stop("damagedThreshold < 0")
  }
  if(damagedThreshold > 100){
    stop("damagedThreshold > 100")
  }
  if(damagedThreshold <= 1){
    damagedThreshold <- damagedThreshold * 100
    stop("damagedThreshold between 0 and 1 has been multiplied by 100")
  }
  
  tryCatch(
    {
      resultsMap = final$final
    },
    error = function(e){
      stop("Input is not a Loss Rate object")
    }
  )

  resultsMap$grp <- sapply(st_equals(resultsMap), max)
  out <- resultsMap %>%
    group_by(grp) %>%
    summarize(avgLR = mean(LR), medianLR = median(LR), 
              maxLR = max(LR), minLR = min(LR), 
              countExposed = n())
  
  resultsMapDamaged <- resultsMap[resultsMap$LR >= damagedThreshold,]
  resultsMapDamaged <- resultsMapDamaged %>%
    group_by(grp) %>%
    summarize(countDamaged = n())
  
  resultsMapDamaged <- st_drop_geometry(resultsMapDamaged)
  
  out <- merge(out, resultsMapDamaged, all.x = T)
  out$countDamaged[is.na(out$countDamaged)] <- 0
  out <- select(out, -grp)
  
  return(out)
  
}