#' Building fire list
#'
#' Build fire list from Burn-P3 output storing Fire Intensity and Iterations
#' @param fileFiresFIraw Path of file containing Fire Intensity
#' @param fileFiresBIraw Path of file containing Iterations
#' @return Fire list of Fire Intensity for each Iteration
#' @examples 
#' fi <- buildFires(fileFiresFIraw, fileFiresBIraw)
#' @export

buildFires <- function(fileFiresFIraw, fileFiresBIraw){
  
  require(data.table)
  require(dplyr)
  require(tidyr)
  
  Lines <- fread(fileFiresFIraw, sep = "", fill=T)[[1]]
  n <- max(count.fields(textConnection(Lines), sep = ","))
  fires <- fread(text = c(toString(1:n), Lines), header = TRUE, fill = TRUE)
  colnames(fires)[1:2] <- c("column", "row")
  
  Lines <- fread(fileFiresBIraw, sep = "", fill=T)[[1]]
  n <- max(count.fields(textConnection(Lines), sep = ","))
  bi <- fread(text = c(toString(1:n), Lines), header = TRUE, fill = TRUE)
  colnames(bi)[1:2] <- c("column", "row")
  
  fires <- gather(fires, !c(column,row), key=variable, value=fi, na.rm=T)
  fires <- dplyr::select(fires, -variable)
  
  bi <- gather(bi, !c(column,row), key=variable, value=iter, na.rm=T)
  bi <- dplyr::select(bi, -variable)
  
  return(cbind(fireID = bi$iter, fires))
}