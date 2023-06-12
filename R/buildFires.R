#' Building fire list
#'
#' Build fire list from Burn-P3 output storing Fire Intensity and Iterations
#' @param fileFiresFIraw Path of file containing Fire Intensity
#' @param fileFiresBIraw Path of file containing Iterations
#' @param method Mode to adopt when importing data: "fast" is quicker bur uses more memory, "slow" is the opposite
#' @return Fire list of Fire Intensity for each Iteration
#' @examples
#' fi <- buildFires(fileFiresFIraw, fileFiresBIraw, method = "fast")
#' @export

buildFires <-
  function(fileFiresFIraw, fileFiresBIraw, method = "fast") {
    require(data.table)
    require(dplyr)
    require(tidyr)
    
    if (!(method %in% c("fast", "slow"))) {
      stop('method accepts only "fast" or "slow"')
    }
    
    if (method == 'fast') {
      Lines <- fread(fileFiresFIraw, sep = "", fill = T)[[1]]
      n <- max(count.fields(textConnection(Lines), sep = ","))
      fires <-
        fread(
          text = c(toString(1:n), Lines),
          header = TRUE,
          fill = TRUE
        )
      colnames(fires)[1:2] <- c("column", "row")
      
      Lines <- fread(fileFiresBIraw, sep = "", fill = T)[[1]]
      n <- max(count.fields(textConnection(Lines), sep = ","))
      bi <-
        fread(
          text = c(toString(1:n), Lines),
          header = TRUE,
          fill = TRUE
        )
      colnames(bi)[1:2] <- c("column", "row")
    } else {
      Lines <- fread(fileFiresFIraw, sep = "", fill = T)[[1]]
      n <- max(count.fields(textConnection(Lines), sep = ","))
      fires <- read.table(
        fileFiresFIraw,
        header = TRUE,
        sep = ",",
        col.names = c("column", "row", paste0("V", seq_len(n - 2))),
        fill = TRUE
      )
      bi <- read.table(
        fileFiresBIraw,
        header = TRUE,
        sep = ",",
        col.names = c("column", "row", paste0("V", seq_len(n - 2))),
        fill = TRUE
      )
    }
    
    fires <- fires[rowSums(is.na(fires)) < (n - 2), ]
    bi <- bi[rowSums(is.na(bi)) < (n - 2), ]
    
    fires <-
      gather(
        fires,
        !c(column, row),
        key = variable,
        value = fi,
        na.rm = T
      )
    fires <- dplyr::select(fires,-variable)
    
    bi <-
      gather(bi,
             !c(column, row),
             key = variable,
             value = iter,
             na.rm = T)
    bi <- dplyr::select(bi,-variable)
    
    return(cbind(fireID = bi$iter, fires))
  }
