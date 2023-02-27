#' Summary Loss Rate
#'
#' Print summary of a Loss Rate object produced with computeLR function
#' @param final Loss Rate object produced with computeLR function
#' @examples 
#' summaryLR(final)
#' @export
#' 
summaryLR <- function(final){
  tryCatch(
    {
      print(final$out)
    },
    error = function(e){
      print("Input is not a Loss Rate object")
    }
  )
}