#' sliver
#' 
#' Run an interface for uploading data to R, and then run iPlot or return the data as a data.table
#' 
#' @import iPlot
#' @import shiny
#' @import sparkle
#' @import ggthemes
#' @import xtable
#' @export

sliver <- function() {
   obj <- readGUI()
   if(class(obj) == "iData") {
      iPlot(obj)
   } else {
      return(obj)
   }
}


