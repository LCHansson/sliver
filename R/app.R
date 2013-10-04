#' runApps
#' 
#' ...
#' 
#' @import datareadR
#' @import iPlot
#' @import shiny
#' @import sparkle
#' @import ggthemes
#' @import xtable
#' @export

runApps <- function() {
   obj <- readGUI()
   if(class(obj) == "iData") {
      iPlot(obj)
   } else {
      return(obj)
   }
}


