#' readData
#' 
#' Function for reading a specified file into R and saving it as a internal dataset
#' 
#' @param filename Filename (indlucing path)
#' @param filetype Custom specified file type
#' @param verbose Verbose output mode
#' 
#' @import tools
#' @import data.table
#' @export

readData <- function(filename, filetype, verbose=TRUE) {
   if(verbose) {
      cat(paste0("Reading file: ",filename,"\n"))
   }
   
   # Get filetype
   if(missing(filetype)) {
      filetype <- tolower(file_ext(filename))
      
      if(verbose) cat(paste0("Automatically identified file type to be: '",filetype, "'\n"))
   }
   
   if(filetype %in% c("csv")) {
      indata <- fread(filename)
   } else {
      stop("ERROR: Unsupported file format")
   }
   
   return(indata)
}