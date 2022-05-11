#' Access Annual Historical ClimateNA Data
#'
#' @param file the prepared DEM as a CSV file. Full file name is required.
#' @param dateR the historical year for the climate data. Make sure it is in the number format of 'XXXX'.
#' @param tFrame the averaged time frame of each climate variable. Use 'M' for monthly, 'A' for annual, and 'S' for seasonal
#' @param exe Full name of the ClimateNA exe file. Make sure that your working directory is the folder in which this file is located in.
#' @param direc a character vector of full path name to the directory where the ClimateNA app is located.
#'
#' @return A CSV file with historical climate data in the current directory
#' @export
#'
#' @examples
#' #setwd("E:/Climatena_v721");getwd() # Set up location for the application
#' #exe <- "E:/Climatena_v721/ClimateNA_v7.21.exe"
#' #files <- list.files(pattern = '*.csv$')
#' #histAnnualClimateNA(files[1],'1981','M',exe)
histAnnualClimateNA <- function(file,dateR,tFrame,exe,direc = getwd()){
  yearPeriod <- paste0('/Year_',dateR,'.ann')
  inputFile <- paste0('/',gsub('/','\\\\',direc),'\\',file)
  outputFile <- paste0('/',gsub('/','\\\\',direc),'\\',substr(file,1,nchar(file)-4),'_',dateR,'.csv')
  system2(exe,args= c(paste0('/',tFrame), yearPeriod, inputFile, outputFile))
}
