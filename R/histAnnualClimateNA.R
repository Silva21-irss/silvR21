#' Access Annual Historical ClimateNA Data
#'
#' This is a specialty function that has several preliminary requirements. The output produces the  historical climate variables for the provided CSV file.
#' In order to properly use this function, you must prepare the CSV file so that it is compatible with the ClimateNA requirements (using the demToCSV tool).
#' The working directory must be set to the location which the ClimateNA app is located.
#'
#' @param file the prepared DEM as a CSV file.
#' @param dateR the historical year for the climate data. Make sure it is in the number format of 'XXXX'.
#' @param tFrame the averaged time frame of each climate variable. Use 'M' for monthly, 'Y' for annual, and 'S' for seasonal
#' @param exe Full name of the ClimateNA exe file. The working directory must be the location of this file.
#'
#'
#' @return A CSV file with historical climate data in the current directory
#' @export
#'
#' @author Michael Burnett - UBC Faculty of Forestry
#'
#' @examples
#' #setwd("E:/Climatena_v721");getwd() # Set up location for the application
#' #exe <- "E:/Climatena_v721/ClimateNA_v7.21.exe"
#' #files <- list.files(pattern = '*.csv$')
#' #histAnnualClimateNA(files[1],'1981','M',exe)
histAnnualClimateNA <- function(file,dateR,tFrame,exe){
  direc <- dirname(file)
  fileN <- basename(file)
  yearPeriod <- paste0('/Year_',dateR,'.ann')
  inputFile <- paste0('/',gsub('/','\\\\',direc),'\\',fileN)
  outputFile <- paste0('/',gsub('/','\\\\',direc),'\\',substr(fileN,1,nchar(fileN)-4),'_',dateR,'.csv')
  system2(exe,args= c(paste0('/',tFrame), yearPeriod, inputFile, outputFile))
}
