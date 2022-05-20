#' Access Decadal Historical ClimateNA Data
#'
#' This is a specialty function that has several preliminary requirements. The output produces the 10 years averaged historical climate variables for the provided CSV file.
#' In order to properly use this function, you must prepare the CSV file so that it is compatible with the ClimateNA requirements (using the demToCSV tool).
#' The working directory must be set to the location which the ClimateNA app is located.
#'
#' @param file the prepared DEM as a CSV file.
#' @param dateR the historical date range for the climate data. Make sure it is in the 10 years format of 'XXX1_XXX0'. Underscore is necessary, as opposed to dash.
#' @param tFrame the averaged time frame of each climate variable. Use 'M' for monthly, 'Y' for annual, and 'S' for seasonal
#' @param exe Full name of the ClimateNA exe file. The working directory must be the location of this file.
#' @param outdir The output directory for the climate data. As a default, it will be outputted to the same location as the input CSV file.
#'
#' @return A CSV file with historical climate data
#' @export
#'
#' @author Michael Burnett - UBC Faculty of Forestry
#'
#' @examples
#' #setwd("E:/Climatena_v721");getwd() # Set up location for the application
#' #exe <- "E:/Climatena_v721/ClimateNA_v7.21.exe"
#' #files <- list.files(pattern = '*.csv$')
#' #hist10YClimateNA(files[1],'1981_1990','M',exe)
hist10YClimateNA <- function(file,dateR,tFrame,exe,outdir = dirname(file)){
  fileN <- basename(file)
  yearPeriod <- paste0('/Decade_',dateR,'.dcd')
  inputFile <- paste0('/',gsub('/','\\\\',dirname(file)),'\\',fileN)
  outputFile <- paste0('/',gsub('/','\\\\',outdir),'\\',substr(fileN,1,nchar(fileN)-4),'_',dateR,'.csv')
  system2(exe,args= c(paste0('/',tFrame), yearPeriod, inputFile, outputFile))
}
