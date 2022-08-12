#' Summarize climate variables by Year
#'
#' Climate data is summarized based on the variable's mean and standard deviation values for each year and is outputted to a summarized table.
#'
#' @param file The climate output from ClimateNA as CSV file format
#' @param startYear numeric. The first year of the time frame
#' @param endYear numeric. The last year of the time frame
#' @param outdir character. The output directory for the summary file
#'
#' @return A CSV file consisting of the summary data
#' @export
#'
#' @author Michael Burnett - UBC Faculty of Forestry
#'
#' @importFrom data.table fread
#' @importFrom utils write.csv
#' @importFrom stats sd
#'
#' @examples
#' ## Input a data file consisting of monthly climate data projections from 2000 to 2050
#' #file <- read.csv('data.csv')
#' #climateSummary(file,2000,2050)
climateSummary <- function(file, startYear, endYear, outdir=dirname(file)){
  csvfile <- data.table::fread(file)
  Year <- as.character(c(startYear:endYear))

  fileN <- names(csvfile)

  climateData <- data.frame(Year) # Create new data frame
  fEl <- which(fileN == 'Elevation') + 1
  climateCols <- fileN[c(fEl:length(fileN))] # Access all climate columns

  counter = 2

  for(a in climateCols){
    climateData[[counter]] <- NA # Create columns to store climate summary results
    climateData[[counter+1]] <- NA
    for(b in Year){
      climateData[[counter]][climateData$Year == b] <- mean(csvfile[[a]][csvfile$Year == b])
      climateData[[counter + 1]][climateData$Year == b] <- sd(csvfile[[a]][csvfile$Year == b])
    }
    names(climateData)[c(counter,counter+1)] <- c(paste0(a,'_mean'),paste0(a,'_StD'))
    counter <- counter + 2
  }

  write.csv(climateData,paste0(outdir,"/",gsub(".csv","",file),"_meanVals.csv"))
}

