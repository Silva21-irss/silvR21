#' Separate Components of ClimateNA File Names
#'
#' Using the file names from the projected ClimateNA future models data, a data frame splitting up the components of the file name (time frames, SSPs, and GCM models).
#'
#' @param files A list of ClimateNA future projection outputs.
#'
#' @return List
#'
#' @author Michael Burnett - UBC Faculty of Forestry
#'
#' @export
#'
#' @examples
#' files <- list.files()
#' models <- gcmSepCols(files)
gcmSepCols <- function(files){
  df <- as.data.frame(files)
  df$Time_Frame <- substr(df$files,nchar(df$files)-12,nchar(df$files)-4)
  df$SSP <- substr(df$files,nchar(df$files)-19,nchar(df$files)-14)
  df$Climate_Model <- substr(df$files,1,nchar(df$files)-21)
  return(df)
}
