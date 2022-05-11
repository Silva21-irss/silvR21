#' Separate Components of ClimateNA File Names
#'
#' @param files A list of ClimateNA future projection outputs.
#'
#' @return List
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
