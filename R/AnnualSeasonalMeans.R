#' Calculate the Annual and Seasonal Summary Values from Monthly ClimateNA Variables
#'
#' This function uses the monthly variables provided by ClimateNA and constructs the annual and seasonal averages/sums for each variable in new columns of the data frame. Averages are calculated for all temperature, solar radiation, and relative humidity values. Sums are calculated for all other variables. This only works with the CSV option in ClimateNA.
#'
#' @param item CSV file containing monthly (NOT primary monthly) ClimateNA variables. You can use primary monthly variables, but you must emphasize which variables to use.
#' @param var Climate variables desired for mean/sum calculations based on annual and seasonal distributions (seasons based on months, not true seasons).  Options include : Tmax, Tmin, Tave, PPT, Rad, DD_5, DD5, DD_18, DD18, NFFD, PAS, Eref, CMD, RH, CMI.  Mean values are calculated for temperature, solar radiation, and relative humidity.  Summed values are calculated for all other variables. Default value indicates only Tmax, Tmin, PPT, DD5, NFFD, PAS, and CMI.
#'
#' @return An expanded data frame
#' @export
#'
#' @author Michael Burnett - UBC Faculty of Forestry
#'
#' @examples
#' ### Processing a single file
#' #files <- list.files(pattern='*csv$')
#' #site <- read.csv(files[1])
#' #annSeaMean <- AnnualSeasonalMeans(site,c('Tmax','Tmin','PPT','CMI'))
#'
#' ## Processing multiple files
#' files <- list.files(pattern='*csv$')
#' for(i in files){
#'   item <- read.csv(i)
#'   item <- AnnualSeasonalMeans(item)
#'   write.csv(item,i)
#' }
AnnualSeasonalMeans <- function(item, var = c('Tmax','Tmin','PPT','DD5','NFFD','PAS','CMI')){
  #if(is.null(var)){var <- c('Tmax','Tmin','PPT','DD5_','NFFD','PAS','CMI')}
  if('Tmax' %in% var){
    item$ANNUAL_Tmax <- rowMeans(item[c(which(names(item)=='Tmax01'):which(names(item)=='Tmax12'))])
    item$WINTER_Tmax <- rowMeans(item[c(which(names(item)=='Tmax01'):which(names(item)=='Tmax03'))])
    item$SUMMER_Tmax <- rowMeans(item[c(which(names(item)=='Tmax07'):which(names(item)=='Tmax09'))])
    item$SPRING_Tmax <- rowMeans(item[c(which(names(item)=='Tmax04'):which(names(item)=='Tmax06'))])
    item$AUTUMN_Tmax <- rowMeans(item[c(which(names(item)=='Tmax10'):which(names(item)=='Tmax12'))])
  }
  if('Tmin' %in% var){
    item$ANNUAL_Tmin <- rowMeans(item[c(which(names(item)=='Tmin01'):which(names(item)=='Tmin12'))])
    item$WINTER_Tmin <- rowMeans(item[c(which(names(item)=='Tmin01'):which(names(item)=='Tmin03'))])
    item$SUMMER_Tmin <- rowMeans(item[c(which(names(item)=='Tmin07'):which(names(item)=='Tmin09'))])
    item$SPRING_Tmin <- rowMeans(item[c(which(names(item)=='Tmin04'):which(names(item)=='Tmin06'))])
    item$AUTUMN_Tmin <- rowMeans(item[c(which(names(item)=='Tmin10'):which(names(item)=='Tmin12'))])
  }
  if('Tave' %in% var){
    item$ANNUAL_Tave <- rowMeans(item[c(which(names(item)=='Tave01'):which(names(item)=='Tave12'))])
    item$WINTER_Tave <- rowMeans(item[c(which(names(item)=='Tave01'):which(names(item)=='Tave03'))])
    item$SUMMER_Tave <- rowMeans(item[c(which(names(item)=='Tave07'):which(names(item)=='Tave09'))])
    item$SPRING_Tave <- rowMeans(item[c(which(names(item)=='Tave04'):which(names(item)=='Tave06'))])
    item$AUTUMN_Tave <- rowMeans(item[c(which(names(item)=='Tave10'):which(names(item)=='Tave12'))])
  }
  if('PPT' %in% var){
    item$ANNUAL_PPT <- rowSums(item[c(which(names(item)=='PPT01'):which(names(item)=='PPT12'))])
    item$WINTER_PPT <- rowSums(item[c(which(names(item)=='PPT01'):which(names(item)=='PPT03'))])
    item$SUMMER_PPT <- rowSums(item[c(which(names(item)=='PPT07'):which(names(item)=='PPT09'))])
    item$SPRING_PPT <- rowSums(item[c(which(names(item)=='PPT04'):which(names(item)=='PPT06'))])
    item$AUTUMN_PPT <- rowSums(item[c(which(names(item)=='PPT10'):which(names(item)=='PPT12'))])
  }
  if('DD5' %in% var){
    item$ANNUAL_DD5 <- rowSums(item[c(which(names(item)=='DD5_01'):which(names(item)=='DD5_12'))])
    item$WINTER_DD5 <- rowSums(item[c(which(names(item)=='DD5_01'):which(names(item)=='DD5_03'))])
    item$SUMMER_DD5 <- rowSums(item[c(which(names(item)=='DD5_07'):which(names(item)=='DD5_09'))])
    item$SPRING_DD5 <- rowSums(item[c(which(names(item)=='DD5_04'):which(names(item)=='DD5_06'))])
    item$AUTUMN_DD5 <- rowSums(item[c(which(names(item)=='DD5_10'):which(names(item)=='DD5_12'))])
  }
  if('NFFD' %in% var){
    item$ANNUAL_NFFD <- rowSums(item[c(which(names(item)=='NFFD01'):which(names(item)=='NFFD12'))])
    item$WINTER_NFFD <- rowSums(item[c(which(names(item)=='NFFD01'):which(names(item)=='NFFD03'))])
    item$SUMMER_NFFD <- rowSums(item[c(which(names(item)=='NFFD07'):which(names(item)=='NFFD09'))])
    item$SPRING_NFFD <- rowSums(item[c(which(names(item)=='NFFD04'):which(names(item)=='NFFD06'))])
    item$AUTUMN_NFFD <- rowSums(item[c(which(names(item)=='NFFD10'):which(names(item)=='NFFD12'))])
  }
  if('PAS' %in% var){
    item$ANNUAL_PAS <- rowSums(item[c(which(names(item)=='PAS01'):which(names(item)=='PAS12'))])
    item$WINTER_PAS <- rowSums(item[c(which(names(item)=='PAS01'):which(names(item)=='PAS03'))])
    item$SUMMER_PAS <- rowSums(item[c(which(names(item)=='PAS07'):which(names(item)=='PAS09'))])
    item$SPRING_PAS <- rowSums(item[c(which(names(item)=='PAS04'):which(names(item)=='PAS06'))])
    item$AUTUMN_PAS <- rowSums(item[c(which(names(item)=='PAS10'):which(names(item)=='PAS12'))])
  }
  if('CMI' %in% var){
    item$ANNUAL_CMI <- rowSums(item[c(which(names(item)=='CMI01'):which(names(item)=='CMI12'))])
    item$WINTER_CMI <- rowSums(item[c(which(names(item)=='CMI01'):which(names(item)=='CMI03'))])
    item$SUMMER_CMI <- rowSums(item[c(which(names(item)=='CMI07'):which(names(item)=='CMI09'))])
    item$SPRING_CMI <- rowSums(item[c(which(names(item)=='CMI04'):which(names(item)=='CMI06'))])
    item$AUTUMN_CMI <- rowSums(item[c(which(names(item)=='CMI10'):which(names(item)=='CMI12'))])
  }
  if('Rad' %in% var){
    item$ANNUAL_Rad <- rowSums(item[c(which(names(item)=='Rad01'):which(names(item)=='Rad12'))])
    item$WINTER_Rad <- rowSums(item[c(which(names(item)=='Rad01'):which(names(item)=='Rad03'))])
    item$SUMMER_Rad <- rowSums(item[c(which(names(item)=='Rad07'):which(names(item)=='Rad09'))])
    item$SPRING_Rad <- rowSums(item[c(which(names(item)=='Rad04'):which(names(item)=='Rad06'))])
    item$AUTUMN_Rad <- rowSums(item[c(which(names(item)=='Rad10'):which(names(item)=='Rad12'))])
  }
  if('DD_0' %in% var){
    item$ANNUAL_DD_0 <- rowSums(item[c(which(names(item)=='DD_0_01'):which(names(item)=='DD_0_12'))])
    item$WINTER_DD_0 <- rowSums(item[c(which(names(item)=='DD_0_01'):which(names(item)=='DD_0_03'))])
    item$SUMMER_DD_0 <- rowSums(item[c(which(names(item)=='DD_0_07'):which(names(item)=='DD_0_09'))])
    item$SPRING_DD_0 <- rowSums(item[c(which(names(item)=='DD_0_04'):which(names(item)=='DD_0_06'))])
    item$AUTUMN_DD_0 <- rowSums(item[c(which(names(item)=='DD_0_10'):which(names(item)=='DD_0_12'))])
  }
  if('DD_18' %in% var){
    item$ANNUAL_DD_18 <- rowSums(item[c(which(names(item)=='DD_18_01'):which(names(item)=='DD_18_12'))])
    item$WINTER_DD_18 <- rowSums(item[c(which(names(item)=='DD_18_01'):which(names(item)=='DD_18_03'))])
    item$SUMMER_DD_18 <- rowSums(item[c(which(names(item)=='DD_18_07'):which(names(item)=='DD_18_09'))])
    item$SPRING_DD_18 <- rowSums(item[c(which(names(item)=='DD_18_04'):which(names(item)=='DD_18_06'))])
    item$AUTUMN_DD_18 <- rowSums(item[c(which(names(item)=='DD_18_10'):which(names(item)=='DD_18_12'))])
  }
  if('DD18' %in% var){
    item$ANNUAL_DD18 <- rowSums(item[c(which(names(item)=='DD18_01'):which(names(item)=='DD18_12'))])
    item$WINTER_DD18 <- rowSums(item[c(which(names(item)=='DD18_01'):which(names(item)=='DD18_03'))])
    item$SUMMER_DD18 <- rowSums(item[c(which(names(item)=='DD18_07'):which(names(item)=='DD18_09'))])
    item$SPRING_DD18 <- rowSums(item[c(which(names(item)=='DD18_04'):which(names(item)=='DD18_06'))])
    item$AUTUMN_DD18 <- rowSums(item[c(which(names(item)=='DD18_10'):which(names(item)=='DD18_12'))])
  }
  if('Eref' %in% var){
    item$ANNUAL_Eref <- rowSums(item[c(which(names(item)=='Eref_01'):which(names(item)=='Eref_12'))])
    item$WINTER_Eref <- rowSums(item[c(which(names(item)=='Eref_01'):which(names(item)=='Eref_03'))])
    item$SUMMER_Eref <- rowSums(item[c(which(names(item)=='Eref_07'):which(names(item)=='Eref_09'))])
    item$SPRING_Eref <- rowSums(item[c(which(names(item)=='Eref_04'):which(names(item)=='Eref_06'))])
    item$AUTUMN_Eref <- rowSums(item[c(which(names(item)=='Eref_10'):which(names(item)=='Eref_12'))])
  }
  if('CMD' %in% var){
    item$ANNUAL_CMD <- rowSums(item[c(which(names(item)=='CMD_01'):which(names(item)=='CMD_12'))])
    item$WINTER_CMD <- rowSums(item[c(which(names(item)=='CMD_01'):which(names(item)=='CMD_03'))])
    item$SUMMER_CMD <- rowSums(item[c(which(names(item)=='CMD_07'):which(names(item)=='CMD_09'))])
    item$SPRING_CMD <- rowSums(item[c(which(names(item)=='CMD_04'):which(names(item)=='CMD_06'))])
    item$AUTUMN_CMD <- rowSums(item[c(which(names(item)=='CMD_10'):which(names(item)=='CMD_12'))])
  }
  if('RH' %in% var){
    item$ANNUAL_RH <- rowMeans(item[c(which(names(item)=='RH_01'):which(names(item)=='RH_12'))])
    item$WINTER_RH <- rowMeans(item[c(which(names(item)=='RH_01'):which(names(item)=='RH_03'))])
    item$SUMMER_RH <- rowMeans(item[c(which(names(item)=='RH_07'):which(names(item)=='RH_09'))])
    item$SPRING_RH <- rowMeans(item[c(which(names(item)=='RH_04'):which(names(item)=='RH_06'))])
    item$AUTUMN_RH <- rowMeans(item[c(which(names(item)=='RH_10'):which(names(item)=='RH_12'))])
  }

  return(item)
}
