#' Calculate the Annual and Seasonal Summary Values from Monthly ClimateNA Variables
#'
#' This function uses the monthly variables provided by ClimateNA and constructs the annual and seasonal averages/sums for each variable in new columns of the data frame. Averages are calculated for all temperature, solar radiation, and relative humidity values. Sums are calculated for all other variables. This only works with the CSV option in ClimateNA.
#'
#' @param item CSV file containing monthly (NOT primary monthly) ClimateNA variables. You can use primary monthly variables, but you must emphasize which variables to use.
#' @param var Climate variables desired for mean/sum calculations based on annual and seasonal distributions (seasons based on months, not true seasons).  Options include : Tmax, Tmin, Tave, PPT, Rad, DD_5, DD5, DD_18, DD18, NFFD, PAS, Eref, CMD, RH, CMI.  Mean values are calculated for temperature, solar radiation, and relative humidity.  Summed values are calculated for all other variables. Default value indicates only Tmax, Tmin, PPT, DD5, NFFD, PAS, and CMI.
#' @param TmaxMax logical. Use to access maximum values for Tmax variable. Default setting is FALSE (use of the mean value)
#' @param TminMin logical. Use to access minimum values for Tmin variable. Default setting is FALSE (use of the mean value)
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
AnnualSeasonalMeans <- function(item, var = c('Tmax','Tmin','PPT','DD5','NFFD','PAS','CMI'),TmaxMax = FALSE, TminMin = FALSE){
  #if(is.null(var)){var <- c('Tmax','Tmin','PPT','DD5_','NFFD','PAS','CMI')}
  if('Tmax' %in% var){
    if(isTRUE(TmaxMax)){
      item_Tmax1 <- which(names(item)=='Tmax01')
      item_Tmax3 <- which(names(item)=='Tmax03')
      item_Tmax4 <- which(names(item)=='Tmax04')
      item_Tmax6 <- which(names(item)=='Tmax06')
      item_Tmax7 <- which(names(item)=='Tmax07')
      item_Tmax9 <- which(names(item)=='Tmax09')
      item_Tmax10 <- which(names(item)=='Tmax10')
      item_Tmax12 <- which(names(item)=='Tmax12')
      item$Tmax <- apply(item[,item_Tmax1:item_Tmax12],1,max)
      item$Tmax_wt <- apply(item[,item_Tmax1:item_Tmax3],1,max)
      item$Tmax_sm <- apply(item[,item_Tmax7:item_Tmax9],1,max)
      item$Tmax_sp <- apply(item[,item_Tmax4:item_Tmax6],1,max)
      item$Tmax_at <- apply(item[,item_Tmax10:item_Tmax12],1,max)
    }
    else{
      item$Tmax <- rowMeans(item[c(which(names(item)=='Tmax01'):which(names(item)=='Tmax12'))])
      item$Tmax_wt <- rowMeans(item[c(which(names(item)=='Tmax01'):which(names(item)=='Tmax03'))])
      item$Tmax_sm <- rowMeans(item[c(which(names(item)=='Tmax07'):which(names(item)=='Tmax09'))])
      item$Tmax_sp <- rowMeans(item[c(which(names(item)=='Tmax04'):which(names(item)=='Tmax06'))])
      item$Tmax_at <- rowMeans(item[c(which(names(item)=='Tmax10'):which(names(item)=='Tmax12'))])
    }
  }
  if('Tmin' %in% var){
    if(isTRUE(TminMin)){
      item_Tmin1 <- which(names(item)=='Tmin01')
      item_Tmin3 <- which(names(item)=='Tmin03')
      item_Tmin4 <- which(names(item)=='Tmin04')
      item_Tmin6 <- which(names(item)=='Tmin06')
      item_Tmin7 <- which(names(item)=='Tmin07')
      item_Tmin9 <- which(names(item)=='Tmin09')
      item_Tmin10 <- which(names(item)=='Tmin10')
      item_Tmin12 <- which(names(item)=='Tmin12')
      item$Tmin <- apply(item[,item_Tmin1:item_Tmin12],1,min)
      item$Tmin_wt <- apply(item[,item_Tmin1:item_Tmin3],1,min)
      item$Tmin_sm <- apply(item[,item_Tmin7:item_Tmin9],1,min)
      item$Tmin_sp <- apply(item[,item_Tmin4:item_Tmin6],1,min)
      item$Tmin_at <- apply(item[,item_Tmin10:item_Tmin12],1,min)
    }
    else{
      item$Tmin <- rowMeans(item[c(which(names(item)=='Tmin01'):which(names(item)=='Tmin12'))])
      item$Tmin_wt <- rowMeans(item[c(which(names(item)=='Tmin01'):which(names(item)=='Tmin03'))])
      item$Tmin_sm <- rowMeans(item[c(which(names(item)=='Tmin07'):which(names(item)=='Tmin09'))])
      item$Tmin_sp <- rowMeans(item[c(which(names(item)=='Tmin04'):which(names(item)=='Tmin06'))])
      item$Tmin_at <- rowMeans(item[c(which(names(item)=='Tmin10'):which(names(item)=='Tmin12'))])
    }
  }
  if('Tave' %in% var){
    item$Tave <- rowMeans(item[c(which(names(item)=='Tave01'):which(names(item)=='Tave12'))])
    item$Tave_wt <- rowMeans(item[c(which(names(item)=='Tave01'):which(names(item)=='Tave03'))])
    item$Tave_sm <- rowMeans(item[c(which(names(item)=='Tave07'):which(names(item)=='Tave09'))])
    item$Tave_sp <- rowMeans(item[c(which(names(item)=='Tave04'):which(names(item)=='Tave06'))])
    item$Tave_at <- rowMeans(item[c(which(names(item)=='Tave10'):which(names(item)=='Tave12'))])
  }
  if('PPT' %in% var){
    item$PPT <- rowSums(item[c(which(names(item)=='PPT01'):which(names(item)=='PPT12'))])
    item$PPT_wt <- rowSums(item[c(which(names(item)=='PPT01'):which(names(item)=='PPT03'))])
    item$PPT_sm <- rowSums(item[c(which(names(item)=='PPT07'):which(names(item)=='PPT09'))])
    item$PPT_sp <- rowSums(item[c(which(names(item)=='PPT04'):which(names(item)=='PPT06'))])
    item$PPT_at <- rowSums(item[c(which(names(item)=='PPT10'):which(names(item)=='PPT12'))])
  }
  if('DD5' %in% var){
    item$DD5 <- rowSums(item[c(which(names(item)=='DD5_01'):which(names(item)=='DD5_12'))])
    item$DD5_wt <- rowSums(item[c(which(names(item)=='DD5_01'):which(names(item)=='DD5_03'))])
    item$DD5_sm <- rowSums(item[c(which(names(item)=='DD5_07'):which(names(item)=='DD5_09'))])
    item$DD5_sp <- rowSums(item[c(which(names(item)=='DD5_04'):which(names(item)=='DD5_06'))])
    item$DD5_at <- rowSums(item[c(which(names(item)=='DD5_10'):which(names(item)=='DD5_12'))])
  }
  if('NFFD' %in% var){
    item$NFFD <- rowSums(item[c(which(names(item)=='NFFD01'):which(names(item)=='NFFD12'))])
    item$NFFD_wt <- rowSums(item[c(which(names(item)=='NFFD01'):which(names(item)=='NFFD03'))])
    item$NFFD_sm <- rowSums(item[c(which(names(item)=='NFFD07'):which(names(item)=='NFFD09'))])
    item$NFFD_sp <- rowSums(item[c(which(names(item)=='NFFD04'):which(names(item)=='NFFD06'))])
    item$NFFD_at <- rowSums(item[c(which(names(item)=='NFFD10'):which(names(item)=='NFFD12'))])
  }
  if('PAS' %in% var){
    item$PAS <- rowSums(item[c(which(names(item)=='PAS01'):which(names(item)=='PAS12'))])
    item$PAS_wt <- rowSums(item[c(which(names(item)=='PAS01'):which(names(item)=='PAS03'))])
    item$PAS_sm <- rowSums(item[c(which(names(item)=='PAS07'):which(names(item)=='PAS09'))])
    item$PAS_sp <- rowSums(item[c(which(names(item)=='PAS04'):which(names(item)=='PAS06'))])
    item$PAS_at <- rowSums(item[c(which(names(item)=='PAS10'):which(names(item)=='PAS12'))])
  }
  if('CMI' %in% var){
    item$CMI <- rowSums(item[c(which(names(item)=='CMI01'):which(names(item)=='CMI12'))])
    item$CMI_wt <- rowSums(item[c(which(names(item)=='CMI01'):which(names(item)=='CMI03'))])
    item$CMI_sm <- rowSums(item[c(which(names(item)=='CMI07'):which(names(item)=='CMI09'))])
    item$CMI_sp <- rowSums(item[c(which(names(item)=='CMI04'):which(names(item)=='CMI06'))])
    item$CMI_at <- rowSums(item[c(which(names(item)=='CMI10'):which(names(item)=='CMI12'))])
  }
  if('Rad' %in% var){
    item$Rad <- rowSums(item[c(which(names(item)=='Rad01'):which(names(item)=='Rad12'))])
    item$Rad_wt <- rowSums(item[c(which(names(item)=='Rad01'):which(names(item)=='Rad03'))])
    item$Rad_sm <- rowSums(item[c(which(names(item)=='Rad07'):which(names(item)=='Rad09'))])
    item$Rad_sp <- rowSums(item[c(which(names(item)=='Rad04'):which(names(item)=='Rad06'))])
    item$Rad_at <- rowSums(item[c(which(names(item)=='Rad10'):which(names(item)=='Rad12'))])
  }
  if('DD_0' %in% var){
    item$DD_0 <- rowSums(item[c(which(names(item)=='DD_0_01'):which(names(item)=='DD_0_12'))])
    item$DD_0_wt <- rowSums(item[c(which(names(item)=='DD_0_01'):which(names(item)=='DD_0_03'))])
    item$DD_0_sm <- rowSums(item[c(which(names(item)=='DD_0_07'):which(names(item)=='DD_0_09'))])
    item$DD_0_sp <- rowSums(item[c(which(names(item)=='DD_0_04'):which(names(item)=='DD_0_06'))])
    item$DD_0_at <- rowSums(item[c(which(names(item)=='DD_0_10'):which(names(item)=='DD_0_12'))])
  }
  if('DD_18' %in% var){
    item$DD_18 <- rowSums(item[c(which(names(item)=='DD_18_01'):which(names(item)=='DD_18_12'))])
    item$DD_18_wt <- rowSums(item[c(which(names(item)=='DD_18_01'):which(names(item)=='DD_18_03'))])
    item$DD_18_sm <- rowSums(item[c(which(names(item)=='DD_18_07'):which(names(item)=='DD_18_09'))])
    item$DD_18_sp <- rowSums(item[c(which(names(item)=='DD_18_04'):which(names(item)=='DD_18_06'))])
    item$DD_18_at <- rowSums(item[c(which(names(item)=='DD_18_10'):which(names(item)=='DD_18_12'))])
  }
  if('DD18' %in% var){
    item$DD18 <- rowSums(item[c(which(names(item)=='DD18_01'):which(names(item)=='DD18_12'))])
    item$DD18_wt <- rowSums(item[c(which(names(item)=='DD18_01'):which(names(item)=='DD18_03'))])
    item$DD18_sm <- rowSums(item[c(which(names(item)=='DD18_07'):which(names(item)=='DD18_09'))])
    item$DD18_sp <- rowSums(item[c(which(names(item)=='DD18_04'):which(names(item)=='DD18_06'))])
    item$DD18_at <- rowSums(item[c(which(names(item)=='DD18_10'):which(names(item)=='DD18_12'))])
  }
  if('Eref' %in% var){
    item$Eref <- rowSums(item[c(which(names(item)=='Eref01'):which(names(item)=='Eref12'))])
    item$Eref_wt <- rowSums(item[c(which(names(item)=='Eref01'):which(names(item)=='Eref03'))])
    item$Eref_sm <- rowSums(item[c(which(names(item)=='Eref07'):which(names(item)=='Eref09'))])
    item$Eref_sp <- rowSums(item[c(which(names(item)=='Eref04'):which(names(item)=='Eref06'))])
    item$Eref_at <- rowSums(item[c(which(names(item)=='Eref10'):which(names(item)=='Eref12'))])
  }
  if('CMD' %in% var){
    item$CMD <- rowSums(item[c(which(names(item)=='CMD01'):which(names(item)=='CMD12'))])
    item$CMD_wt <- rowSums(item[c(which(names(item)=='CMD01'):which(names(item)=='CMD03'))])
    item$CMD_sm <- rowSums(item[c(which(names(item)=='CMD07'):which(names(item)=='CMD09'))])
    item$CMD_sp <- rowSums(item[c(which(names(item)=='CMD04'):which(names(item)=='CMD06'))])
    item$CMD_at <- rowSums(item[c(which(names(item)=='CMD10'):which(names(item)=='CMD12'))])
  }
  if('RH' %in% var){
    item$RH <- rowMeans(item[c(which(names(item)=='RH01'):which(names(item)=='RH12'))])
    item$RH_wt <- rowMeans(item[c(which(names(item)=='RH01'):which(names(item)=='RH03'))])
    item$RH_sm <- rowMeans(item[c(which(names(item)=='RH07'):which(names(item)=='RH09'))])
    item$RH_sp <- rowMeans(item[c(which(names(item)=='RH04'):which(names(item)=='RH06'))])
    item$RH_at <- rowMeans(item[c(which(names(item)=='RH10'):which(names(item)=='RH12'))])
  }

  return(item)
}
