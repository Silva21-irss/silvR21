#' Access 20 Years Average Climate Projections from ClimateNA
#'
#' This is a specialty function that has several preliminary requirements for accessing climate projections within a given site. The output produces the projected climate variables for the provided CSV file. In order to properly use this function, the following steps much be taken. The steps include : 1. Set the working directory to where the ClimateNA app is located on your computer. 2. Prepare the CSV file so that it is compatible with the ClimateNA requirements. 3. Relocate the CSV file so it is located within the ClimateNA app directory.
#'
#' @param file The prepared DEM as a CSV file. This file must be located within the same directory which the ClimateNA application is located.
#' @param tFrame The averaged time frame of each climate variable. Use 'M' for monthly, 'Y' for annual, and 'S' for seasonal
#' @param exe File name of the ClimateNA exe file. This must be located within the current working directory.
#' @param scen The GCM selection. Options include : '8GCM' (8GCMs_ensemble); '13GCM' (13GCMs_ensemble); 'ACC' (ACCESS-ESM1-5); 'BCC' (BCC-CSM2-MR); 'CAN' (CanESM5); 'CNRM' (CNRM-ESM2-1); 'EC' (EC-Earth3); 'GFDL' (GFDL-ESM4); 'GISS' (GISS-E2-1-G); 'INM' (INM-CM5-0); 'ISPL' (IPSL-CM6A-LR); 'MIR' (MIROC6); 'MPI' (MPI-ESM1-2-HR); 'MRI' (MRI-ESM2-0); 'UK' (UKESM1-0-LL). As default, only the scenarios used by Amy Wotherspoon for Silva21 are selected.
#' @param ssp The SSP scenario selection. Options include : 'S1' (ssp126); 'S2' (ssp245); 'S3' (ssp370); 'S5' (ssp585).
#' @param years The 20-years time frames selection. Options include : 'Y1' (2001-2020); 'Y2' (2021-2040); 'Y3' (2041-2060); 'Y4' (2061-2080); 'Y5' (2081-2100).
#'
#' @return CSV files within a new folder within the current directory
#' @export
#'
#' @author Michael Burnett - UBC Faculty of Forestry
#'
#' @examples
#' rm(list=ls())
#' #setwd("E:/Climatena_v721");getwd() # Set up location for the application
#' #exe <- "E:/Climatena_v721/ClimateNA_v7.21.exe"
#'
#' #files <- list.files(pattern = '*.csv$')
#'
#' ## Access specific climate scenarios
#' #projClimateNA20Y(files[1],'M',exe,c('ACC','BCC','MRI','UK'),c('S2','S3'))
#' #projClimateNA20Y(files[2],'M',exe,getwd()) # Access all climate scenarios
projClimateNA20Y <- function(file,tFrame,exe,scen = c('ACC','CNRM','EC','GFDL','GISS','MIR','MPI','MRI','UK'),ssp = c('S1','S2','S3'),years = c('Y2','Y3','Y4','Y5')){
  direc = dirname(exe)
  file = basename(file)
  GCMs20 <- list.files(path=paste0(direc,'/GCMdat/20YearPeriod'))

  sce <- list()
  #if(is.null(scen)){scen <- c('13GCM','ACC','BCC','CAN','CNRM','EC','GFDL','GISS','INM','IPSL','MIR','MPI','MRI','UK')}
  if('13GCM' %in% scen){sce <- append(sce, '13GCMs_ensemble')}
  if('ACC' %in% scen){sce <- append(sce, 'ACCESS-ESM1-5')}
  if('BCC' %in% scen){sce <- append(sce,"BCC-CSM2-MR")}
  if('CAN' %in% scen){sce <- append(sce,"CanESM5")}
  if('CNRM' %in% scen){sce <- append(sce,"CNRM-ESM2-1")}
  if('EC' %in% scen){sce <- append(sce,"EC-Earth3")}
  if('GFDL' %in% scen){sce <- append(sce,"GFDL-ESM4")}
  if('GISS' %in% scen){sce <- append(sce,"GISS-E2-1-G")}
  if('INM' %in% scen){sce <- append(sce,"INM-CM5-0")}
  if('IPSL' %in% scen){sce <- append(sce,"IPSL-CM6A-LR")}
  if('MIR' %in% scen){sce <- append(sce,"MIROC6")}
  if('MPI' %in% scen){sce <- append(sce,"MPI-ESM1-2-HR")}
  if('MRI' %in% scen){sce <- append(sce,"MRI-ESM2-0")}
  if('UK' %in% scen){sce <- append(sce,"UKESM1-0-LL")}

  SSPs <- list()
  #if(is.null(ssp)){ssp <- c('S1','S2','S3','S5')}
  if('S1' %in% ssp){SSPs <- append(SSPs,"ssp126")}
  if('S2' %in% ssp){SSPs <- append(SSPs,"ssp245")}
  if('S3' %in% ssp){SSPs <- append(SSPs,"ssp370")}
  if('S5' %in% ssp){SSPs <- append(SSPs,"ssp585")}

  ye <- list()
  #if(is.null(years)){years <- c('Y1','Y2','Y3','Y4','Y5')}
  if('Y1' %in% years){ye <- append(ye,"2001-2020")}
  if('Y2' %in% years){ye <- append(ye,"2021-2040")}
  if('Y3' %in% years){ye <- append(ye,"2041-2060")}
  if('Y4' %in% years){ye <- append(ye,"2061-2080")}
  if('Y5' %in% years){ye <- append(ye,"2081-2100")}

  # Sieve through the GCMs20 list and keep only the relevant GCMs
  sce <- paste(sce,collapse = '|')
  GCMs20 <- grep(pattern = sce,x = GCMs20,value = TRUE)
  SSPs <- paste(SSPs, collapse = '|')
  GCMs20 <- grep(pattern = SSPs,x = GCMs20,value = TRUE)
  ye <- paste(ye,collapse = '|')
  GCMs20 <- grep(pattern = ye,x = GCMs20,value = TRUE)

  fileType = substr(file,nchar(file)-2,nchar(file))

  #GCMs20 <- GCMs20[-c(1:20,36:80,96:100,116:120,136:140,156:200,216:220,236:240,256:260,276:280)]
  inputFile <- paste0('/',gsub('/','\\\\',direc),'\\',file)
  dir.create(paste0(gsub('/','\\\\',direc),'\\',substr(file,1,nchar(file)-4)),showWarnings = FALSE)
  for(i in 1:length(GCMs20)){GCMs20[i] <- paste0('/',GCMs20[i])} # Get the names of all 280 gcm files
  for(i in GCMs20){ # Produce all future climate projections
    if(fileType == 'csv'){outputFile = paste0('/',gsub('/','\\\\',direc),'\\',substr(file,1,nchar(file)-4),'\\',substr(i,2,nchar(i)-4),'.csv')} # Output location
    else{outputFile = paste0('/',gsub('/','\\\\',direc),'\\',substr(file,1,nchar(file)-4),'\\',substr(i,2,nchar(i)-4),'\\')} # Output location
    system2(exe,args= c(paste0('/',tFrame), i, inputFile, outputFile)) # Use '/M' for monthly data
    gc()
  }
}
