#' ClimateNA Ensemble Generator
#'
#' This function uses the desired SSPs and variables to generate averaged ensembles. Its outputs will consist of a series of ensemble CSV files, each consisting of the average value for each variable under every SSP scenario and time frames. This function is only used with monthly data outputs.
#'
#' @param files The list of files provided by the projClimateNA20Y output, after using the AnnualSeasonalMeans function to process annual and seasonal averages/sums.
#' @param tFrame The variable time frames included in the climate data. If the AnnualSeasonalMeans tool has been used, then the default value is desired. Otherwise, 'M' represents monthly, 'S' represents seasonal, and 'Y' represents annual time frames.
#' @param var The desired variables to produce averages of. Variables include : 'Tmax','Tmin','Tavg','PPT','Rad','DD_0','DD5','DD18','DD_18','NFFD','CMI','PAS','Eref','CMD','RH'. Default value includes 'Tmin','Tmax','PPT','DD5_','NFFD','CMI','PAS'.
#' @param ssp The SSP scenarios. 'S1' = 'ssp126' ; 'S2' = 'ssp245' ; 'S3' = 'ssp370' ; 'S5' = 'ssp585'. Default value is c('S1','S2','S3').
#' @param years The 20-years time frames selection. Options include : 'Y1' (2001-2020); 'Y2' (2021-2040); 'Y3' (2041-2060); 'Y4' (2061-2080); 'Y5' (2081-2100).
#' @param concatenate logical. If TRUE, all of the GCM ensemble data is concatenated into one mega file and producing a second file containing the GCM emsemble area's mean for each variable.
#'
#' @return Directory with ensemble files as CSVs
#' @export
#'
#' @author Michael Burnett - UBC Faculty of Forestry
#'
#' @importFrom utils read.csv
#' @importFrom dplyr %>%
#' @importFrom utils write.csv
#' @importFrom data.table fread fwrite
#' @importFrom stringr str_extract
#'
#' @examples
#' #files <- list.files(pattern='*.csv$') # Access all CSV files
#' #ensembleGenerator(files) # Generate ensemble using the default parameters
ensembleGenerator <- function(files,tFrame = c('M','S','Y'),var = c('Tmax','Tmin','PPT','DD5','NFFD','PAS','CMI'),ssp = c('S1','S2','S3'),years = c('Y2','Y3','Y4','Y5'),concatenate=TRUE){
  #library(stringr)

  # SSPs <- list()
  # #if(is.null(ssp)){ssp <- c('S1','S2','S3')}
  # if('S1' %in% ssp){SSPs <- append(SSPs,c("ssp126_2001-2020.csv", "ssp126_2021-2040.csv", "ssp126_2041-2060.csv", "ssp126_2061-2080.csv","ssp126_2081-2100.csv"))
  # }
  # if('S2' %in% ssp){SSPs <- append(SSPs,c("ssp245_2001-2020.csv", "ssp245_2021-2040.csv", "ssp245_2041-2060.csv", "ssp245_2061-2080.csv","ssp245_2081-2100.csv"))
  # }
  # if('S3' %in% ssp){SSPs <- append(SSPs,c("ssp370_2001-2020.csv", "ssp370_2021-2040.csv", "ssp370_2041-2060.csv", "ssp370_2061-2080.csv","ssp370_2081-2100.csv"))
  # }
  # if('S5' %in% ssp){SSPs <- append(SSPs,c("ssp585_2001-2020.csv", "ssp585_2021-2040.csv", "ssp585_2041-2060.csv", "ssp585_2061-2080.csv","ssp585_2081-2100.csv"))
  # }

  outdir = getwd()
  SSPs <- list()
  #if(is.null(ssp)){ssp <- c('S1','S2','S3','S5')}
  if('S1' %in% ssp){SSPs <- append(SSPs,"ssp126")}
  if('S2' %in% ssp){SSPs <- append(SSPs,"ssp245")}
  if('S3' %in% ssp){SSPs <- append(SSPs,"ssp370")}
  if('S5' %in% ssp){SSPs <- append(SSPs,"ssp585")}

  if(is.numeric(years)){stop("Don't be silly! Use characters for your year")}

  ye <- list()
  #if(is.null(years)){years <- c('Y1','Y2','Y3','Y4','Y5')}
  if('Y1' %in% years){ye <- append(ye,"_2001-2020.csv")}
  if('Y2' %in% years){ye <- append(ye,"_2021-2040.csv")}
  if('Y3' %in% years){ye <- append(ye,"_2041-2060.csv")}
  if('Y4' %in% years){ye <- append(ye,"_2061-2080.csv")}
  if('Y5' %in% years){ye <- append(ye,"_2081-2100.csv")}
  # Run through 30 years periods
  if('Y_1' %in% years){ye <- append(ye,"_2011-2040.csv")}
  if('Y_2' %in% years){ye <- append(ye,"_2041-2070.csv")}
  if('Y_3' %in% years){ye <- append(ye,"_2071-2100.csv")}

  sce <- unique(str_extract(files,"[A-Za-z0-9-]*"))

  # Run through individual years
  for(i in years){
    if(substr(i,1,1) == "2"){
      if(as.numeric(i) < 2010){stop("Don't be silly! Must be a year after 2010")}
      if(as.numeric(i) > 2100){stop("Don't be silly! Must be a year before 2101")}
      ye <- append(ye,paste0("@",i))
      GCMs20 <- list()
      for(a in sce){ # Loop through other projection parameters
        for(b in SSPs){
          GCMs20 <- rbind(GCMs20,paste0(a,'_',b,'@',i,'.csv'))
        }
      }
    }
  }

  a <- 1
  for(s in SSPs){
    for(y in ye){
      SSPs[a] <- paste0(s,y)
      a <- a+1
    }
  }

  GCM.var <- c("ID","Latitude","Longitude","Elevation")

  #if(is.null(var)){var <- c('Tmax','Tmin','PPT','DD5_','NFFD','PAS','CMI')}
  mnts <- sprintf('%0.2d',1:12) # Make months list
  seas <- c('wt','sm','sp','at')
  if('Tmax' %in% var){
    if('M' %in% tFrame){for(a in mnts){GCM.var <- append(GCM.var,paste0('Tmax',a))}}
    if('S' %in% tFrame){for(b in seas){GCM.var <- append(GCM.var,paste0('Tmax_',b))}}
    if('Y' %in% tFrame){GCM.var <- append(GCM.var,'Tmax')}
  }
  if('Tmin' %in% var){
    if('M' %in% tFrame){for(a in mnts){GCM.var <- append(GCM.var,paste0('Tmin',a))}}
    if('S' %in% tFrame){for(b in seas){GCM.var <- append(GCM.var,paste0('Tmin_',b))}}
    if('Y' %in% tFrame){GCM.var <- append(GCM.var,'Tmin')}
  }
  if('Tave' %in% var){
    if('M' %in% tFrame){for(a in mnts){GCM.var <- append(GCM.var,paste0('Tave',a))}}
    if('S' %in% tFrame){for(b in seas){GCM.var <- append(GCM.var,paste0('Tave_',b))}}
    if('Y' %in% tFrame){GCM.var <- append(GCM.var,'Tave')}
  }
  if('PPT' %in% var){
    if('M' %in% tFrame){for(a in mnts){GCM.var <- append(GCM.var,paste0('PPT',a))}}
    if('S' %in% tFrame){for(b in seas){GCM.var <- append(GCM.var,paste0('PPT_',b))}}
    if('Y' %in% tFrame){GCM.var <- append(GCM.var,'PPT')}
  }
  if('DD5' %in% var){
    if('M' %in% tFrame){for(a in mnts){GCM.var <- append(GCM.var,paste0('DD5_',a))}}
    if('S' %in% tFrame){for(b in seas){GCM.var <- append(GCM.var,paste0('DD5_',b))}}
    if('Y' %in% tFrame){GCM.var <- append(GCM.var,'DD5')}
  }
  if('NFFD' %in% var){
    if('M' %in% tFrame){for(a in mnts){GCM.var <- append(GCM.var,paste0('NFFD',a))}}
    if('S' %in% tFrame){for(b in seas){GCM.var <- append(GCM.var,paste0('NFFD_',b))}}
    if('Y' %in% tFrame){GCM.var <- append(GCM.var,'NFFD')}
  }
  if('PAS' %in% var){
    if('M' %in% tFrame){for(a in mnts){GCM.var <- append(GCM.var,paste0('PAS',a))}}
    if('S' %in% tFrame){for(b in seas){GCM.var <- append(GCM.var,paste0('PAS_',b))}}
    if('Y' %in% tFrame){GCM.var <- append(GCM.var,'PAS')}
  }
  if('CMI' %in% var){
    if('M' %in% tFrame){for(a in mnts){GCM.var <- append(GCM.var,paste0('CMI',a))}}
    if('S' %in% tFrame){for(b in seas){GCM.var <- append(GCM.var,paste0('CMI_',b))}}
    if('Y' %in% tFrame){GCM.var <- append(GCM.var,'CMI')}
  }
  if('Rad' %in% var){
    if('M' %in% tFrame){for(a in mnts){GCM.var <- append(GCM.var,paste0('Rad',a))}}
    if('S' %in% tFrame){for(b in seas){GCM.var <- append(GCM.var,paste0('Rad_',b))}}
    if('Y' %in% tFrame){GCM.var <- append(GCM.var,'Rad')}
  }
  if('DD_0' %in% var){
    if('M' %in% tFrame){for(a in mnts){GCM.var <- append(GCM.var,paste0('DD_0_',a))}}
    if('S' %in% tFrame){for(b in seas){GCM.var <- append(GCM.var,paste0('DD_0_',b))}}
    if('Y' %in% tFrame){GCM.var <- append(GCM.var,'DD_0')}
  }
  if('DD_18' %in% var){
    if('M' %in% tFrame){for(a in mnts){GCM.var <- append(GCM.var,paste0('DD_18_',a))}}
    if('S' %in% tFrame){for(b in seas){GCM.var <- append(GCM.var,paste0('DD_18_',b))}}
    if('Y' %in% tFrame){GCM.var <- append(GCM.var,'DD_18')}
  }
  if('DD18' %in% var){
    if('M' %in% tFrame){for(a in mnts){GCM.var <- append(GCM.var,paste0('DD18_',a))}}
    if('S' %in% tFrame){for(b in seas){GCM.var <- append(GCM.var,paste0('DD18_',b))}}
    if('Y' %in% tFrame){GCM.var <- append(GCM.var,'DD18')}
  }
  if('Eref' %in% var){
    if('M' %in% tFrame){for(a in mnts){GCM.var <- append(GCM.var,paste0('Eref',a))}}
    if('S' %in% tFrame){for(b in seas){GCM.var <- append(GCM.var,paste0('Eref_',b))}}
    if('Y' %in% tFrame){GCM.var <- append(GCM.var,'Eref')}
  }
  if('CMD' %in% var){
    if('M' %in% tFrame){for(a in mnts){GCM.var <- append(GCM.var,paste0('CMD',a))}}
    if('S' %in% tFrame){for(b in seas){GCM.var <- append(GCM.var,paste0('CMD_',b))}}
    if('Y' %in% tFrame){GCM.var <- append(GCM.var,'CMD')}
  }
  if('RH' %in% var){
    if('M' %in% tFrame){for(a in mnts){GCM.var <- append(GCM.var,paste0('RH',a))}}
    if('S' %in% tFrame){for(b in seas){GCM.var <- append(GCM.var,paste0('RH_',b))}}
    if('Y' %in% tFrame){GCM.var <- append(GCM.var,'RH')}
  }


  for(i in SSPs){
    ssp.list = list()
    for(x in files){ # Find all the gcms with that specific SSP scenario
      x.str <- stringr::str_remove(x,'^.+?(?=ssp)')
      if(x.str == i){
        ssp.list <- rbind(ssp.list,x) # Get all GCMs with the same scenario
      }
    }
    myfiles <- lapply(ssp.list,data.table::fread) # Select the relevant GCMs
    list.length <- length(myfiles)
    #print(ssp.list)
    for(a in 1:list.length){
      m_files <- myfiles[[a]]
      GCM.sub <- m_files %>% dplyr::select(GCM.var[2:length(GCM.var)])
      myfiles[[a]] <- GCM.sub # Make changes for all data frames
    }
    names(myfiles[[1]])
    for(x in colnames(myfiles[[1]])[4:ncol(myfiles[[1]])]){
      GCM <- myfiles[[1]][,1:3] # Use the first 4 columns and save them
      for(y in 1:nrow(myfiles[[1]])){
        px <- list()
        for(a in 1:list.length){
          my_file <- myfiles[[a]]
          data.table::setDF(my_file)
          px = rbind(px,my_file[y,x]) # Add all pixel values for an indexed location into a list
        }
        GCM$Value[y] <- unlist(px) %>% mean() # Add mean pixel values into ensemble
      }
      dir.create(paste0(getwd(),'/GCMensemble'),showWarnings = FALSE)
      data.table::fwrite(GCM,paste0(outdir,'/GCMensemble/',as.character(list.length),'GCMensemble_',x,'_',i))
      gc()
      rm(GCM)
    }
  }
  if(isTRUE(concatenate)){
    setwd(paste0(getwd(),'/GCMensemble'))
    files <- list.files(pattern='*csv$')

    # Create data frame for all data
    data <- data.frame(matrix(ncol = length(files)+3,nrow = nrow(data.table::fread(files[1]))))
    n <- c('Latitude','Longitude','Elevation',files) # Add column names
    n <- gsub('.csv','',n) # Remove .csv from column names
    names(data) <- n

    data[c(1:4)] <- data.table::fread(files[1])
    #names(data)[4] <- gsub('.csv','',files[1])
    for(i in c(2:length(files))){
      item <- data.table::fread(files[i])
      if('Value' %in% names(item)){data[i+3] <- item$Value}
      else{stop("Don't be all silly and change the names of your raw climate data columns. The value column MUST be named 'Value'")}
      #names(data)[i+4] <- gsub('.csv','',files[i])
    }
    setwd('..') # Go back one folder
    data.table::fwrite(data,paste0(as.character(list.length),'GCMensemble_Megafile.csv'))

    # Create area mean file
    areaMean <- data.frame(names(data))
    names(areaMean) <- 'Scenario'
    areaMean$Mean <- colMeans(data)
    data <- areaMean[c(5:nrow(areaMean)),]

    for(i in 1:nrow(data)){
      a <- data$Scenario[i]
      data$SSP[i] <- substr(a,nchar(a)-15,nchar(a)-10)
      data$timeFrame[i] <- substr(a,nchar(a)-8,nchar(a))
      data$variable[i] <- substr(a,14,nchar(a)-17)
    }
    unique_sce <- unique(paste(data$SSP,data$timeFrame,sep='_'))
    data_tbl <- data.frame(unique_sce)
    var <- unique(data$variable)
    for(i in 1:length(var)){
      data_tbl[i+1] <- NA
      names(data_tbl)[i+1] <- var[i]
    }
    for(a in 2:ncol(data_tbl)){
      d_val <- names(data_tbl)[a]
      for(b in 1:nrow(data_tbl)){
        d_sce <- data_tbl$unique_sce[b]
        for(c in 1:nrow(data)){
          it <- data$Scenario[c]
          if(d_sce == substr(it,nchar(it)-15,nchar(it)) && d_val == substr(it,14,nchar(it)-17)){
            data_tbl[b,a] <- data$Mean[c]
          }
        }
      }
    }

    data.table::fwrite(data_tbl,paste0(as.character(list.length),'GCMensemble_areaMean.csv'))
  }
}
