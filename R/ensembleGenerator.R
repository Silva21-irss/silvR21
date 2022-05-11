#' ClimateNA Ensemble Generator
#'
#' @param files The list of files provided by the projClimateNA20Y output, after using the AnnualSeasonalMeans function to process annual and seasonal averages/sums.
#' @param outdir The output directory to store the ensembles. This does not need to be the current working directory.
#' @param var The desired variables to produce averages of. Variables include : 'Tmax','Tmin','Tavg','PPT','Rad','DD_0','DD5','DD18','DD_18','NFFD','CMI','PAS','Eref','CMD','RH'. Default value includes 'Tmin','Tmax','PPT','DD5_','NFFD','CMI','PAS'.
#' @param ssp The SSP scenarios. 'S1' = 'ssp126' ; 'S2' = 'ssp245' ; 'S3' = 'ssp370' ; 'S5' = 'ssp585'. Default value is c('S1','S2','S3').
#' @param years The 20-years time frames selection. Options include : 'Y1' (2001-2020); 'Y2' (2021-2040); 'Y3' (2041-2060); 'Y4' (2061-2080); 'Y5' (2081-2100).
#' @param concatenate logical. If TRUE, all of the GCM ensemble data is concatenated into one mega file and producing a second file containing the GCM emsemble area's mean for each variable.
#'
#' @return Directory with ensemble files as CSVs
#' @export
#'
#' @importFrom utils read.csv
#' @importFrom dplyr %>%
#' @importFrom utils write.csv
#'
#' @examples
#' #files <- list.files(pattern='*.csv$') # Access all CSV files
#' #ensembleGenerator(files) # Generate ensemble using the default parameters
ensembleGenerator <- function(files,outdir = getwd(),var = c('Tmax','Tmin','PPT','DD5','NFFD','PAS','CMI'),ssp = c('S1','S2','S3'),years = c('Y2','Y3','Y4','Y5'),concatenate=TRUE){
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
  SSPs <- list()
  #if(is.null(ssp)){ssp <- c('S1','S2','S3','S5')}
  if('S1' %in% ssp){SSPs <- append(SSPs,"ssp126")}
  if('S2' %in% ssp){SSPs <- append(SSPs,"ssp245")}
  if('S3' %in% ssp){SSPs <- append(SSPs,"ssp370")}
  if('S5' %in% ssp){SSPs <- append(SSPs,"ssp585")}

  ye <- list()
  #if(is.null(years)){years <- c('Y1','Y2','Y3','Y4','Y5')}
  if('Y1' %in% years){ye <- append(ye,"2001-2020.csv")}
  if('Y2' %in% years){ye <- append(ye,"2021-2040.csv")}
  if('Y3' %in% years){ye <- append(ye,"2041-2060.csv")}
  if('Y4' %in% years){ye <- append(ye,"2061-2080.csv")}
  if('Y5' %in% years){ye <- append(ye,"2081-2100.csv")}

  a <- 1
  for(s in SSPs){
    for(y in ye){
      SSPs[a] <- paste(s,y,sep='_')
      a <- a+1
    }
  }

  GCM.var <- c("ID","Latitude","Longitude","Elevation")
  #if(is.null(var)){var <- c('Tmax','Tmin','PPT','DD5_','NFFD','PAS','CMI')}
  if('Tmax' %in% var){GCM.var <- append(GCM.var,c("Tmax01","Tmax02","Tmax03","Tmax04","Tmax05","Tmax06","Tmax07","Tmax08","Tmax09","Tmax10","Tmax11","Tmax12","ANNUAL_Tmax","WINTER_Tmax","SUMMER_Tmax","SPRING_Tmax","AUTUMN_Tmax"))
  }
  if('Tmin' %in% var){GCM.var <- append(GCM.var,c("Tmin01","Tmin02","Tmin03","Tmin04","Tmin05","Tmin06","Tmin07","Tmin08","Tmin09","Tmin10","Tmin11","Tmin12","ANNUAL_Tmin","WINTER_Tmin","SUMMER_Tmin","SPRING_Tmin","AUTUMN_Tmin"))
  }
  if('Tave' %in% var){GCM.var <- append(GCM.var,c("Tave01","Tave02","Tave03","Tave04","Tave05","Tave06","Tave07","Tave08","Tave09","Tave10","Tave11","Tave12","ANNUAL_Tave","WINTER_Tave","SUMMER_Tave","SPRING_Tave","AUTUMN_Tave"))
  }
  if('PPT' %in% var){GCM.var <- append(GCM.var,c("PPT01","PPT02","PPT03","PPT04","PPT05","PPT06","PPT07","PPT08","PPT09","PPT10","PPT11","PPT12","ANNUAL_PPT","WINTER_PPT","SUMMER_PPT","SPRING_PPT","AUTUMN_PPT"))
  }
  if('DD5' %in% var){GCM.var <- append(GCM.var,c("DD5_01","DD5_02","DD5_03","DD5_04","DD5_05","DD5_06","DD5_07","DD5_08","DD5_09","DD5_10","DD5_11","DD5_12","ANNUAL_DD5","WINTER_DD5","SUMMER_DD5","SPRING_DD5","AUTUMN_DD5"))
  }
  if('NFFD' %in% var){GCM.var <- append(GCM.var,c("NFFD01","NFFD02","NFFD03","NFFD04","NFFD05","NFFD06","NFFD07","NFFD08","NFFD09","NFFD10","NFFD11","NFFD12","ANNUAL_NFFD","WINTER_NFFD","SUMMER_NFFD","SPRING_NFFD","AUTUMN_NFFD"))
  }
  if('PAS' %in% var){GCM.var <- append(GCM.var,c("PAS01","PAS02","PAS03","PAS04","PAS05","PAS06","PAS07","PAS08","PAS09","PAS10","PAS11","PAS12","ANNUAL_PAS","WINTER_PAS","SUMMER_PAS","SPRING_PAS","AUTUMN_PAS"))
  }
  if('CMI' %in% var){GCM.var <- append(GCM.var,c("CMI01","CMI02","CMI03","CMI04","CMI05","CMI06","CMI07","CMI08","CMI09","CMI10","CMI11","CMI12","ANNUAL_CMI","WINTER_CMI","SUMMER_CMI","SPRING_CMI","AUTUMN_CMI"))
  }
  mnts <- sprintf('%0.2d',1:12) # Make months list
  seas <- c('ANNUAL_','WINTER_','SPRING_','SUMMER_','AUTUMN_')
  if('Rad' %in% var){
    for(a in mnts){GCM.var <- append(GCM.var,paste0('Rad_',a))}
    for(b in seas){GCM.var <- append(GCM.var,paste0(b,'Rad'))}
  }
  if('DD_0' %in% var){
    for(a in mnts){GCM.var <- append(GCM.var,paste0('DD_0_',a))}
    for(b in seas){GCM.var <- append(GCM.var,paste0(b,'DD_0'))}
  }
  if('DD_18' %in% var){
    for(a in mnts){GCM.var <- append(GCM.var,paste0('DD_18_',a))}
    for(b in seas){GCM.var <- append(GCM.var,paste0(b,'DD_18'))}
  }
  if('DD18' %in% var){
    for(a in mnts){GCM.var <- append(GCM.var,paste0('DD18_',a))}
    for(b in seas){GCM.var <- append(GCM.var,paste0(b,'DD18'))}
  }
  if('Eref' %in% var){
    for(a in mnts){GCM.var <- append(GCM.var,paste0('Eref_',a))}
    for(b in seas){GCM.var <- append(GCM.var,paste0(b,'Eref'))}
  }
  if('CMD' %in% var){
    for(a in mnts){GCM.var <- append(GCM.var,paste0('CMD_',a))}
    for(b in seas){GCM.var <- append(GCM.var,paste0(b,'CMD'))}
  }
  if('RH' %in% var){
    for(a in mnts){GCM.var <- append(GCM.var,paste0('RH_',a))}
    for(b in seas){GCM.var <- append(GCM.var,paste0(b,'RH'))}
  }


  for(i in SSPs){
    ssp.list = list()
    for(x in files){ # Find all the gcms with that specific SSP scenario
      x.str <- stringr::str_remove(x,'^.+?(?=ssp)')
      if(x.str == i){
        ssp.list <- rbind(ssp.list,x) # Get all GCMs with the same scenario
      }
    }
    myfiles <- lapply(ssp.list,read.csv) # Select the relevant GCMs
    list.length <- length(myfiles)
    #print(ssp.list)
    for(a in 1:list.length){
      m_files <- myfiles[[a]]
      GCM.sub <- m_files %>% dplyr::select(GCM.var[2:length(GCM.var)])
      myfiles[[a]] <- GCM.sub # Make changes for all data frames
    }
    names(myfiles[[1]])
    for(x in 4:ncol(myfiles[[1]])){
      GCM <- myfiles[[1]][1:3] # Use the first 4 columns and save them
      for(y in 1:nrow(myfiles[[1]])){
        px <- list()
        for(a in 1:list.length){
          px = rbind(px,myfiles[[a]][y,x]) # Add all pixel values for an indexed location into a list
        }
        GCM$Value[y] <- unlist(px) %>% mean() # Add mean pixel values into ensemble
      }
      dir.create(paste0(getwd(),'/GCMensemble'),showWarnings = FALSE)
      write.csv(GCM,paste0(outdir,'/GCMensemble/',as.character(list.length),'GCMensemble_',names(myfiles[[1]])[x],'_',i))
      gc()
      rm(GCM)
    }
  }
  if(isTRUE(concatenate)){
    setwd(paste0(getwd(),'/GCMensemble'))
    files <- list.files(pattern='*csv$')

    data <- read.csv(files[1])
    names(data)[5] <- gsub('.csv','',files[1])
    for(i in 2:length(files)){
      item <- read.csv(files[i])
      data[i+4] <- item[5]
      names(data)[i+4] <- gsub('.csv','',files[i])
    }
    setwd('..') # Go back one folder
    write.csv(data,paste0(as.character(list.length),'GCMensemble_Megafile.csv'))

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

    write.csv(data_tbl,paste0(as.character(list.length),'GCMensemble_areaMean.csv'))
  }
}
