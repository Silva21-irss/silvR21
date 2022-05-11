#' Convert a Raster DEM file to a Useable CSV that can be inputted into ClimateNA
#'
#' @param file A TIFF file at the desired spatial resolution.
#' @param outdir The output directory for the CSV file.
#'
#' @return A CSV file representing the inputted DEM
#' @export
#' @importFrom utils write.csv
#'
#' @examples
#' #files <- list.files(pattern='*.tif$')
#' #roiDEM <- files[3]
#' #demToCSV(roiDEM)
demToCSV <- function(file,outdir = getwd()){
  ras <- raster::raster(file) # Read raster
  pts <- raster::rasterToPoints(ras,spatial=TRUE) # Convert raster to points featureclass
  raster::shapefile(pts,paste0(substr(file,1,nchar(file)-4),'.shp')) # Save shapefile

  shp <- sf::st_read(paste0(substr(file,1,nchar(file)-4),'.shp'))
  shp <- sf::st_transform(shp, "+proj=longlat +ellps=WGS84 +datum=WGS84")
  latLong <- data.frame(sf::st_coordinates(shp))
  shp$lat <- latLong$Y
  shp$long <- latLong$X
  csv <- as.data.frame(shp)
  csv <- csv[-2] # Remove point geometry layer
  csv$long <- csv$long * -1 # Make all Longitude values positive
  csv$ID1 <- rownames(csv)
  csv$ID2 <- rownames(csv) # ClimateNA won't run unless there are two rows with ID numbers, oddly enough
  csv <- csv[c(4,5,2,3,1)]
  names(csv) <- c('ID1','ID2','lat','long','el')
  write.csv(csv,paste0(outdir,'/',substr(basename(file),1,nchar(file)-4),'.csv'),row.names=FALSE)
}
