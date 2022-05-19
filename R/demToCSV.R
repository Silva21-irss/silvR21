#' Convert a Raster DEM file to a Useable CSV that can be inputted into ClimateNA
#'
#' Input a TIFF file containing the DEM elevation values clipped to the area of focus. This tool will convert and produce a point featureclass shapefile with an identical name and use that shapefile's points and point coordinates to produce a CSV file. This CSV file is formatted so it can be inputted into ClimateNA.
#'
#' @param file A TIFF file at the desired spatial resolution.
#' @param outdir The output location for the formatted CSV file. The default is the same location as input.
#'
#' @return A CSV file representing the inputted DEM
#' @export
#' @importFrom utils write.csv
#'
#' @author Michael Burnett - UBC Faculty of Forestry
#'
#' @examples
#' #files <- list.files(pattern='*.tif$')
#' #roiDEM <- files[3]
#' #demToCSV(roiDEM)
demToCSV <- function(file,outdir = dirname(file)){
  fileN <- basename(file)
  ras <- raster::raster(file) # Read raster
  pts <- raster::rasterToPoints(ras,spatial=TRUE) # Convert raster to points featureclass
  raster::shapefile(pts,paste0(substr(file,1,nchar(file)-4),'.shp'),overwrite=TRUE) # Save shapefile

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
  write.csv(csv,paste0(outdir,'/',substr(fileN,1,nchar(fileN)-4),'.csv'),row.names=FALSE)
}
