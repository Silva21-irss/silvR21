#' Convert a Raster DEM file to a Useable CSV that can be inputted into ClimateNA
#'
#' Input a TIFF file containing the DEM elevation values clipped to the area of focus. This tool will convert and produce a point featureclass shapefile with an identical name and use that shapefile's points and point coordinates to produce a CSV file. This CSV file is formatted so it can be inputted into ClimateNA.
#'
#' @param file A TIFF file at the desired spatial resolution.
#' @param outdir The output location for the formatted CSV file. The default is the same location as input.
#' @param srs string. A valid WKT string or SRS definition, such as "EPSG:4326" or "ESRI:102761" or NULL
#'
#' @return A CSV file representing the inputted DEM
#' @export
#' @importFrom utils write.csv
#' @importFrom data.table fwrite
#'
#' @author Michael Burnett - UBC Faculty of Forestry
#'
#' @examples
#' #files <- list.files(pattern='*.tif$')
#' #roiDEM <- files[3]
#' #demToCSV(roiDEM)
demToCSV <- function(file,outdir = dirname(file), srs = "EPSG:4326"){
  fileN <- basename(file)
  ras <- raster::raster(file) # Read raster
  r <- raster::projectRaster(ras,crs=sp::CRS(SRS_string = srs))
  r_df <- raster::as.data.frame(r, xy = T, na.rm=  T)
  # pts <- raster::rasterToPoints(ras,spatial=TRUE) # Convert raster to points featureclass
  # raster::shapefile(pts,paste0(substr(file,1,nchar(file)-4),'.shp'),overwrite=TRUE) # Save shapefile
  #
  # shp <- sf::st_read(paste0(substr(file,1,nchar(file)-4),'.shp'))
  # shp <- sf::st_transform(shp, "+proj=longlat +ellps=WGS84 +datum=WGS84")
  # latLong <- data.frame(sf::st_coordinates(shp))
  # shp$lat <- latLong$Y
  # shp$long <- latLong$X
  # csv <- as.data.frame(shp)
  r_df$x <- r_df$x * -1
  r_df$ID1 <- rownames(r_df)
  r_df$ID2 <- rownames(r_df) # A second ID column must be made to produce the climate variables
  # csv <- csv[-2] # Remove point geometry layer
  # csv$long <- csv$long * -1 # Make all Longitude values positive
  # csv$ID1 <- rownames(csv)
  # csv$ID2 <- rownames(csv) # ClimateNA won't run unless there are two rows with ID numbers, oddly enough
  # csv <- csv[c(4,5,2,3,1)]
  csv <- r_df[c(4,5,2,1,3)]
  names(csv) <- c('ID1','ID2','lat','long','el')
  fwrite(csv,paste0(outdir,'/',substr(fileN,1,nchar(fileN)-4),'.csv'),row.names=FALSE)
}
