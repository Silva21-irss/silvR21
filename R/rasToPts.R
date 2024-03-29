#' List of Rasters To Shapefile Points
#'
#' Use a list of rasters in a directory (ASCII output files from ClimateNA) to produce point feature class shapefiles.
#'
#' @param files A list of raster files.
#'
#' @return Point Featureclass Shapefile
#' @export
#'
#' @author Michael Burnett - UBC Faculty of Forestry
#'
#' @examples
#' files <- list.files(pattern='*asc$') # Access all ASCII raster files in directory
#' rasToPts(files) # Produce shapefile outputs
rasToPts <- function(files){
  for(ras in files){
    r <- raster::raster(ras)
    pts <- raster::rasterToPoints(r,spatial=TRUE)
    raster::shapefile(pts,paste0(substr(ras,1,nchar(ras)-4),'.shp'))
  }
}
