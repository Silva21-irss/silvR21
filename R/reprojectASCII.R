#' Reproject ASCII raster file
#'
#' @param file ASCII file that requires reprojection
#' @param direc The working directory. This needs to be the same directory as the file location so the basename can be drawn and the reprojected ASCII file can be written.
#' @param NAflag NA value within the raster to mask when reprojecting.
#' @param projection The output projection in which the ASCII input requires to be reprojected to. The default value is "EPSG:4326"
#'
#' @return Same ASCII file reprojected
#' @export
#'
#' @importFrom terra rast
#' @importFrom terra writeRaster
#'
#'
#' @examples
#' files <- list.files(pattern='*asc$')
#' for(i in files){
#'   reprojectASCII(i,NAflag = 32647)
#' }
reprojectASCII <- function(file,direc=getwd(),NAflag=-9999,projection = "EPSG:4326"){

  name <- tools::file_path_sans_ext(basename(file)) #this gets the filename (useful for naming outputs)

  raster <- rast(file) #load your .asc raster

  #crs(r) <- "EPSG:3005" Set the original crs if needed

  raster_reprojected <- terra::project(raster,projection) #this takes the raster i from the list and reporjects it to what ever EPSG code you need

  raster_reprojected[is.na(raster_reprojected)] <- NAflag # if needed this sets NA values to -9999 that are then returned into NA values by the NAflaG argument

  writeRaster(raster_reprojected, paste0(direc,'/',name,"_reprojected.asc"), overwrite=TRUE, NAflag = NAflag) #<- remove NAflag if not needed
}
