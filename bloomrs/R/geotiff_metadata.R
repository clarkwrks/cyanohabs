library(raster)

#' Read geotiff raster brick from file and reattach layer z-values from metadata file.
#'
#' @param gtiff.path filename (character) of raster brick stored in geotiff format.
#' @param meta.path optional. Filename (character) of geotiff metadata. Default is paste0(\code{gtiff.brick}, ".xeta")
#' @return \code{gtiff.path} with z values from \code{meta.path}
readGTiffBrick <- function(gtiff.path, meta.path = NULL){
  if(is.null(meta.path)){
    meta.path <- paste0(gtiff.path, ".xeta")
  }
  if(!file.exists(meta.path)) {
    stop(paste0("Error attaching metadata: ", meta.path, " not found."))
  }
  meta.dates <- read.csv(meta.path)$bin.start.date
  gtiff.brick <- raster::brick(gtiff.path)
  if(length(meta.dates) != raster::nlayers(gtiff.brick)){
    stop("Error attaching metadata: nlayers(brick) != length(dates)")
  }
  gtiff.brick.out <- setZ(gtiff.brick, as.Date(meta.dates))
  return(gtiff.brick.out)
}

#' Wrapper for raster::writeRaster which appends an additional meta (".xeta") file.
#'
#' @param gtiff geotiff raster brick object
#' @param filename output filename
#' @param metadata data.frame
#' @param ... additional arguments to be passed to raster::writeRaster
writeGTiffBrick <- function(gtiff, filename, meta.df, ...){
  print("Just kidding... under construction")
}
copyGTiffMeta <- function(source.tiff, dest.tiff){
  source.meta <- read.csv(paste0(filename(source.tiff), ".xeta"))
  write.csv(source.meta, paste0(filename(dest.tiff), ".xeta"))
}


