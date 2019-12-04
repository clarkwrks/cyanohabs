library(raster)

#' Prepare a raster brick from a list of rasters.
#' 
#' @param raster.list list of file paths to rasters with uniform resolution and extent.
#' @param raster.dates vector of class Date and length(raster.list)
#' @param output.prefix prefix for output files (eg "fl", "oh_cicyano")
#' @param input.crs optional CRS projection string of input rasters
#' @param output.crs optional CRS projection string for output rasters
#' @param shapefile or NA raster for masking raster brick
#' @param reclass.matrix matrix of values for reclassifying raster brick, see raster::reclassify
#' @examples 
#' nhd.wb <- shapefile("data/nhd_wb_subset")
#' # crs string for albers_equal_area, named "USAContiguousAlbersEqualAreaConicUSGS" in arcGIS
#' proj.albers <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")
#' # matrix used by reclassify() to remove all non index values (nodata, cloud, land)
#' cyano.reclass <- c(-Inf, 0, NA, 250, Inf, NA)
#' 
#' fl.list <- list.files(path = "data/cicyano/fl_cicyano/", pattern = '.tif$', full.names = TRUE, recursive = FALSE)
#' fl.dates <- as.Date(substr(basename(fl.list), 9, 15), format="%Y%j")
#' fl.footprint <- as(extent(raster(fl.list[[1]])), 'SpatialPolygons')
#' crs(fl.footprint) <- crs(raster(fl.list[[1]]))
#' fl.footprint <- spTransform(fl.footprint, proj.albers)
#' fl.wb <- crop(nhd.wb, region.footprint)
#' fl.cicyano <- PrepRasterBrick(fl.list, fl.dates, "fl_cicyano", output.crs = proj.albers, reclass.matrix = cyano.reclass)
#' 
PrepRasterBrick <- function(raster.list, raster.dates, output.prefix, input.crs = NULL, output.crs = NULL, raster.mask = NULL, reclass.matrix = NULL){
  print(paste0(Sys.time(), ": creating raster brick from file list"))
  raster.brick <- brick(stack(raster.list))
  if(!is.null(reclass.matrix)){
    print(paste0(Sys.time(), ": reclassifying raster brick"))
    raster.brick <- reclassify(raster.brick, reclass.matrix, NAflag = 255)
  }
  if(!is.null(output.crs)){
    print(paste0(Sys.time(), ": projecting raster brick"))
    raster.brick <- projectRaster(raster.brick, res = 300, crs = output.crs)
  }
  print(paste0(Sys.time(), ": setting raster brick z-values"))
  if(!is.null(raster.mask)){
    raster.brick <- raster::mask(raster.brick, raster.mask)
  }
  names(raster.brick) <- basename(raster.list)
  raster.brick <- setZ(raster.brick, as.Date(raster.dates))
  print(paste0(Sys.time(), ": writing raster brick to disk"))
  raster.brick <- writeRaster(raster.brick, output.prefix, "raster", NAflag = 255, overwrite = TRUE, datatype = 'INT1U')
}



# # 
# nhd.wb <- shapefile("data/nhd_wb_subset")
# # crs string for albers_equal_area, named "USAContiguousAlbersEqualAreaConicUSGS" in arcGIS
# proj.albers <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")
# # matrix used by reclassify() to remove all non index values (nodata, cloud, land)
# cyano.reclass <- c(-Inf, 0, NA, 250, Inf, NA)
# 
# fl.list <- list.files(path = "data/cicyano/fl_cicyano/", pattern = '.tif$', full.names = TRUE, recursive = FALSE)
# fl.dates <- as.Date(substr(basename(fl.list), 9, 15), format="%Y%j")
# fl.footprint <- as(extent(raster(fl.list[[1]])), 'SpatialPolygons')
# crs(fl.footprint) <- crs(raster(fl.list[[1]]))
# fl.footprint <- spTransform(fl.footprint, proj.albers)
# fl.wb <- crop(nhd.wb, region.footprint)
# fl.cicyano <- PrepRasterBrick(fl.list, fl.dates, "fl_cicyano", output.crs = proj.albers, reclass.matrix = cyano.reclass)
# 
# ne.list <- list.files(path = "data/cicyano/ne_cicyano/", pattern = '.tif$', full.names = TRUE, recursive = FALSE)
# ne.dates <- as.Date(substr(basename(ne.list), 9, 15), format="%Y%j")
# ne.footprint <- as(extent(raster(ne.list[[1]])), 'SpatialPolygons')
# crs(ne.footprint) <- crs(raster(ne.list[[1]]))
# ne.footprint <- spTransform(ne.footprint, proj.albers)
# ne.wb <- crop(nhd.wb, region.footprint)
# ne.cicyano <- PrepRasterBrick(ne.list, ne.dates, "ne_cicyano", output.crs = proj.albers, reclass.matrix = cyano.reclass)
# 
# oh.list <- list.files(path = "data/cicyano/oh_cicyano/", pattern = '.tif$', full.names = TRUE, recursive = FALSE)
# oh.dates <- as.Date(substr(basename(oh.list), 9, 15), format="%Y%j")
# oh.footprint <- as(extent(raster(oh.list[[1]])), 'SpatialPolygons')
# crs(oh.footprint) <- crs(raster(oh.list[[1]]))
# oh.footprint <- spTransform(oh.footprint, proj.albers)
# oh.wb <- crop(nhd.wb, region.footprint)
# oh.cicyano <- PrepRasterBrick(oh.list, oh.dates, "oh_cicyano", output.crs = proj.albers, reclass.matrix = cyano.reclass)
