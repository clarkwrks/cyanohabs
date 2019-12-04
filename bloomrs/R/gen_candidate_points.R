library(sp)
library(rgdal)
library(raster)
library(rgeos)

#' Generates a point shapefile from a raster data.mask with waterbody COMID, huc12 watershed, and distance to shore attributes.
#' 
#' @param data.mask binary raster representing locations with valid data (from bloomr:SummarizeBrick())
#' @param output.prefix prefix for output files (eg "fl", "oh_cicyano")
#' @param nhd.wb NHD waterbodies subset to region
#' @param nhd.huc12 HUC12 watersheds subset to region 
#' @return SpatialPointsDataFrame with one point for each pixel of valid raster data with waterbody COMID, huc12 watershed, and distance to shore attributes.
GenCandidatePoints <- function(data.mask, output.prefix, nhd.wb, nhd.huc12){
  print("Creating land mask")
  land.mask <- is.na(data.mask)
  land.mask[land.mask == 0] <- NA
  print("Calculating distance to land")
  shore.distance <- distance(land.mask)
  shore.distance[shore.distance == 0] <- NA
  writeRaster(shore.distance, filename = paste0(output.prefix, "_shore_dist.tif"), datatype = 'FLT4S', overwrite = TRUE)
  print("Performing 4-pass 2x2 focal filter to remove isolated points")
  # Want to focus on contiguous pixels in patches at least 2x2 to reduce mixed pixel effects.  Can't do 2x2 filter directly as there is
  # no 'center' for the focal function with even number of pixels.  Work around is a 4-pass filter, one pass for each 'corner' of a 3x3.
  data.mask.2x2.focal.br <- focal(data.mask, w=matrix(c(0,0,0,0,1,1,0,1,1), nrow=3), na.rm = T, pad=T, padValue = NA)
  data.mask.2x2.focal.tl <- focal(data.mask, w=matrix(c(1,1,0,1,1,0,0,0,0), nrow=3), na.rm = T, pad=T, padValue = NA)
  data.mask.2x2.focal.tr <- focal(data.mask, w=matrix(c(0,0,0,1,1,0,1,1,0), nrow=3), na.rm = T, pad=T, padValue = NA)
  data.mask.2x2.focal.bl <- focal(data.mask, w=matrix(c(0,1,1,0,1,1,0,0,0), nrow=3), na.rm = T, pad=T, padValue = NA)
  mask.brick <- brick(c(data.mask.2x2.focal.tl, data.mask.2x2.focal.tr, data.mask.2x2.focal.bl, data.mask.2x2.focal.br))
  # for each layer (filter pass) in brick, if focal sum < 4, set NA
  mask.brick[mask.brick < 4] <- NA
  # if a pixel is NA for all 4 filter passes, set NA. all remaining pixels occur in at least a 2x2 pixel patch.
  mask.sum.narm <- sum(mask.brick, na.rm=T)
  mask.sum.narm[mask.sum.narm == 0] <- NA
  # may want to look at the filter if results unexpected
  mask.sum.narm <- writeRaster(mask.sum.narm, filename = paste0(output.prefix, "4-pass_2x2_focal_sum.tif"))
  print("Converting raster to points")
  candidate.points <- rasterToPoints(mask.sum.narm, spatial=T)
  candidate.points <- extract(shore.distance, candidate.points, sp = T)
  candidate.points <- candidate.points[,-1]
  names(candidate.points) <- "shr_dst_m"
  print("Overlaying waterbodies")
  candidate.points <- spTransform(candidate.points, crs(nhd.wb))
  candidate.points$wb_comid <- over(candidate.points, nhd.wb)$COMID
  candidate.points <- candidate.points[!is.na(candidate.points$wb_comid), ]
  candidate.points$huc12 <- over(candidate.points, nhd.huc12)$HUC_12
  shapefile(candidate.points, paste0(output.prefix, "_candidate_points.shp"))
  return(candidate.points)
}

# fl.cand.points <- GenCandidatePoints(fl.data.mask, "fl", fl.nhd.wb, fl.nhd.huc12)
# 
# setwd("D:/HAB/20160419/output.prefix")
# fl.data.mask <- raster("D:/HAB/MERIS_20151210/output.prefix/heatmap/fl_water_mask.tif")
# 
# nhd.huc12 <- shapefile("D:/HAB/20160419/aux_data/NHD_HUC12")
# nhd.wb <- shapefile("D:/HAB/20160419/output.prefix/shore_dist/nhd_wb_subset")
# 
# crs(nhd.wb) <- crs(fl.data.mask)
# crs(nhd.huc12) <- crs(fl.data.mask)
# # crop to regional extent
# fl.nhd.wb <- crop(nhd.wb, fl.data.mask)
# fl.nhd.huc12 <- crop(nhd.huc12, fl.data.mask)
# 
# 
# fl.cand.points <- GenCandidatePoints(fl.data.mask, "fl", fl.nhd.wb, fl.nhd.huc12)
