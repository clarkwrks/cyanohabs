library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(plyr)

#' Extracts and summarizes raster brick pixel values at point locations.
#' 
#' @param aoi.points binary raster representing locations with valid data (from bloomr:SummarizeBrick())
#' @param raster.brick prefix for output files (eg "fl", "oh_cicyano")
#' @param summary.only NHD waterbodies subset to region
#' @return If summary.only = TRUE, a single row of metrics aggregating the raster time series spatially and temporally.  If summary.only = 
#' FALSE, a list of: 1.) 3-dimensional matrix of all pixel values, 2.) data.frame of summary metrics with one row per raster brick layer, 3.) 
#' a single row of metrics aggregating the raster time series spatially and temporally.
ExtractTS <- function(aoi.points, raster.brick, summary.only = FALSE){
  print(paste0("extracting timeseries from raster brick at ", nrow(aoi.points), " locations"))
  aoi.extract <- extract(raster.brick, y=aoi.points, na.rm = FALSE)
  aoi.scene.metrics <- data.frame(aoi.date = as.POSIXct(getZ(raster.brick)),
                                  aoi.unique.id = rep(aoi.points$unique_id[[1]], nlayers(raster.brick)),
                                  aoi.pixels = apply(aoi.extract, 2, length),
                                  aoi.obs = apply(aoi.extract, 2, function(x) sum(!is.na(x))),
                                  aoi.na = apply(aoi.extract, 2, function(x) sum(is.na(x))),
                                  aoi.mean = apply(aoi.extract, 2, mean, na.rm = T),
                                  aoi.median = apply(aoi.extract, 2, median, na.rm = T),
                                  # suppressWarnings for when scene is all NA (returning Inf)
                                  aoi.max = suppressWarnings(apply(aoi.extract, 2, max, na.rm = T)),
                                  aoi.min = suppressWarnings(apply(aoi.extract, 2, min, na.rm = T)),
                                  aoi.sd = apply(aoi.extract, 2, sd, na.rm = T),
                                  aoi.mad = apply(aoi.extract, 2, mad, na.rm = T),
                                  aoi.high.obs = apply(aoi.extract, 2, function(x) sum(x >= 100, na.rm =T)),
                                  aoi.med.obs = apply(aoi.extract, 2, function(x) sum(x >= 30 & x < 100, na.rm =T)),
                                  aoi.low.obs = apply(aoi.extract, 2, function(x) sum(x < 30, na.rm =T)))
  aoi.scene.metrics[mapply(is.infinite, aoi.scene.metrics)] <- NA
  aoi.summary <- summarise(aoi.scene.metrics,
                           unique_id = aoi.unique.id[[1]],
                           aoi_pix = aoi.pixels[[1]],
                           mn_spa_cvr = mean(aoi.obs/aoi.pixels, na.rm=TRUE),
                           periods = length(aoi.date),
                           na_periods = sum(aoi.obs == 0, na.rm=TRUE),
                           ts_mean = mean(aoi.mean, na.rm=TRUE),
                           ts_max = max(aoi.max, na.rm=TRUE),
                           # plyr doesn't like using sum(aoi.mean >= 100), must use subset
                           p_mean_ovr = length(aoi.mean[aoi.mean >= 100 & !is.na(aoi.mean)])/length(aoi.date),
                           p_max_ovr = length(aoi.max[aoi.max >= 100 & !is.na(aoi.max)])/length(aoi.date),
                           p_obs_ovr = sum(aoi.high.obs)/sum(aoi.obs))
  if("case" %in% colnames(aoi.points@data)){
    aoi.scene.metrics$snap.case <- rep(aoi.points$case[[1]], nlayers(raster.brick))
    aoi.summary$snap_case <- I(aoi.points$case[[1]])
  }
  if(summary.only) return(aoi.summary)
  return(list(pixel.ts = aoi.extract, feature.ts = aoi.scene.metrics, aoi.summary = aoi.summary))
}

PolyToCandiPoints <- function(target.poly, candidate.points, unique.id){
  poly.points <- candidate.points[target.poly, ]
  if(length(poly.points) > 0){
    poly.points$unique_id <- I(unique.id)
    return(poly.points)
  } else {
    return(NULL)
  }
}

# # extract and summarize timeseries for a single set of snap points
# test.aoi <- fl.snap.points[[1]]
# test.extract <- ExtractTS(test.aoi, fl.brick, F)
# # or batch it out.  also returning only summaries here, no timeseries
# test.summaries <- lapply(fl.snap.points, ExtractTS, raster.brick = fl.brick, summary.only = T)
# # flatten to dataframe with a row for each set of snapping points
# test.summaries.df <- do.call("rbind", test.summaries)
#
# # can easily extract a polygon (waterbody) instead.
# # grab a decent sized lake
# test.wb <- fl.nhd.wb[with(fl.nhd.wb@data, order(-AREASQKM)),][18,]
# # clip points from candidate.points
# test.wb.points <- fl.candidate.points[test.wb, ]
# test.wb.points$unique_id <- I(test.wb$COMID[[1]])
# test.wb.extract <- ExtractTS(test.wb.points, fl.brick)
