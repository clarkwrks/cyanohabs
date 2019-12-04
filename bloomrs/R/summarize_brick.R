library(raster)
library(tools)

#' Summarizes a raster time series of cyanobacteria concentration by generating 
#' descriptive rasters.
#' 
#' @param raster.brick Time series raster brick prepared by bloomr::PrepRasterBrick
#' @param output.prefix prefix for output files (eg "fl", "oh_cicyano")
#' @return A list of three summary rasters: temporal coverage, data mask, and bloom frequency
SummarizeBrick <- function(raster.brick, output.prefix){
  print("counting observations")
  observation.count <- sum(!is.na(raster.brick), na.rm=T)
  print("calculating temporal coverage")
  temporal.coverage <- observation.count/nlayers(raster.brick)
  temporal.coverage <- writeRaster(temporal.coverage, filename = paste0(output.prefix, "_temporal_coverage.tif"))
  print("creating data mask")
  data.mask <- temporal.coverage > 0
  data.mask <- subs(data.mask, data.frame(oldval = 0, newval = NA), subsWithNA=FALSE) 
  data.mask <- writeRaster(data.mask, filename = paste0(output.prefix, "_data_mask.tif"))
  print("classifying blooms")
  # any values >= 100 are equivalent to the WHO high risk guideline threshold
  raster.brick.bloom <- reclassify(raster.brick, c(0, 99, NA, 99, 250, 1, 250, 254, NA))
  print("counting blooms")
  bloom.count <- sum(raster.brick.bloom, na.rm=T)
  print("calculating bloom frequency")
  bloom.freq <- bloom.count/observation.count * data.mask
  bloom.freq <- writeRaster(bloom.freq, filename = paste0(output.prefix, "_bloom_freq.tif"))
  return(list(temporal.coverage, data.mask, bloom.freq))
}

# test.brick.summary <- SummarizeBrick(test.cicyano, "test")

# 
# setwd("D:/HAB/MERIS_20151210/output/heatmap")
# rasterOptions(progress = "text", timer = TRUE, tmpdir = "D:/Scratch")
# 
# fl.brick <- brick("D:/HAB/MERIS_20151210/output/fl_cicyano_masked.grd")
# ne.brick <- brick("D:/HAB/MERIS_20151210/output/ne_cicyano_masked.grd")
# oh.brick <- brick("D:/HAB/MERIS_20151210/output/oh_cicyano_masked.grd")
# fl.7day.brick <- brick("D:/HAB/MERIS_20151210/output/tbin_test/fl_7day.grd")
# ne.7day.brick <- brick("D:/HAB/MERIS_20151210/output/tbin_test/ne_7day.grd")
# oh.7day.brick <- brick("D:/HAB/MERIS_20151210/output/tbin_test/oh_7day.grd")
# 
# fl.brick.summaries <- SummarizeBrick(fl.brick, "fl")
# ne.brick.summaries <- SummarizeBrick(ne.brick, "ne")
# oh.brick.summaries <- SummarizeBrick(oh.brick, "oh")
# fl.7day.brick.summaries <- SummarizeBrick(fl.7day.brick, "fl_7day")
# ne.7day.brick.summaries <- SummarizeBrick(ne.7day.brick, "ne_7day")
# oh.7day.brick.summaries <- SummarizeBrick(oh.7day.brick, "oh_7day")
