library(raster)

#' Prepare a raster brick from a list of rasters. Current implementation automatically sets 1 > x > 249 to NA in accordance with NOAA CI-cyano geotiff DN flags.
#' 
#' @param raster.list list of file paths to rasters with uniform resolution and extent.
#' @param raster.dates vector of class Date and length(raster.list)
#' @param output.prefix prefix for output files (eg "fl", "oh_cicyano")
#' @param bin.period character string defining bin period, seee base::cut.POSIXt for alternatives, this function has only been tested with "week"
#' @param out.crs optional CRS projection string for output rasters
#' @param raster.mask not implemented, perform masking on function output instead
#' @param over.write overwrite existing raster brick if necessary
#' @return geotiff raster brick of binned input imagery.  Optionally reprojected and masked
TemporalBinRasterList <- function(raster.list, raster.dates, output.prefix, bin.period = "week", out.crs = NULL, raster.mask = NULL, over.write = FALSE){
  raster.dates.binned <-
    as.Date(cut(raster.dates, bin.period, start.on.monday = FALSE))
  all.bin.periods <-
    seq(
      from = min(raster.dates.binned),
      to = max(raster.dates.binned),
      by = bin.period
    )
  na.fill.raster <- setValues(raster(raster.list[[1]]), NA)
  dir.create(output.prefix, showWarnings = FALSE, recursive = TRUE)
  BinPeriod <- function(target.bin.period, raster.dates.binned, raster.list, output.prefix, na.fill.raster, over.write = FALSE){
    indices <- which(raster.dates.binned == target.bin.period)
    period.end <- seq(target.bin.period - 1, by = bin.period, length.out = 2)[2]
    period.label <- paste(basename(output.prefix), strftime(target.bin.period, format="%Y%j"), strftime(period.end, format="%Y%j"), sep = "_")
    print(paste0(Sys.time(), ": binning ", period.label))
    if(length(indices) == 1){
      period.raster <- raster(raster.list[[indices]])
      out.raster <- clamp(period.raster, lower = 1, upper = 249, useValues = FALSE)
    }
    if(length(indices) > 1){
      period.stack <- stack(raster.list[indices])
      period.stack.clamp <- clamp(period.stack, lower = 1, upper = 249, useValues = FALSE)
      out.raster <- max(period.stack.clamp, na.rm=T)
    }
    else{
      out.raster <- na.fill.raster
    }
    #     print(out.file.name)
    binned.raster <-
      writeRaster(
        out.raster,
        filename = paste0(output.prefix, "/", period.label),
        format = "GTiff",
        overwrite = over.write,
        datatype = 'INT1U'
      )
    bin.result <-
      data.frame(
        bin.start.date = target.bin.period,
        bin.end.date = period.end,
        nscenes = length(indices),
        file.name = filename(binned.raster),
        stringsAsFactors = FALSE
      )
    write.csv(raster.list[indices], paste0(output.prefix, "/", period.label, "_scenes.txt"))
    return(bin.result)
  }
  binned.list <- lapply(all.bin.periods, BinPeriod, raster.dates.binned, raster.list, output.prefix, na.fill.raster, over.write = over.write)
  bin.results <- do.call("rbind", binned.list)
  raster.stack <- stack(bin.results$file.name)
  raster.brick <- brick(raster.stack, filename = paste0(output.prefix, "_brick"), format = "GTiff", datatype = 'INT1U')
  write.csv(bin.results, paste0(filename(raster.brick), ".xeta"))
  if(!is.null(out.crs)){
      print("reprojecting raster brick")
      raster.brick <- projectRaster(raster.brick, res = res(raster.brick), crs = out.crs, filename = paste0(output.prefix, "_brick_proj"), format = "GTiff", datatype = 'INT1U')
      write.csv(bin.results, paste0(filename(raster.brick), ".xeta"))
  }
  if(!is.null(raster.mask)){
    if(any(class(raster.mask) == c("SpatialPolygons", "SpatialPolygonsDataFrame"))){
      blank.raster <- setValues(raster.brick[[1]], value = 1)
      print("rasterizing mask shapefile")
      rasterized.mask <- rasterize(raster.mask, blank.raster, getCover = T)
      print("clamping mask values")
      raster.mask <- clamp(rasterized.mask, lower = 100, useValues = F)
    }
    print("masking raster brick")
    raster.brick <- raster::mask(raster.brick, raster.mask, filename = paste0(output.prefix, "_brick_mask"), format = "GTiff", datatype = 'INT1U')
    write.csv(bin.results, paste0(filename(raster.brick), ".xeta"))
  }
  return(raster.brick)
}

RasterizeWaterbodyWithHole <- function(wb.poly, template.raster){
  target.raster <- crop(template.raster, wb.poly)
  wb.rasterize <- rasterize(wb.poly, target.raster, getCover = T)
  wb.rasterize <- clamp(wb.rasterize, lower = 100, useValues = F)
}

# 
# setwd("HAB/20160503")
# rasterOptions(progress = "text")

# start.t <- Sys.time()
# prfx.list <- list.files(path = "data/cicyano/prfx_cicyano", pattern = '.tif$', full.names = TRUE, recursive = FALSE)[1:10]
# prfx.dates <- as.Date(substr(basename(prfx.list), 9, 15), format="%Y%j")
# prfx.weekly <- TemporalBinRasterList(prfx.list, prfx.dates, "output/weekly/prfx_cicyano_weekly", out.crs = CRS("+init=EPSG:6350"), over.write = TRUE)
# Sys.time() - start.t
# 
# start.t <- Sys.time()
# fl.list <- list.files(path = "data/cicyano/fl_cicyano", pattern = '.tif$', full.names = TRUE, recursive = FALSE)
# fl.dates <- as.Date(substr(basename(fl.list), 9, 15), format="%Y%j")
# fl.weekly <- TemporalBinRasterList(fl.list, fl.dates, "output/weekly/fl_cicyano_weekly", out.crs = CRS("+init=EPSG:6350"), over.write = TRUE)
# Sys.time() - start.t
# 
# start.t <- Sys.time()
# ne.list <- list.files(path = "data/cicyano/ne_cicyano", pattern = '.tif$', full.names = TRUE, recursive = FALSE)
# ne.dates <- as.Date(substr(basename(ne.list), 9, 15), format="%Y%j")
# ne.weekly <- TemporalBinRasterList(ne.list, ne.dates, "output/weekly/ne_cicyano_weekly", out.crs = CRS("+init=EPSG:6350"), over.write = TRUE)
# Sys.time() - start.t
# 
# start.t <- Sys.time()
# oh.list <- list.files(path = "data/cicyano/oh_cicyano", pattern = '.tif$', full.names = TRUE, recursive = FALSE)
# oh.dates <- as.Date(substr(basename(oh.list), 9, 15), format="%Y%j")
# oh.weekly <- TemporalBinRasterList(oh.list, oh.dates, "output/weekly/oh_cicyano_weekly", out.crs = CRS("+init=EPSG:6350"), over.write = TRUE)
# Sys.time() - start.t
# 
# start.t <- Sys.time()
# can.list <- list.files(path = "data/cicyano/can_cicyano", pattern = '.tif$', full.names = TRUE, recursive = FALSE)
# can.dates <- as.Date(substr(basename(can.list), 9, 15), format="%Y%j")
# can.weekly <- TemporalBinRasterList(can.list, can.dates, "output/weekly/can_cicyano_weekly", out.crs = CRS("+init=EPSG:6350"), over.write = TRUE)
# Sys.time() - start.t

