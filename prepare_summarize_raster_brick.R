library(raster)

source("bloomrs/R/geotiff_metadata.R")
source("bloomrs/R/temporal_bin_raster_list.R")
source("bloomrs/R/summarize_brick.R")
nhd.wb <- shapefile("input_data/nhd_nla_subset_shore_dist.shp")
crs.alb <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")


fl.list <- list.files(path = "input_data/cicyano/fl_cicyano", pattern = '.tif$', full.names = TRUE, recursive = FALSE)[1:10]
fl.dates <- as.Date(substr(basename(fl.list), 9, 15), format="%Y%j")
fl.weekly <- TemporalBinRasterList(fl.list, fl.dates, "output/fl_cicyano_weekly", out.crs = crs.alb, over.write = TRUE)
fl.cicyano.weekly <- brick("output/fl_cicyano_weekly_brick_proj.tif")

fl.extent <- as(extent(raster(fl.list[[1]])), 'SpatialPolygons')  
crs(fl.extent) <- crs(raster(fl.list[[1]]))
fl.extent <- spTransform(fl.extent, crs.alb)
fl.nhd.wb <- nhd.wb[fl.extent, ]

fl.wb.rasterize <- lapply(1:nrow(fl.nhd.wb), function(i) RasterizeWaterbodyWithHole(fl.nhd.wb[i, ], fl.cicyano.weekly[[1]]))
fl.wb.rasterize.merge <- do.call("merge", fl.wb.rasterize)
fl.wb.rasterize.merge[fl.wb.rasterize.merge == 100] <- 1
fl.wb.rasterize.merge <- extend(fl.wb.rasterize.merge, fl.cicyano.weekly[[1]])
writeRaster(fl.wb.rasterize.merge, "output/fl_nhd_wb_mask.tif", format = "GTiff", datatype = 'INT1U') 
fl.mask <- raster("output/fl_nhd_wb_mask.tif")
fl.cicyano.weekly.mask <- raster::mask(fl.cicyano.weekly, fl.mask, filename = "output/fl_cicyano_weekly_mask.tif", format = "GTiff", datatype = 'INT1U')
copyGTiffMeta(fl.cicyano.weekly, fl.cicyano.weekly.mask)

fl.brick.summary <- SummarizeBrick(fl.cicyano.weekly.mask, "output/fl")

oh.list <- list.files(path = "input_data/cicyano/oh_cicyano", pattern = '.tif$', full.names = TRUE, recursive = FALSE)[1:10]
oh.dates <- as.Date(substr(basename(oh.list), 9, 15), format="%Y%j")
oh.weekly <- TemporalBinRasterList(oh.list, oh.dates, "output/oh_cicyano_weekly", out.crs = crs.alb, over.write = TRUE)
oh.cicyano.weekly <- brick("output/oh_cicyano_weekly_brick_proj.tif")

oh.extent <- as(extent(raster(oh.list[[1]])), 'SpatialPolygons')  
crs(oh.extent) <- crs(raster(oh.list[[1]]))
oh.extent <- spTransform(oh.extent, crs.alb)
oh.nhd.wb <- nhd.wb[oh.extent, ]

oh.wb.rasterize <- lapply(1:nrow(oh.nhd.wb), function(i) RasterizeWaterbodyWithHole(oh.nhd.wb[i, ], oh.cicyano.weekly[[1]]))
oh.wb.rasterize.merge <- do.call("merge", oh.wb.rasterize)
oh.wb.rasterize.merge[oh.wb.rasterize.merge == 100] <- 1
oh.wb.rasterize.merge <- extend(oh.wb.rasterize.merge, oh.cicyano.weekly[[1]])
writeRaster(oh.wb.rasterize.merge, "output/oh_nhd_wb_mask.tif", format = "GTiff", datatype = 'INT1U') 
oh.mask <- raster("output/oh_nhd_wb_mask.tif")
oh.cicyano.weekly.mask <- raster::mask(oh.cicyano.weekly, oh.mask, filename = "output/oh_cicyano_weekly_mask.tif", format = "GTiff", datatype = 'INT1U')
copyGTiffMeta(oh.cicyano.weekly, oh.cicyano.weekly.mask)

oh.brick.summary <- SummarizeBrick(oh.cicyano.weekly.mask, "output/oh")