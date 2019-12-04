library(raster)
library(tables)

crs.alb <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")

source("bloomrs/R/geotiff_metadata.R")
oh.cicyano <- readGTiffBrick("input_data/oh_cicyano_weekly_mask.tif")
fl.cicyano <- readGTiffBrick("input_data/fl_cicyano_weekly_mask.tif")
nhd.wb <- shapefile("input_data/nhd_nla_subset_shore_dist.shp")
nhd.huc12 <- shapefile("input_data/nhd_huc12.shp")
meris.regions <- shapefile("input_data/meris_regions.shp")

# Prepare POI (PWSI) point data -------------------------------------------
# required fields
# # generate simulated PSWI locations
# fl.test.coords <- data.frame(x = c(1451894, 1446547, 1448165, 1450797), 
#                              y = c(579348.6, 586258.3, 580445.4, 574166.3))
# fl.test.attrs <- data.frame(unique_id=c("fl1", "fl2", "fl3", "fl4"),
#                          wb_comid = c(21489874, 21489752, 21489802, 21489874))
# fl.test.spdf <- SpatialPointsDataFrame(fl.test.coords, fl.test.attrs, proj4string=crs.alb)
# oh.test.coords <- data.frame(x = c(964535, 960408, 970357, 1042634, 1049121), 
#                              y = c(2004298, 2000977, 2005271, 2153659, 2144026))
# oh.test.attrs <- data.frame(unique_id=c("oh1", "oh2", "oh3", "oh4", "oh5"),
#                             wb_comid = c(120052700, 120052700, 120052700, 904140245, 904140245))
# oh.test.spdf <- SpatialPointsDataFrame(oh.test.coords, oh.test.attrs, proj4string=crs.alb)
# simulated.pws <- rbind(fl.test.spdf, oh.test.spdf)
# simulated.pws$huc12 <- over(simulated.pws, nhd.huc12)$HUC_12
# simulated.pws$region <- over(simulated.pws, meris.regions)$region
# shapefile(simulated.pws, "input_data/simulated_pws.shp")
# 
# # or use actual PWSI locations (sensitive)
# pws <- shapefile(file.choose())
# pws$huc12 <- over(pws, nhd.huc12)$HUC_12
# pws$region <- over(pws, meris.regions)$region
# pws$wb_comid <- pws$COMID_JCjo
# pws$unique_id <- paste0(pws$PWSID, pws$FACILITY_I)
# pws <- pws[pws$wb_comid != 0,]

pws <- shapefile("input_data/simulated_pws.shp")


# Subset NHD shapefiles ---------------------------------------------------

fl.nhd.wb <- nhd.wb[meris.regions[meris.regions$region == "Florida", ], ]
fl.nhd.huc12 <- nhd.huc12[meris.regions[meris.regions$region == "Florida", ], ]

oh.nhd.wb <- nhd.wb[meris.regions[meris.regions$region == "Ohio", ], ]
oh.nhd.huc12 <- nhd.huc12[meris.regions[meris.regions$region == "Ohio", ], ]

region.nhd.wb <- rbind(fl.nhd.wb, oh.nhd.wb)
region.nhd.huc12 <- rbind(fl.nhd.huc12, oh.nhd.huc12)
rm(nhd.wb)
rm(nhd.huc12)

# Extract POIs ------------------------------------------------------------
source("bloomrs/R/gen_candidate_points.R")
source("bloomrs/R/snap_points.R")
source("bloomrs/R/extract_ts.R")

fl.data.mask <- raster("input_data/fl_data_mask.tif")
fl.bloom.freq <- raster("input_data/fl_bloom_freq.tif")
fl.region <- "Florida"
fl.pws <- pws[which(pws$region == fl.region), ]

fl.candidate.points <- GenCandidatePoints(fl.data.mask, "output/fl", fl.nhd.wb, fl.nhd.huc12)
fl.snap.points <-  lapply(1:length(fl.pws), function(i) SnapPoints(fl.pws[i,], fl.candidate.points, output = "output/fl"))
fl.pws.extract <- lapply(unlist(fl.snap.points), ExtractTS, fl.cicyano, T)
fl.pws.summary <- do.call("rbind", fl.pws.extract)
fl.pws.summary$region <- fl.region

fl.wb.points <- lapply(1:length(fl.nhd.wb), function(i) PolyToCandiPoints(fl.nhd.wb[i,], fl.candidate.points, I(fl.nhd.wb[i,"COMID"][[1]])))
# remove NULL list items (no candidate points within waterbody)
fl.wb.points[sapply(fl.wb.points, is.null)] <- NULL
# loop through and extract from raster brick
fl.wb.extract <- lapply(unlist(fl.wb.points), ExtractTS, fl.cicyano, T)
fl.wb.summary <- do.call("rbind", fl.wb.extract)
fl.wb.summary$region <- fl.region

fl.huc12.points <- lapply(1:length(fl.nhd.huc12), function(i) PolyToCandiPoints(fl.nhd.huc12[i,], fl.candidate.points, I(fl.nhd.huc12[i,"HUC_12"][[1]])))
fl.huc12.points[sapply(fl.huc12.points, is.null)] <- NULL
fl.huc12.extract <- lapply(unlist(fl.huc12.points), ExtractTS, fl.cicyano, T)
fl.huc12.summary <- do.call("rbind", fl.huc12.extract)
fl.huc12.summary$region <- fl.region

fl.bloom.freq.focal <- focal(fl.bloom.freq, w=matrix(1/9, nc=3, nr=3), filename = "output/fl_bloom_freq_3x3focal.tif", na.rm=FALSE)
fl.nine.pixel.bloom.freq <- values(fl.bloom.freq.focal)

oh.data.mask <- raster("input_data/oh_data_mask.tif")
oh.bloom.freq <- raster("input_data/oh_bloom_freq.tif")
oh.region <- "Ohio"
oh.pws <- pws[which(pws$region == oh.region), ]

oh.candidate.points <- GenCandidatePoints(oh.data.mask, "output/oh", oh.nhd.wb, oh.nhd.huc12)
oh.snap.points <-  lapply(1:length(oh.pws), function(i) SnapPoints(oh.pws[i,], oh.candidate.points, output = "output/oh"))
oh.pws.extract <- lapply(unlist(oh.snap.points), ExtractTS, oh.cicyano, T)
oh.pws.summary <- do.call("rbind", oh.pws.extract)
oh.pws.summary$region <- oh.region

oh.wb.points <- lapply(1:length(oh.nhd.wb), function(i) PolyToCandiPoints(oh.nhd.wb[i,], oh.candidate.points, I(oh.nhd.wb[i,"COMID"][[1]])))
oh.wb.points[sapply(oh.wb.points, is.null)] <- NULL
oh.wb.extract <- lapply(unlist(oh.wb.points), ExtractTS, oh.cicyano, T)
oh.wb.summary <- do.call("rbind", oh.wb.extract)
oh.wb.summary$region <- oh.region

oh.huc12.points <- lapply(1:length(oh.nhd.huc12), function(i) PolyToCandiPoints(oh.nhd.huc12[i,], oh.candidate.points, I(oh.nhd.huc12[i,"HUC_12"][[1]])))
oh.huc12.points[sapply(oh.huc12.points, is.null)] <- NULL
oh.huc12.extract <- lapply(unlist(oh.huc12.points), ExtractTS, oh.cicyano, T)
oh.huc12.summary <- do.call("rbind", oh.huc12.extract)
oh.huc12.summary$region <- oh.region

oh.bloom.freq.focal <- focal(oh.bloom.freq, w=matrix(1/9, nc=3, nr=3), filename = "output/oh_bloom_freq_3x3focal.tif", na.rm=FALSE)
oh.nine.pixel.bloom.freq <- values(oh.bloom.freq.focal)

all.pws.summary <- rbind(fl.pws.summary, oh.pws.summary)
write.csv(all.pws.summary, "output/all_pws_summary.csv")
all.wb.summary <- rbind(fl.wb.summary, oh.wb.summary)
write.csv(all.wb.summary, "output/all_wb_summary.csv")
all.huc12.summary <- rbind(fl.huc12.summary, oh.huc12.summary)
write.csv(I(all.huc12.summary), "output/all_huc12_summary.csv")
all.nine.pixel.bloom.freq <- c(fl.nine.pixel.bloom.freq, oh.nine.pixel.bloom.freq)
all.nine.pixel.bloom.freq <- all.nine.pixel.bloom.freq[!is.na(all.nine.pixel.bloom.freq)]
write.csv(all.nine.pixel.bloom.freq, "output/all_nine_pixel_bloom_freq.csv")


# Snapping results summary ------------------------------------------------

fl.snaps <- do.call('rbind', unlist(fl.snap.points, recursive = T))
fl.snaps$region <- "Florida"
oh.snaps <- do.call('rbind', unlist(oh.snap.points, recursive = T))
oh.snaps$region <- "Ohio"
all.snaps <- rbind(fl.snaps, oh.snaps)
shapefile(all.snaps, "output/all_pws_snaps.shp")
all.snaps.df <- all.snaps@data[ , c("unique_id", "case", "region")]
all.snaps.df.unique <- unique(all.snaps.df)
all.snaps.pws <- all.snaps.df
all.snaps.pws$case <- NULL
all.snaps.pws <- unique(all.snaps.pws)
all.snaps.pws$case <- "pws total"
all.snaps.df.unique <- rbind(all.snaps.df.unique, all.snaps.pws)
all.snaps.df.unique$case <- factor(all.snaps.df.unique$case, levels = c("adjacent", "proximate", "waterbody", "watershed", "unresolved", "pws total"))
pws.snap.summary <- tabular((Heading("Region")*(as.factor(region) + 1)) ~ Heading("Snapping Case")*(as.factor(case)), data=all.snaps.df.unique)
write.csv.tabular(pws.snap.summary, "output/pws_snap_summary.csv")


# Quantile comparisons ----------------------------------------------------

all.pws.summary <- read.csv("output/all_pws_summary.csv", colClasses = c(unique_id = "character"))
all.wb.summary <- read.csv("output/all_wb_summary.csv", colClasses = c(unique_id = "character"))
all.huc12.summary <- read.csv("output/all_huc12_summary.csv", colClasses = c(unique_id = "character"))
all.nine.pixel.bloom.freq <- read.csv("output/all_nine_pixel_bloom_freq.csv")[ , 2]

wb.freq <- data.frame(freq = as.numeric(all.wb.summary[all.wb.summary$aoi_pix > 8, ]$p_obs_ovr), case = "waterbody", unique_id = all.wb.summary[all.wb.summary$aoi_pix > 8, ]$unique_id)
huc12.freq <- data.frame(freq = as.numeric(all.huc12.summary[all.huc12.summary$aoi_pix > 8, ]$p_obs_ovr), case = "watershed", unique_id = all.huc12.summary[all.huc12.summary$aoi_pix > 8, ]$unique_id)

adj.quantiles <- quantile(all.nine.pixel.bloom.freq)[c(1,2,3,4,5)]
prox.quantiles <- adj.quantiles
wb.quantiles <- quantile(wb.freq$freq)[c(1,2,3,4,5)]
huc12.quantiles <- quantile(huc12.freq$freq)[c(1,2,3,4,5)]

pws.adj.case <- all.pws.summary[all.pws.summary$snap_case == "adjacent", ]
pws.adj.case$freq.quant <- cut(pws.adj.case$p_obs_ovr, breaks = adj.quantiles, 
                              include.lowest=T, right = T, labels = c("Q1", "Q2", "Q3", "Q4"))

pws.prox.case <- all.pws.summary[all.pws.summary$snap_case == "proximate", ]
pws.prox.case$freq.quant <- cut(pws.prox.case$p_obs_ovr, breaks = prox.quantiles, 
                               include.lowest=T, right = T, labels = c("Q1", "Q2", "Q3", "Q4"))

pws.wb.case <- all.pws.summary[all.pws.summary$snap_case == "waterbody", ]
pws.wb.case$freq.quant <- cut(pws.wb.case$p_obs_ovr, breaks = wb.quantiles, 
                             include.lowest=T, right = T, labels = c("Q1", "Q2", "Q3", "Q4"))

pws.huc12.case <- all.pws.summary[all.pws.summary$snap_case == "watershed", ]
pws.huc12.case$freq.quant <- cut(pws.huc12.case$p_obs_ovr, breaks = huc12.quantiles, 
                             include.lowest=T, right = T, labels = c("Q1", "Q2", "Q3", "Q4"))

all.pws.summary.quants <- rbind(pws.adj.case, pws.prox.case, pws.wb.case, pws.huc12.case)
all.pws.summary.quants.table <- tabular((Heading("Case")*as.factor(snap_case))*(Heading("Region")*(as.factor(region))) ~
                                Heading("") * freq.quant + 1, data = all.pws.summary.quants)

write.csv.tabular(all.pws.summary.quants.table, "output/pws_quantiles_table.csv")
write.csv(all.pws.summary.quants, "output/all_pws_summary_quantiles.csv")

write.csv(prox.quantiles, "output/proximate_quantiles.csv")
write.csv(adj.quantiles, "output/adjacent_quantiles.csv")
write.csv(wb.quantiles, "output/waterbody_quantiles.csv")
write.csv(huc12.quantiles, "output/huc12_quantiles.csv")


# Percent pixels over WHO threshold ---------------------------------------

wb.by.freq.rank <- wb.freq[with(wb.freq, order(freq)), ]
wb.by.freq.rank$freq_rank <- seq(1, nrow(wb.by.freq.rank), 1)
wb.pws.freq.match <- wb.by.freq.rank[wb.by.freq.rank$freq %in% pws.wb.case$p_obs_ovr, ]
wb.pws.freq.match <- data.frame(wb_freq_rank = wb.pws.freq.match$freq_rank, freq = wb.pws.freq.match$freq)
pws.by.wb.freq <- merge(pws.wb.case, wb.pws.freq.match, by.x = "p_obs_ovr", by.y = "freq", all.x = T)
pws.wb.freq.rank.plot <- ggplot(wb.by.freq.rank, aes(x = freq_rank, y = freq)) +
  geom_point() +
  geom_point(data=pws.by.wb.freq, aes(x=wb_freq_rank, y=p_obs_ovr), col = "red") +
  geom_hline(yintercept = quantile(wb.freq$freq)[c(2,3,4)])

huc12.by.freq.rank <- huc12.freq[with(huc12.freq, order(freq)), ]
huc12.by.freq.rank$freq_rank <- seq(1, nrow(huc12.by.freq.rank), 1)
huc12.pws.freq.match <- huc12.by.freq.rank[huc12.by.freq.rank$freq %in% pws.huc12.case$p_obs_ovr, ]
huc12.pws.freq.match <- data.frame(huc12_freq_rank = huc12.pws.freq.match$freq_rank, freq = huc12.pws.freq.match$freq)
pws.by.huc12.freq <- merge(pws.huc12.case, huc12.pws.freq.match, by.x = "p_obs_ovr", by.y = "freq", all.x = T)
pws.huc12.freq.rank.plot <- ggplot(huc12.by.freq.rank, aes(x = freq_rank, y = freq)) +
  geom_point() +
  geom_point(data=pws.by.huc12.freq, aes(x=huc12_freq_rank, y=p_obs_ovr), col = "red") +
  geom_hline(yintercept = quantile(huc12.freq$freq)[c(2,3,4)])

nine.pixel.by.freq.rank <- data.frame(freq = all.nine.pixel.bloom.freq[order(all.nine.pixel.bloom.freq)])
nine.pixel.by.freq.rank$freq_rank <- seq(1, nrow(nine.pixel.by.freq.rank), 1)

nine.pixel.prox.pws.match <- sapply(pws.prox.case$p_obs_ovr, function(x) which.min(abs(x - nine.pixel.by.freq.rank$freq)))
nine.pixel.prox.pws.match <- nine.pixel.by.freq.rank[nine.pixel.prox.pws.match, ]
pws.by.nine.pixel.freq <- merge(pws.prox.case, nine.pixel.prox.pws.match, by.x = "p_obs_ovr", by.y = "freq", all.x = T)
pws.prox.freq.rank.plot <- ggplot(nine.pixel.by.freq.rank, aes(x = freq_rank, y = freq)) +
  geom_point() +
  geom_point(data=nine.pixel.prox.pws.match, aes(x=freq_rank, y=freq), col = "red") +
  geom_hline(yintercept = quantile(nine.pixel.by.freq.rank$freq)[c(2,3,4)])

nine.pixel.adj.pws.match <- sapply(pws.adj.case$p_obs_ovr, function(x) which.min(abs(x - nine.pixel.by.freq.rank$freq)))
nine.pixel.adj.pws.match <- nine.pixel.by.freq.rank[nine.pixel.adj.pws.match, ]
pws.by.nine.pixel.freq <- merge(pws.adj.case, nine.pixel.adj.pws.match, by.x = "p_obs_ovr", by.y = "freq", all.x = T)
pws.adj.freq.rank.plot <- ggplot(nine.pixel.by.freq.rank, aes(x = freq_rank, y = freq)) +
  geom_point() +
  geom_point(data=nine.pixel.adj.pws.match, aes(x=freq_rank, y=freq), col = "red") +
  geom_hline(yintercept = quantile(nine.pixel.by.freq.rank$freq)[c(2,3,4)])

write.csv(nine.pixel.by.freq.rank, "output/freq_rank_focal.csv")
write.csv(nine.pixel.adj.pws.match, "output/freq_rank_focal_adjacent.csv")
write.csv(nine.pixel.prox.pws.match, "output/freq_rank_focal_proximate.csv")
write.csv(wb.by.freq.rank, "output/freq_rank_wb.csv")
write.csv(pws.by.wb.freq, "output/freq_rank_wb_pws.csv")
write.csv(huc12.by.freq.rank, "output/freq_rank_huc12.csv")
write.csv(pws.by.huc12.freq, "output/freq_rank_huc12_pws.csv")


# Export shapefiles -------------------------------------------------------

wb.summary.spdf <- all.wb.summary[all.wb.summary$aoi_pix > 8, ]
wb.summary.spdf <- wb.summary.spdf[with(wb.summary.spdf, order(p_obs_ovr)), ]
wb.summary.spdf$freq_rank <- seq(1, nrow(wb.summary.spdf), 1)
wb.summary.spdf <- merge(rbind(fl.nhd.wb, oh.nhd.wb), wb.summary.spdf, by.x = "COMID", by.y = "unique_id", all.x = F)
wb.summary.spdf$freq_quant <- cut(wb.summary.spdf$p_obs_ovr, breaks = wb.quantiles, 
                                 include.lowest=T, right = T, labels = c("Q1", "Q2", "Q3", "Q4"))
shapefile(wb.summary.spdf, filename = "output/wb_hab_results.shp", overwrite = T)

huc12.summary.spdf <- all.huc12.summary[all.huc12.summary$aoi_pix > 8, ]
huc12.summary.spdf <- huc12.summary.spdf[with(huc12.summary.spdf, order(p_obs_ovr)), ]
huc12.summary.spdf$freq_rank <- seq(1, nrow(huc12.summary.spdf), 1)
huc12.summary.spdf <- merge(rbind(fl.nhd.huc12, oh.nhd.huc12), huc12.summary.spdf, by.x = "HUC_12", by.y = "unique_id", all.x = F)
huc12.summary.spdf$freq_quant <- cut(huc12.summary.spdf$p_obs_ovr, breaks = huc12.quantiles, 
                                 include.lowest=T, right = T, labels = c("Q1", "Q2", "Q3", "Q4"))
shapefile(huc12.summary.spdf, filename = "output/huc12_hab_results.shp", overwrite = T)


pws.summary.spdf <- merge(pws, all.pws.summary, by = "unique_id", all.x = F, duplicateGeoms = T)
pws.summary.spdf$snap_case <- as.character(pws.summary.spdf$snap_case)
pws.summary.spdf$region <- pws.summary.spdf$region.x
pws.summary.spdf$region.x <- NULL
pws.summary.spdf$region.y <- NULL
pws.summary.spdf$quantile <- NA
pws.summary.spdf[pws.summary.spdf$snap_case == "waterbody", "quantile"] <- 
  cut(pws.summary.spdf[pws.summary.spdf$snap_case == "waterbody", ]$p_obs_ovr, 
      breaks = wb.quantiles, include.lowest=T, right = T, labels = c("Q1", "Q2", "Q3", "Q4"))
pws.summary.spdf[pws.summary.spdf$snap_case == "watershed", "quantile"] <- 
  cut(pws.summary.spdf[pws.summary.spdf$snap_case == "watershed", ]$p_obs_ovr, 
      breaks = huc12.quantiles, include.lowest=T, right = T, labels = c("Q1", "Q2", "Q3", "Q4"))
pws.summary.spdf[pws.summary.spdf$snap_case == "adjacent", "quantile"] <- 
  cut(pws.summary.spdf[pws.summary.spdf$snap_case == "adjacent", ]$p_obs_ovr, 
      breaks = adj.quantiles, include.lowest=T, right = T, labels = c("Q1", "Q2", "Q3", "Q4"))
pws.summary.spdf[pws.summary.spdf$snap_case == "proximate", "quantile"] <- 
  cut(pws.summary.spdf[pws.summary.spdf$snap_case == "proximate", ]$p_obs_ovr,
      breaks = prox.quantiles, include.lowest=T, right = T, labels = c("Q1", "Q2", "Q3", "Q4"))
shapefile(pws.summary.spdf, filename = "output/pws_hab_results.shp", overwrite = T)
