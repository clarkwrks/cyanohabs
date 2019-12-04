library(raster)
library(rgdal)
library(scales) # only needed for percentage formatting

CalcShoreDist <- function(index, wbs, out.dir){
  # 
  # Intended to be called via lapply(seq(1, nrow(wbs)), ...). The iterator is left
  # outside the function to allow the user to resume processing after interuption.  
  # This is certainly not the most elegant or efficient way to go about this,
  # but it worked (ArcGIS misbehaved), so slow and steady it is.
  # Iterates through a shapefile by row:
  #  1. Rasterizes a single feature @ 30 m
  #  2. Calculates the maximum euclidian distance to shore
  #  3. Appends COMID and max distance (m) to csv
  # index = integer, row of the waterbody shapefile to subset and process
  # wbs = shapefile of all waterbodies
  #
  wb <- wbs[index, ]
  print(paste0("Processing COMID ", wb$COMID))
  # create a blank raster for rasterizing and masking
  blank.raster <- raster(extent(wb), crs = crs(wb), resolution = 30, vals = 1)
  # believe it or not, there are square lakes... need to pad the boundaries or wander into infinity
  blank.raster <- extend(blank.raster, c(1,1), value = 1) 
  print("rasterizing")
  wb.raster <- rasterize(wb, blank.raster, 'COMID')
  print("masking")
  landmask <- raster::mask(blank.raster, mask = wb.raster, inverse = TRUE)
  print("calculating distance")
  shore.dist <- distance(landmask)
  print("finding max distance")
  max.shore.dist <- raster::zonal(shore.dist, wb.raster, fun='max')
  print(paste0(percent(index/nrow(wbs)), " of waterbodies processed."))
  # scientific = FALSE is critical to prevent innapropriate rounding (eg comid 21000001, 21000003 recorded as 2.1+e07)
  write(format(max.shore.dist, scientific = FALSE), paste0(out.dir, "/wb_shore_dist.txt"), append = T, ncolumns = 2)
}

# set an output directory.  recommend isolating from other outputs.
out.dir <- "output/shore_dist"
dir.create(out.dir)


# Subset NHD to NLA -------------------------------------------------------
# NHDwaterbody polygons of FTYPE = "LakePond" or "Reservoir" for all regions, merged, and reprojected 
# to albers equal area. If memory limitations are an issue, consider processing the regions individually 
# and then merging
nhd.wb.all <- shapefile("input_data/nhd_nla_subset_shore_dist.shp")
# subset to waterbody fcodes targeted by the NLA 2012 survey design
valid.fcodes <- c(39000, 39004, 39009, 39010, 39011, 39012, 43600, 43613, 43615, 43617, 43618, 43619, 43621)
nhd.wb.fcode.subset <- nhd.wb.all[nhd.wb.all$FCODE %in% valid.fcodes, ]
# subset to waterbodies > 1 hectare (0.01 sqkm)
nhd.wb.minarea.subset <- nhd.wb.fcode.subset[nhd.wb.fcode.subset$AREASQKM > .01,]
# remove waterbodies with duplicate COMIDs
nhd.wbs.nodup <- nhd.wb.minarea.subset[!(duplicated(nhd.wb.minarea.subset$COMID)),]
# write subset shapefile
writeOGR(nhd.wbs.nodup, out.dir, layer = "nhd_wb_nla_subset", driver = "ESRI Shapefile")

nhd.wbs <- shapefile(paste0(out.dir, "/nhd_wb_nla_subset.shp"))
rm(nhd.wb.all, nhd.wb.fcode.subset, nhd.wb.minarea.subset, nhd.wbs.nodup)


# Loop through polygons ---------------------------------------------------

nhd.seq <- seq(1, nrow(nhd.wbs))
nhd.wbs.shoredist <- lapply(nhd.seq, CalcShoreDist, wbs = nhd.wbs, out.dir = out.dir)
# to resume after interruption, something like: nhd.seq <- seq(nrow(shore.dist.tbl) + 1, nrow(nhd.wbs))

#### ...~36 hours later...
shore.dist.tbl <- read.table(paste0(out.dir, "/wb_shore_dist.txt"), col.names = c('comid', 'shore_dist'))
# cleanup, need to discard some trailing zeros
shore.dist.tbl$comid <- as.integer(shore.dist.tbl$comid)
# check if all comids are replicated in the table
shore.dist.check <- shore.dist.tbl[!(shore.dist.tbl$comid %in% nhd.wbs$COMID), ]
# yep, look at range of distances
range(shore.dist.tbl$shore_dist)
# merge with shapefile by comid
nhd.wbs.shore.dist.spdf <- merge(nhd.wbs, shore.dist.tbl, by.x = 'COMID', by.y = 'comid')
nhd.wbs.shore.dist.spdf$max_window <- nhd.wbs.shore.dist.spdf$shore_dist*2/sqrt(2)

writeOGR(nhd.wbs.shore.dist.spdf, dsn = out.dir, layer = "nhd_nla_subset_shore_dist", driver = "ESRI Shapefile")



# Generate summary table without PWS locations ----------------------------

library(tables)

# Calculates the radius of the circumcircle for a square of length window.width
minDist <- function(window.width) (sqrt(2*window.width^2))/2

# load extent/footprints of regional MERIS product  
meris.regions <- shapefile("input_data/meris_regions.shp")

# load nhd waterbodies with $shore_dist field from "calc_shore_dist.R", max distance from shore in meters
nhd.wb <- shapefile("input_data/nhd_wb_nlasub.shp")
nhd.wb$region <- over(nhd.wb, meris.regions)$region
# geospatial processes completed, can remove nhd.wb shapefile to free up memory
nhd.wb.df <- nhd.wb@data

# determine whether each waterbody polygon has sufficient open water area to accomodate
# focal windows of various target widths.  This could be wrapped into the tabular() call
# below, seperated for clarity.
nhd.wb.df$w30 <- nhd.wb.df$shore_dist >= minDist(30) # 21
nhd.wb.df$w90 <- nhd.wb.df$shore_dist >= minDist(90)
nhd.wb.df$w300 <- nhd.wb.df$shore_dist >= minDist(300)
nhd.wb.df$w900 <- nhd.wb.df$shore_dist >= minDist(900) # 636

nhd.wb.df$type <- 'WB'

# the syntax for tabular() can be a little confusing. See bottom of doc for example output
wb.coverage.table <- tabular((Heading("Region") * (as.factor(region) + 1)) * # group data by region
                               Heading(Feature) * (Heading("Waterbody")*(type == 'WB')) ~ 
                               Heading("Window Size") * # add col for each window width
                               (Heading("30x30") * w30 + Heading("90x90") * w90 + Heading("300x300") * w300 + Heading("900x900") * w900) * 
                               (Heading("n")*1 +Heading("%")*Format(digits = 1)*Percent("row")), data = wb.df) # display count and percentage

write.csv.tabular(wb.coverage.table, "output/wb_estimated_coverage_no_pws.csv")


# Generate summary table with PWS locations -------------------------------

library(tables)
library(splitstackshape) # for expandRows()

# Calculates the radius of the circumcircle for a square of length window.width
minDist <- function(window.width) (sqrt(2*window.width^2))/2

# PWS intake location to NHD waterbody nearest spatial join 
# performed in ArcGIS 10.2.2, 100 m max search radius
# arcpy.SpatialJoin_analysis(
#   target_features = "PWS_AEA",
#   join_features = "nhd_wb_subset",
#   out_feature_class = "PWS_AEA_100m_spatialJoin_NHD_NLAsubset.shp",
#   join_operation = "JOIN_ONE_TO_ONE",
#   join_type = "KEEP_ALL",
#   field_mapping = """PWSID "PWSID" true true false 254 Text 0 0 ,First,#,PWS_AEA,PWSID,-1,-1;
#   FACILITY_I "FACILITY_I" true true false 254 Text 0 0 ,First,#,PWS_AEA,FACILITY_I,-1,-1;
#   AVAILABILI "AVAILABILI" true true false 254 Text 0 0 ,First,#,PWS_AEA,AVAILABILI,-1,-1;
#   LATITUDE "LATITUDE" true true false 19 Double 0 0 ,First,#,PWS_AEA,LATITUDE,-1,-1;
#   LONGITUDE "LONGITUDE" true true false 19 Double 0 0 ,First,#,PWS_AEA,LONGITUDE,-1,-1;
#   In_lake "In_lake" true true false 1 Text 0 0 ,First,#,PWS_AEA,In_lake,-1,-1;
#   Distance_t "Distance_t" true true false 19 Double 0 0 ,First,#,PWS_AEA,Distance_t,-1,-1;
#   COMID_OW "COMID_OW" true true false 9 Long 0 9 ,First,#,PWS_AEA,COMID,-1,-1;
#   PWS_TYPE_C "PWS_TYPE_C" true true false 254 Text 0 0 ,First,#,PWS_AEA,PWS_TYPE_C,-1,-1;
#   POPULATION "POPULATION" true true false 19 Double 0 0 ,First,#,PWS_AEA,POPULATION,-1,-1;
#   SERVICE_CO "SERVICE_CO" true true false 19 Double 0 0 ,First,#,PWS_AEA,SERVICE_CO,-1,-1;
#   COMID_JCjoin "COMID_JCjoin" true true false 10 Double 0 10 ,First,#,nhd_wb_subset,COMID,-1,-1;
#   FDATE "FDATE" true true false 80 Text 0 0 ,First,#,nhd_wb_subset,FDATE,-1,-1;
#   RESOLUTION "RESOLUTION" true true false 80 Text 0 0 ,First,#,nhd_wb_subset,RESOLUTION,-1,-1;
#   GNIS_ID "GNIS_ID" true true false 80 Text 0 0 ,First,#,nhd_wb_subset,GNIS_ID,-1,-1;
#   GNIS_NAME_1 "GNIS_NAME_1" true true false 80 Text 0 0 ,First,#,nhd_wb_subset,GNIS_NAME,-1,-1;
#   ELEVATION "ELEVATION" true true false 24 Double 15 23 ,First,#,nhd_wb_subset,ELEVATION,-1,-1;
#   REACHCODE "REACHCODE" true true false 80 Text 0 0 ,First,#,nhd_wb_subset,REACHCODE,-1,-1;
#   FTYPE "FTYPE" true true false 80 Text 0 0 ,First,#,nhd_wb_subset,FTYPE,-1,-1;
#   FCODE "FCODE" true true false 10 Double 0 10 ,First,#,nhd_wb_subset,FCODE,-1,-1;
#   SHAPE_LENG "SHAPE_LENG" true true false 24 Double 15 23 ,First,#,nhd_wb_subset,SHAPE_LENG,-1,-1;
#   SHAPE_AREA "SHAPE_AREA" true true false 24 Double 15 23 ,First,#,nhd_wb_subset,SHAPE_AREA,-1,-1;
#   MAX "MAX" true true false 24 Double 15 23 ,First,#,nhd_wb_subset,MAX,-1,-1""",
#   match_option = "CLOSEST",
#   search_radius = "100 Meters",
#   distance_field_name = "NEAR_NHD"
# )

# load ArcGIS output
pws <- shapefile(file.choose())
pws$comid <- pws$COMID_JCjo
# discard pws locations >100 m from NHD
pws <- pws[pws$comid != 0,] # ArcGIS output has attribute value of 0 when no spatial join found

# load extent/footprints of regional MERIS product  
meris.regions <- shapefile("input_data/meris_regions.shp")

# load nhd waterbodies with $shore_dist field from "calc_shore_dist.R", max distance from shore in meters
nhd.wb <- shapefile("input_data/nhd_wb_nlasub.shp")
nhd.wb$region <- over(nhd.wb, meris.regions)$region
# geospatial processes completed, can remove nhd.wb shapefile to free up memory
nhd.wb.df <- nhd.wb@data

# calculate frequency of PWS intake by NHD COMID (from ArcGIS spatial join)
pws.table <- as.data.frame(table(pws$comid), responseName = "pws.count")
nhd.wb.df <- merge(nhd.wb.df, pws.table, by.x = 'COMID', by.y = 1, all.x = T)

# determine whether each waterbody polygon has sufficient open water area to accomodate
# focal windows of various target widths.  This could be wrapped into the tabular() call
# below, seperated for clarity.
nhd.wb.df$w30 <- nhd.wb.df$shore_dist >= minDist(30) # 21
nhd.wb.df$w90 <- nhd.wb.df$shore_dist >= minDist(90)
nhd.wb.df$w300 <- nhd.wb.df$shore_dist >= minDist(300)
nhd.wb.df$w900 <- nhd.wb.df$shore_dist >= minDist(900) # 636

# finagle tabular() into counting multiple PWS intakes on the same waterbody
# by duplicating rows by the number of PWS intakes.
nhd.wb.df.expand <- nhd.wb.df[!is.na(nhd.wb.df$pws.count),]
nhd.wb.df.expand <- expandRows(nhd.wb.df.expand, 'pws.count', drop = F)
nhd.wb.df.expand$type <- 'PWS'
nhd.wb.df$type <- 'WB'
wb.df <- rbind(nhd.wb.df.expand, nhd.wb.df)

# the syntax for tabular() can be a little confusing. See bottom of doc for example output
wb.coverage.table <- tabular((Heading("Region") * (as.factor(region) + 1)) * # group data by region
                               Heading(Feature) * (Heading("Waterbody")*(type == 'WB') + # only waterbody records
                                                     Heading("Waterbody w/ PWS") * (pws.count > 0 & type == 'WB') + # only waterbody w/ pws records
                                                     Heading("PWS") * (type == 'PWS')) ~  #only pws records
                               Heading("Window Size") * # add col for each window width
                               (Heading("30x30") * w30 + Heading("90x90") * w90 + Heading("300x300") * w300 + Heading("900x900") * w900) * 
                               (Heading("n")*1 +Heading("%")*Format(digits = 1)*Percent("row")), data = wb.df) # display count and percentage

write.csv.tabular(wb.coverage.table, "output/wb_estimated_coverage.csv")

# Example spatial coverage estimate table output --------------------------

# > wb.coverage.table
#                       
#                         Window Size                                               
#                         30x30             90x90        300x300       900x900      
# Region      Feature      n           %     n      %     n       %     n       %    
# Florida     Waterbody   10910     100.0   8752  80.2   905     8.3  140      1.3
#   Waterbody w/ PWS      5         100.0      5 100.0     4    80.0    3     60.0
#   PWS                   10        100.0     10 100.0     8    80.0    7     70.0
# New England Waterbody   10968     100.0   7089  64.6  1016     9.3   73      0.7
#   Waterbody w/ PWS      446       100.0    416  93.3   149    33.4   15      3.4
#   PWS                   595       100.0    559  93.9   249    41.8   67     11.3
# Ohio        Waterbody   4591      100.0   2067  45.0   130     2.8   18      0.4
#   Waterbody w/ PWS      98        100.0     91  92.9    32    32.7   10     10.2
#   PWS                   187       100.0    178  95.2    88    47.1   51     27.3
# All         Waterbody   275897    100.0 170240  61.7 15545     5.6 1862      0.7
#   Waterbody w/ PWS      1991      100.0   1849  92.9   860    43.2  300     15.1
#   PWS                   3086      100.0   2920  94.6  1754    56.8 1007     32.6