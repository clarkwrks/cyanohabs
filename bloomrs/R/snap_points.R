library(sp)
library(rgdal)
library(raster)
library(rgeos)

#' Snaps an input location to the nearest availabile data. "snapping.cases" 
#' dictate selection criteria and precision. See powerpoint.
#' 
#' @param focal.point reported location of feature (intake). Required fields: unique_id, huc12, wb_comid
#' @param candidate.points shapefile of locations with available data (created by bloomr::GenCandidatePoints)
#' @param output.prefix
#' @return list with one SpatialPointsDataFrame for each snapping case, possibly empty
SnapPoints <- function(focal.point, candidate.points, output.prefix = "test", 
                       snap.cases = c("proximate", "adjacent", "waterbody", "watershed")){
  # assign focal.point's unique_id to all candidate.points 
  candidate.points$unique_id <- focal.point$unique_id
  # caculate the distance from each canidate point to the focal point
  candidate.points$snap_dist <- spDistsN1(candidate.points, focal.point)
  # select all candidate points at least 600 m from shore.  For the spatial
  # coverage estimate a value of 636 m is used.  Here we round down to the
  # nearest pixel (300 m).
  core.points <- candidate.points[candidate.points$shr_dst_m >= 600, ]
  # initialize an empty list for appending valid snap cases
  snap.results <- list()
  if("adjacent" %in% snap.cases){
    # select all core.points within 300 m of focal.point
    near.points <- core.points[core.points$snap_dist <= 300, ]
    if(nrow(near.points) > 0){
      print(paste0(focal.point$unique_id, ": Snapping to 3x3 within 300 m"))
      # find closest point
      snap.point <- near.points[which.min(near.points$snap_dist), ]
      # calculate the distance from candidate.points to the snap.point
      candidate.points$window.dist <- spDistsN1(candidate.points, snap.point)
      # select the 9 points closest to the snap.point
      adjacent.points <- candidate.points[with(candidate.points@data, order(window.dist, snap_dist)), ][1:9,]
      adjacent.points$case <- "adjacent"
      # not all cases will add a window.dist value, remove to avoid errors later
      adjacent.points$window.dist <- NULL
      candidate.points$window.dist <- NULL
#       adjacent.points$snap_dist <- snap.point$snap_dist
      snap.results <- c(snap.results, adjacent.points)
    } else {
      print(paste0(focal.point$unique_id, ": No adjacent points available"))
    }
  }
  if("proximate" %in% snap.cases){
    # this is identical to the code block for the adjacent case, except that
    # we select all core.point within 900 m of the focal.point
    near.points <- core.points[core.points$snap_dist <= 900, ]
    if(nrow(near.points) > 0){
      print(paste0(focal.point$unique_id, ": Snapping to 3x3 within 900 m"))
      snap.point <- near.points[which.min(near.points$snap_dist), ]
      candidate.points$window.dist <- spDistsN1(candidate.points, snap.point)
      proximate.points <- candidate.points[with(candidate.points@data, order(window.dist, snap_dist)), ][1:9,]
      proximate.points$case <- "proximate"
      proximate.points$window.dist <- NULL
      candidate.points$window.dist <- NULL
#       proximate.points$snap_dist <- snap.point$snap_dist
      snap.results <- c(snap.results, proximate.points)
      
    } else {
      print(paste0(focal.point$unique_id, ": No proximate points available"))
    }
  }
  if("waterbody" %in% snap.cases){
    # select candidate.points within 900 m of poi.point
    waterbody.points <- candidate.points[candidate.points$snap_dist <= 900, ]
    # expand selection to all candidate.points with matching comids
    waterbody.points <- candidate.points[candidate.points$wb_comid %in% waterbody.points$wb_comid, ]
    # select only comids with >= 9 candidate.points
    waterbody.points <- waterbody.points[as.data.frame(table(waterbody.points$wb_comid))$Freq > 8, ]
    # select closest comid
    wb.point <- waterbody.points[which.min(waterbody.points$snap_dist), ]
    # select all candidate.points with matching comid
    waterbody.points <- candidate.points[candidate.points$wb_comid %in% wb.point$wb_comid,]
    if(nrow(waterbody.points) > 8){
      print(paste0(focal.point$unique_id, ": Snapping to nearest waterbody"))
      waterbody.points$case <- "waterbody"
      snap.results <- c(snap.results, waterbody.points)
    } else {
      print(paste0(focal.point$unique_id, ": No waterbody points available"))
    }
  }
  if("watershed" %in% snap.cases){
    # select all candidate.points matching focal.point comid
    watershed.points <- candidate.points[which(candidate.points$huc12 == focal.point$huc12), ]
    if(nrow(watershed.points) > 8){
      print(paste0(focal.point$unique_id, ": Snapping to nearest watershed"))
      watershed.points$case <- "watershed"
      snap.results <- c(snap.results, watershed.points)
    } else {
      print(paste0(focal.point$unique_id, ": No watershed points available"))
    }
  } 
  if(length(unlist(snap.results)) == 0){
    null.point <- candidate.points[which.min(candidate.points$snap_dist), ]
    null.point$case <- "unresolved"
    print(paste0(focal.point$unique_id, ": Unable to resolve"))
    return(null.point)
  }
  snap.dir <- paste0(output.prefix, "_snapping/")
  dir.create(snap.dir, showWarnings = FALSE)
  
  # write each set of snap points to a seperate shapefile "/snapping/output.prefix_case_uniqueid.shp" 
  lapply(1:length(snap.results), function(i) shapefile(x = snap.results[[i]], 
                                                       filename = paste0(snap.dir, snap.results[[i]]$case[1], "_", snap.results[[i]]$unique_id[1])))
  return(snap.results)
}

MultipointToSingle <- function(snap.points){
  snap.point <- snap.points[which.min(snap.points$snap_dist), ]
}


# # create dummy points and required attributes
# test.coords <- data.frame(x = c(1451894, 1446547, 1448165, 1450797), y = c(579348.6, 586258.3, 580445.4, 574166.3))
# test.attrs <- data.frame(unique_id=as.factor(c("a111", "b222", "c333", "d444")), 
#                         huc12 = rep("030901011703", 4), 
#                         wb_comid = c(21489874, 21489752, 21489802, NA))
# proj.albers <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")
# test.spdf <- SpatialPointsDataFrame(test.coords, test.attrs, proj4string=proj.albers)
# 
# # need candidate.points pre-generated by "gen_candidate_points.R"
# fl.candidate.points <- GenCandidatePoints(fl.water.mask, "test", fl.nhd.wb, fl.nhd.huc12)
# 
# # a single focal.point and one snapping case
# test.snap <- SnapPoints(test.spdf[3,], fl.candidate.points, c("proximate"), output.prefix = "test")
# # loop through all points in a shapefile and all snapping cases
# test.snaps <- lapply(1:length(test.spdf), function(i) 
#   SnapPoints(test.spdf[i,], fl.candidate.points, output.prefix = "test1"))
# 
# 
# pws <- shapefile(file.choose())
# # discard pws locations >100 m from NHD
# pws <- pws[pws$COMID_JCjo != 0,]
# pws$unique_id <- paste0(pws$PWSID, pws$FACILITY_I)
# crs(pws) <- crs(fl.water.mask)
# fl.pws <- crop(pws, fl.water.mask)
# fl.pws$huc12 <- over(fl.pws, fl.nhd.huc12)$HUC_12
# fl.pws$wb_comid <- over(fl.pws, fl.nhd.wb)$COMID
# 
# fl.pws.snaps <- lapply(1:length(fl.pws), function(i) 
#   SnapPoints(fl.pws[i,], fl.candidate.points, output.prefix = "flpwstest"))