John M. Clark
9/20/2016
EPA/ORISE

This directory contains instructions, R code, and data for "An approach to monitoring cyanobacteria blooms 
at surface drinking water intakes using satellite imagery". See manuscript for details. 

Directory structure:
JClark_CyAN_2016_final/
	bloomrs/ - r functions developed for this study
	input_data/ - source and preprocessed intermediary spatial data
	manuscript/ - text and figures
	output/ - target for R script outputs
	output_archives/ - archived runs of the analysis.  Zip files marked SENSITIVE contain SENSITIVE public 
		health information and MUST BE REMOVED before distributing
	packrat/ - source files for R package versions required by this analysis
	supporting_documents/
	
The R code will ultimately be combined into an R package, 'bloomrs', but is currently a collection of discrete 
R scripts. The analysis has been structured into three stages.  Subsequent stages may require data inputs from 
preceding stages.  Example intermediary outputs have been provided in "input_data" to allow users to bypass 
earlier processing stages.

Analysis stages:
1.) estimate_spatial_coverage.R
	stand alone analysis
	input:
		NHD waterbodies shapefile: Example data = "input_data/nhd_nla_subset_shore_dist.shp". See additional notes.
		regions shapefile: Polygon boundaries of available raster imagery. 
			Example data = "input_data/meris_regions.shp". See "prepare_summarize_raster_brick.R" for code to 
			derive custom extents.
		PWSI locations (optional): Point locations of interest.  
			Simulated example data = "input_data/simulated_pws.shp". See additional notes.
	output:
		"nhd_nla_subset_shore_dist.shp": NHD waterbodies shapefile with additional "shore_dist" and 
			"max_window" fields.
		"wb_estimated_coverage.csv": Results summary table.
2.) prepare_summarize_raster_brick.R
	input:
		MERIS geotiffs folder: Daily CI-cyano raster data provided by NOAA. 
			Florida and Ohio region archive = "input_data/cicyano_scenes.zip". 
			See "additional_documents/ReadMe_MERIS_8bit_description.rtf"
		NHD waterbodies shapefile (optional): For masking the output raster brick.  
			Example data = "input_data/nhd_nla_subset_shore_dist.shp". See additional notes.
	output:
		Geotiff raster brick:  Binned input imagery, optionally reprojected and masked.
		"*_nhd_wb_mask.tif": Rasterized mask of waterbodies.
		"*_temporal_coverage.tif": Ratio of valid observations (non-NA values) to number of raster brick layers.
		"*_data_mask.tif": Mask of all locations with at least one valid observation.
		"*_bloom_freq.tif": Ratio of observations above WHO high threshold to valid observations.
3.) extract_summarize_pws.R
	input:
		CI-cyano raster brick: Preprocessed regions = "input_data/fl_cicyano_weekly_mask.tif", 
			"input_data/oh_cicyano_weekly_mask.tif".
		NHD waterbodies shapefile: Example data = "input_data/nhd_nla_subset_shore_dist.shp". Must have 
			"shore_dist" field from "estimate_spatial_coverage.R".
		NHD HUC12 watersheds: Example data = "input_data/nhd_huc12.shp".
		regions shapefile: Polygon boundaries of available raster imagery. 
			Example data = "input_data/meris_regions.shp". 
			See "prepare_summarize_raster_brick.R" for code to derive custom extents.
		PWSI locations: Point locations of interest.  
			Simulated example data = "input_data/simulated_pws.shp". See additional notes.
		"*_data_mask.tif": Mask of all locations with at least one valid observation.
		"*_bloom_freq.tif": Ratio of observations above WHO high threshold to valid observations.
	output:
		"pws_snap_summary.csv": PWS snapping (relating to nearby MERIS data) results table.
		"pws_quantiles_table.csv": Summary PWS bloom frequency classified by quantile break points calculated 
			from all features (e.g. all waterbodies for snap_case="waterbody").
		"freq_rank_*.csv": PWS, waterbody, and HUC12 features ranked by bloom frequency.
		"*_hab_results.shp": PWS, waterbody, and HUC12 shapefiles with appended summary statistics.


Notes

Processing environment
	R version: 3.3.0 (2016-05-03) -- "Supposedly Educational"
	Platform: x86_64-w64-mingw32/x64 (64-bit)
	RStudio version: 0.99.896
	Dell Precision T7610
	Windows 7 Enterprise x64
	Intel Xeon E5-2630 v2 @ 2.60GHz x 2
	32 GB RAM

Packrat
	Packrat is an r package designed to "manage the R packages your project depends on in an isolated, portable, 
	and reproducible way." At the time of writing this document, the current implementation is not functioning 
	properly. However, this directory does contain "packrat.lock" which describes all packages and version 
	numbers used in this study, sources for those packages in "src/", and installers for R and RStudio. If 
	you're from the future, information in this directory may assist with replicating this study.  Also, have 
	they built a better mousetrap yet?

Geotiff raster brick metadata
	Processing the MERIS imagery to a geotiff, rather than grid, raster brick greatly reduced file size and 
	subsequent processing times. However, using setZ() to store date/time information is not persistent 
	when writing to disk. "geotiff_metadata.R" is a simple workaround which writes and transfers these data 
	in a .csv (".xeta").

Altering bloom thresholds
	Pixels were classified as "bloom" if DN(CI-cyano) > 100, reflecting the WHO high threshold (see manuscript). 
	Currently, substituting alternative threshold values within function calls is not implemented. However, 
	this can be accomplished with relatively simple alterations to "summarize_brick.R" and "extract_ts.R".

"nhd_nla_subset_shore_dist.shp"
	-merged regional NHD waterbody shapefile
	-projected to Albers Equal Area
	-subset to FCodes (39000, 39004, 39009, 39010, 39011, 39012, 43600, 43613, 43615, 43617, 43618, 43619, 43621)
	--excludes intermittent or estuarine waterbodies
	-subset to waterbodies > 0.01 sqkm
	-"shore_dist" field: maximum euclidian distance to shore, calculated from 30-m raster grid in R
	-"max_window" field: estimated maximum window width. max_window = shore_dist*2/sqrt(2)

Projection
	Analysis was performed using Albers Equal Area projection for CONUS.  However, minor variations in the CRS 
	string formatting can lead to superficial errors:
	# > identicalCRS(test.cicyano, fl.wb)
	# [1] FALSE
	# > crs(test.wb)
	# CRS arguments:
	#   +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 
	# > crs(test.cicyano)
	# CRS arguments:
	#   +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0 
 
PWS to NHD waterbody spatial join
	This is the only step that was processed outside of R. ArcGIS 10.2.2). Log:
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