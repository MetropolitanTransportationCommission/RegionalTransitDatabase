#be sure to set the project path
PROJECT_PATH <- "C:/projects/RTD/RegionalTransitDatabase"

GTFS_PATH <- paste0(PROJECT_PATH,"/data/gtfs_interpolated_05_01_20175/",collapse="")
R_HELPER_FUNCTIONS_PATH <- paste0(PROJECT_PATH,"/R/r511.R",collapse="")
source(R_HELPER_FUNCTIONS_PATH)
CREDENTIALS_PATH <- paste0(PROJECT_PATH,"/credentials.R",collapse="") 

# if (!require(devtools)) {
#   install.packages('devtools')
# }
# devtools::install_github('MetropolitanTransportationCommission/gtfsr')

library(gtfsr)
library(dplyr)
library(rgeos)
library(reshape2)
library(sp)

setwd(GTFS_PATH)

library(rjson)
json_file <- paste0(PROJECT_PATH,"/data/orgs.json",collapse="")
providers <- fromJSON(paste(readLines(json_file), collapse=""))

################################################
# Section 3. Read a single provider set using GTFSr

l_gtfs_obj <- list()

l_high_frqncy_stops <- list()
l_high_frqncy_rt_bffrs_1_4 <- list()
l_high_frqncy_rt_bffrs_1_2 <- list()
l_priority_routes_no_geom <- list()

for (provider in providers) {

  zip_path <- paste0(provider,".zip",collapse="")
  gtfs_obj <- import_gtfs(zip_path, local=TRUE)
  
  l_gtfs_obj[[provider]] <- get_priority_routes(gtfs_obj)
}
  
for (provider in providers) {
  if(exists(provider, where=l_gtfs_obj)){
    gtfs_obj_mtc <- l_gtfs_obj[[provider]]
    #this is a legacy setup--looping through these lists
    if(exists("mtc_priority_stops", where=gtfs_obj_mtc) && 
       dim(gtfs_obj_mtc$mtc_priority_stops)[1]>0) {
      l_high_frqncy_stops[[provider]] <- gtfs_obj_mtc$mtc_priority_stops
    }
    #l_high_frqncy_routes[[provider]] <- gtfs_objp$mtc_priority_routes
    
    ##TPA Geometry output - Line Buffers
    if(exists("mtc_priority_routes", where=gtfs_obj_mtc) && 
      dim(gtfs_obj_mtc$mtc_priority_stops)[1]>0) {
      l_high_frqncy_rt_bffrs_1_4[[provider]] <- get_buffered_tpa_routes(gtfs_obj_mtc$mtc_priority_routes, 
                                                                        gtfs_obj_mtc, 
                                                                        buffer=402.336)
      l_high_frqncy_rt_bffrs_1_2[[provider]] <- get_buffered_tpa_routes(gtfs_obj_mtc$mtc_priority_routes, 
                                                                        gtfs_obj_mtc, 
                                                                        buffer=804.672)
      l_priority_routes_no_geom[[provider]] <- l_gtfs_obj$SF$mtc_priority_routes$route_id[!l_gtfs_obj$SF$mtc_priority_routes$route_id %in% l_high_frqncy_rt_bffrs_1_2$SF$route_id]
                                                                        
    }
  }
}

#################
#bind outputs
#together
################

l_rts <- lapply(l_gtfs_obj,function(x) x$mtc_routes_df)
df_mtc_routes <- bind_df_list(l_rts)

l_rts_drctnl <- lapply(l_gtfs_obj,function(x) x$mtc_routes_df_directional_stats)
df_rts_drctnl <- bind_df_list(l_rts_drctnl)

l_priority_routes <- lapply(l_gtfs_obj,function(x) x$mtc_priority_routes)
df_rts_priority <- bind_df_list(l_priority_routes)

hf_rts_1_4_ml_buf <- bind_list_of_routes_spatial_dataframes(x$mtc_priority_routes)

hf_rts_1_2_ml_buf <- bind_list_of_routes_spatial_dataframes(l_high_frqncy_rt_bffrs_1_2)

hf_stops <- bind_list_of_routes_spatial_dataframes(l_high_frqncy_stops)

# names character fix
# for why, see: http://r-sig-geo.2731867.n2.nabble.com/Bug-in-writeOGR-MSSQLSpatial-driver-td7583633.html
row.names(hf_stops) <- as.character(1:nrow(hf_stops))

#buffer the stops
hf_stops_with_hf_neighbors <- hf_stops[hf_stops$lgcl_adjacent_hf_routes==TRUE,]

hf_stops_with_hf_neighbors_buffer <- SpatialPolygonsDataFrame(
  gBuffer(hf_stops_with_hf_neighbors,width=804.672,byid = TRUE),
  data=hf_stops_with_hf_neighbors@data)

############
##Write to Files
############

write_to_geopackage_with_date(hf_stops)
write_to_geopackage_with_date(hf_stops_with_hf_neighbors_buffer)
write_to_geopackage_with_date(hf_rts_1_2_ml_buf)
write_to_geopackage_with_date(hf_rts_1_4_ml_buf)

library(knitr)
sink(paste0(PROJECT_PATH,"/data/routes_tpa_flags_headways.md"))
kable(mtc_routes_df)
sink()
write.csv(mtc_routes_df, file=paste0(PROJECT_PATH,"/data/routes_tpa_flags_headways.csv"))

sink(paste0(PROJECT_PATH,"/data/routes_directional_headways.md"))
kable(df_rts_drctnl)
sink()
write.csv(df_rts_drctnl, file=paste0(PROJECT_PATH,"/data/routes_directional_headways.csv"))

sink(paste0(PROJECT_PATH,"/data/routes_priority.md"))
kable(df_rts_priority)
sink()
write.csv(df_rts_priority, file=paste0(PROJECT_PATH,"/data/routes_priority.csv"))

# #check that all priority routes have a buffered geometry
# sink(paste0(PROJECT_PATH,"/data/priority_routes_no_geom.log"))
# cat(l_priority_routes_no_geom)
# sink()




