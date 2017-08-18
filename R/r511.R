#'Analyze a GTFS priority object for a provider to determine which bus stops are eligible for Transit Priority Areas (http://www.fehrandpeers.com/sb743/).
#'@param A gtfsr priority object as returned by get_priority_routes()
#'@returns A dataframe for high frequency stops
get_priority_stops <- function(gtfs_objp) {
  #only calculate and save distance information if the routes dataframe exists for the provider
  #drop stops not on hf routes
  ###########################################################################################
  # Section 6. Join the calculated am and pm peak routes (tpa eligible) back to stop tables
  
  df_rt_hf <- join_high_frequency_routes_to_stops(gtfs_objp$mtc_am_stops,
                                                  gtfs_objp$mtc_pm_stops,
                                                  gtfs_objp$mtc_am_routes,
                                                  gtfs_objp$mtc_pm_routes)
  
  #df_rt_hf <- gtfs_objp$mtc_routes_df[gtfs_objp$mtc_routes_df$f_tpa_qual==TRUE,]
  df_sr <- gtfs_objp$mtc_combined_gtfs
  
  # df_rt_hf_non_qualifying <- join_high_frequency_routes_to_stops(am_stops,pm_stops,am_routes_nonq,pm_routes_nonq)
  # 
  # ###########################################################################################
  df_stp_rt_hf <- join_mega_and_hf_routes(df_sr,df_rt_hf)
  
  df_stp_rt_hf <- deduplicate_final_table(df_stp_rt_hf)
  
  #Remove select cols.
  df_stp_rt_hf <- df_stp_rt_hf[-c(1:13)]
  # 
  
  df_stp_rt_hf$cnt_adjacent_hf_routes <- rep(0,nrow(df_stp_rt_hf))
  df_stp_rt_hf$lgcl_adjacent_hf_routes <- rep(FALSE,nrow(df_stp_rt_hf))
  
  #############################
  ##Put stops into a 
  ##SpatialPointsDataFrame
  ###########################
  
  df_stp_rt_hf_xy <- as.data.frame(df_stp_rt_hf)
  coordinates(df_stp_rt_hf_xy) = ~stop_lon + stop_lat
  proj4string(df_stp_rt_hf_xy) <- CRS("+proj=longlat +datum=WGS84")
  df_stp_rt_hf_xy <- spTransform(df_stp_rt_hf_xy, CRS("+init=epsg:26910"))
  
  ################
  ###############
  
  #########
  ###Route Distance (02. miles)
  ###from hf Stops
  ########
  
  #get high freq stops with hf neigbors
  m1 <- gWithinDistance(df_stp_rt_hf_xy, df_stp_rt_hf_xy, byid = TRUE, dist = 321.869)
  m2 <- outer(df_stp_rt_hf_xy$route_id,df_stp_rt_hf_xy$route_id, FUN= "!=")
  m3 <- m1 == TRUE & m2 == TRUE
  
  number_of_routes_within_distance <- function(x){table(x)['TRUE']}
  l1 <- apply(m3,2,number_of_routes_within_distance)
  df_stp_rt_hf_xy$cnt_adjacent_hf_routes <- l1
  
  within_distance_of_more_than_one_route <- function(x){table(x)['TRUE']>0}
  l2 <- apply(m3,2,within_distance_of_more_than_one_route)
  l2[is.na(l2)] <- FALSE
  
  df_stp_rt_hf_xy$lgcl_adjacent_hf_routes <- l2
  
  hf_routes <- gtfs_objp$mtc_routes_df_sp[gtfs_objp$mtc_routes_df_sp$f_tpa_qual==TRUE,]
  df_stp_rt_hf_xy$dst_frm_rte <- get_stops_distances_from_routes(df_stp_rt_hf_xy,hf_routes)
  
  return(df_stp_rt_hf_xy)
}

#'Get a buffer around a priority route from a gtfs object
#'@param df_qualifying_routes tpa qualifying routes subset from gtfs_obj routes table
#'@param gtfs_obj a gtfsr object
#'@returns a buffered SpatialPolygons dataframe around that route
get_buffered_tpa_routes <- function(df_qualifying_routes, gtfs_obj, buffer=402.336) {
  tpa_route_ids <- names(table(df_qualifying_routes$route_id))
  row.names(df_qualifying_routes) <- df_qualifying_routes$route_id
  spply <- get_non_directional_route_geometries(tpa_route_ids, gtfs_obj, buffer=buffer)
  df_rt_frqncy_sptl <- SpatialPolygonsDataFrame(Sr=spply, data=as.data.frame(df_qualifying_routes),FALSE)
  row.names(df_rt_frqncy_sptl) <- as.character(seq(1,nrow(df_rt_frqncy_sptl)))
  return(df_rt_frqncy_sptl)                                   
}


#'Analyze a GTFS object for a provider to determine which bus routes are eligible for Transit Priority Areas (http://www.fehrandpeers.com/sb743/). Return a spatial dataframe with data attached for inspection and review of methodology.
#'@param A gtfsr object
#'@returns A gtfsr object with the following tables added to it:
#       mtc_combined_gtfs - the GTFS tables all joined together into 1 table
#       mtc_routes_df - the gtfs routes table with statistics on am and pm headways and TPA qualifications (http://www.fehrandpeers.com/sb743/) 
#       mtc_routes_df_sp - same as above table but with a 10 cm SpatialPolygons object for each route (to represent coverage in all directions)
get_priority_routes <- function(gtfs_obj) {
  #######
  ##Stops
  #######
  
  ###############################################
  # Section 4. Join all the GTFS provider tables into 1 table based around stops
  
  df_sr <- join_all_gtfs_tables(gtfs_obj)
  df_sr <- make_arrival_hour_less_than_24(df_sr)
  
  gtfs_obj$mtc_combined_gtfs <- as.data.frame(df_sr)
  
  ###########################################################################################
  # Section 5. Create Peak Headway tables for weekday trips 
  
  am_stops <- flag_and_filter_peak_periods_by_time(df_sr,"AM")
  am_stops <- remove_duplicate_stops(am_stops) #todo: see https://github.com/MetropolitanTransportationCommission/RegionalTransitDatabase/issues/31
  am_stops <- count_trips(am_stops) 
  gtfs_obj$mtc_am_stops <- am_stops
  
  pm_stops <- flag_and_filter_peak_periods_by_time(df_sr,"PM")
  pm_stops <- remove_duplicate_stops(pm_stops) #todo: see https://github.com/MetropolitanTransportationCommission/RegionalTransitDatabase/issues/31
  pm_stops <- count_trips(pm_stops)
  gtfs_obj$mtc_pm_stops <- pm_stops 
  
  #create an mtc routes table on the gtfs object
  #we will use this to store tpa qualification flags
  gtfs_obj$mtc_routes_df <- gtfs_obj$routes_df[,-6][,-6]
  
  #########
  ######get all routes with stats
  ########
  am_routes <- get_routes(am_stops)
  pm_routes <- get_routes(pm_stops)
  
  #subset stops to those that meet the headway  
  am_stops_hdwy <- subset(am_stops,
                          am_stops$Headways < 16)
  am_routes_hdwy <- get_routes(am_stops_hdwy)
  gtfs_obj$mtc_am_routes <- am_routes_hdwy
  
  pm_stops_hdwy <- subset(pm_stops,
                          pm_stops$Headways < 16)
  pm_routes_hdwy <- get_routes(pm_stops_hdwy)
  gtfs_obj$mtc_pm_routes <- am_routes_hdwy
  
  #update the flags for these on the mtc routes df
  gtfs_obj$mtc_routes_df$f_am <- gtfs_obj$mtc_routes_df$route_id %in% am_routes$route_id
  gtfs_obj$mtc_routes_df$f_pm <- gtfs_obj$mtc_routes_df$route_id %in% pm_routes$route_id
  gtfs_obj$mtc_routes_df$f_am_ls_thn_15m <- gtfs_obj$mtc_routes_df$route_id %in% am_routes_hdwy$route_id
  gtfs_obj$mtc_routes_df$f_pm_ls_thn_15m <- gtfs_obj$mtc_routes_df$route_id %in% pm_routes_hdwy$route_id
  
  ########
  ##Routes-Qualifying Check
  ########
  
  # Input: am_routes and pm_routes dataframes
  # Output: qualifying routes and nearly qualifying routes spatial dataframes
  
  # divide into 2 dataframes:
  # 1) routes that are in both am and pm peak periods (qualifying)
  # 2) routes that are in one or the other but not both, or not in both (nearly qualifying)
  
  ###### 1

  gtfs_obj$mtc_routes_df$f_am_bdl <- rep(FALSE,nrow(gtfs_obj$mtc_routes_df))
  gtfs_obj$mtc_routes_df$f_pm_bdl <- rep(FALSE,nrow(gtfs_obj$mtc_routes_df))
  gtfs_obj$mtc_routes_df$f_tpa_qual <- rep(FALSE,nrow(gtfs_obj$mtc_routes_df))
  
  if (dim(am_routes_hdwy)[1] > 0) {
    am_routes_qual <- am_routes_hdwy[!duplicated(as.list(am_routes_hdwy)),]
    am_routes_qual <- am_routes_qual[is_in_both_directions(am_routes_qual[,c("route_id","direction_id")]) 
                                     | is_loop_route(am_routes_qual$trip_headsign),]
    gtfs_obj$mtc_routes_df$f_am_bdl <- gtfs_obj$mtc_routes_df$route_id %in% am_routes_qual$route_id
  }
  
  if (dim(pm_stops_hdwy)[1] > 0) {
    pm_routes_qual <- pm_routes[!duplicated(as.list(pm_stops_hdwy)),]
    pm_routes_qual <- pm_routes_qual[is_in_both_directions(pm_routes_qual[,c("route_id","direction_id")]) 
                                     | is_loop_route(pm_routes_qual$trip_headsign),]
    gtfs_obj$mtc_routes_df$f_pm_bdl <- gtfs_obj$mtc_routes_df$route_id %in% pm_routes_qual$route_id
  }
  
  #this is a terrible way to do things--should just be flagging and uysing route_id
  if (exists("am_routes_qual") && is.data.frame(get("am_routes_qual")) &&
      exists("pm_routes_qual") && is.data.frame(get("pm_routes_qual")) &&
      dim(am_routes_qual)[1] > 0 && dim(pm_routes_qual)[1] > 0) {
        am_pm_union <- union(am_routes_qual$route_id, pm_routes_qual$route_id)
        am_pm_intersection <- intersect(am_routes_qual$route_id, pm_routes_qual$route_id)
        df1 <- rbind(am_routes_qual,pm_routes_qual)
        df_qualifying_routes <- df1[df1$route_id %in% am_pm_intersection,]
        gtfs_obj$mtc_routes_df$f_tpa_qual <- gtfs_obj$mtc_routes_df$route_id %in% df_qualifying_routes$route_id
  }
  
  ########
  ##Routes - End qualifying check
  ########      

  #also terrible--see above
  if (exists("am_routes") && is.data.frame(get("am_routes")) &&
      exists("pm_routes") && is.data.frame(get("pm_routes")) &&
      dim(am_routes)[1] > 0 && dim(pm_routes)[1] > 0) {
    
    df_am_pm_headways <- rbind(am_routes,pm_routes)
    
    #todo: get the route stats onto the routes in a way 
    #that lets you look at inbound and outbound stats
    #seems like you should also be able to just ask: 
    #whats the headway between these hours, etc
    df_am_pm_headways_stats <- get_route_stats(df_am_pm_headways)
    gtfs_obj$mtc_routes_df_directional_stats <- df_am_pm_headways_stats
    
    df_am_pm_headways_stats_non_directional <- get_route_stats_no_direction(df_am_pm_headways)
    
    #put the stats in the gtfs_obj
    gtfs_obj$mtc_routes_df <- left_join(gtfs_obj$mtc_routes_df,df_am_pm_headways_stats_non_directional)
    
    #get route polygons
    spply1 <- get_non_directional_route_geometries(gtfs_obj$mtc_routes_df$route_id,gtfs_obj,buffer=1)
    
    #cache the routes ids of routes without geomtries in a slot
    #these are probably weekend routes
    no_geom_route_ids <- !gtfs_obj$mtc_routes_df$route_id %in% getSpPPolygonsIDSlots(spply1)
    gtfs_obj$mtc_routes_without_geometry <- gtfs_obj$mtc_routes_df$route_id[no_geom_route_ids]
    
    #subset those without geometries form the data
    df_tmp1 <- gtfs_obj$mtc_routes_df[gtfs_obj$mtc_routes_df$route_id %in% getSpPPolygonsIDSlots(spply1),]
  
    #add a spatial dataframe to the gtfs_obj
    row.names(df_tmp1) <- df_tmp1$route_id
    gtfs_obj$mtc_routes_df_sp <- SpatialPolygonsDataFrame(spply1,data=df_tmp1,match.ID = TRUE)
  }
  
  #add on the priority stops dataframe where applicable
  if(dim(table(gtfs_obj$mtc_routes_df$f_tpa_qual))>1 &&
     table(gtfs_obj$mtc_routes_df$f_tpa_qual)['TRUE']>0){
    gtfs_obj$mtc_priority_stops <- get_priority_stops(gtfs_obj)
    gtfs_obj$mtc_priority_routes <- gtfs_obj$mtc_routes_df[gtfs_obj$mtc_routes_df$f_tpa_qual==TRUE,]
  }
  
  gtfs_obj[which(names(gtfs_obj) %in% c("mtc_am_stops",
                                        "mtc_pm_stops",
                                        "mtc_am_routes",
                                        "mtc_pm_routes",
                                        "mtc_combined_gtfs"))] <- NULL

  return(gtfs_obj)
}

#' Make a dataframe GTFS tables all joined together for route frequency calculations
#' @param a GTFSr object for a given provider with routes, stops, stop_times, etc
#' @return a mega-GTFSr data frame with stops, stop_times, trips, calendar, and routes all joined
join_all_gtfs_tables <- function(g) {
  df <- list(g$stops_df,g$stop_times_df,g$trips_df,g$calendar_df,g$routes_df)
  Reduce(inner_join,df) %>%
    select(agency_id, stop_id, trip_id, service_id,
           monday, tuesday, wednesday, thursday, friday,
           route_id, trip_headsign, direction_id,
           arrival_time, stop_sequence,
           route_type, stop_lat, stop_lon) %>%
    arrange(agency_id, trip_id, service_id,
            monday, tuesday, wednesday, thursday, friday,
            route_id, trip_headsign, direction_id,
            arrival_time, stop_sequence) -> df_sr
  #clean up source data
  rm(df)
  df_sr$Route_Pattern_ID<-paste0(df_sr$agency_id,
                                 "-",df_sr$route_id,"-",
                                 df_sr$direction_id)
  return(df_sr)
}


######
##Custom Time Format Functions
######

#' Make a dataframe GTFS arrival_time column into standard time variable
#' @param dataframe containing a GTFS-style "arrival_time" column (time values at +24:00:00)
#' @return dataframe containing a GTFS-style "arrival_time" column (no time values at +24:00:00)
make_arrival_hour_less_than_24 <- function(df) {
  t1 <- df$arrival_time
  if (!(typeof(t1) == "character")) {
    stop("column not a character string--may already be fixed")
  }
  df$arrival_time <- sapply(t1,FUN=fix_hour)
  df$arrival_time <- as.POSIXct(df$arrival_time, format= "%H:%M:%S")
  df
}

#' Format a time string in the expected format
#' @param x a GTFS hour string with an hour greater than 24
#' @param hour_replacement the hour to replace the >24 value with
#' @return a string formatted hh:mm:ss 
format_new_hour_string <- function(x,hour_replacement) {
  xl <- length(unlist(strsplit(x,":")))
  if (xl > 3){
    stop("unexpected time string")
  }
  minute <- as.integer(unlist(strsplit(x,":"))[[2]])
  second <- as.integer(unlist(strsplit(x,":"))[[3]])
  x <- paste(c(hour_replacement,minute,second),collapse=":")
  return(x)
}

#' Format GTFS Time strings as standard time string
#' @param a GTFS Time string
#' @return Time string with no hours greater than 24
fix_hour <- function(x) {
  # use:
  #   t1 <- stop_times$arrival_time
  #   stop_times$arrival_time <- sapply(t1,FUN=fix_hour)
  if(!is.na(x)) {
    hour <- as.integer(unlist(strsplit(x,":"))[[1]])
    if(!is.na(hour) & hour > 23) {
      hour <- hour-24
      x <- format_new_hour_string(x, hour)
      if (hour > 47){
        stop("hour is greater than 47 in stop times")
      }
    }
  }
  x
}

######
##Custom Bus Frequency Functions
######

#' Filter a mega-GTFSr dataframe to rows/stops that occur on all weekdays, are buses, and
#' have a stop_time between 2 time periods
#' @param a mega-GTFSr dataframe made with the join_all_gtfs_tables() function
#' @param period - "AM" or "PM"
#' @return a mega-GTFSr dataframe filtered to TPA peak periods and flagged as AM or PM peak
flag_and_filter_peak_periods_by_time <- function(mega_df, period) {
  if (period=="AM"){
    time_start <- "06:00:00"
    time_end <- "09:59:00"
  } else {
    time_start <- "15:00:00"
    time_end <- "18:59:00"
  }
  
  mega_df <- filter_by_time(mega_df,
                                  time_start,
                                  time_end)
  
  if (!(is.data.frame(mega_df) && nrow(mega_df)==0) && period=="AM"){
    mega_df["Peak_Period"] <-"AM Peak"
  } else if (!(is.data.frame(mega_df) && nrow(mega_df)==0) && period=="PM" ) {
    mega_df["Peak_Period"] <-"PM Peak" 
  } else
  {
  mega_df$Peak_Period <-  mega_df$route_id
  }
  return(mega_df)
}

#' Filter a mega-GTFSr dataframe to rows/stops that occur on all weekdays, are buses, and
#' have a stop_time between 2 time periods
#' @param a dataframe made by joining all the GTFS tables together
#' @param a start time filter hh:mm:ss
#' @param an end time filter hh:mm:ss
#' @return a mega-GTFSr dataframe filtered to rows of interest
filter_by_time <- function(rt_df, start_filter,end_filter) {
  time_start <- paste(c(format(Sys.Date(), "%Y-%m-%d"),
                        start_filter),collapse=" ")
  time_end <- paste(c(format(Sys.Date(), "%Y-%m-%d"),
                      end_filter),collapse=" ")
  rt_df_out <- subset(rt_df, rt_df$monday == 1
                           & rt_df$tuesday == 1
                           & rt_df$wednesday == 1
                           & rt_df$thursday == 1
                           & rt_df$friday == 1
                           & rt_df$route_type == 3
                           & rt_df$arrival_time >time_start
                           & rt_df$arrival_time < time_end)
  return(rt_df_out)
}

#' for a mega-GTFSr dataframe, remove rows with duplicate stop times 
#' @param a dataframe of stops with a stop_times column 
#' @return a dataframe of stops with a stop_times column in which there are no duplicate stop times for a given stop
remove_duplicate_stops <- function(rt_df){
  rt_df %>%
    distinct(agency_id, route_id, direction_id,
             trip_headsign, stop_id, stop_sequence, arrival_time, Peak_Period) %>%
    arrange(agency_id, route_id, direction_id,
            arrival_time,stop_sequence)->rt_df_out
  return(rt_df_out)
}

#' for a mega-GTFSr dataframe, count the number of trips a bus takes through a given stop within a given time period
#' @param a mega-GTFSr dataframe
#' @return a dataframe of stops with a "Trips" variable representing the count trips taken through each stop for a route within a given time frame
count_trips<- function(rt_df) {
  rt_df_out <- rt_df %>%
    group_by(agency_id,
             route_id,
             direction_id,
             trip_headsign,
             stop_id,
             Peak_Period) %>%
    count(stop_sequence) %>%
    mutate(Headways = round(240/n,0))
  colnames(rt_df_out)[colnames(rt_df_out)=="n"] <- "Trips"
  return(rt_df_out)
}

#' for a mega-GTFSr dataframe, reduce it to just a listing of routes
#' @param a mega-GTFSr dataframe
#' @return a dataframe of routes  
get_routes <- function(rt_df) {
  group_by(rt_df,
           agency_id,
           route_id,
           direction_id,
           trip_headsign,
           Peak_Period) %>%
    mutate(Total_Trips = round(mean(Trips),0),
           Headway = round(mean(Headways),0)) %>%
    distinct(agency_id,
             route_id,
             direction_id,
             trip_headsign,
             Total_Trips,
             Headway) ->
    rt_df_out
}

#' 
#' @param a mega-GTFSr dataframe filtered to AM peak commute period stops
#' @param a mega-GTFSr dataframe filtered to PM peak commute period stops
#' @param a mega-GTFSr get_routes reduced dataframe filtered to AM peak commute period stops
#' @param a mega-GTFSr get_routes reduced dataframe filtered to PM peak commute period stops
#' @return a dataframe of stops/routes flagged as TPA eligible or not
join_high_frequency_routes_to_stops <- function(am_stops,pm_stops,am_routes,pm_routes){
  # Combine Weekday High Frequency Bus Service Data Frames for AM/PM Peak Periods
  df1 <- rbind(am_routes,
               pm_routes)

  # This ID is used for grouping and headway counts 
  #(same name as another id in here but dropped anyway)
  #should probably replace at some point
  if (!(is.data.frame(am_routes) && nrow(am_routes)==0)){
    df1$Route_Pattern_ID<-paste0(df1$agency_id,
                                 "-",df1$route_id,"-",
                                 df1$Peak_Period)
  } else 
  {
    df1$Route_Pattern_ID <-  df1$Peak_Period
  }

  # Count number of routes that operate in both directions during peak periods.
  #TPA_Criteria = 2 or 3 then Route operates in both directions during peak periods
  #TPA Criteria = 1 possible loop route or route only operates in ection during peak periods.

  df2 <- df1 %>%
    group_by(agency_id, route_id, Peak_Period, Route_Pattern_ID) %>%
    summarise(TPA_Criteria = n())

  # 6C. Join Total By Direction with Weekday High Frequency Bus Service tables to flag those routes that meet the criteria.
  df3 <- list(df1,df2)
  df4 <- Reduce(inner_join,df3) %>%
    select(agency_id, route_id, direction_id, trip_headsign,Total_Trips, Headway, Peak_Period, TPA_Criteria) %>%
    arrange(agency_id, route_id, direction_id, Peak_Period)

  # 6D. Update values in TPA Criteria field. 2,3 = Meets Criteria, 1 = Review for Acceptance
  df4$TPA_Criteria[df3$TPA_Criteria==3] <- "Meets TPA Criteria"
  df4$TPA_Criteria[df3$TPA_Criteria==2] <- "Meets TPA Criteria"
  df4$TPA_Criteria[df3$TPA_Criteria==1] <- "Does Not Meet TPA Criteria"
  # 6D-1. Update values in TPA Criteria field.  All Loops in AM/PM Peak periods that have 15 mins or better headways = Meets TPA Criteria
  df4$TPA_Criteria[grepl('loop', df3$trip_headsign, ignore.case = TRUE)] <- "Meets TPA Criteria"

  df5 <- rbind(am_stops,pm_stops)

  # 6G. Join Weekday_Peak_Bus_Routes with df3 to generate a stop schedule for all AM/PM Peak Period stops that have headways of 15 mins. or better.
  df6 <- list(df5,df4)
  df7 <- Reduce(inner_join,df6) %>%
    select(agency_id, route_id, direction_id, trip_headsign, stop_id, stop_sequence, Total_Trips, Headway, Peak_Period, TPA_Criteria)

  return(df7)
}


#' 
#' @param a mega-GTFSr dataframe 
#' @param a mega-GTFSr get_routes reduced dataframe
#' @return a dataframe of stops/routes flagged as TPA eligible, with some of the variables dropped from the stops table above joined back on the table
join_mega_and_hf_routes <- function(df_sr,df_rt_hf){
  df<- list(df_sr,df_rt_hf)
  
  df_stp_rt_hf <- Reduce(inner_join,df) %>%
    group_by(agency_id, route_id, direction_id, trip_id,Peak_Period, Route_Pattern_ID,
             trip_headsign, stop_id, stop_sequence, Total_Trips, Headway, Peak_Period,
             TPA_Criteria, stop_lon, stop_lat) %>%
    select(agency_id, route_id, direction_id, trip_id, Route_Pattern_ID,
           trip_headsign, stop_id, stop_sequence, Total_Trips,
           Headway, Peak_Period, TPA_Criteria,
           stop_lon, stop_lat) %>%
    arrange(agency_id, route_id, direction_id,
            trip_id, Peak_Period, stop_sequence)
  
  rm(df)
  return(df_stp_rt_hf)
}

#` Select Distinct Records based upon Agency Route Direction values.  Removes stop ids from output.
#' @param a dataframe output by join_mega_and_hf_routes()
#' @return a deduplicated version of the input dataframe
deduplicate_final_table <- function(df_stp_rt_hf) {
  df_stp_rt_hf <- group_by(df_stp_rt_hf,
                           agency_id, route_id, direction_id, Route_Pattern_ID,trip_headsign,
                           stop_id, stop_sequence, Total_Trips, Headway, Peak_Period,
                           TPA_Criteria, stop_lon, stop_lat) %>%
    distinct(agency_id, route_id, direction_id, Route_Pattern_ID,
             trip_headsign, stop_id, stop_sequence, Total_Trips,
             Headway, Peak_Period, TPA_Criteria, stop_lon, stop_lat)
  return(df_stp_rt_hf)
}

#' Make a dataframe GTFS tables all joined together for route frequency calculations
#' @param a GTFSr object for a given provider with routes, stops, stop_times, etc
#' @return a mega-GTFSr data frame with stops, stop_times, trips, calendar, and routes all joined
join_all_gtfs_tables <- function(g) {
  df <- list(g$stops_df,g$stop_times_df,g$trips_df,g$calendar_df,g$routes_df)
  Reduce(inner_join,df) %>%
    select(agency_id, stop_id, trip_id, service_id,
           monday, tuesday, wednesday, thursday, friday,
           route_id, trip_headsign, direction_id,
           arrival_time, stop_sequence,
           route_type, stop_lat, stop_lon) %>%
    arrange(agency_id, trip_id, service_id,
            monday, tuesday, wednesday, thursday, friday,
            route_id, trip_headsign, direction_id,
            arrival_time, stop_sequence) -> df_sr
  #clean up source data
  rm(df)
  df_sr$Route_Pattern_ID<-paste0(df_sr$agency_id,
                                 "-",df_sr$route_id,"-",
                                 df_sr$direction_id)
  return(df_sr)
}

######
##Custom Time Format Functions
######

#' Make a dataframe GTFS arrival_time column into standard time variable
#' @param dataframe containing a GTFS-style "arrival_time" column (time values at +24:00:00)
#' @return dataframe containing a GTFS-style "arrival_time" column (no time values at +24:00:00)
make_arrival_hour_less_than_24 <- function(df) {
  t1 <- df$arrival_time
  if (!(typeof(t1) == "character")) {
    stop("column not a character string--may already be fixed")
  }
  df$arrival_time <- sapply(t1,FUN=fix_hour)
  df$arrival_time <- as.POSIXct(df$arrival_time, format= "%H:%M:%S")
  df
}

#' Format a time string in the expected format
#' @param x a GTFS hour string with an hour greater than 24
#' @param hour_replacement the hour to replace the >24 value with
#' @return a string formatted hh:mm:ss 
format_new_hour_string <- function(x,hour_replacement) {
  xl <- length(unlist(strsplit(x,":")))
  if (xl > 3){
    stop("unexpected time string")
  }
  minute <- as.integer(unlist(strsplit(x,":"))[[2]])
  second <- as.integer(unlist(strsplit(x,":"))[[3]])
  x <- paste(c(hour_replacement,minute,second),collapse=":")
  return(x)
}

#' Format GTFS Time strings as standard time string
#' @param a GTFS Time string
#' @return Time string with no hours greater than 24
fix_hour <- function(x) {
  # use:
  #   t1 <- stop_times$arrival_time
  #   stop_times$arrival_time <- sapply(t1,FUN=fix_hour)
  if(!is.na(x)) {
    hour <- as.integer(unlist(strsplit(x,":"))[[1]])
    if(!is.na(hour) & hour > 23) {
      hour <- hour-24
      x <- format_new_hour_string(x, hour)
      if (hour > 47){
        stop("hour is greater than 47 in stop times")
      }
    }
  }
  x
}

######
##Custom Bus Frequency Functions
######

#' Filter a mega-GTFSr dataframe to rows/stops that occur on all weekdays, are buses, and
#' have a stop_time between 2 time periods
#' @param a mega-GTFSr dataframe made with the join_all_gtfs_tables() function
#' @param period - "AM" or "PM"
#' @return a mega-GTFSr dataframe filtered to TPA peak periods and flagged as AM or PM peak
flag_and_filter_peak_periods_by_time <- function(mega_df, period) {
  if (period=="AM"){
    time_start <- "06:00:00"
    time_end <- "09:59:00"
  } else {
    time_start <- "15:00:00"
    time_end <- "18:59:00"
  }
  
  mega_df <- filter_by_time(mega_df,
                                  time_start,
                                  time_end)
  
  if (!(is.data.frame(mega_df) && nrow(mega_df)==0) && period=="AM"){
    mega_df["Peak_Period"] <-"AM Peak"
  } else if (!(is.data.frame(mega_df) && nrow(mega_df)==0) && period=="PM" ) {
    mega_df["Peak_Period"] <-"PM Peak" 
  } else
  {
  mega_df$Peak_Period <-  mega_df$route_id
  }
  return(mega_df)
}

#' Filter a mega-GTFSr dataframe to rows/stops that occur on all weekdays, are buses, and
#' have a stop_time between 2 time periods
#' @param a dataframe made by joining all the GTFS tables together
#' @param a start time filter hh:mm:ss
#' @param an end time filter hh:mm:ss
#' @return a mega-GTFSr dataframe filtered to rows of interest
filter_by_time <- function(rt_df, start_filter,end_filter) {
  time_start <- paste(c(format(Sys.Date(), "%Y-%m-%d"),
                        start_filter),collapse=" ")
  time_end <- paste(c(format(Sys.Date(), "%Y-%m-%d"),
                      end_filter),collapse=" ")
  rt_df_out <- subset(rt_df, rt_df$monday == 1
                           & rt_df$tuesday == 1
                           & rt_df$wednesday == 1
                           & rt_df$thursday == 1
                           & rt_df$friday == 1
                           & rt_df$route_type == 3
                           & rt_df$arrival_time >time_start
                           & rt_df$arrival_time < time_end)
  return(rt_df_out)
}

#' for a mega-GTFSr dataframe, remove rows with duplicate stop times 
#' @param a dataframe of stops with a stop_times column 
#' @return a dataframe of stops with a stop_times column in which there are no duplicate stop times for a given stop
remove_duplicate_stops <- function(rt_df){
  rt_df %>%
    distinct(agency_id, route_id, direction_id,
             trip_headsign, stop_id, stop_sequence, arrival_time, Peak_Period) %>%
    arrange(agency_id, route_id, direction_id,
            arrival_time,stop_sequence)->rt_df_out
  return(rt_df_out)
}

#' for a mega-GTFSr dataframe, count the number of trips a bus takes through a given stop within a given time period
#' @param a mega-GTFSr dataframe
#' @return a dataframe of stops with a "Trips" variable representing the count trips taken through each stop for a route within a given time frame
count_trips<- function(rt_df) {
  rt_df_out <- rt_df %>%
    group_by(agency_id,
             route_id,
             direction_id,
             trip_headsign,
             stop_id,
             Peak_Period) %>%
    count(stop_sequence) %>%
    mutate(Headways = round(240/n,0))
  colnames(rt_df_out)[colnames(rt_df_out)=="n"] <- "Trips"
  return(rt_df_out)
}

#' for a mega-GTFSr dataframe, reduce it to just a listing of routes
#' @param a mega-GTFSr dataframe
#' @return a dataframe of routes  
get_routes <- function(rt_df) {
  group_by(rt_df,
           agency_id,
           route_id,
           direction_id,
           trip_headsign,
           Peak_Period) %>%
    mutate(Total_Trips = round(mean(Trips),0),
           Headway = round(mean(Headways),0)) %>%
    distinct(agency_id,
             route_id,
             direction_id,
             trip_headsign,
             Total_Trips,
             Headway) ->
    rt_df_out
}

#' 
#' @param a mega-GTFSr dataframe filtered to AM peak commute period stops
#' @param a mega-GTFSr dataframe filtered to PM peak commute period stops
#' @param a mega-GTFSr get_routes reduced dataframe filtered to AM peak commute period stops
#' @param a mega-GTFSr get_routes reduced dataframe filtered to PM peak commute period stops
#' @return a dataframe of stops/routes flagged as TPA eligible or not
join_high_frequency_routes_to_stops <- function(am_stops,pm_stops,am_routes,pm_routes){
  # Combine Weekday High Frequency Bus Service Data Frames for AM/PM Peak Periods
  df1 <- rbind(am_routes,
               pm_routes)

  # This ID is used for grouping and headway counts 
  #(same name as another id in here but dropped anyway)
  #should probably replace at some point
  if (!(is.data.frame(am_routes) && nrow(am_routes)==0)){
    df1$Route_Pattern_ID<-paste0(df1$agency_id,
                                 "-",df1$route_id,"-",
                                 df1$Peak_Period)
  } else 
  {
    df1$Route_Pattern_ID <-  df1$Peak_Period
  }

  # Count number of routes that operate in both directions during peak periods.
  #TPA_Criteria = 2 or 3 then Route operates in both directions during peak periods
  #TPA Criteria = 1 possible loop route or route only operates in ection during peak periods.

  df2 <- df1 %>%
    group_by(agency_id, route_id, Peak_Period, Route_Pattern_ID) %>%
    summarise(TPA_Criteria = n())

  # 6C. Join Total By Direction with Weekday High Frequency Bus Service tables to flag those routes that meet the criteria.
  df3 <- list(df1,df2)
  df4 <- Reduce(inner_join,df3) %>%
    select(agency_id, route_id, direction_id, trip_headsign,Total_Trips, Headway, Peak_Period, TPA_Criteria) %>%
    arrange(agency_id, route_id, direction_id, Peak_Period)

  # 6D. Update values in TPA Criteria field. 2,3 = Meets Criteria, 1 = Review for Acceptance
  df4$TPA_Criteria[df3$TPA_Criteria==3] <- "Meets TPA Criteria"
  df4$TPA_Criteria[df3$TPA_Criteria==2] <- "Meets TPA Criteria"
  df4$TPA_Criteria[df3$TPA_Criteria==1] <- "Does Not Meet TPA Criteria"
  # 6D-1. Update values in TPA Criteria field.  All Loops in AM/PM Peak periods that have 15 mins or better headways = Meets TPA Criteria
  df4$TPA_Criteria[grepl('loop', df3$trip_headsign, ignore.case = TRUE)] <- "Meets TPA Criteria"

  df5 <- rbind(am_stops,pm_stops)

  # 6G. Join Weekday_Peak_Bus_Routes with df3 to generate a stop schedule for all AM/PM Peak Period stops that have headways of 15 mins. or better.
  df6 <- list(df5,df4)
  df7 <- Reduce(inner_join,df6) %>%
    select(agency_id, route_id, direction_id, trip_headsign, stop_id, stop_sequence, Total_Trips, Headway, Peak_Period, TPA_Criteria)

  return(df7)
}


#' 
#' @param a mega-GTFSr dataframe 
#' @param a mega-GTFSr get_routes reduced dataframe
#' @return a dataframe of stops/routes flagged as TPA eligible, with some of the variables dropped from the stops table above joined back on the table
join_mega_and_hf_routes <- function(df_sr,df_rt_hf){
  df<- list(df_sr,df_rt_hf)
  
  df_stp_rt_hf <- Reduce(inner_join,df) %>%
    group_by(agency_id, route_id, direction_id, trip_id,Peak_Period, Route_Pattern_ID,
             trip_headsign, stop_id, stop_sequence, Total_Trips, Headway, Peak_Period,
             TPA_Criteria, stop_lon, stop_lat) %>%
    select(agency_id, route_id, direction_id, trip_id, Route_Pattern_ID,
           trip_headsign, stop_id, stop_sequence, Total_Trips,
           Headway, Peak_Period, TPA_Criteria,
           stop_lon, stop_lat) %>%
    arrange(agency_id, route_id, direction_id,
            trip_id, Peak_Period, stop_sequence)
  
  rm(df)
  return(df_stp_rt_hf)
}

#` Select Distinct Records based upon Agency Route Direction values.  Removes stop ids from output.
#' @param a dataframe output by join_mega_and_hf_routes()
#' @return a deduplicated version of the input dataframe
deduplicate_final_table <- function(df_stp_rt_hf) {
  df_stp_rt_hf <- group_by(df_stp_rt_hf,
                           agency_id, route_id, direction_id, Route_Pattern_ID,trip_headsign,
                           stop_id, stop_sequence, Total_Trips, Headway, Peak_Period,
                           TPA_Criteria, stop_lon, stop_lat) %>%
    distinct(agency_id, route_id, direction_id, Route_Pattern_ID,
             trip_headsign, stop_id, stop_sequence, Total_Trips,
             Headway, Peak_Period, TPA_Criteria, stop_lon, stop_lat)
  return(df_stp_rt_hf)
}


#' make high frequency routes df into just routes and directions df, for use in construction geoms by route and direction sldf
#' @param a dataframe made of am_routes and pm_routes
#' @return a dataframe of routes by direction with headway stats for peak periods
get_route_stats <- function(df1) {
  df2 <- dcast(df1,route_id+direction_id~Peak_Period, value.var="Headway", fun.aggregate=mean)
  names(df2)[3:4] <- c("am_headway","pm_headway")
  df3 <- dcast(df1,route_id+direction_id~Peak_Period, value.var="Total_Trips", fun.aggregate=mean)
  names(df3)[3:4] <- c("am_trips","pm_trips")
  df4 <- inner_join(df2,df3)
  row.names(df4) <- paste(df2$route_id,df2$direction_id,sep="-")
  return(df4)
}

#' make high frequency routes df into just routes and directions df, for use in construction geoms by route and direction sldf
#' @param a dataframe made of am_routes and pm_routes
#' @return a dataframe of routes with headway stats for peak periods averaged over both directions
get_route_stats_no_direction <- function(df1) {
  df2 <- dcast(df1,route_id~Peak_Period, value.var="Headway", fun.aggregate=mean)
  names(df2)[2:3] <- c("avg_am_headway","avg_pm_headway")
  df3 <- dcast(df1,route_id~Peak_Period, value.var="Total_Trips", fun.aggregate=mean)
  names(df3)[2:3] <- c("avg_am_trips","avg_pm_trips")
  df4 <- inner_join(df2,df3,by=c("route_id"))
  return(df4)
}

#' get a Route Pattern ID
#' @param dataframe
#' @returns a vector which combines the agency id, route id, and direction id in a string 
get_route_pattern_id <- function(df) {
  df$Route_Pattern_ID<-paste0(df$agency_id,
                              "-",df$route_id,"-",
                              df$direction_id)
}

#' string match check for loop routes
#' @param headsign vector
#' @return boolean/logical vector indicating whether its a loop
is_loop_route <- function(headsign){
  grepl('loop', headsign, ignore.case = TRUE)
}

#' check for bidirectional routes
#' @param dataframe with route_id and direction_id columns
#' @return boolean/logical vector indicating whether the route goes in both directions
is_in_both_directions <- function(df_rt_dr){
  g1 <- group_by(df_rt_dr,route_id)
  s1 <- summarise(g1, both=both_directions_bool_check(direction_id))
  s2 <- df_rt_dr$route_id %in% s1[s1$both==TRUE,]$route_id
  return(s2)
}

#' check for dual period stops
#' @param dataframe with stop_id and "peak" columns
#' @return boolean/logical vector indicating whether the stop is in both am and pm periods
is_in_both_periods <- function(df_rt_dr){
  g1 <- group_by(df_rt_dr,stop_id)
  s1 <- summarise(g1, both=both_periods_bool_check(Peak_Period))
  s2 <- df_rt_dr$stop_id %in% s1[s1$both==TRUE,]$stop_id
  return(s2)
}

#'given a string vector, check whether both 0 and 1 are in it
#' @param string vector of
#' @return logical vector
both_periods_bool_check <- function(direction_ids){
  "AM Peak" %in% direction_ids & "PM Peak" %in% direction_ids
}



#'given a string vector, check whether both 0 and 1 are in it
#' @param string vector of
#' @return logical vector
both_directions_bool_check <- function(direction_ids){
  1 %in% direction_ids & 0 %in% direction_ids
}


##################
#geospatial work
###############

#' Return a SpatialPolygons with the geometries of a list of high frequency routes
#' @param route_ids is a list of route id's
#' @buffer buffer the buffered distance for the route geometry-default set to 1/4 mile
#' @param gtfs_obj is a gtfsr list of gtfs dataframes
#' @return A SpatialPolygons object of routes geometries as polygons, for weekday service
get_non_directional_route_geometries <- function(route_ids,buffer,gtfs_obj){
  l3 <- lapply(route_ids,FUN=get_non_directional_route_geometry,gtfs_obj=gtfs_obj, buffer=buffer)
  
  list.condition <- sapply(l3, function(x) class(x)=="SpatialPolygons")
  l3  <- l3[list.condition]
  
  l3_flattened <- SpatialPolygons(lapply(l3, function(x){{x@polygons[[1]]}}))
  
  return(l3_flattened)
}

#' Return a spatial dataframe with the polygon geometries of a single high frequency route in all directions
#' @param route_id is a route id from gtfs
#' @param gtfs_obj is a gtfsr list of gtfs dataframes
#' @param buffer - the buffered distance for the route geometry-default set to 1/4 mile
#' @param weekday - whether or not to return just the weekday service - default to TRUE
#' @return routes geometries as polygons, for weekday service for 1 route
get_non_directional_route_geometry <- function(route_id,gtfs_obj,weekday=TRUE,buffer=402.336) {
  out <- tryCatch({
    #get the spatial dataframe list from gtfsr
    l2 <- get_routes_sldf(gtfs_obj,route_id,NULL,NULL)
    names(l2$gtfslines) <- c("shape_id")
    
    #subset the sldf from gtfsr for weekday only
    if(weekday==FALSE) stop("we only handle weekdays")
    weekday_subset <- gtfs_obj$calendar_df$monday==1 & 
      gtfs_obj$calendar_df$tuesday==1 & 
      gtfs_obj$calendar_df$wednesday==1 & 
      gtfs_obj$calendar_df$thursday==1 & 
      gtfs_obj$calendar_df$friday==1
    chosen_services <- gtfs_obj$calendar_df[weekday_subset,c("service_id")]
    df1 <- l2$shapes_routes_df
    weekday_service_shapes <- df1[df1$service_id %in% chosen_services$service_id,]$shape_id
    lines_df <- l2$gtfslines
    df2 <- lines_df[lines_df$shape_id %in% weekday_service_shapes,]
      
    #collapse to the filtered sldf list to 1 sp Polygons class per route
    #needed to use polygons since these aren't proper Lines (connected at endpoints)
    g1 <- geometry(df2)
    g1 <- spTransform(g1, CRS("+init=epsg:26910"))
    g1 <- gBuffer(g1,width=buffer)
    g2 <- gUnaryUnion(g1,id=route_id)
    return(g2)
    }, 
  error = function(e) {NULL})
  return(out)
}



#' Return the geometries for a route as single line
#' @param a list with route_id and direction id
#' @param a list output by get_hf_geoms 
#' @return linestring with an id for route and direction
get_directional_route_geom <- function(x,hf_l) {
  r_id <- x["route_id"]
  d_id <- x["direction_id"]
  rd_id <- paste(r_id,d_id,sep="-")
  t1 <- as.data.frame(hf_l$df[hf_l$df$route_id == r_id & hf_l$df$direction_id == d_id,"shape_id"])
  dfsp1 <- hf_l$sldf[hf_l$sldf$shape_id %in% t1[,1],]
  g1 <- geometry(dfsp1)
  g2 <- gLineMerge(g1,byid=FALSE,id=rd_id)
  if(length(g2)>1){stop("more than 1 sp Line after merge of gtfs shapes for route")}
  l1 <- Line(coordinates(g2))
  l2 <- Lines(list(l1),ID=rd_id)
  return(l2)
}

#tried doing this with "outer" but ran into s4 class coersion error
get_stop_distance_from_route <- function(bus_stop,routes) {
  route_id <- bus_stop$route_id
  route <- routes[routes$route_id==route_id,]
  distance <- gDistance(route, bus_stop)
  return(distance)
}

get_stops_distances_from_routes <- function(stops,routes) {
  distances <- numeric()
  dflength <- dim(stops)[1]
  
  #tried lapply but had s4 class issue
  for(bstop_ix in 1:dflength) {
    distances <- c(distances,get_stop_distance_from_route(stops[bstop_ix,],routes))
  }
  return(distances)
}

#' put a list of spatial dataframes (with agency_id as the list key) together into one
#' @param a list of routes sp class dataframes
#' @return an sp class dataframe
#' 
bind_list_of_routes_spatial_dataframes <- function(l1) { 
  spdf <- l1[[1]]
  spdf$agency <- names(l1[1])
  for (s in names(l1[2:length(l1)])) {
    if(dim(l1[[s]])[1]>0) {
      tmp_sdf <- l1[[s]]
      tmp_sdf$agency <- rep(s,nrow(tmp_sdf))
      spdf <- rbind(spdf,tmp_sdf)
    }
  }
  proj4string(spdf) <- CRS("+init=epsg:26910")
  return(spdf)
}

bind_df_list <- function(l1) {
  df <- l1[[1]]
  for (s in names(l1[2:length(l1)])) {
    if(!is.null(dim(l1[[s]])) && dim(l1[[s]])[1]>0) {
      tmp_df <- l1[[s]]
      df <- bind_rows(df,tmp_df)
    }
  }
  return(df)
}



#'write a spatial dataframe to the current working directory as a geopackage (with date in name-seconds since the epoch)
#'@param spatial dataframe
#'@return nothing
write_to_geopackage_with_date <- function(spdf, project_data_path="C:/projects/RTD/RegionalTransitDatabase/data") {
  library(rgdal)
  the_name <- deparse(substitute(spdf))
  writeOGR(spdf,
           paste0(project_data_path,format(Sys.time(),"%s"),the_name,"_",".gpkg"),
           driver="GPKG",
           layer = the_name, 
           overwrite_layer = TRUE)
}

#'given am routes and pm routes, return a spatial dataframe with the routes and their stats
#'@param df_routes
#'@return spatial dataframe (polygons) of routes
get_routes_with_geoms_and_stats <- function(df_routes) {
  route_ids <- names(table(df1$route_id))
  spply_rts <- get_non_directional_route_geometries(route_ids, buffer=0.10)
  
  df1_stats <- get_route_stats_no_direction(df1)
  row.names(df1_stats) <- df1_stats$route_id
  
  df1_sbst <- df1_stats[df1_stats$route_id %in% getSpPPolygonsIDSlots(spply_rts),]
  spdf <- SpatialPolygonsDataFrame(Sr=spply_rts, data=as.data.frame(df1_sbst),FALSE)
  return(spdf)
}