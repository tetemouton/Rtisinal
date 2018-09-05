data.extraction <- function(sv.dir="C:/Tokelau_Artisinal/Data/", cnt.code="TK"){
  
  require(RODBC)

  myConn<-odbcDriverConnect(connection="Driver=SQL Server Native Client 10.0;Server=nousql03;Database=tufman2;
                            Trusted_Connection=yes;")

# tp<-sqlTables(myConn) # to see all the tables the artisinal data are all on the art2. bla tables 
# 
# sqlColumns(myConn, "art2.trips")$"COLUMN_NAME"   
# sqlColumns(myConn, "art2.catch")$"COLUMN_NAME" 
# sqlColumns(myConn, "art2.effort")$"COLUMN_NAME"
# sqlColumns(myConn, "art2.fishing_activity_log")$"COLUMN_NAME"
# sqlColumns(myConn, "art2.size_data")$"COLUMN_NAME"
# sqlColumns(myConn, "art2.landing_sites")$"COLUMN_NAME"
# sqlColumns(myConn, "art2.vessels")$"COLUMN_NAME"
# sqlColumns(myConn, "art2.vw_fishing_activity_log_TK_all")$"COLUMN_NAME"

dat_act <- sqlQuery(myConn, paste0("SELECT YEAR(art2.vw_fishing_activity_log_", cnt.code, "_all.activity_date) as yy, 
                       MONTH(art2.vw_fishing_activity_log_", cnt.code ,"_all.activity_date) as mm,
                       DAY(art2.vw_fishing_activity_log_", cnt.code, "_all.activity_date) as dd,
                       art2.landing_sites.landing_site_name, 
                       art2.vw_fishing_activity_log_", cnt.code, "_all.motor_vessels_n as motor, 
                       art2.vw_fishing_activity_log_", cnt.code, "_all.paddle_vessels_n as paddle, 
                       art2.vw_fishing_activity_log_", cnt.code, "_all.sail_vessels_n as sail,
                       art2.vw_fishing_activity_log_", cnt.code, "_all.start_time,
                       art2.vw_fishing_activity_log_", cnt.code, "_all.end_time
                       FROM 
                       art2.vw_fishing_activity_log_", cnt.code, "_all INNER JOIN art2.landing_sites ON art2.vw_fishing_activity_log_", cnt.code, "_all.landing_site_id = art2.landing_sites.landing_site_id
                       "),
                       stringsAsFactors = FALSE)
#head(dat_act)

write.csv(dat_act, file=paste0(sv.dir, "Activity_Data.csv"), row.names=FALSE)


trip_effort_length <- sqlQuery(myConn, paste0("SELECT art2.vw_trips_", cnt.code, "_all.art_trip_id,
                               YEAR(art2.vw_trips_", cnt.code, "_all.trip_date) as yy,
                               MONTH(art2.vw_trips_", cnt.code, "_all.trip_date) as mm,
                               DAY(art2.vw_trips_", cnt.code, "_all.trip_date) as dd,
                               art2.areas.art_area_name,
                               art2.landing_sites.landing_site_name,
                               art2.effort.fad_fishing, 
                               art2.effort.fish_method_id, 
                               art2.effort.hours_fished_n, 
                               art2.effort.lines_n, 
                               art2.effort.fuel_used_n, 
                               art2.effort.fuel_type_id, 
                               art2.effort.hooks_per_line_n,
                               art2.effort.live_bait_used hooks_n,
                               art2.vw_trips_", cnt.code, "_all.boat_power_id,
                               art2.vw_trips_", cnt.code, "_all.return_date, 
                               art2.vw_trips_", cnt.code, "_all.fuel_used, 
                               art2.vw_trips_", cnt.code, "_all.fuel_cost,
                               art2.vw_trips_", cnt.code, "_all.bait_used,  
                               art2.vw_trips_", cnt.code, "_all.bait_cost, 
                               art2.vw_trips_", cnt.code, "_all.ice_used, 
                               art2.vw_trips_", cnt.code, "_all.ice_cost,
                               art2.vessels.vessel_name,
                               art2.vw_trips_", cnt.code, "_all.depart_time,
                               art2.vw_trips_", cnt.code, "_all.return_time,
                               art2.catch.sp_code,
                               art2.size_data.sp_len
                               FROM  
                               art2.effort INNER JOIN art2.vw_trips_", cnt.code, "_all ON art2.effort.art_trip_id = art2.vw_trips_", cnt.code, "_all.art_trip_id 
                               INNER JOIN art2.catch ON art2.catch.art_effort_id = art2.effort.art_effort_id
                               INNER JOIN art2.size_data ON art2.size_data.art_catch_id = art2.catch.art_catch_id
                               INNER JOIN art2.vessels ON art2.vessels.vessel_id = art2.vw_trips_", cnt.code, "_all.vessel_id
                               INNER JOIN art2.landing_sites ON art2.landing_sites.landing_site_id = art2.vw_trips_", cnt.code, "_all.landing_site_id
                               INNER JOIN art2.areas ON art2.areas.art_area_id = art2.effort.art_area_id
                               "), stringsAsFactors = FALSE)

#head(trip_effort_length)
#tail(trip_effort_length)

write.csv(trip_effort_length, file=paste0(sv.dir, "Effort_Length_Data.csv"), row.names=FALSE)


trip_effort_catch <- sqlQuery(myConn, paste0("SELECT art2.vw_trips_", cnt.code, "_all.art_trip_id,
                              YEAR(art2.vw_trips_", cnt.code, "_all.trip_date) as yy,
                              MONTH(art2.vw_trips_", cnt.code, "_all.trip_date) as mm,
                              DAY(art2.vw_trips_", cnt.code, "_all.trip_date) as dd,
                              art2.areas.art_area_name,
                              art2.landing_sites.landing_site_name,
                              art2.vessels.vessel_name,
                              art2.vessels.boat_power_id,
                              art2.vw_trips_", cnt.code, "_all.depart_time,
                              art2.vw_trips_", cnt.code, "_all.return_time,
                              art2.vw_trips_", cnt.code, "_all.return_date, 
                              art2.vw_trips_", cnt.code, "_all.fuel_used, 
                              art2.vw_trips_", cnt.code, "_all.fuel_cost,
                              art2.vw_trips_", cnt.code, "_all.bait_used,  
                              art2.vw_trips_", cnt.code, "_all.bait_cost, 
                              art2.vw_trips_", cnt.code, "_all.ice_used, 
                              art2.vw_trips_", cnt.code, "_all.ice_cost,
                              art2.effort.fad_fishing, 
                              art2.effort.fish_method_id,
                              tp.value_en,
                              art2.effort.hours_fished_n, 
                              art2.effort.lines_n, 
                              art2.effort.fuel_used_n, 
                              art2.effort.fuel_type_id, 
                              art2.effort.hooks_per_line_n,
                              art2.effort.hooks_n,
                              art2.effort.live_bait_used,
                              art2.catch.sp_code,
                              art2.catch.sp_n,
                              art2.catch.sp_kg
                              FROM  
                              art2.effort INNER JOIN art2.vw_trips_", cnt.code, "_all ON art2.effort.art_trip_id = art2.vw_trips_", cnt.code, "_all.art_trip_id 
                              LEFT JOIN art2.catch ON art2.catch.art_effort_id = art2.effort.art_effort_id
                              INNER JOIN art2.vessels ON art2.vessels.vessel_id = art2.vw_trips_", cnt.code, "_all.vessel_id
                              INNER JOIN art2.areas ON art2.areas.art_area_id = art2.effort.art_area_id
                              INNER JOIN art2.landing_sites ON art2.landing_sites.landing_site_id = art2.vw_trips_", cnt.code, "_all.landing_site_id
                              LEFT OUTER JOIN tuf2.translations tp ON tp.translation_key = 'LookupList_ArtisanalFishingMethods' + CONVERT(VARCHAR,art2.effort.fish_method_id)
                              "), stringsAsFactors = FALSE)

#head(trip_effort_catch)

write.csv(trip_effort_catch, file=paste0(sv.dir, "Effort_Catch_Data.csv"), row.names=FALSE)


# Save them all together
save(trip_effort_length, dat_act, trip_effort_catch, file=paste0(sv.dir, "Raw_Extract.RData"))


# Extract other tables of interest
#sqlColumns(myConn, "ref.species")$"COLUMN_NAME"

# Species Info
species_info <- sqlQuery(myConn, "SELECT *
                              FROM  
                              ref.species", stringsAsFactors = FALSE)

write.csv(species_info, file=paste0(sv.dir, "Spp_Info.csv"), row.names=FALSE)


#sqlColumns(myConn, "tuf2.translations")$"COLUMN_NAME"

# Vessel Info
vessel_info <- sqlQuery(myConn, "SELECT *
                                 FROM
                                 art2.Vessels", stringsAsFactors = FALSE)

write.csv(vessel_info, file=paste0(sv.dir, "Vessel_Info.csv"), row.names=FALSE)


}


