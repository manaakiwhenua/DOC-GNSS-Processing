# A utility for processing GNSS solutions produced by the software Emlid Studio

# Author: Andrew McMillan

# Recommended settings for Emlid Studio

# Output solution: All solutions
# Filter type    : Combined
# Elevation mask : 15
# SNR mask:
#   L1           : 20
#   L2           : 20
# Integer ambiguity resolution:
#   GPS          : Fix and hold
#   GNSS         : Fix and hold
#   BDS          : On
# Solution format: Lat/Lon/Height
# Time format    : hh:mm:ss GPST
# Latitude/Longitude format : ddd.ddddddd
# Debug trace    : Level 2



if (!require("tidyverse", character.only = TRUE)) {
  # If not installed, install the package
  install.packages("tidyverse")
  # Load the package
  library("tidyverse")
} else {
  # If already installed, just load the package
  library("tidyverse")
}

if (!require("sf", character.only = TRUE)) {
  # If not installed, install the package
  install.packages("sf")
  # Load the package
  library("sf")
} else {
  # If already installed, just load the package
  library("sf")
}


if (!require("mapview", character.only = TRUE)) {
  # If not installed, install the package
  install.packages("mapview")
  # Load the package
  library("mapview")
} else {
  # If already installed, just load the package
  library("mapview")
}



# Top Level Function to process a POS file produced by Emlid Studio 
procpos = function(FileName = NULL, writeResults = T){
  
  
  
  #Prompt the Use for the location of the POS file
  if (is.null(FileName)){
    POS_file_to_process = file.choose()
  } else {
    POS_file_to_process = FileName
  }
  
  
  print("================================================")
  print(paste("Processing", basename(POS_file_to_process)))
  print("================================================")
  
  #Process the POS file
  POS_STATS = GetPosStats(POS_file_to_process)
  
  if (writeResults){
    Output_filename = paste0(
      tools::file_path_sans_ext(POS_file_to_process),
      "_summary.csv")
    
    POS_STATS_2_write = POS_STATS %>% 
      dplyr::select(-Q) %>% 
      mutate(Filename = basename(POS_file_to_process)) %>% 
      dplyr::select(Filename, everything())
    
    write_csv(POS_STATS_2_write, Output_filename)
    print(paste("Summary data written to", Output_filename))
    print("================================================")
    print("================================================")
    
    }
  #
  
  
}


# A function to read the POS file and calculate the statistics from the float solution

GetPosStats = function(POS_Filename, target_CRS = 2193){
  
  # dn = "C:/Users/McMillanAn/OneDrive - MWLR/Projects/PRJ3820-DOC-GNSS/data/FINAL_SOLUTIONS/"
  # fn = "JVD-OCC5-P5-PPK-R10.pos"
  # target_CRS = 2193
  #POS_Filename = paste0(dn,fn)
  
  POS_data = read_POS(POS_Filename, FMT_OPT = 5)
  
  
  #create a spatial simple feature object 
  POS_data_sf = POS_data %>% 
    st_as_sf(coords = c("Lon_DD", "Lat_DD"), crs = 4326) 
  
  #transform the coordinates to the chose target CRS
  POS_data_sf_NZTM = POS_data_sf %>% 
    st_transform(crs = target_CRS)
  
  #extract the coordinates as a data frame
  POS_data_sf_NZTM_coordinates = st_coordinates(POS_data_sf_NZTM) %>% 
    as_tibble()
  
  #convert the simple feature back to a data frame and add the NZTM Coordinates
  POS_data_df_NZTM = POS_data_sf_NZTM %>% 
    st_set_geometry(NULL) %>% 
    mutate(X = POS_data_sf_NZTM_coordinates$X, Y = POS_data_sf_NZTM_coordinates$Y)
  
  
  
  
  
  
  # filter the data for just the float solution
  POS_stats = POS_data_df_NZTM %>% 
    group_by(Q) %>% 
    summarise(
      meanX = mean(X),
      meanY = mean(Y),
      meanZ = mean(Ht_ell),
      sigmaX = sd(X),
      sigmaY = sd(Y),
      sigmaZ = sd(Ht_ell),
      DRMS = sqrt(sigmaX^2 + sigmaY^2),
      DRMS2 = 2* DRMS,
      nSolutions = length(Q)
      
    ) 
  
  
    
    
    
  
  POS_stats_float_only = POS_stats %>% filter(Q==2)
  
  POS_stats_4326_coords  = POS_stats_float_only %>% 
    mutate(
      meanX_2193 = meanX,
      meanY_2193 = meanY) %>% 
    st_as_sf(coords = c("meanX_2193", "meanY_2193"), crs = 2193) %>% 
    st_transform(4326) %>% 
    st_coordinates() %>% 
    as_tibble()
  
  POS_stats_float_only = POS_stats_float_only %>% 
    mutate(
      LonDD = POS_stats_4326_coords$X,
      LatDD = POS_stats_4326_coords$Y,
      )
  
  #print results
  
  print(paste("--------------------------------------------------------------------"))
  print(paste("Float Solution"))
  print(paste("--------------------------------------------------------------------"))
  
  print(paste("Easting: ", formatC(POS_stats_float_only$meanX, digits = 3, format = "f"), 
              "m, Northing: ", formatC(POS_stats_float_only$meanY, digits = 3, format = "f"), "m, ",
              "Elevation (Ht above ellipsoid) :", formatC(POS_stats_float_only$meanZ, digits = 3, format = "f"), "m"))
  
  print(paste("Latitude = ", formatC(POS_stats_float_only$LatDD, digits = 7, format = "f"), 
              "Longitude = ", formatC(POS_stats_float_only$LonDD, digits = 7, format = "f")))
  
  
  print(paste("Sigma X: ", formatC(POS_stats_float_only$sigmaX, digits = 3, format = "f"),"m,  ", 
              "Sigma Y: ", formatC(POS_stats_float_only$sigmaY, digits = 3, format = "f"),"m,  ",
              "Sigma Z: ", formatC(POS_stats_float_only$sigmaZ, digits = 3, format = "f"), "m"))
  
  print(paste("--------------------------------------------------------------------"))
  
  return(POS_stats_float_only)
  
  
}



# a function to read the POS file. the FMT_OPT is set to 5 to read POS files generated by Emlid Studio

read_POS = function(POS_ffn,FMT_OPT=NULL){
  
  lines = readLines(POS_ffn,100)
  NSKIP = min(which(!( str_detect(lines, "^\\%"))))-1
  
  if (is.null(FMT_OPT)){
    
    D = read_csv(POS_ffn, skip = NSKIP, col_names =F, show_col_types = FALSE)
    
    
    
    HDRS = c("GPS_Week", "GPS_Time",  "Lat_DD",  "Lon_DD",  "Ht_ell", "Q",
             "nvsv","sdn", "sde", "sdu", "sdne" , "sdeu","sdun", "age" ,"ratio")
    names(D) <- HDRS
    D %>% pull(GPS_Week) %>% unique()
    D = D %>% 
      mutate(
        across(Lat_DD:ratio, as.numeric),
        melap = (GPS_Time - GPS_Time[1])/60,
        UTC_TIME_PX = GPS_tm_2_UTC(GPS_Week , GPS_Time )) 
    
    
  }else{
    
    leapseconds_curr = 18
    
    if (FMT_OPT==2){
      
      D = read_csv(POS_ffn, skip = NSKIP, col_names =F)
      HDRS = c("GPS_DateTime",   "Lat_DD",  "Lon_DD",  "Ht_ell", "Q",
               "nvsv","sdn", "sde", "sdu", "sdne" , "sdeu","sdun", "age" , "ratio")
      names(D) <- HDRS
      D = D %>%
        dplyr::filter(!is.na(GPS_DateTime) | GPS_DateTime != "" | GPS_DateTime != "NA") %>% 
        mutate(
          GPS_Time = as.POSIXct(GPS_DateTime, format = "%Y/%m/%d %H:%M:%S"),
          melap = (as.numeric(GPS_Time) - as.numeric(GPS_Time[1]))/60,
          UTC_TIME_PX = GPS_Time + seconds(leapseconds_curr)
        )
      
    } else if (FMT_OPT==3){
      
      # This is for the EMLID LLH files
      
      D = read.table(POS_ffn, skip = 0, header =F, sep = "") %>% as_tibble()
      HDRS = c("GPS_YMD", "GPS_HMS",   "Lat_DD",  "Lon_DD",  "Ht_ell", "Q",
               "nvsv","sdn", "sde", "sdu", "sdne" , "sdeu","sdun", "age" , "ratio")
      names(D) = HDRS
      D = D %>%
        mutate(
          GPS_DateTime = paste(GPS_YMD,GPS_HMS),
          GPS_Time = as.POSIXct(GPS_DateTime, format = "%Y/%m/%d %H:%M:%OS"),
          melap = (as.numeric(GPS_Time) - as.numeric(GPS_Time[1]))/60,
          UTC_TIME_PX = GPS_Time + seconds(leapseconds_curr)
        )
      
    } else if (FMT_OPT==4){
      
      # This is for the EMLID  files created by EMLID studio
      
      D = read.table(POS_ffn, skip = 11, header =F, sep = "") %>% as_tibble()
      HDRS = c("GPS_YMD", "GPS_HMS",   "Lat_DD",  "Lon_DD",  "Ht_ell", "Q",
               "nvsv","sdn", "sde", "sdu", "sdne" , "sdeu","sdun", "age" , "ratio")
      names(D) = HDRS
      D = D %>%
        mutate(
          GPS_DateTime = paste(GPS_YMD,GPS_HMS),
          GPS_Time = as.POSIXct(GPS_DateTime, format = "%Y/%m/%d %H:%M:%OS"),
          melap = (as.numeric(GPS_Time) - as.numeric(GPS_Time[1]))/60,
          UTC_TIME_PX = GPS_Time + seconds(leapseconds_curr)
        )
      
    } else if (FMT_OPT==5){
      
      # This is for the EMLID  files created by DEMO Version of RTKLIB
      
      D = read.table(POS_ffn, skip = NSKIP, header =F, sep = "") %>% as_tibble()
      HDRS = c("GPS_YMD", "GPS_HMS",   "Lat_DD",  "Lon_DD",  "Ht_ell", "Q",
               "nvsv","sdn", "sde", "sdu", "sdne" , "sdeu","sdun", "age" , "ratio")
      names(D) = HDRS
      D = D %>%
        mutate(
          GPS_DateTime = paste(GPS_YMD,GPS_HMS),
          GPS_Time = as.POSIXct(GPS_DateTime, format = "%Y/%m/%d %H:%M:%OS"),
          melap = (as.numeric(GPS_Time) - as.numeric(GPS_Time[1]))/60,
          UTC_TIME_PX = GPS_Time + seconds(leapseconds_curr)
        )
      
    } else {
      
      D = read_csv(POS_ffn, skip = NSKIP, col_names =F)
      HDRS = c("GPS_Week", "GPS_Time",  "Lat_DD",  "Lon_DD",  "Ht_ell", "Q",
               "nvsv","sdn", "sde", "sdu", "sdne" , "sdeu","sdun", "age" ,"ratio")
      names(D) <- HDRS
      D %>% pull(GPS_Week) %>% unique()
      D = D %>% 
        mutate(
          across(Lat_DD:ratio, as.numeric),
          melap = (GPS_Time - GPS_Time[1])/60,
          UTC_TIME_PX = GPS_tm_2_UTC(GPS_Week , GPS_Time )) 
      
      
    }
  }
  return(D)
}




