#' Adds distance to first and last locations for each day
#'
#' Adds columns with distance to first and last location of each day
#'
#' @usage AddFirstLastDistance(df, id, datetime, tz)
#' @param df dataframe with locations
#' @param by column name of unique identifier, default is "id"
#' @param datetime column name of datetime in POSIXct format or as a character
#'   in the format (\%Y/\%m/\%d \%H:\%M)
#' @param tz return to this timezome. Default "Etc/GMT+5".
#'
#' @return dataframe with "datetime", "first", "last"
#' @export
#'
#' @details Caution with use of tz, may cause problems
AddFirstLastDistance <- function(df = df,
                                 by = "id",
                                 datetime = "datetime",
                                 tz = "Etc/GMT+5"){
  df$by <- df[,by]
  df$datetime <- df[,datetime]
  df$date <- as.Date(df[,datetime], tz=tz)
  df$datetime <- as.character(df$datetime)  # convert to character
  df$date <- as.character(df$date)  # convert to character
  first <- plyr::ddply(df, plyr::.(date, by), function(x) x[1, ])# first records
  last <- plyr::ddply(df, plyr::.(date, by), function(x) x[(nrow(x)), ])
    # last records
  first$first <- "First"  # populates "first" column
  last$last <- "Last"  # populates "last" column
  first <- subset(first, select = c(by, first, date, datetime, long_utm,
    lat_utm))  # subsets first records
  colnames(first) <- c("by", "first", "date", "datetime","long_utm_first",
    "lat_utm_first")  # need to id first locations
  last <- subset(last, select = c(by, last, date, datetime, long_utm,
    lat_utm))  # subsets last records
  colnames(last) <- c("by", "last", "date", "datetime","long_utm_last",
    "lat_utm_last")  # need to id last locations
  first_date <- subset(first, select=c(by, date, long_utm_first,
    lat_utm_first))  # all first locations
  last_date <- subset(last, select=c(by, date, long_utm_last, lat_utm_last))
    # all last locations
  first_last_date <- merge(first_date, last_date, by = c("by","date"),
    all.x = TRUE)  # first and last record locations
  first_last <- merge(first, last, all=TRUE)  # merges first and last records
  first_last_datetime <- subset(first_last, select = c(by, first, last,
    datetime))  # only first and last records and their times
  df <- merge(df, first_last_datetime, by=c("by", "datetime"), all.x=TRUE)
  df <- merge(df, first_last_date, by=c("by", "date"), all.x=TRUE)
      #adds the lat and long for the first and last records to the dataframe
  df <- plyr::adply(df, 1, transform, dist_first = as.integer(sqrt(sum((c(
    long_utm, lat_utm) - c(long_utm_first, lat_utm_first))^2))))  # Pythagorean
  df <- plyr::adply(df, 1, transform, dist_last = as.integer(sqrt(sum((c(
    long_utm, lat_utm) - c(long_utm_last, lat_utm_last))^2)))) # Pythagorean
  df$date <- as.Date(df$date, "%Y-%m-%d") #convert date into date format
  df$datetime <- strptime(df$datetime, "%Y-%m-%d %H:%M:%S")  # back to POSIXct
  df$datetime <- as.POSIXct(df$datetime, tz=tz, usetz=FALSE)  # returns to tz
  drops <- c("long_utm_first", "lat_utm_first", "long_utm_last",
             "lat_utm_last", "by")  # vector of columns to drop
  df <- df[ ,!(names(df) %in% drops)]
  return(df)
}

#' Add step length and angle to location data
#'
#' Calculates step length, absolute angle (N=0), and turn angle between
#'   successive point locations
#'
#' @usage AddStepLengthAndAngles(df, by, datetime, long, lat)
#' @param df dataframe with locations and datetime data
#' @param by column name to use to split, analyze, and merge data, default is
#'   "id"
#' @param long longitude, must be in UTM, default is "long_utm"
#' @param lat latitude, must be in UTM, default is "lat_utm"
#'
#' @return dataframe with "step_length", "abs_angle", and "turn_angle" columns
#' @export
#'
#' @details Coordinates must be in identically-scaled units (e.g. UTM meters).
#'   If lat and long are in degrees, project coordinates to UTM with 'rgdal'
#'   package before running this function.
#'
AddStepLengthAndAngles <- function(df,
                                   by = "id",
                                   long = "long_utm",
                                   lat = "lat_utm"){
  df <- df
  ifelse(is.null(by), df$by <- "all", df$by <- df[,by])
  StepLengthAndAngles <- function(df=df, lat=lat, long=long){
    xy <- data.frame(x = df[,long], y = df[,lat])
    xy1 <- xy[-1, ]
    xy2 <- xy[-nrow(xy), ]
    step_length <- c(sqrt((xy1$x - xy2$x)^2 + (xy1$y - xy2$y)^2), NA)
    dx <- c(xy1$x - xy2$x, NA)
    dy <- c(xy1$y - xy2$y, NA)
    abs_angle <- ifelse(step_length < 1e-07, NA, (atan2(dy, dx)))
    abs_angle <- ifelse(abs_angle < 0, (2*pi) + abs_angle, abs_angle)
    turn_angle <- c(abs_angle, 0) - c(0, abs_angle)
    turn_angle <- ifelse(turn_angle < 0, (2*pi) + turn_angle, turn_angle)
    turn_angle <- turn_angle[-length(turn_angle)]
    turn_angle[1] <- NA
    out <- cbind.data.frame(dx=dx, dy=dy, step_length=step_length,
      abs_angle=abs_angle, turn_angle=turn_angle) #,
  }
  uniques <- unique(df[,"by"])
  out <- data.frame()
  for (j in uniques){
    sv = df[,by] %in% j
    data <- subset (df, by==j)
    df2<- cbind(data, StepLengthAndAngles(df=data, lat=lat, long=long))
    out <- rbind(out, df2)
  }
  out$by<-NULL
  return(out)
}

#' Add step time
#'
#' Calculates time between successive point locations
#'
#' @usage AddStepTime(df, by, datetime)
#'
#' @param df Dataframe with locations and datetime data
#' @param by Column name to use to split, analyze, and merge data, default is
#'   "id".
#' @param datetime Column name of datetime, default is "datetime"
#'
#' @return Dataframe with "step_time" column
#' @export
#'
AddStepTime<- function(df,
                       by = "id",
                       datetime = "datetime"){
  df <- df
  df$by <- df[,by]
  StepTime <- function(df, datetime=datetime){
    t <- df[,datetime]
    diff <- difftime(head(t, -1), tail(t, -1))
    step_time <- as.integer(c(-1*as.numeric(diff, units = "mins"), NA))
  }
  uniques <- unique(df[,by])
  out <- data.frame()
  for (j in uniques){
    sv = df[,by] %in% j
    data <- subset (df, by==j)
    df2 <- cbind(data, step_time = StepTime(df=data, datetime=datetime))
    out <- rbind(out, df2)
  }
  out$by<-NULL
  return(out)
}

#' Add solar times
#'
#' Add sunrise, sunset, and solarnoon to a dataframe of location data
#'
#' @usage AddSolarTimes(df, id, tz)
#'
#' @param df Dataframe
#' @param by Column name to use to split, analyze, and merge data. Default is
#'   "id".
#' @param tz Column name of timezone. Default is "Etc/GMT+5".
#'
#' @return Dataframe with sunrise, sunset, and solarnoon times
#' @export
#'
#' @details Coordinates in "lat" and "long" must be WGS84, sunrise and
#'   solarnoon based on first location, sunset based on last location.
AddSolarTimes <- function(df = df,
                          by = "id",
                          tz = "Etc/GMT+5"){
  df <- df
  df$by <- df[,by]
  if( ! ("date" %in% colnames(df))) {
    df$date <- as.Date(df$datetime,tz = tz)
  }
  AddTimes<-function (df=df){
    first <- plyr::ddply(df, plyr::.(date), function(x) x[1, ])  # first records
    sunrise_coords <- cbind(first$long, first$lat)
    sunrise_datetime <- first$datetime
    sunrise <- maptools::sunriset(sunrise_coords, sunrise_datetime,
      proj4string = sp::CRS("+proj=longlat +datum=WGS84"), direction ="sunrise",
      POSIXct.out= TRUE)
    solarnoon <- maptools::solarnoon(sunrise_coords, sunrise_datetime,
      proj4string = sp::CRS("+proj=longlat +datum=WGS84"), POSIXct.out = TRUE)
    sunrise$date <- as.Date(sunrise$time, tz = tz)
    sunrise$sunrise <- sunrise$time
    sunrise <- subset(sunrise, select = c(date, sunrise))
    solarnoon$date <- as.Date(solarnoon$time, tz=tz)
    solarnoon$solarnoon <- solarnoon$time
    solarnoon <- subset(solarnoon, select = c(date, solarnoon))
    last <- plyr::ddply(df, plyr::.(date), function(x) x[(nrow(x)), ])  # last
    sunset_coords <- cbind(last$long, last$lat)
    sunset_datetime <- last$datetime
    sunset <- maptools::sunriset(sunset_coords, sunset_datetime, proj4string =
      sp::CRS("+proj=longlat +datum=WGS84"), direction = "sunset",
      POSIXct.out= TRUE)
    sunset$date <- as.Date(sunset$time, tz=tz)
    sunset$sunset <- sunset$time
    sunset <- subset(sunset, select = c(date, sunset))
    df <- merge(df, sunrise, by="date", all.x = TRUE)
    df <- merge(df, solarnoon, by="date", all.x = TRUE)
    df <- merge(df, sunset, by="date", all.x = TRUE)
    df$hr_before_sunrise <- df$sunrise - 3600  # subtracts an hour from sunset
    tz(df$sunrise) <- tz  # sets timezone for sunrise times
    tz(df$hr_before_sunrise) <- tz  # sets timezone for sunrise times
    df$hr_after_sunset <- df$sunset + 3600  # adds an hour to sunset
    tz(df$sunset) <- tz  # sets timezone for sunrise times
    tz(df$hr_after_sunset) <- tz  # sets timezone for sunrise times
  return(df)
  }
  df2 <- plyr::ddply(df, plyr::.(by), AddTimes)
  df2$by <- NULL
  return(df2)
}

#' Compile CTT downloads
#'
#' Compiles the downloaded CTT files
#'
#' @usage CompileDownloads(units, compile, tz)
#'
#' @param units String, "deployed" or "reserve". Default is "deployed".
#' @param compile String, "all" or "recent". Default is "recent"
#' @param tz Timezone, default is "Etc/GMT+5".
#'
#' @return Dataframe of compiled files
#' @export
#'
#' @import dplyr
#'
#' @details Internal parameters are set specifically for Maine data
#'
CompileDownloads <- function(units = "deployed",
                             compile = "all",
                             tz = "Etc/GMT+5") {
  `%>%` <- magrittr::`%>%`
  infile <- paste("C:/Work/R/Data/BAEA/Telemetry/", units, "/",
    compile, sep = "")
  filenames <- list.files(path=infile, full.names=TRUE)
  output <- paste("into ", units, "_", compile, sep="")
  writeLines(noquote(paste(c("Compiling files:", filenames, output))))
#  suppressWarnings(df <- do.call("rbind", lapply(filenames, read.csv,colClasses=
#    c("character", "character", "character", "character", "character",
#    "character", "numeric", "integer", "integer", "numeric",
#    "numeric", "numeric", "character", "character","character",
#    "integer", "integer", "character", "character"), header = TRUE,
#    na.strings = "")))
  units_list <- list()
  for (i in 1:length(filenames)){
    if(length(read.csv(filenames[i])) > 1) {
      suppressWarnings(unit <- read.csv(filenames[i], colClasses= c("character",
      "character", "character", "character", "numeric",  "numeric", "numeric",
      "integer", "integer", "numeric", "numeric", "numeric", "numeric",
      "numeric", "integer", "numeric"), header = TRUE, na.strings = ""))
      name <- paste('item:',i,sep='')
      units_list[[name]] <- unit
    }
  }
  suppressWarnings(df <- do.call("rbind", units_list))
  df <- subset(df, select=serial:alt)
  df[which(colnames(df) == "speed")] <- as.integer(round(df$speed))
  df[which(colnames(df) == "alt")] <- as.integer(round(df$alt))
  date <- Sys.Date()
  if (compile == "recent" || compile == "all") {
    outfile <- paste("C:/Work/R/Data/BAEA/Telemetry/", units, "/Archive/",
      compile, "/",date,".csv", sep ="")
    if ( !file.exists(outfile)) {
      writeLines(noquote(c("Writing: ", outfile)))
      write.csv(df, file=outfile, row.names=FALSE)
    }
  }
  df$serial <- substr(df$serial, nchar(df$serial)-5+1, nchar(df$serial))
    # removes first 15 digits in serial, then convert to integer
  colnames(df)[2] <- "date"  # "GPS_date_DDMMYYYY" to "date"
  colnames(df)[3] <- "time"  # "GPS_utc_HH:MM:SS" to "time"
  colnames(df)[4] <- "datetime"  # "GPS_YYYY..." to "datetime"
  df$datetimeUTC <- as.POSIXct(df$datetime, tz="GMT", usetz=FALSE)
    # set time to UTC and convert to POSIXct format
  df$datetime <- format(df$datetimeUTC, tz=tz, usetz=FALSE)  # convert to EST
  df$datetime <- as.POSIXct(df$datetime, tz=tz, usetz=FALSE)  # convert to EST
  df$year <- as.numeric(strftime(df$datetime, format="%Y", usetz=FALSE))  # year
  df$date <- as.POSIXct(df$datetime, tz=tz, "%Y-%m-%d", usetz=FALSE)  # date
  df$date <- as.Date(df$datetime, tz=tz,"%Y-%m-%d", usetz=FALSE)  # only date
  if("lon" %in% colnames(df)){
     colnames(df)[which(names(df) == "lon")] <- "long"
  }
  df$long <- as.numeric(gsub("W", "", df$long))
  df$long <- as.numeric(gsub("E", "", df$long))
  if (all(df$long >= 0)) df$long <- (df$long)*-1
  df$lat <- as.numeric(gsub("N", "", df$lat))
  df$lat <- as.numeric(gsub("S", "", df$lat))
  gps_data <- read.csv("C:/Work/R/Data/BAEA/GPS_Deployments.csv",
    header=TRUE, as.is=TRUE, na.strings = "")
  gps_data <-  subset(gps_data, select=serial:notes)  # keeps id:notes col
  gps_data$id <- NA
  date_cols <- c("on_hand","deployed", "end_data",  "failed",  "removed",
      "recovered")
  for (i in date_cols) {
    gps_data[,i] <- as.character(gps_data[,i])
    gps_data[,i] <- as.Date(gps_data[,i], "%Y%m%d")
  }
  gps_blank <- gps_data[0,]
  gps_blank[1:nrow(df),] <- NA
  gps_blank$serial <- NULL  # removes the redudant serial column
  df <- cbind(df, gps_blank)
  if (units == "deployed") {
    deployed <- gps_data[which(!is.na(gps_data$deploy_location)),]
    for (i in 1:nrow(deployed)) {
      record <- deployed[i,]
      if (is.na(record$end_data)) {
        end_date <- Sys.Date() + 1
      } else {
        end_date <- record$end_data
      }
      sv <- df$serial == record$serial & df$date > record$deployed &
      df$date < end_date
      record$id <- record$deploy_location
      record$serial <- NULL # prevents a redundant "serial.1" column
      df[sv, (ncol(df)-length(record)+1):ncol(df)] <- record[1,]
    }
    df <- subset(df, (!is.na(deploy_location)))
    df <- df %>% arrange(deploy_location, datetime)
  }
  if (units == "reserve") {
    reserve <- gps_data
    for (i in 1:nrow(reserve)) {
      record <- reserve[i,]
      if (is.na(record$end_data)) {
        end_date <- Sys.Date() + 1
      } else {
        end_date <- record$end_data
      }
      if (is.na(record$deployed)) {
        deploy_date <- Sys.Date() + 1
      } else {
        deploy_date <- record$deployed
      }
      sv <- df$serial == record$serial & df$date > record$on_hand  & df$date <
        deploy_date & df$date < end_date
      record$id <- record[1,"serial"]
      record$serial <- NULL  # prevents a redundant "serial.1" column
      record$deploy_location <- NA
      record$bird_ID <- NA
      record$sex <- NA
      record$deploy_seq <- NA
      df[sv, (ncol(df)-length(record)+1):ncol(df)] <- record[1,]
    }
    df <- subset(df, !is.na(id))
    df <- df %>% arrange(id, datetime)
  }
  xy <- cbind(df$long,df$lat) # 2 col for next step
  xy <- rgdal::project(xy, "+proj=utm +zone=19 ellps=WGS84")  # to WGS84 UTM N19
  colnames(xy) <- c("long_utm", "lat_utm")  # name columns
  xy <- round(xy)  # rounds lat and long to zero decimal places
  df <- cbind(df, xy)  # combines lat long with data
  df$long_utm <- as.integer(df$long_utm)
  df$lat_utm <- as.integer(df$lat_utm)
  drops <- c("time", "utc", "cog", "data_voltage", "capacity",
    "pow_voltage", "pow_timestamp", "barfcn", "dbm", "netnameasc",
    "serv_timestamp", "id_long",
    "datetimeUTC")  # vector of columns to drop
  df <- df[ ,!(names(df) %in% drops)]
  df <- subset(df, !is.na(df$lat))
  df <- subset(df, !is.na(df$long))
  df <- df[df[,"lat"] != 0,] # removes rows with 0 for latitude
  df <- df[df[,"long"] != 0,] # removes rows with 0 for longitude
  df <- unique(df)  # removes duplicate rows, e.g. 72179 2013-12-29 06:28:39
  df <- df[,c("id",setdiff(names(df),"id"))]  # puts "id" column first
  row.names(df) <- NULL
  return(df)
}

#' Create 'move' object
#'
#' A wrapper function for creating a 'move' object
#'
#' @param df Dataframe with locations
#'
#' @return A 'move'object
#' @export
#'
#' @details Internal parameters set specifically for my BAEA GPS data.
CreateMove <- function(df){
  df <- dplyr::arrange(df, id, datetime)
  df$order <- NULL
  move <- move::move(x = df$long, y = df$lat, time = df$datetime,
    proj = sp::CRS("+proj=longlat"), animal = df$id, sensor = "GPS")
  return(move)
}

#' Download CTT data
#'
#' Downloads data files from CTT's website
#'
#' @param units String, "deployed", "reserve", or "none".
#' @param download String, "all" or "recent". Default is "recent".
#'
#' @return Dataframe of downloaded files
#' @export
#'
#' @examples This is ENTIRELY DEPENDENT on my Python scripts/locations.
DownloadCTT <- function(units="",
                        download="recent") {
  if (units == "deployed" && download == "all") {
    system('python C:/Work/Python/Scripts/BAEA_CTT/Import_Deployed_All.py')
    writeLines(noquote("Downloading all data for deployed units from CTT"))
  }
  if (units == "deployed" && download == "recent") {
    system('python C:/Work/Python/Scripts/BAEA_CTT/Import_Deployed_Recent.py')
    writeLines(noquote("Downloading recent data for deployed units from CTT"))
  }
  if (units == "reserve" && download == "all") {
    system('python C:/Work/Python/Scripts/BAEA_CTT/Import_Reserve_All.py')
    writeLines(noquote("Downloading all data for reserve units from CTT"))
  }
  if (units == "reserve" && download == "recent") {
    system('python C:/Work/Python/Scripts/BAEA_CTT/Import_Reserve_Recent.py')
    writeLines(noquote("Downloading recent data for reserve units from CTT"))
  }
  if (units == ""  | units == "none") {
    writeLines(noquote("Not downloading data from CTT"))
  }
}

#' Import units
#'
#' Imports location records and merges with existing
#'
#' @param units String, units to download either "deployed" or "reserve".
#' @param existing Exisiting file to merge with import records.
#' @param import Logical, whether to import previous records. Default is TRUE,
#'
#' @return Dataframe
#' @export
#'
#' @details Defaults are specific to my file directories and locations
ImportUnits <- function(units = "deployed",
                        existing = NULL,
                        import = TRUE) {
  if (import == TRUE) {
    units_import <- read.csv(file = paste("C:/Work/R/Data/BAEA/Telemetry/",
      units, "/", units, ".csv", sep =""), header=TRUE, stringsAsFactors=FALSE)
    units_import$datetime <- as.POSIXct(units_import$datetime,
      tz="Etc/GMT+5", usetz=FALSE) #convert to POSIXct in EST
    if (!is.null(existing)) {
      units_merge <- rbind(existing, units_import)
      units_full <- unique(units_merge)
      units_full <- units_full[with(units_full,order(id,datetime)),]
      row.names(units_full) <- NULL
      date <- Sys.Date()
      outfile <- paste("C:/Work/R/Data/BAEA/Telemetry/", units, "/Archive/",
        units, "/", units, "_", date, ".csv", sep ="")
      if (!file.exists(outfile)) {
        writeLines(noquote(paste("Merging existing and import")))
        write.csv(units_full, file=outfile, row.names=FALSE)
        writeLines(noquote(c("Writing: ", outfile, sep = "")))
        units_full_rewrite<-paste("C:/Work/R/Data/BAEA/Telemetry/", units,
          "/", units, ".csv", sep ="")
        write.csv(units_full, file=units_full_rewrite, row.names=FALSE)
          # rewrites import file
      }
    } else {
      units_full <- units_import
      date_cols<- c("date","on_hand","deployed", "end_data",  "failed",
        "removed", "recovered")  # corresponds to list in CompileDownloads
      for (i in date_cols) {
        units_full[,i] <- as.character(units_full[,i])
        units_full[,i] <- as.Date(units_full[,i], "%Y-%m-%d")
      }
    }
  }
  if (import == FALSE) {
    writeLines(noquote(paste("Previous records NOT imported")))
    if (!is.null(existing)) {
      writeLines(noquote(paste("Coverted existing to output", sep="")))
      units_full <- existing
    }
  }
  return(units_full)
}

#' Plots 3D interactive
#'
#' Makes a 3D plot with 'rgl' package
#'
#' @param df Dataframe with x, y, z data, Default is 'baea'.
#' @param x Column name of x (longitude) locations.
#' @param y Column name of y (latitude) locations.
#' @param z Column name of z (elevation) locations
#'
#' @return 3D plot from 'rgl'package
#' @export
#'
#' @details Can be any z, y, z, but labels are set for lat, long, elev data.
Plot3DInteractive <- function(df = baea,
                              x = "x",
                              y = "y",
                              z = "z"){
  df$x <- df[,x]
  df$y <- df[,y]
  df$z <- df[,z]
  xyz <- cbind(df$x, df$y, df$z)  # df of x, y, z data
  colnames(xyz) <- c("x", "y", "z")
  rgl::open3d()  # open an RGL window
  rgl::bg3d("white")  # make the background white
  rgl::plot3d(xyz, aspect=c(1, 1, .5), col = "darkblue",
         xlab = "longitude", ylab = "latitude", zlab = "elevation(m)")
  rgl::lines3d(xyz, color="red")
}

#' Plot location counts
#'
#' Plots a linegraph of daily locations
#'
#' @usage PlotLocationsCount(df, id, individual, color_factor, start, end,
#'   breaks, tz, wrap)
#'
#' @param df Dataframe with id, datetime.
#' @param id Column name of unique identifier.
#' @param individual String, individual/s (from id column) to keep, format
#'   should be c(id, id). Default is all.
#' @param color_factor Column names, factor to determine location point color.
#' @param start Date, filter start date. Default is 1970-01-01.
#' @param end Date, filter end date. Default is current date.
#' @param breaks Breaks on x-axis (i.e., "7 days"). Default is "14 days"
#' @param tz String, timezone. Default is "Etc/GMT+5".
#' @param wrap Logical, whether to wrap ribbon of panels into 2d. Default TRUE.
#'
#' @return Daily locations over a range of dates
#' @export
#'
#' @details  Using color_factor on "behavior"
PlotLocationCount <- function(df = df,
                              id = "id",
                              individual = "",
                              color_factor = NULL,
                              start = NULL,
                              end = NULL,
                              breaks = "14 days",
                              tz = "Etc/GMT+5",
                              wrap = FALSE) {
  if(!is.null(color_factor)) {
    cf_name <- color_factor
    df$color_factor <- df[,color_factor]
  }
  if ( ! (individual == "" || individual == "all")){
    df = df[which(df[,id] == individual), ]  # to extract individuals
  }
  if(!is.null(start)) {
    start = as.Date(start)
  } else {
    start = min(df$date)
  }
  if(!is.null(end)) {
    end = as.Date(end)
  } else {
    end = max(df$date)
  }
  limits_x = c(start, end)
  sumstats <- SummarizeDailyLocations(df=df)
  p <- ggplot(data=sumstats, aes(x=date, y=total_loc)) +
  labs(title = "Daily Locations", x="Date", y="Locations") +
  scale_x_date(breaks=scales::date_breaks(breaks), labels=date_format("%m/%d"),
  limits=limits_x)
  if(!is.null(color_factor)) {
    p <-  p + geom_line(aes(group=factor(id), colour = factor(id))) +
    geom_point(aes(group=factor(id), colour = factor(id))) +
    labs(shape=cf_name, colour=cf_name)
  } else {
    p <- p + geom_line() + geom_point()
  }
  p <- p + theme_bw() + theme(plot.title = element_text(size = 22)) +
  theme(text = element_text(size = 18, colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) +
  theme(text = element_text(size = 20, colour = "black")) +
  theme(axis.text = element_text(colour = "black"))
  if (wrap == TRUE) {
    p + facet_wrap(~ id, scales= "free_x")
  } else {
    p
  }
}

#' Plots locations related to sunrise and sunset
#'
#' Plots locations in relation to sunrise, sunset, and solarnoon.
#'
#' @param df Dataframe with id, lat, long, datetime.
#' @param by Column name to use to split, analyze, and merge data. Default is
#'   "id".
#' @param individual String, individual/s (from id column) to keep, format
#'   should be c(id, id), default is all
#' @param color_factor Column names, factor to determine location point color.
#' @param pal String, color palette for CreateVarsColors(). Default is NULL.
#' @param b_pal String, color palette from RColorBrewer for CreateVarsColors(),
#'   default is "Set1"
#' @param color_factor Column names, factor to determine location point color.
#' @param start Date, filter start date. Default is 1970-01-01.
#' @param end Date, filter end date. Default is current date.
#' @param breaks Breaks on x-axis, i.e., "7 days".
#' @param tz Timezome. Default "Etc/GMT+5".
#' @param addsolartimes Logical, whether to not to run the AddSolarTimes() on
#'   data first. Default FALSE.
#' @param wrap Logical, whether or not to wrap ribbon of panels into 2d.
#'   Default is TRUE.
#'
#' @return Plot of locations over a range of dates
#' @export
#'
#' @import ggplot2
#'
#' @details Using color_factor on "behavior"
PlotLocationSunriseSunset <- function(df = df,
                                      by = "id",
                                      individual = "",
                                      color_factor = NULL,
                                      pal = NULL,
                                      b_pal = "Set1",
                                      start = NULL,
                                      end = NULL,
                                      breaks = "1 days",
                                      tz = "Etc/GMT+5",
                                      addsolartimes = FALSE,
                                      wrap = TRUE) {
  df <- df
  df$by <- df[,by]
  if(!is.null(color_factor)) {
    cf_name <- color_factor
    df$color_factor<-df[,color_factor]
    by_colors <- CreateColorsByAny(by=color_factor, df=df, output=TRUE, pal=pal,
    b_pal=b_pal)
  } else {
    ifelse(is.null(by),  df$by <- "all", df$by <- df[,by])
    by_colors <- CreateColorsByAny(by=by, df=df, output=TRUE, pal=pal,
      b_pal=b_pal)
  }
  if ( ! (individual == "" || individual == "all" || is.null(individual))){
    df = df[which(df[,"id"] == individual), ]  # to extract individuals
  }
  if (addsolartimes == TRUE) {
    df <- gisr::AddSolarTimes(df = df, by = by,  tz = tz)
    df$by <- df[,by]  # AddSolorTimes removes df$by
  }
  df$loc_time <- format(df$datetime, format = "%H:%M:%S")
  df$loc_time <- as.POSIXct(df$loc_time, tz="GMT", format = "%H:%M:%S")
  df$sunrise <- format(df$sunrise, format = "%H:%M:%S")
  df$sunrise <- as.POSIXct(df$sunrise, tz="GMT", format = "%H:%M:%S")
  df$solarnoon <- format(df$solarnoon, format = "%H:%M:%S")
  df$solarnoon <- as.POSIXct(df$solarnoon, tz="GMT", format ="%H:%M:%S")
  df$sunset <- format(df$sunset, format = "%H:%M:%S")
  df$sunset <- as.POSIXct(df$sunset, tz="GMT", format = "%H:%M:%S")
  if(!is.null(start) && start != "") {
    start = as.POSIXct(start)
  } else {
    start = min(df$datetime)
  }
  if(!is.null(end) && end != "") {
    end = as.POSIXct(end)
  } else {
    end = max(df$datetime)
  }
  limits_x = c(start, end)
  p <- ggplot(data = df) +
    geom_line(aes(datetime, solarnoon), colour = "red", size = 2) +
    geom_line(aes(datetime, sunset), colour = "orange" , size = 2) +
    geom_line(aes(datetime, sunrise), colour = "orange", size = 2) +
    labs(title = "Locations in Relation to Sunrise and Sunset",
      x="Date", y="Time") +
    scale_y_datetime(breaks=scales::date_breaks("1 hour"), labels =
        scales::date_format("%H")) +
    scale_x_datetime(breaks=scales::date_breaks(breaks), labels =
        scales::date_format("%m/%d"),limits=limits_x)
  if(!is.null(color_factor)) {
    p <-  p + geom_point(aes(datetime, loc_time, colour = factor(color_factor),
     shape = factor(color_factor)), size = 2) +
      labs(shape=cf_name, colour=cf_name) +  scale_fill_brewer(palette="Set1")
  } else {
    p <- p + geom_point(aes(datetime, loc_time), size=2)
  }
  p <- p + theme_bw() + theme(plot.title = element_text(size = 22)) +
    theme(text = element_text(size = 18, colour = "black")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) +
    theme(text = element_text(size = 20, colour = "black")) +
    theme(axis.text = element_text(colour = "black"))  +
    theme(panel.background = element_rect(fill = "gray90")) +
    theme(panel.grid.major = element_line(color = "gray80")) +
    theme(panel.grid.minor = element_line(color = "gray80")) +
    scale_colour_manual(values = by_colors)
  if (wrap == TRUE) {
  p + facet_wrap(~ by)
  } else {
  p
  }
}

#' Plot move object
#'
#' Wrapper for plotting a 'move' object
#'
#' @usage PlotMove(move)
#'
#' @param move A 'move' object
#'
#' @return Plot of move object
#' @export
#'
#' @details Only work on move objects
#'
PlotMove <- function(move = move){
  units <- nlevels(idData(move))
  plot(move, type="o", col=seq(1, units, by=1), lwd=2, pch=20,
    xlab="Longitude", ylab="Latitude")
}

#' Plot overnight distances
#'
#' Scatterplot of overnight distances. Non-sequential days are removed.
#'
#' @usage PlotOvernightDistances(df, individual, ylim, point_size)
#'
#' @param df Dataframe of locations with "date", "last", "step_length"
#' @param individual String, individual/s (from id column) to keep, format
#'   should be id. Default is 'all'.
#' @param ylim Numeric, y-axis limit
#' @param point_size Numeric, point size for time datapoints
#'
#' @return A scatterplot
#' @export
#'
PlotOvernightDistances <- function(df = baea,
                                   individual = "",
                                   ylim = NULL,
                                   point_size = 3){
  df <- RemoveNonsequentialDays(df = df)
  if (individual == ""|individual == "all"){
    df = df[which(df$last == "Last"),]
  } else {
    df = df[which(df$last == "Last" & df$id == individual),]
  }
  if (length(unique(df$id)) == 1) {
    title = paste("Overnight Distance by Date: Unit ", unique(df$id), sep = "")
    if(ylim == "" || is.null(ylim)){
      g<-ggplot(df, aes(x = date, y = step_length))
      g + geom_point(size = point_size, colour="dark blue") +
        theme(text = element_text(size = 20, colour="black")) +
        theme(axis.text.y = element_text(colour="black")) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1,
          colour= "black")) +
        scale_x_date(labels = date_format("%m/%d"), breaks="7 days") +
        labs(title=title, x="Date", y="Distance (m)", text="black") +
        guides(shape = guide_legend("ID #"))
    } else {
      title = paste("Plot of Overnight Distance by Date (y axis limited to ",
        ylim, "m)", sep = "")
      g <- ggplot(df, aes(x = date, y = step_length))
      g + ylim(0, ylim) +
        geom_point(size = point_size, colour = "darl blue") +
        theme(text = element_text(size = 20, colour = "black")) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1,
          colour= "black")) +
        scale_x_date(labels = date_format("%m/%d"), breaks = "7 days")
      labs(title = title, x = "Date", y = "Distance (m)", text="black") +
        guides(shape = guide_legend("ID #"))
    }
  } else {
    if(ylim == "" || is.null(ylim)){
      g <- ggplot(df, aes(x = date, y = step_length, colour = factor(id)))
      g + geom_point(size = point_size) +
        theme(text = element_text(size = 20, colour = "black")) +
        theme(axis.text.y = element_text(colour = "black")) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1,
          colour = "black")) +
        scale_x_date(labels = date_format("%m/%d"), breaks = "7 days") +
        labs(title = "Overnight Distances by Date", x = "Date",
             y = "Distance (m)", text="black") +
        guides(colour = guide_legend("ID #"))
    } else {
      title = paste("Plot of Overnight Distances by Date (y axis limited to ",
        ylim, "m)", sep = "")
      g <- ggplot(df, aes(x = date, y = step_length, colour = factor(id)))
      g + ylim(0, ylim) +
        geom_point(size = point_size) +
        theme(text = element_text(size = 20, colour="black")) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1,
          colour= "black")) +
        scale_x_date(labels = date_format("%m/%d"), breaks = "7 days")
      labs(title="Overnight Distance by Date", x = "Date", y = "Distance (m)",
        text = "black") +
        guides(colour = guide_legend("ID #"))
    }
  }
}

#' Plot overnight distances empirical distribution function
#'
#' Empirical cumulative distribution; non-sequential days removed.
#'
#' @param df Dataframe of locations with "date", "last", "step_length".
#' @param id Column name of unique identifier.
#' @param xlim Numeric, x-axis limit. Default is NULL.
#' @param line_size Numeric, point size for time datapoints.
#'
#' @return Scatterplot
#' @export
#'
#' @details Requires RemoveNonsequentialDays() function.
#'
PlotOvernightDistancesECDF <- function(df = baea,
                                       id = NULL,
                                       xlim = NULL,
                                       line_size = 2){
  df <- RemoveNonsequentialDays(df = df)
  if (id == "" || is.null(id)){
    df = df[which(df$last == "Last"), ]
  } else {
    df = df[which(df$last == "Last" & df$id == id),]
  }
  if (is.null(xlim)){
    g <- ggplot(df, aes(step_length, colour = factor(id)))
    g + stat_ecdf(size = line_size) +
      theme(text = element_text(size = 20, colour = "black")) +
      theme(axis.text = element_text(colour = "black")) +
      labs(title = "ECDF of Overnight Distances", x = "Distance (m)",
        y = "Proportion", text = "black") +
      guides(colour = guide_legend("ID #"))
  } else {
    title = paste("Plot of Overnight Distance by Date (x axis limited to ",
                xlim, "m)", sep = "")
    g <- ggplot(df, aes(step_length, colour = factor(id)))
    g + stat_ecdf() +
      xlim(0, xlim) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"),
        axis.text.y = element_text(colour = "black")) +
      theme(text = element_text(size = 20, colour = "black")) +
      theme(axis.text = element_text(colour = "black")) +
      labs(title=title, x="Distance (m)", y="Proportion",
           text="black") +
      guides(colour = guide_legend("ID #"))
  }
}

#' Remove non-sequential days
#'
#' Removes days that do not have GPS data for the next day
#'
#' @usage RemoveNonsequentialDays(df)
#'
#' @param df Dataframe of location, default is "baea".
#'
#' @return Dataframe
#' @export
#'
#' @details used in PlotOvernightDistancesECD().
#'
RemoveNonsequentialDays <- function(df = baea){
  sumstats <- plyr::ddply(df, plyr::.(id, date), summarize,
    date=as.Date(unique(date)), total_loc = length(deploy_seq))
  nextday <- function(data = data){
    out <- sapply(2:nrow(data), function(i){data$date[i] - data$date[i-1]})
    next_day <- c(out, NA)
    return(next_day)
  }
  list <- by(sumstats, sumstats$id, function(x) nextday(x))  # need a list
  sumstats$nextdayGPS <- unlist(list)  # list into array, combines with sumstats
  df <- merge(df, sumstats, by = c("id","date"), all.x = TRUE)
  df <- subset(df, nextdayGPS == 1)
  df
}

#' Summarize daily locations
#'
#' Gets the daily number of locations for a location dataset
#'
#' @param df Dataframe of locations, needs "id" and "date" columns
#'
#' @return Dataframe of total daily location, including 0 for days w/o data.
#' @export
#'
#' @details Used in PlotDailyLocationsCount()
SummarizeDailyLocations <- function(df = df){
  sumstats <- plyr::ddply(df, plyr::.(id, date), summarize,
    date = as.Date(unique(date)), total_loc = length(id))
  sumstats_filled <- sumstats[0,]
  unique_id <- unique(df$id)
  for (i in unique_id) {
    df <- subset(sumstats, id == i)
    date_min <- min(df$date)
    date_max <- max(df$date)
    all_dates <- seq(date_min, date_max, by="day")
    all_dates_frame <- data.frame(list(date=all_dates))
    merged_frame <- merge(all_dates_frame, df, all=T)
    merged_frame$id <- i
    merged_frame$total_loc[which(is.na(merged_frame$total_loc))] <- 0
    sumstats_filled <- rbind(sumstats_filled, merged_frame)
  }
  return(sumstats_filled)
}

#' Summarize locations
#'
#' Calculates summarize stats for altitude, speed, moving and total locations
#'
#' @usage SummarizeLocations(df)
#'
#' @param df Dataframe with location data
#' @param pdf Logical, whether or not to write PDF. Default is FALSE.
#'
#' @return Table and PDF (optional)
#' @export
#'
#' @details default outputs are specific to my file directories and locations
#'
SummarizeLocations <- function(df = df,
                               pdf = FALSE) {
  sumstats <- plyr::ddply(df, plyr::.(id, date), summarize,
    date = as.Date(unique(date)), max_alt = round(max(alt), 0),
    min_alt = round(min(alt), 0), max_speed = round(max(speed), 0),
    moving_loc = sum(speed>2), total_loc = length(id))
  sumstats_filled <- sumstats[0,]
  unique_id<- unique(df$id)
  for (i in unique_id) {
    df <- subset(sumstats, id == i)
    date_min <- min(df$date)
    date_max <- max(df$date)
    all_dates <- seq(date_min, date_max, by="day")
    all_dates_frame <- data.frame(list(date=all_dates))
    merged_frame <- merge(all_dates_frame, df, all=T)
    merged_frame$id <- i
    merged_frame$total_loc[which(is.na(merged_frame$total_loc))] <- 0
    sumstats_filled<-rbind(sumstats_filled,merged_frame)
  }
  sumstats <- sumstats_filled
  write.csv(sumstats, "C:/Work/R/Data/Output/Summary Stats.csv",
    row.names = FALSE)
  if (pdf == TRUE){
    maxrow = 30
    npages = ceiling(nrow(sumstats)/maxrow); #multi-page pdf
    pdf("C:/Users/Blake/Desktop/Summary Stats.pdf", height=11, width=8.5)
    for (i in 1:npages){idx = seq(1+((i-1)*maxrow), i*maxrow); grid.newpage();
      grid.table(sumstats[idx, ])}
    dev.off();
  }
  return(sumstats)
}

#' Summarizes overnight distances
#'
#' Summarizes overnight distances
#'
#' @param df Dataframe
#' @param individual String, individual (from "id" column) to keep, optional.
#' @param cuts Numeric, proportional quantile cuts. Default is 0.05.
#'
#' @return Dataframe of quantiles
#' @export
#'
#' @details Require RemoveNonsequentialDays() function
SummarizeOvernightDistances <- function(df = df,
                                        individual = NULL,
                                        cuts = (0.05)) {
  df <- RemoveNonsequentialDays(df = df)
  if (individual == "" || is.null(individual)){
    df = df[which(df$last == "Last"),]
  } else {
    df = df[which(df$last == "Last" && df$id == individual),]
  }
  quantiles <- quantile(df$step_length, seq(0, 1, cuts))
  return(quantiles)
}
