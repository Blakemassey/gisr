#' Adds elevation data to locations dataframe
#'
#' Adds the cell values from an elevation rasters to an input locations and
#'   calculates the 'agl' (above ground level) of the locations.
#'
#' @usage AddElevationData(df, long, lat)
#' @param df dataframe of location data
#' @param long column name of longitude data, default = lat_utm
#' @param lat column name of latitude data, default = long_utm
#'
#' @return The original locations df with additional landscape data columns.
#' @details The locations dataset and elevation data must have the same CRS.
#' @export
#'
AddElevationData <- function(df,
                             long = "long_utm",
                             lat = "lat_utm"){
  df <- df
  elev_30mc <-raster::raster("C:/ArcGIS/Data/R_Input/BAEA/elev_30mc.tif")
  locs_xy <- cbind(df[,long], df[,lat])
  df[, "elev"] <- raster::extract(elev_30mc, locs_xy)
  df$agl <- df$alt - df$elev
  return(df)
}

#' Adds landscape data to dataframe
#'
#' Adds the cell values from a list of landscape rasters to an input locations
#'   dataset, usually a subset of BAEA.
#'
#' @usage AddLandscapeValues(df, raster_stack, long, lat, clean_up)
#' @param df dataframe of location data
#' @param raster_stack a RasterStack of landscape data
#' @param long column name of longitude data, default = lat_utm
#' @param lat column name of latitude data, default = long_utm
#' @param clean_up logical, will run a series of steps to clean up the returned
#'   output (e.g. removing "_30mc" from column names and deleting unneeded
#'   landcover "definition" column).
#'
#' @return The original locations df with additional landscape data columns.
#' @details The locations dataset and rasters should have the same CRS,
#'   otherwise function stops and returns an error message.
#' @export
AddLandscapeValues <- function(df,
                               raster_stack,
                               long = "long_utm",
                               lat = "lat_utm",
                               clean_up = TRUE){
  df <- df
  raster_stack <- raster_stack
  locs_xy <- cbind(df[,long], df[,lat])
  raster_crs <- sapply(raster_stack@layers, crs)
  compare <- function(v) all(sapply(v[-1], FUN=function(z)compareCRS(z,v[[1]])))
  if (compare(raster_crs) == FALSE) {
     stop("Input rasters have different coordinate reference systems.")
  }
  col_names_list <- {}
  for (i in 1:raster::nlayers(raster_stack)){
    raster_layer <- raster_stack[[i]]
    col_name <- raster_layer@data@names
    if (clean_up == TRUE){
      col_name <- gsub("_30mc", "", col_name)
    }
    df[,col_name] <- raster::extract(raster_layer, locs_xy)
    col_names_list <- append(col_names_list, col_name)
  }
  if (clean_up == TRUE){
    if ("lc" %in% colnames(df)) {
      nlcd_classes <- read.csv(file.path("C:/ArcGIS/Data/Landcover",
        "NCLD_Landcover_Class_Definitions.csv"), header=TRUE, as.is=TRUE,
        na.strings = "")
#  if(is.integer(nlcd_classes$lc)) nlcd_classes$lc <-as.numeric(nlcd_classes$lc)
      df <- plyr::join(x=df, y=nlcd_classes, by="lc")
      df$definition <- NULL
    }
  }
  cat("The following columns were added to the dataframe:", sep="\n")
  names_sorted <- rev(sort(col_names_list, decreasing=TRUE))
  results <- sapply(names_sorted, function(i) paste(" ", i))
  cat(results, sep="\n")
  return(df)
}

#' Add UTM coordinates to a dataframe
#'
#' Adds UTM_lat and UTM_long values to a dataframe with "long", "lat"
#'   coordinates
#'
#' @usage AddUTMCoordinates(df, long, lat, crs)
#' @param df dataframe of location data
#' @param long column name of longitude data, default = "lat"
#' @param lat column name of latitude data, default = "long"
#' @param crs Projected Coordinate System, default is
#'   "+proj=utm +zone=19 ellps=WGS84"
#'
#' @return The original locations df with "long_utm" and "lat_utm" values.
#' @export
#'
AddUTMCoordinates <- function(df,
                              long = "long",
                              lat = "lat",
                              crs = "+proj=utm +zone=19 ellps=WGS84"){
  xy <- cbind(df[, long], df[, lat]) # 2 col for next step
  xy <- project(xy, crs)  # projects to UTM Zone 19
  colnames(xy) <- c("long_utm", "lat_utm")  # name columns
  xy <- round(xy)  # rounds lat and long to zero decimal places
  df <- cbind(df, xy)  # combines lat long with data
  df$long_utm <- as.integer(df$long_utm)
  df$lat_utm <- as.integer(df$lat_utm)
  return(df)
}

#' Calculate terrain metrics on elevation RasterLayer
#'
#' @param elev RasterLayer, elevation data raster
#' @param size numeric, dimensions of analysis window (nrow and ncol). Must be
#'   odd number. When size == 0, it is internally converted to size == 1 which
#'   is necessary for the metric to be calculated (and not just revert to the
#'   'elev' layer. This was done to allow an optimization procedure to include
#'   a "sigma = 0", even though that would be identical to size == 1.
#' @param metric character, vector containing one of these options: tpi, tri,
#'   roughness (see Details)
#'
#' @details metrics based on examples given in terrain() in 'raster' package
#' @return RasterLayer
#' @export
#'
CalculateTerrainMetric <- function(elev,
                                   size,
                                   metric){
  if(size == 0) size <- 1
  x <- elev
  weight_matrix <- matrix(1, nrow=size, ncol=size)
  center <- ceiling(0.5 * length(weight_matrix))
  window <- length(weight_matrix)-1
  if (metric == "tri"){
    tri <- focal(x, w=weight_matrix,
      fun=function(x, ...) sum(abs(x[-center]-x[center]))/window,
      pad=TRUE, padValue=NA)
      out_matrix <- tri
  } else if (metric == "tpi"){
    tpi <- focal(x, w = weight_matrix,
      fun=function(x, ...) x[center] - mean(x[-center]),
      pad=TRUE, padValue=NA)
    out_matrix <- tpi
  } else if (metric == "roughness"){
    rough <- focal(x, w = weight_matrix,
      fun = function(x, ...) max(x) - min(x),
      pad = TRUE, padValue =NA, na.rm=TRUE)
    out_matrix <- rough
  } else {
    stop("'metric' must equal 'tpi', 'tri', or 'roughness'", call. = FALSE)
  }
  return(out_matrix)
}

#' Centers x,y data based on a 'base' raster
#'
#' Centers x and y values of a dataframe into the center of raster cells based
#'   on the base layer
#'
#' @usage CenterXYWithBase(df, base)
#' @param df dataframe with x and y columns
#' @param base raster base layer
#'
#' @return Dataframe with centered x and y (i.e., (x,y))
#' @export
#'
CenterXYWithBase <- function(df,
                             base) {
  df <- df
  base <- base
  for (i in 1:nrow(df)){
    df$x[i] <- CenterXYInCell(df$x[i], df$y[i], xmin(base), ymin(base),
      res(base)[1])[1]
    df$y[i] <- CenterXYInCell(df$x[i], df$y[i], xmin(base), ymin(base),
      res(base)[1])[2]
  }
  return(df)
}

#' Converts raster for use in ggplot
#'
#' A function for converting a Raster file into a df for use in a ggplot
#'
#' @usage ConvertRasterForGGPlot(raster, sample_size, na.rm)
#' @param raster Raster file
#' @param sample_size integer, raster sample size. Default is 10000.
#' @param na.rm logical, remove NA values. Default is TRUE.
#'
#' @return dataframe
#' @export
#'
#' @details most of script is from rasterVis package
ConvertRasterForGGPlot <- function(raster,
                                   sample_size = 10000,
                                   na.rm = TRUE){
  `%>%` <- magrittr::`%>%`
  samp <- sampleRegular(raster, sample_size, asRaster = TRUE)
  coords <- xyFromCell(samp, seq_len(ncell(samp)))
  df <- stack(as.data.frame(getValues(samp)))
  names(df) <- c("value", "variable")
  df <- cbind(coords, df)
  if(na.rm) df <- df %>% filter(!is.na(value))
  return(df)
}

#' Converts points to Voronoi map
#'
#' Converts a SpatialPointsDataFrame to a Voronoi map as dataframe or a
#' SpatialPolygonDataFrame
#'
#' @param spdf a SpatialPointsDataFrame
#' @param return_df logical, whether to return a dataframe. Default is TRUE.
#'
#' @return dataframe or SpatialPolygonDataFrame
#' @export
#'
#' @details Modified from R-Bloggers - "Making Static/Interactive Voronoi Map"
#'
ConvertToVoronoi <- function(spdf,
                             return_df=TRUE) {
  suppressPackageStartupMessages(library(broom))
  suppressPackageStartupMessages(library(deldir))
  vor_desc <- tile.list(deldir(spdf@coords[,1], spdf@coords[,2],
    suppressMsge=TRUE))
  vor_polygons <- lapply(1:(length(vor_desc)), function(i) {
    tmp <- cbind(vor_desc[[i]]$x, vor_desc[[i]]$y)
    tmp <- rbind(tmp, tmp[1,])
    Polygons(list(Polygon(tmp)), ID=i)
    })
  spdf_data <- spdf@data
  rownames(spdf_data) <- sapply(slot(SpatialPolygons(vor_polygons), 'polygons'),
    slot, 'ID')
  spdf <- SpatialPolygonsDataFrame(SpatialPolygons(vor_polygons),data=spdf_data)
  if (return_df){
    df <- suppressMessages(tidy(spdf))
    return(df)
  } else {
    return(spdf)
  }
}

#' Create ArcGIS maps of Bald Eagle data
#'
#' A wrapper function for creating an ArcGIS map from location data
#'
#' @usage CreateArcGISMaps(df)
#' @param df dataframe with locations
#'
#' @return overwrites files in "C:/Work/Python" folders
#' @export
#'
#' @details internal parameters of Python script specific to my BAEA GPS data.
#'   Identical to CreateArcGISandPDFMaps(), except it does not create PDFs.
#'
CreateArcGISMaps <- function(df = df){
  write.csv(df, file="C:/Work/Python/Data/CSV/BAEA.csv", row.names=FALSE)
  system('python C:/Work/Python/Scripts/BAEA/Create_ArcGIS_Maps.py')
}

#' Create ArcGIS and PDF maps of Bald Eagle data
#'
#' A wrapper function for creating an ArcGIS map and a series of PDFs
#'
#' @usage CreateArcGISandPDFMaps(df)
#' @param df dataframe with locations
#'
#' @return overwrites files in "C:/Work/Python" folders
#' @export
#'
#' @details  internal parameters of Python script specific to my BAEA GPS data.
#'   Identical to CreateArcGISMaps(), except it also creates PDFs.
#'
CreateArcGISandPDFMaps <- function(df = df){
  write.csv(df, file="C:/Work/Python/Data/CSV/BAEA.csv", row.names=FALSE)
  system('python C:/Work/Python/Scripts/BAEA/Create_ArcGISandPDF_Maps.py')
}

#' Creates a categorical legend for a map
#'
#' Creates a legend for categorical data
#'
#' @usage Usage: CreateCategoricalLegend (metadata, metadata_layer, color_alpha,
#'   pos_x, pos_y, main, main_cex, main_col, lab_col, signif_digits, left,
#'   plot_new)
#' @param metadata location of metadata .csv file. Metadata file must contain
#'   "id" and "icon_color" columns.
#' @param metadata_layer not required, column name in metadata used for legend
#'   labels
#' @param color_alpha two-digit rbga alpha value, "00" to "ff".
#' @param pos_x relative position of left and right edge of color bar on first
#'   axis, [0,1]. Default is c(0.5, 0.55).
#' @param pos_y relative position on lower and upper edge of colar bar on second
#'   axis, [0,1]. Default is c(0.05, 0.9).
#' @param main main title, written above the color bar. Default is NA.
#' @param main_cex relative size of main title. Default is 1.
#' @param main_col color of main title, default is "black".
#' @param lab_col color labels, default is "black".
#' @param left logical indicating whether to put the labels on the left (TRUE)
#'   or on the right (FALSE). Default is FALSE.
#' @param plot_new logical indicating whether to create a new plot, default is
#'   TRUE.
#'
#' @return Plot of legend for categorical data
#' @export
#'
#' @details  Used in ExportKMLPolygon()
#'
CreateCategoricalLegend <-function(metadata,
                                   metadata_layer = NULL,
                                   color_alpha = 99,
                                   pos_x = c(0.25, 0.3),
                                   pos_y = c(0.25, 0.75),
                                   main = NULL,
                                   main_cex = 1,
                                   main_col = "black",
                                   lab_col = "black",
                                   left = FALSE,
                                   plot_new = TRUE){
  if (plot_new == TRUE){
  plot.new()
  }
  suppressMessages(par(new = TRUE))  # gave warning about calling par w/o a plot
  omar <- nmar <- par("mar")
  nmar[c(2, 4)] <- 0
  par(mar = nmar)
  emptyplot()
  pars <- par("usr")
  par(plt = c(0.01, .99, 0.01, 0.99)) # altered to extend graph plotting area
  dim_x <- pars[2] - pars[1]
  x_min <- pars[1] + pos_x[1] * dim_x
  x_max <- pars[1] + pos_x[2] * dim_x
  dim_y <- pars[4] - pars[3]
  y_min <- pars[3] + pos_y[1] * dim_y
  y_max <- pars[3] + pos_y[2] * dim_y
  metadata<-read.csv(metadata, header=TRUE, as.is=TRUE, na.strings = "")
  if(!is.null(metadata_layer)){
    metadata$id<-metadata[,metadata_layer]
  }
  colors_kml <- metadata$icon_color
  colors <- colors_kml
  legend_interval_seq <- metadata$id  # this can be character or number
  colors <- sub("#ff", color_alpha, colors_kml, ignore.case = FALSE)
  color_seq <- 1:length(legend_interval_seq)
  z_min <- color_seq[1]
  z_max <- color_seq[length(color_seq)]
  y_seq <- seq(y_min, y_max, length.out = length(colors_kml) +1)
  rect(x_min, y_seq[-(length(colors_kml)+1)], x_max, y_seq[-1], col =colors_kml,
    border = "black") # this draws the vertical color ramp
  dim_x <- (x_max - x_min)
  dim_y <- (y_max - y_min)
  if (left) {
    x_width <- -dim_x
    pos <- 2
    x_pos <- x_min + x_width * 0.1
  } else {
    x_width <- +dim_x
    pos <- 4
    x_pos <- x_max + x_width * 0.1
  }
  midpoints <- function(x){
    midpoints <-{}
    for (i in 1:length(x[-1])){
      lower <- x[i]
      upper <- x[i+1]
      midpoints <- append(midpoints,((lower+upper)/2))
    }
    return(midpoints)
  }
  y_pos <- midpoints(y_seq)
  text(x_pos,  y_pos, legend_interval_seq, pos = 4,
     col = lab_col)
  if (!is.null(main)) {
    for (i in length(main):1) {
      text(x = mean(c(x_min, x_max)), y = y_max + 0.075 * (length(main) - i+1),
      labels = main[i], adj = c(0, 0.5), cex = main_cex, col = main_col)
    }
  }
  par(new = FALSE)
  par(mar = omar)
}

#' Create a Matrix with a specific value in the center cell
#'
#' @param nrow integer, number of rows
#' @param center_value numeric, number in center cell
#' @return a matrix
#' @export
CreateCenterValueMatrix <- function(nrow,
                                    center_value){
  center_cell <- ceiling(nrow/2)
  dimnames <- list(X=seq_len(nrow), Y=seq_len(nrow))
  mat <- t(matrix(0, nrow=nrow, ncol=nrow, dimnames = dimnames))
  mat[center_cell, center_cell] <- center_value
  return(mat)
}

#' Creates a color interval sequence for color ramps
#'
#' Creates an interval sequence used for assigning color ramp values to objects.
#'
#' @usage CreateColorIntervalSequence(color_range, color_alpha, color_increment,
#'   log)
#'
#' @param color_range two-valued vector, the minimum and maximum values.
#' @param color_levels one value, number of color levels. Ignored if
#'   color_interval not equal to NA.
#' @param color_increment one value, increment value of color breakpoints, one
#'   value.
#' @param log logical of whether to log transform.
#'
#' @return vector of colors
#' @export
#'
#' @details Parameter defaults are object names because this function is used
#'   internally in CreateColorPaletteLegend() & ExportKMLPolygon() to ensure
#'   the interval sequence selection is identical. The code uses pretty(), so
#'   the returned color levels and legend levels may not be identical to the
#'   input parameter value.
#'
CreateColorIntervalSequence<- function(color_range = color_range,
                                       color_levels = color_levels,
                                       color_increment = color_increment,
                                       log = log) {
  if (log == TRUE && (min(color_range) <= 0) == TRUE ){
    stop ("Unable to log-transform data: min(color_range) <= 0")
  }
  if(!is.null(color_increment)){
    if (log){
      pretty_color_range <- 10^pretty(log10(color_range), n = (color_levels +1))
      color_interval_seq <- seq(log10(min(pretty_color_range)),
        log10(max(pretty_color_range)), by = log10(color_increment))
    }
    if (!log){
      pretty_color_range <- pretty(color_range, n = (color_levels +1))
      color_interval_seq <- seq(min(color_range), max(color_range),
        by = color_increment)
    }
  } else {
    if (log){
      color_interval_seq <- pretty(log10(color_range), n = (color_levels + 1))
    }
    if (!log){
      pretty_color_range <- pretty(color_range, n = (color_levels + 1))
      color_interval_seq <- seq(min(pretty_color_range),max(pretty_color_range),
        length.out = color_levels + 1)
    }
  }
  return(color_interval_seq)
}

#' Creates a color palette legend for maps
#'
#' Creates a display of selected color palettes
#'
#' @usage CreateColorPaletteLegend (color_pal, color_range, color_alpha,
#'   color_levels, color_increment, legend_levels, log, pos_x, pos_y, main,
#'   main_cex, main_col, lab_col, signif_digits, left, plot_new)
#'
#' @param color_pal color palette to be used, also allowed are two extremes or
#'   one value. Default is: c("yellow","red").
#' @param color_range two-valued vector, the minimum and maximum values.
#' @param color_alpha two-digit rbga alpha value, "00" to "ff", default is "ff".
#'   Note: ExportKMLPOlygon() uses default "cc"
#' @param color_levels one value, number of color levels. Ignored if
#'   color_interval not equal to NA. Default is 5.
#' @param color_increment one value, increment value of color breakpoints, one
#'   value. Default is NA.
#' @param legend_levels one value, increment in legend values, ignored if
#'   legend_values not equal to NA. Default is 5.
#' @param legend_values vector of specific legend labels. Default is NA.
#' @param log logical of whether to log transform. Default is FALSE.
#' @param pos_x relative position of left and right edge of color bar on first
#'   axis, [0,1]. Default is c(0.5, 0.55).
#' @param pos_y relative position on lower and upper edge of colar bar on second
#'   axis, [0,1]. Default is c(0.05, 0.9).
#' @param main main title, written above the color bar. Default is NA.
#' @param main_cex relative size of main title. Default is 1.
#' @param main_col color of main title. Default is "black".
#' @param lab_col color labels, default is "black".
#' @param signif_digits integer, number of signifcant digits
#' @param left logical indicating whether to put the labels on the left (TRUE)
#'   or on the right (FALSE). Default is FALSE.
#' @param plot_new logical indicating whether to create a new plot. Default is
#'   TRUE.
#'
#' @return Plot of legend with color palette
#' @export
#'
#' @details Used in ExportKMLPolygon, make sure code used for color range,
#'   selction, log-tranformation, and color palette creation is identical or
#'   produces same results.  Color levels and legend levels use the pretty(), so
#'   the actual number of levels may not be identical to parameter values.
CreateColorPaletteLegend <- function (color_pal = c("yellow","red"),
                                      color_range,
                                      color_alpha = "ff",
                                      color_levels = 10,
                                      color_increment = NULL,
                                      legend_levels = 10,
                                      legend_values = NULL,
                                      log = FALSE,
                                      pos_x = c(0.3, 0.35),
                                      pos_y = c(0.0, 1),
                                      main = NULL,
                                      main_cex = 1,
                                      main_col = "black",
                                      lab_col = "black",
                                      signif_digits = 2,
                                      left = FALSE,
                                      plot_new = TRUE){
  if (plot_new == TRUE){
  plot.new()
  }
  suppressMessages(par(new = TRUE))  # gave warning about calling par w/o a plot
  omar <- nmar <- par("mar")
  nmar[c(2, 4)] <- 0
  par(mar = nmar)
  emptyplot()
  pars <- par("usr")
  par(plt = c(0.01, .99, 0.01, 0.99)) # altered to extend graph plotting area
  dim_x <- pars[2] - pars[1]
  x_min <- pars[1] + pos_x[1] * dim_x
  x_max <- pars[1] + pos_x[2] * dim_x
  dim_y <- pars[4] - pars[3]
  y_min <- pars[3] + pos_y[1] * dim_y
  y_max <- pars[3] + pos_y[2] * dim_y
  color_interval_seq <- CreateColorIntervalSequence(color_range=color_range,
    color_levels=color_levels, color_increment=color_increment, log=log)
  colors_rbg <- colorRampPalette(color_pal)(length(color_interval_seq)-1)
  colors_rbg <- paste(colors_rbg, color_alpha, sep= "")
  if (all(!is.null(legend_values))) {
    legend_interval_seq <- legend_values
  } else { # if only legend_levels is set
    if (log){
      legend_interval_seq <- pretty(log10(color_range), n = (legend_levels + 1))
    }
    if (!log){
      pretty_legend_range <- pretty(color_range, n = (legend_levels + 1))
      legend_interval_seq <- seq(min(pretty_legend_range),
        max(pretty_legend_range), length.out = (legend_levels + 1))
    }
  }
  z_min <- min(color_interval_seq)
  z_max <- max(color_interval_seq)
  y_seq <- seq(y_min, y_max, length.out = length(colors_rbg)+1 )
  rect(x_min, y_seq[-(length(colors_rbg)+1)], x_max, y_seq[-1], col=colors_rbg,
    border=NA)
  rect(x_min, y_min, x_max, y_max, border = lab_col)
    dim_x <- (x_max - x_min)
    dim_y <- (y_max - y_min)
    if (left) {
      x_width <- -dim_x
      pos <- 2
      x_pos <- x_min + x_width * 0.5
    } else {
      x_width <- +dim_x
      pos <- 4
      x_pos <- x_max + x_width * 0.5
    }
    y_pos <- y_min + (legend_interval_seq - z_min)/(z_max - z_min) * dim_y
    segments(x_min, y_pos, x_max, y_pos, col = lab_col)
    segments(x_pos + x_width * 0.2, y_pos, x_min, y_pos, col = lab_col)
  if(log==TRUE) {
    legend_interval_labels <- sapply(legend_interval_seq, function(i)
      as.expression(bquote(10^ .(i))))
    } else {
      legend_interval_labels <- format(legend_interval_seq, nsmall=0)
    }
  text(x_pos, y_pos, legend_interval_labels, pos = pos, col = lab_col)
  if (!is.null(main)) {
    for (i in length(main):1) {
      text(x = mean(c(x_min, x_max)), y = y_max + 0.1 * (length(main) - i + 1),
      labels = main[i], adj = c(0, 0.5), cex = main_cex, col = main_col)
    }
  }
  par(new = FALSE)
  par(mar = omar)
}

#' Creates a buffer around a spatial extent
#'
#' Create buffer around extent
#'
#' @usage CreateExtentBuffer(df, buffer)
#'
#' @param df dataframe with columns: long_utm, lat_utm
#' @param buffer minimum buffer extent, default = 500
#'
#' @return vector of extent (long_min, long_max, lat_min, lat_max)
#' @export
#'
#' @details finds lat and long extents, applies minimum buffer, and rounds to
#'   the same significance value as the buffer
CreateExtentBuffer <- function(df = df,
                               buffer = 500) {
  extent_matrix <- c(plyr::round_any(min(df$long_utm) - buffer, buffer, floor),
    plyr::round_any(max(df$long_utm) + buffer, buffer, ceiling),
    plyr::round_any(min(df$lat_utm) - buffer, buffer, floor),
    plyr::round_any(max(df$lat_utm) + buffer, buffer, ceiling))
  extent(extent_matrix)
}

#' Creates an 'sf' polygon based an 'sf' object's extent and aspect ratio
#' @usage CreateMapExtentBB(sf_object, ext, asp)
#' @param sf_object 'sf' object
#' @param ext extension factor of the extent, default = 1.15.
#' @param asp ratio of the extent (width/height), default = 1
#' @return 'sf' object
#' @details Useful for making a main-map extent box within a overview map. Not
#'     guaranteed to be identical to a tmap's actual main map extent.
#' @export
CreateMapExtentBB <- function(sf_object,
                              ext = 1.15,
                              asp = 1){
  map_asp <- asp
  sf_asp <- tmaptools::get_asp_ratio(tmaptools::bb(sf_object, ext = ext))
  if (sf_asp >= map_asp){
    bb_width = ext     # width should not be reduced
    bb_height = (sf_asp/map_asp)*ext
  } else {
    bb_width = (map_asp/sf_asp)*ext
    bb_height = ext # height should not be reducted
  }
  map_bb <- sf::st_as_sfc(tmaptools::bb(sf_object, relative = TRUE,
    width = bb_width, height = bb_height))
  sf::st_crs(map_bb) <- sf::st_crs(sf_object)
  return(map_bb)
}

#' Create a Gaussian Kernel RasterLayer
#' @usage CreateGaussKernRaster(sigma, nrow, shift_n, cell_size)
#'
#' @param sigma numeric, sigma (or sd) of distribution - scaled to cell_size
#' @param nrow numeric, number of rows/columns in RasterLayer
#' @param shift logical, whether to shift cells to center. If TRUE, center
#'    middle cell at c(0,0). Default is FALSE.
#' @param shift_n numeric, shift in lat/long origin position
#' @param cell_size numeric, cell size of RasterLayer
#'
#' @return RasterLayer
#' @export
#'
CreateGaussKernRaster <- function(sigma,
                                  nrow = NA,
                                  shift = FALSE,
                                  shift_n = 0,
                                  cell_size = 30){
  if(is.na(nrow)) nrow <- (2*floor(2*sigma)+1)
  if(isTRUE(shift)) shift_n <- -1*((nrow * 30)/2)
  ncol <- nrow
  cell_edges <- seq(0, ncol*cell_size, cell_size)
  cell_centers <- cell_edges[-length(cell_edges)] + diff(cell_edges)/2
  center_x <- floor(nrow/2)
  g_kernel <- gaussian.kernel(sigma=sigma, n=nrow)
  g_raster <- raster(nrows = nrow, ncols = ncol, xmn=0,
    xmx = nrow*cell_size, ymn=0, ymx = ncol*cell_size, resolution = cell_size)
  g_raster[] <- g_kernel
  g_raster_out <- shift(g_raster, x = shift_n, y = shift_n)
  return(g_raster_out)
}

#' Creates kernel density estimate (kde) raster based on spatial points
#' Creates a 'kde' object using a df of location points to calculate probability
#'   estimates for an input raster's cell centers
#'
#' @usage CreateKDERaster(df, df_lat, df_long, blank_raster)
#'
#' @param df dataframe of location data
#' @param df_long df col name of longitude values, default is "long_utm"
#' @param df_lat df col name of latitude values, default is "lat_utm"
#' @param buffer value used in CreateExtentBuffer() around df locations, default
#'   is 500m
#' @param blank_raster file name or object name of raster file used to create
#'   output. This raster is cropped based on the df's extent and kde estimates
#'   are based on each cell's center. Default file is:
#'   "C:/ArcGIS/Data/BlankRaster/maine_30mc.tif".
#'
#' @return A 'kde' object with a kde estimate for the center point of each blank
#'   raster cell that falls within the buffer around the df locations
#' @export
#'
#' @details Ouput RasterLayer has the same CRS as the blank_raster input
#'
CreateKDEPoints <- function(df = df,
                            df_long = "long_utm",
                            df_lat = "lat_utm",
                            buffer = 500,
                            blank_raster = file.path("C:/ArcGIS/Data",
                              "BlankRaster/maine_30mc.tif")) {
  df_extent <- CreateExtentBuffer(df=df, buffer=500)
  if (is.raster(blank_raster) == TRUE){
    blank_raster <- blank_raster
  } else {
    blank_raster <- raster(blank_raster)
  }
  blank_cropped <- crop(blank_raster, df_extent, snap="out")
  blank_points <- rasterToPoints(blank_cropped)
  df_xy <- cbind(df[,df_long], df[,df_lat])
  raster_xy <- cbind(blank_points[,1], blank_points[,2])
  hpi_df <- Hpi(x=df_xy, pilot="unconstr")
  kde_points <- kde(x=df_xy, H=hpi_df, eval.points=raster_xy, verbose=TRUE)
  return (kde_points)
}

#' Creates a probability raster layer based on a 'kde' layer
#'
#' Calculates kernel probability levels based on 'kde' object and use them to
#'   recalssify the values in an kde 'RasterLayer' to the probability levels.
#'
#' @usage ExportKMLProbs(kde_df, kde_raster, probs)
#'
#' @param kde_object 'kde' object, usually made by CreateKDEPoints()
#' @param kde_raster 'RasterLayer' created using estimates from a 'kde' object,
#'   usually made by CreateKDERaster()
#' @param probs vector [0,1] of probability values, default value is seq(0, 1,
#'   by=.1)
#'
#' @return A 'RasterLayer' with reclassified probabilities from using
#'   contourLevels() in the 'ks' package.
#' @export
#'
#' @details To be used between CreateKDERaster() and ExportKMLProbs()
#'
CreateKDEProbs <- function(kde_object = kde_object,
                           kde_raster = kde_raster,
                           probs = seq(0,1,by=.1)) {
  kde_object <- kde_object
  kde_raster <- kde_raster
  probs <- probs
  cl <- (contourLevels(kde_object, prob=probs, approx=FALSE))
  cl2 <- c(cl[-length(cl)],1)  # top contour level will include all > probs
  min <- min(unlist(kde_raster@data@values))  # min raster value, not always 0
  m <- matrix(nrow=length(probs), ncol=3)
  if (1 %in% probs){
    m[,2]<-c(cl2[-1],1)
  } else {
    m[,2]<-c(cl[-1],1)
  }
  if (0 %in% probs){
    if (1 %in% probs){
      m[,1] <- c(min, cl2[-1])
    } else {
      m[,1] <- c(min,cl[-1])
    }
  } else {
    if (1 %in% probs) {
      m[,1] <- cl2
    } else {
      m[,1] <- cl
    }
  }
  m[,3]<-c(probs)
  probs_raster <- reclassify(kde_raster, m)
  if (0 %in% probs == FALSE) {
  m2 <- c(0, cl[1], NA)  # removes all raster values below first contour level
  probs_raster <- reclassify(probs_raster, m2)
  }
  return(probs_raster)
}

#' CreateKDERaster
#'
#' Creates a 'RasterLayer' based on a 'kde' object of gridded point location
#'   probability estimates
#'
#' @usage CreateKDERaster(kde_points, cell_size, crs)
#'
#' @param kde_points dataframe of kde estimates at gridded points
#' @param cell_size cell size of output raster, default = 30
#' @param crs crs of output raster, default is UTM 19N, NAD83
#'
#' @return  A 'RasterLayer' object with kde estimates for each of the blank
#'   raster cells that fall within the buffered df locations.
#' @export
#'
#' @details The kde_points file must have "eval.points" and "estimate", as
#'   created by CreateKDEPoints(). Returned 'RasterLayer' has the same CRS as
#'   blank_raster input.
CreateKDERaster <- function(kde_points = kde_points,
                            cell_size = 30,
                            crs = paste("+proj=utm +zone=19 +datum=NAD83",
                                        "+units=m +no_defs +ellps=GRS80",
                                        "+towgs84=0,0,0")) {
  kde_points <- kde_points
  kde_xyz <- data.frame(kde_points["eval.points"], kde_points["estimate"])
  kde_raster <- rasterFromXYZ(kde_xyz, res=c(cell_size,cell_size), crs=crs,
    digits=5)
  return(kde_raster)
}

#' Create bounding box needed for downloading an 'OpenStreetMap' or 'rosm'
#'     baselayer
#' @param sf_obj a 'sf' object, for determing bounding box coordinates
#' @param type character, either "om_type" or "rosm_type", default = "om_type"
#' @return a vector (for type = "rosm_type") or list (for type = "om_type")
#' @export
CreateOSMBaseBB <- function(sf_obj,
                          type = c("om_type")){
  sf_coords <- as.numeric(rev(sf::st_bbox(sf_obj %>%
    sf::st_transform(crs = 4326))))
  rosm_bb <- prettymapr::makebbox(sf_coords[1], sf_coords[2], sf_coords[3],
    sf_coords[4])
  om_bb <- list(c(rosm_bb[2,2], rosm_bb[1,1]), c(rosm_bb[2,1], rosm_bb[1,2]))
  if(type == "rosm_type") return(rosm_bb)
  if(type == "om_type") return(om_bb)
}

#' CreateProbIsoplethRaster
#'
#' A wrapper function of used to create a RasterLayer of probabilities based on
#'   a df of location points
#'
#' @usage CreateProbIsoplethRaster(df, buffer, cell_size, probs)
#'
#' @param df dataframe of location data
#' @param buffer value used in CreateExtentBuffer() around df locations, default
#'   is 500 m
#' @param cell_size cell size of output raster, default = 30
#' @param probs vector [0,1] of probability values, default value is seq(0, 1,
#'   by=.1)
#'
#' @return A Raster object with a probability estimate for each blank raster
#'   cell that falls within the buffer around the df locations
#' @export
#'
#' @details  Ouput RasterLayer has the same CRS as the blank_raster input
CreateProbIsoplethRaster <- function(df,
                                     buffer = 500,
                                     cell_size = 30,
                                     probs = seq(0,1, by=.1)) {
  kde_points <- CreateKDEPoints(df = df, df_long = "long_utm",
    df_lat = "lat_utm", buffer = buffer, blank_raster = file.path("C:/ArcGIS",
    "Data/BlankRaster/maine_50mc.tif"))
  kde_raster <- CreateKDERaster(kde_points = kde_points, cell_size = cell_size,
    crs = paste("+proj=utm +zone=19 +datum=NAD83", "+units=m +no_defs",
    "+ellps=GRS80 +towgs84=0,0,0"))
  kde_probs <- CreateKDEProbs(kde_object = kde_points, kde_raster = kde_raster,
    probs = probs)
  return(kde_probs)
}

#' CreateRasterFromPointsAndBase
#'
#' Creates a 'RasterLayer' from an input dataframe of point locations and a
#'   base RasterLayer that sets the CRS and cell locations for the output
#'
#' @usage CreateRasterFromPointsAndBase(df, value, x, y, buffer, base)
#'
#' @param df input dataframe with columns for lat, long, and value
#' @param value column name with data for cell values
#' @param x column name for latitude coordinates
#' @param y column name for longitude coordinates
#' @param buffer distance in cell units to buffer around 'df' points for output
#'   Rasterlayer. If NULL(default), the entire extent of the base parameter is
#'   used for the output Rasterlayer.
#' @param base raster that is used to rasterize() the input data, default is set
#'   to a RasterLayer specific to my BAEA project
#'
#' @return A 'RasterLayer' object with values in all the cells that had
#'   locations.
#' @export
#'
#' @details Input latitude and longitude must match the CRS of base raster.
#'   Returned 'RasterLayer' has same CRS as base raster.
#'
CreateRasterFromPointsAndBase <- function(df,
                                          value,
                                          x,
                                          y,
                                          buffer = NULL,
                                          base = raster(file.path("C:/ArcGIS",
                                            "Data/BlankRaster/maine_30mc.tif"))
                                          ){
  df <- df
  base <- base
  xy <- data.frame(x=df[,x], y=df[,y])
  coordinates(xy) <- c("x", "y")
  proj4string(xy)  <- raster::crs(base)
  if (!is.null(buffer)) {
    xy_area <- raster::extend(extent(xy), .01)  # needs to have area
    base_crop <- raster::crop(base, xy_area, snap='out')
    base_buffer <- raster::extend(base_crop, buffer)
    base_extent <- raster::crop(base, base_buffer)
  } else {
    base_extent <- base
  }
  output <- raster::rasterize(xy, base_extent, field=df[,value],
    updateValue='all', fun='last', background = NA)
  return(output)
}

#' CreateRasterNestConDist
#'
#' Creates a list of RasterLayers of distances to study nests (nests to create
#'   rasters for) and all nests (all conspecific nests in the area)
#'
#' @usage CreateRasterNestConDist(years, buffer, study_nests, all_nests, base)
#' @param years vector of years to calculate distances
#' @param buffer distance in cell units to buffer around nests for calculating
#'   the distance to conspecifics. Default is 1000.
#' @param study_nests nests with "active_(year)", "eagle_id", "lat_utm", and
#'   "long_utm" columns
#' @param all_nests nests with "active_(year)", "lat_utm", and "long_utm"
#' @param base raster that is used to rasterize() the input data, default is set
#'   to a RasterLayer specific to my BAEA project
#'
#' @return  A list of Rasters with nest and conspecific distances for all cells
#'   within buffer distance of the study nests. List is structured as:
#'   list[[year]][[nest_id]]
#' @export
#'
#' @details Some of the functions are hardwired with specific column names
#'
CreateRasterNestConDist <- function(years,
                                    buffer = 1000,
                                    study_nests,
                                    all_nests,
                                    base = raster(file.path("C:/ArcGIS/Data",
                                      "BlankRaster/maine_30mc.tif"))) {
  years <- years
  nest_con_dist <- as.list(setNames(rep(NA,length(years)), as.numeric(years)),
    years)
  for (i in 1:length(years)){
    year <- years[i]
    active_year <- paste0("active_", year)
    study_nests_yr <- study_nests[which(study_nests[,active_year] == TRUE), ]
    all_nests_yr <- all_nests[which(all_nests[,active_year] == TRUE), ]
    study_nests_yr <- ConvertNestIdToNum(study_nests_yr)
    all_nests_yr <- ConvertNestIdToNum(all_nests_yr)
    study_nests_yr_30mc <- CreateRasterFromPointsAndBase(study_nests_yr,
      value="nest_id_num", x="long_utm", y="lat_utm", base)
    all_nests_yr_30mc <- CreateRasterFromPointsAndBase (all_nests_yr,
      value="nest_id_num", x="long_utm", y="lat_utm", base)
    coordinates(study_nests_yr) <- c("long_utm", "lat_utm")
    proj4string(study_nests_yr)  <- CRS("+proj=utm +zone=19 +datum=WGS84")
    coordinates(all_nests_yr) <- c("long_utm", "lat_utm")
    proj4string(all_nests_yr)  <- CRS("+proj=utm +zone=19 +datum=WGS84")
    study_nests_ids <- unique(study_nests_yr$nest_id)
    study_nests_list <- as.list(setNames(rep(NA,length(study_nests_ids)),
      study_nests_ids), study_nests_ids)
    for (j in 1:nrow(study_nests_yr)){
      nest <- study_nests_yr[j,]
      nest_id <- study_nests_yr@data[j, "nest_id"]
      nest_cell_extent <- extend(extent(nest), .01)
      nest_cell <- crop(study_nests_yr_30mc, nest_cell_extent, snap='out')
      nest_area <- extend(nest_cell, buffer)
      nest_dist <- distance(nest_area)
      nest_dist@file@name <- "nest_dist"
      con_area <- crop(all_nests_yr_30mc, nest_area, snap='out')
      con_area_mask <- mask(con_area, nest_area, inverse=TRUE)
      con_dist <- distance(con_area_mask)
      con_dist@file@name <- "con_dist"
      nest_stack <- stack(nest_dist,con_dist)
      names(nest_stack) <- c("nest_dist", "con_dist")
      nest_stack@filename <- nest_id
      study_nests_list[[j]] <- nest_stack
    }
    nest_con_dist[[i]] <- study_nests_list
  }
  return(nest_con_dist)
}

#' CreateSpatialLines
#'
#' A wrapper function for creating a 'SpatialLines' object
#'
#' @usage CreateSpatialLines(df, long, lat, crs)
#'
#' @param df dataframe with locations
#' @param long df column name of longitude, default is "long_utm"
#' @param lat df column name of latitude, default is "long_utm"
#' @param crs coordinate reference system, default is NA.
#'
#' @return a 'SpatialLines'object
#' @export
#'
CreateSpatialLines <- function (df = df,
                                long = "long_utm",
                                lat = "lat_utm",
                                crs = as.character(NA)){
  df <- df
  sp::coordinates(df) <- c(long, lat)
  coords <- coordinates(df)
  spatial_lines <- sp::SpatialLines(list(Lines(list(Line(coords)), "1")),
    proj4string = sp::CRS(crs))
  return(spatial_lines)
}

#' CreateSpatialPolygons
#'
#' A wrapper function for creating a 'SpatialPolygons' object
#'
#' @usage CreateSpatialPolygons(df, long, lat, crs)
#'
#' @param df dataframe with locations
#' @param long df column name of longitude, default is "long_utm"
#' @param lat df column name of latitude, default is "long_utm"
#' @param crs coordinate reference system, default is NA.
#'
#' @return a 'SpatialPolygons'object
#' @export
#'
CreateSpatialPolygons <- function (df = df,
                                   long = "long_utm",
                                   lat = "lat_utm",
                                   crs = as.character(NA)){
  df <- df
  coordinates(df) <- c(long, lat)
  coords <- sp::coordinates(df)
  spatial_polygon <- sp::SpatialPolygons(list(Polygons(list(Polygon(coords)),
    "1")), proj4string = sp::CRS(crs))
  return(spatial_polygon)
}

#' Deg2Rad
#'
#' Converts degrees to radians
#'
#' @usage Deg2Rad(degree)
#'
#' @param degress numeric, angle in degrees
#'
#' @return Angle in radians
#' @export
Deg2Rad <- function(degree) {
  radian <- (degree * pi) / (180)
  return(radian)
}

#' ExportRasterNestConDist
#'
#' Creates a list of RasterLayers of distances to study nests (nests to create
#'   rasters for) and all nests (all conspecific nests in the area)
#'
#' @usage  ExportRasterNestConDist(nest_con_dist, dir)
#'
#' @param nest_con_dist list of RasterLayers of distances with the structure of
#'   raster[[year]][[nest_id]]
#' @param dir directory for the output raster files
#'
#' @return Exports rasters to directory location
#' @export
#'
#' @details Some of the functions are hardwired with specific column names
#'
ExportRasterNestConDist <- function(nest_con_dist = nest_con_dist,
                                    dir = file.path("C:/ArcGIS/Data/BAEA/Nests",
                                      "Distance_Rasters/")){
  nest_con_dist <- nest_con_dist
  for (i in 1:length(nest_con_dist)){
    year <- nest_con_dist[[i]]
    names(nest_con_dist[1])
    for (j in 1:length(year)) {
      nest <- year[[j]]
      nest_id <- nest@filename
      writeRaster(nest_con_dist[[i]][[j]][[1]], filename=file.path(dir,
        paste0(nest_id, "_nest_dist_" , names(nest_con_dist[i]), ".tif")),
        format="GTiff", overwrite=TRUE)
      writeRaster(nest_con_dist[[i]][[j]][[2]],
        filename=file.path(dir, paste0(nest_id,"_con_dist_",
        names(nest_con_dist[i]),".tif")), format="GTiff", overwrite=TRUE)
    }
  }
}

#' ExportShapefileFromPoints
#'
#' Exports shapefile of a dataframe's location data
#'
#' @usage ExportShapefileFromPoints(df, lat, long, name, folder, crs, overwrite)
#'
#' @param df dataframe with locations
#' @param lat  latitude column name, in same coordinates as 'crs'. Default
#'   "lat"
#' @param long longitude column name, in same coordinates as 'crs'. Default
#'   "long"
#' @param name name of shapefile output
#' @param folder location for shapefile files
#' @param crs coordinate reference system, default is
#'   "+proj=longlat +datum=WGS84"
#' @param overwrite logical, overwrite outfile. Use with caution, default FALSE.
#'
#' @return creates shapefile
#' @export
#'
#' @details requires "lat" and "long" columns
ExportShapefileFromPoints <- function(df = df,
                                      lat = "lat",
                                      long = "long",
                                      name = "baea",
                                      folder = "Data/Output",
                                      crs = "+proj=longlat +datum=WGS84",
                                      overwrite = FALSE){
  xy <- (cbind(df[,long], df[,lat]))
  classes <- as.character(sapply(df, class))
  colClasses <- which(classes == "c(\"POSIXct\", \"POSIXt\")")
  df[, colClasses] <- sapply(df[, colClasses], as.character)
  df_sp <- SpatialPointsDataFrame(xy, df, coords.nrs = numeric(0),
    proj4string = CRS(crs), match.ID = TRUE,
    bbox = NULL)  # fails if there are spaces in CRS
  writeLines(noquote(paste("Writing: ", folder, "/", name, ".shp", sep="")))
  rgdal::writeOGR(df_sp, folder, layer=name, driver = "ESRI Shapefile",
    overwrite_layer = overwrite)
}

#' Extract values in center row of matrix and convert to a dataframe
#'
#' @param matrix_in matrix
#'
#' @return matrix
#' @export
#'
ExtractMatrixCenterRow <- function(matrix_in){
  nrow <- nrow(matrix_in)
  center_row <- ceiling(nrow/2)
  df_out <- data.frame(x = seq_len(nrow), y = matrix_in[, center_row])
  return(df_out)
}

#' Extract values in center row of RasterLayer and convert to a dataframe
#'
#' @param raster_in matrix
#'
#' @return matrix
#' @export
#'
ExtractRasterCenterRow <- function(raster_in){
  nrow <- nrow(raster_in)
  center_row <- ceiling(nrow/2)
  df_out <- data.frame(x = xFromCol(raster_in, col=1:ncol(raster_in)),
    y = getValues(raster_in, row = center_row))
  return(df_out)
}


#' Get Intersection Area
#' Gets the overlapping area of two sf polygon objects
#'
#' @param poly_a sf polygon object
#' @param poly_b sf polygon object
#'
#' @return
#' @export
#' @details When two polygons have no overlap, the result is 0 in the units of
#'     the respective polygons
#'
GetIntersectionArea <- function(poly_a, poly_b){
  x <- st_area(st_intersection(poly_a, poly_b))
  y <- 0
  units(y) <- as_units(units(x))
  if(length(x) == 1) result <- x
  if(length(x) == 0) result <- y
  return(result)
}

#' ImportLandscapeRasterStack
#'
#' Imports raster layers and combines them into a RasterStack
#'
#' @usage ImportLandscapeRasterStack()
#'
#' @return The original locations df with additional landscape data columns.
#' @export
#' @importFrom raster raster
#' @importFrom raster stack
#'
#' @details This function is specific to my directory and datafiles
#'
ImportLandscapeRasterStack <- function(){
  hydro_30mc <- raster("C:/ArcGIS/Data/Hydrology/NHD_rasters/hydro_30mc.tif")
  hydro_dir_30mc <- raster(file.path("C:/ArcGIS/Data/Hydrology/NHD_rasters",
    "hydro_dir_30mc.tif"))
  hydro_dist_30mc <- raster(file.path("C:/ArcGIS/Data/Hydrology/NHD_rasters",
    "hydro_dist_30mc.tif"))
  lc_30mc <- raster("C:/ArcGIS/Data/Landcover/Landcover/lc_30mc.tif")
  maine_30mc <- raster("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif")
  turbine_30mc <- raster("C:/ArcGIS/Data/Wind/turbine_dist_30mc.tif")
  raster_stack <- stack(elev_30mc, hydro_30mc, hydro_dir_30mc, hydro_dist_30mc,
    lc_30mc, maine_30mc, turbine_30mc)
  cat("The following layers were used to create the RasterStack:",sep="\n")
  names <- rev(sort(names(raster_stack), decreasing=TRUE))
  layers <- sapply(names, function(i) paste(" ", i))
  cat(layers, sep="\n")
  return(raster_stack)
}

#' Keep values only in center row of matrix (other cells converted to zero)
#'
#' @param matrix_in matrix
#'
#' @return matrix
#' @export
#'
KeepMatrixCenterRow <- function(matrix_in){
  matrix_slice <- matrix_in
  matrix_slice[] <- 0
  nrow <- nrow(matrix_in)
  center_x <- ceiling(nrow/2)
  matrix_slice[,center_x] <- matrix_in[,center_x]
  return(matrix_slice)
}

#' Keep values only in center row of Raster (convert other cells to zero)
#'
#' @param raster_in Raster
#'
#' @return Raster
#' @export
#'
KeepRasterCenterRow <- function(raster_in){
  raster_slice <- raster_in
  raster_slice[] <- 0
  nrow <- nrow(as.matrix(raster_in))
  center_row <- ceiling(nrow/2) # evens and odds # evens and odds
  raster_slice[center_row,] <- getValues(raster_in, row = center_row)
  return(raster_slice)
}

#' PackCircles
#'
#' Simple circle packing algorithm based on inverse size weighted repulsion
#'
#' @usage PackCircles(config, extent, edge_buffered, max_iter, overlap)
#'
#' @param config matrix with two cols: radius, n
#' @param extent extent of bounding rectangle: c(x_min, x_max, y_min,y_max).
#'   Default is (0, 10000, 0, 10000).
#' @param edge_buffer distance from edge of extent that circle center's must be
#'   placed
#' @param max_iter maximum number of iterations to try
#' @param overlap allowable overlap expressed as proportion of joint radii
#' @param plot logical, whether or not to draw a plot
#'
#' @return A dataframe of points
#' @export
#'
#' @details Original code from: http://www.r-bloggers.com/circle-packing-with-r
#'   Slight modifications were made for code consistency and additional
#'   functionality.
PackCircles <- function(config,
                        extent = c(0, 10000, 0, 10000),
                        edge_buffer = 1000,
                        max_iter = 1000,
                        overlap = 0,
                        plot = TRUE) {
  tol <- 0.0001 # round-off tolerance
  if (overlap < 0 | overlap >= 1) { # convert overlap to proportion of radius
    stop("overlap should be in the range [0, 1)")
  }
  extent_buffered <- c(x_min+edge_buffer, x_max-edge_buffer, y_min+edge_buffer,
    y_max-edge_buffer)
  size <- c((extent_buffered[2] - extent_buffered[1]), (extent_buffered[4] -
    extent_buffered[3]))
  p_radius <- 1 - overlap
  n_circles <- sum(config[,2])
  Repel <- function(xyr, c0, c1) {
    dx <- xyr[c1, 1] - xyr[c0, 1]
    dy <- xyr[c1, 2] - xyr[c0, 2]
    d <- sqrt(dx*dx + dy*dy)
    r <- xyr[c1, 3] + xyr[c0, 3]
    w0 <- xyr[c1, 3] / r
    w1 <- xyr[c0, 3] / r
    if (d < r - tol) {
      p <- (r - d) / d
      xyr[c1, 1] <<- Toroid(xyr[c1, 1] + p*dx*w1, 1)
      xyr[c1, 2] <<- Toroid(xyr[c1, 2] + p*dy*w1, 2)
      xyr[c0, 1] <<- Toroid(xyr[c0, 1] - p*dx*w0, 1)
      xyr[c0, 2] <<- Toroid(xyr[c0, 2] - p*dy*w0, 2)
      return(TRUE)
    }
    return(FALSE)
  }
  Toroid <- function(coord, axis) {
    tcoord <- coord
    if (coord < 0) {
      tcoord <- coord + size[axis]
    } else if (coord >= size[axis]) {
      tcoord <- coord - size[axis]
    }
    tcoord
  }
  xyr <- matrix(0, n_circles, 3)
  pos0 <- 1
  for (i in 1:nrow(config)) {
    pos1 <- pos0 + config[i,2] - 1
    xyr[pos0:pos1, 1] <- runif(config[i, 2], 0, size[1])
    xyr[pos0:pos1, 2] <- runif(config[i, 2], 0, size[2])
    xyr[pos0:pos1, 3] <- config[i, 1] * p_radius
    pos0 <- pos1 + 1
  }
  for (iter in 1:max_iter) {
    moved <- FALSE
    for (i in 1:(n_circles-1)) {
      for (j in (i+1):n_circles) {
        if (Repel(xyr, i, j)) {
          moved <- TRUE
        }
      }
    }
    if (!moved) break
  }
  cat(paste(iter, "iterations\n"));
  if (max(xyr[,1:2]) > 1) {
    xyr[,1] <- as.integer(xyr[,1])
    xyr[,2] <- as.integer(xyr[,2])
  }
  xyr[,1] <- xyr[,1] + extent_buffered[1]
  xyr[,2] <- xyr[,2] + extent_buffered[3]
  if (plot == TRUE) {
    DrawCircle <- function(x, y, r, col) {   #  helper function
      lines(cos(seq(0, 2*pi, pi/180)) * r+x, sin(seq(0, 2*pi, pi/180)) * r+y,
      col=col)
    }
    plot(0, type="n", xlab="x", xlim=c(extent[1],extent[2]), ylab="y",
      ylim=c(extent[3], extent[4]))
    xyr[, 3] <- xyr[, 3] / p_radius
    for (i in 1:nrow(xyr)) {
      DrawCircle(xyr[i, 1], xyr[i, 2], xyr[i, 3], "gray")
    }
  }
  colnames(xyr) <- c("x", "y", "radius")
  xy <- xyr[,1:2]
  return(xy)
}

#' Plot 4 ggplots for optimization procedure
#' @param covar_ras RasterLayer
#' @param covar_ras_smooth RasterLayer
#' @param prob_ras RasterLayer
#' @param pred_ras RasterLayer
#' @param in_intercept numeric, input value for intercept of Logistic
#' @param in_beta1 numeric, input value for beta of Logisitic
#' @param in_sigma numeric, input value for sigma of Gaussian Kernel Smoothing
#' @return  ggplot
#' @export
PlotAllSigmaOptimRasters <- function(covar_ras,
                                     covar_ras_smooth,
                                     prob_ras,
                                     pred_ras,
                                     in_intercept = in_intercept,
                                     in_beta1 = in_beta1,
                                     in_sigma = in_sigma,
                                     color_pal = "Greens"){
  gg1 <- ggplot(ConvertRasterForGGPlot(covar_ras, 100000), aes(x, y)) +
    geom_tile(aes(fill = value)) +
    scale_fill_distiller(name = "Value", palette=color_pal, direction = 1) +
    scale_x_continuous(expand =c(0,0)) + scale_y_continuous(expand =c(0,0)) +
    coord_fixed(ratio = 1) +
    ggtitle("Original Raster")
  gg2 <- ggplot(ConvertRasterForGGPlot(covar_ras_smooth, 100000), aes(x, y)) +
    geom_tile(aes(fill = value)) +
    scale_fill_distiller(name = "Value", palette=color_pal, direction = 1) +
    scale_x_continuous(expand =c(0,0)) + scale_y_continuous(expand =c(0,0)) +
    coord_fixed(ratio = 1) +
    ggtitle(paste0("Smoothed (sigma = ", in_sigma, ")"))
  gg3 <- ggplot(ConvertRasterForGGPlot(prob_ras, 100000), aes(x, y)) +
    geom_tile(aes(fill = value)) +
    scale_fill_distiller(name = "Probability", palette=color_pal, direction = 1) +
    scale_x_continuous(expand =c(0,0)) + scale_y_continuous(expand =c(0,0)) +
    coord_fixed(ratio = 1) +
    ggtitle(paste0("Logistic (int = ", in_intercept, ", beta1 = ",
      in_beta1,")"))
  gg4 <- ggplot(ConvertRasterForGGPlot(pred_ras, 100000), aes(x, y)) +
    geom_tile(aes(fill = value)) +
    scale_fill_distiller(name = "Value", palette=color_pal, direction = 1) +
    scale_x_continuous(expand =c(0,0)) + scale_y_continuous(expand =c(0,0)) +
    coord_fixed(ratio = 1) +
    ggtitle("Predicted Surface")
  grid.arrange(gg1, gg2, gg3, gg4, nrow = 2, ncol = 2)
}

#' Plot 4 ggplots for optimization procedure
#' @param covar_ras RasterLayer
#' @param covar_ras_smooth RasterLayer
#' @param prob_ras RasterLayer
#' @param pred_ras RasterLayer
#' @param in_intercept numeric, input value for intercept of Logistic
#' @param in_beta1 numeric, input value for beta of Logisitic
#' @param in_sigma numeric, input value for sigma of Gaussian Kernel Smoothing
#' @return ggplot
#' @export
PlotBWOptimRasters <- function(covar_ras,
                               covar_ras_smooth,
                               prob_ras,
                               pred_ras,
                               in_intercept = in_intercept,
                               in_beta1 = in_beta1,
                               in_sigma = in_sigma,
                               color_pal = "Greens"){
  gg1 <- ggplot(ConvertRasterForGGPlot(covar_ras, 100000), aes(x, y)) +
    geom_tile(aes(fill = value)) +
    scale_fill_distiller(name = "Value", palette=color_pal, direction = 1) +
    scale_x_continuous(expand =c(0,0)) + scale_y_continuous(expand =c(0,0)) +
    coord_fixed(ratio = 1) +
    ggtitle("Original Raster")
  gg2 <- ggplot(ConvertRasterForGGPlot(covar_ras_smooth, 100000), aes(x, y)) +
    geom_tile(aes(fill = value)) +
    scale_fill_distiller(name = "Value", palette=color_pal, direction = 1) +
    scale_x_continuous(expand =c(0,0)) + scale_y_continuous(expand =c(0,0)) +
    coord_fixed(ratio = 1) +
    ggtitle(paste0("Smoothed (sigma = ", in_sigma, ")"))
  gg3 <- ggplot(ConvertRasterForGGPlot(prob_ras, 100000), aes(x, y)) +
    geom_tile(aes(fill = value)) +
    scale_fill_distiller(name = "Probability", palette=color_pal, direction = 1) +
    scale_x_continuous(expand =c(0,0)) + scale_y_continuous(expand =c(0,0)) +
    coord_fixed(ratio = 1) +
    ggtitle(paste0("Logistic (int = ", in_intercept, ", beta1 = ",
      in_beta1,")"))
  gg4 <- ggplot(ConvertRasterForGGPlot(pred_ras, 100000), aes(x, y)) +
    geom_tile(aes(fill = value)) +
    scale_fill_distiller(name = "Value", palette=color_pal, direction = 1) +
    scale_x_continuous(expand =c(0,0)) + scale_y_continuous(expand =c(0,0)) +
    coord_fixed(ratio = 1) +
    ggtitle("Predicted Surface")
  grid.arrange(gg1, gg2, gg3, gg4, nrow = 2, ncol = 2)
}

#' Plot 4 ggplots for optimization procedure
#' @param covar_ras RasterLayer
#' @param covar_ras_smooth RasterLayer
#' @param prob_ras RasterLayer
#' @param pred_ras RasterLayer
#' @param in_intercept numeric, input value for intercept of Logistic
#' @param in_beta1 numeric, input value for beta of Logisitic
#' @param in_sigma numeric, input value for sigma of Gaussian Kernel Smoothing
#' @param col_option color option for color viridis
#' @return ggplot
#' @export
PlotBWOptimRasters2 <- function(covar_ras,
                               covar_ras_smooth,
                               prob_ras,
                               pred_ras,
                               in_intercept = in_intercept,
                               in_beta1 = in_beta1,
                               in_sigma = in_sigma,
                               col_option = "D"){
  color_pal <- "Greens"
  gg1 <- ggplot(ConvertRasterForGGPlot(covar_ras, 100000), aes(x, y)) +
    geom_tile(aes(fill = value)) +
    scale_fill_viridis_c(option = col_option,
      guide = guide_colorbar(title = "Probability", title.hjust = .5)) +
    scale_x_continuous(expand =c(0,0)) + scale_y_continuous(expand =c(0,0)) +
    coord_fixed(ratio = 1) +
    ggtitle("Original Raster")
  gg2 <- ggplot(ConvertRasterForGGPlot(covar_ras_smooth, 100000), aes(x, y)) +
    geom_tile(aes(fill = value)) +
    scale_fill_viridis_c(option = col_option,
      guide = guide_colorbar(title = "Probability", title.hjust = .5)) +
    scale_x_continuous(expand =c(0,0)) + scale_y_continuous(expand =c(0,0)) +
    coord_fixed(ratio = 1) +
    ggtitle(paste0("Smoothed (sigma = ", in_sigma, ")"))
  gg3 <- ggplot(ConvertRasterForGGPlot(prob_ras, 100000), aes(x, y)) +
    geom_tile(aes(fill = value)) +
    scale_fill_viridis_c(option = col_option,
      guide = guide_colorbar(title = "Probability", title.hjust = .5)) +
    scale_x_continuous(expand =c(0,0)) + scale_y_continuous(expand =c(0,0)) +
    coord_fixed(ratio = 1) +
    ggtitle(paste0("Logistic (int = ", in_intercept, ", beta1 = ",
      in_beta1,")"))
  gg4 <- ggplot(ConvertRasterForGGPlot(pred_ras, 100000), aes(x, y)) +
    geom_tile(aes(fill = value)) +
    scale_fill_viridis_c(option = col_option,
      guide = guide_colorbar(title = "Probability", title.hjust = .5)) +
    scale_x_continuous(expand =c(0,0)) + scale_y_continuous(expand =c(0,0)) +
    coord_fixed(ratio = 1) +
    ggtitle("Predicted Surface")
  grid.arrange(gg1, gg2, gg3, gg4, nrow = 2, ncol = 2)
}

#' Plot Rastet showing bandwidth
#' @param in_raster RasterLayer
#' @param title character, title of plot
#' @return ggplot
#' @export
PlotBWRaster <- function(in_raster,
                         title = NA,
                         color_pal = "Greens"){
  gg <- ggplot(ConvertRasterForGGPlot(in_raster, 100000), aes(x, y)) +
    geom_tile(aes(fill = value)) +
    scale_fill_distiller(name = "Value", palette=color_pal, direction = 1) +
    scale_x_continuous(expand =c(0,0)) + scale_y_continuous(expand =c(0,0)) +
    coord_fixed(ratio = 1) +
    ggtitle(title)
  gg
}

#' Plot a Logisitic function (with probability on the y-axis)
#' @param beta0 numeric, intercept used in function
#' @param beta1 numeric, slope used in function
#' @param min_X numeric, minimum on x axis
#' @param max_X numeric, maximum on x axis
#' @return ggplot
#' @export
#' @examples PlotLogisticRange(beta0 = 5, beta1 = -10)
PlotLogisticRange <- function(beta0,
                              beta1,
                              min_x = -.25,
                              max_x = 1.25){
  predictors <- seq(-min_x, max_x, by = .01)
  predictors_logit <- beta0 + beta1*(predictors)
  df <- data.frame(predictors, probs = plogis(predictors_logit))
  (y_mid_int <- (-(1*beta0/beta1)))
  rect_df <- data.frame(xmin = c(-.25, 1), ymin = c(0,0), xmax = c(0,1.25),
    ymax = c(1,1))
  ggplot(df) + geom_line(aes(predictors, probs), color = "blue") +
    geom_segment(aes(x = y_mid_int, y = 0, xend = y_mid_int, yend = 1),
      color = "red") +
    geom_rect(data = rect_df, alpha = .5, aes(xmin = xmin, ymin = ymin,
      xmax = xmax, ymax = ymax)) +
    annotate("text", x = y_mid_int + .03, y = .03, label = signif(y_mid_int, 2),
      color = "red") +
    ylim(0,1) + labs(x = "Predictor", y = "Probability") +
    ggtitle(paste0("Logistic (", "beta0 = ", beta0, ", beta1 = ",
      beta1, ")")) +
    theme(plot.title = element_text(size = 24, face = "bold", vjust = 1.5,
      hjust = 0.5)) +
    theme(axis.title = element_text(size = 20, face = "bold")) +
    theme(axis.text = element_text(colour = "black")) +
    theme(axis.text.x = element_text(size = 16, angle = 50, vjust = 0.5)) +
    theme(axis.text.y = element_text(size = 16, vjust = 0.5)) +
    theme(legend.position = "none")
}

#' Plot Logistic Function for two continous variables [0, 1]
#' @param beta0 numeric, intercept
#' @param beta1 numeric, slope parameter
#' @param beta2 numeric, slope parameter
#' @param main_only logical, whether to return only main plot. Default is FALSE.
#' @import purrr
#' @import ggplot2
#' @importFrom plotly plot_ly add_surface add_trace layout subplot
#' @return plotly plot
#' @export
#' @details Requres a Mapbox token and plotly username and api_key.
PlotLogisticRange2Betas <- function(beta0 = -5,
                                    beta1 = 5,
                                    beta2 = 20,
                                    main_only = FALSE){
  viridis_col <- list(seq(0, 1, length.out = 100), (viridis::viridis(100,
    option ="C")))
  predictor1 <- seq(0, 1, by = .1)  #.01
  predictor2 <- seq(0, 1, by = .1)  #.01
  df <- tidyr::crossing(predictor1, predictor2)
  f <- function(x, y) { r <- plogis(beta0 + x*beta1 + y*beta2)}
  #  f <- function(x, y) { r <- beta0 + x + y^2} # FOR TESTING PLOT AXES
  df <- df %>%
    mutate(prob = purrr::map2_dbl(predictor1, predictor2, f)) %>%
    mutate(pred1 = as.factor(predictor1)) %>%
    mutate(pred2 = as.factor(predictor2))
  title <- paste0("Logistic (", "beta0 = ", round(beta0, 2), ", beta1 = ",
    beta1, ", beta2 = ", beta2,")")
  z <- t(outer(predictor1, predictor2, f))
  ls <- list(predictor1 = predictor1, predictor2 = predictor2, z = z)
  # Individual Graphs (USED FOR REFERENCE AND AD HOC PLOTTING) --
  theme_legend <-
    theme(plot.title=element_text(size=24, face="bold", vjust=1.5))+
    theme(axis.title=element_text(size=20, face="bold")) +
    theme(axis.text=element_text(colour="black")) +
    theme(axis.text.x=element_text(size=16, angle=50, vjust=0.5)) +
    theme(axis.text.y=element_text(size=16, vjust=0.5))
  g1 <- ggplot(df, aes(x=predictor1, y=prob, color=pred2)) +
    geom_line(lwd=1.5) + scale_color_viridis_d(option = "C") +
    labs(x="Predictor 1", y="Probability", title=title) +
    guides(colour = guide_legend(reverse=T)) + theme_legend
  g2 <- ggplot(df, aes(x=predictor2, y=prob, color=pred1)) +
    geom_line(lwd=2) + scale_color_viridis_d(option = "C") +
    labs(x="Predictor 2", y="Probability", title=title) +
    guides(colour = guide_legend(reverse=T)) + theme_legend
  # Main plot only (returned when main_only == TRUE)
  p3_only <- plot_ly(width = 1024, height = 768) %>%
    add_surface(data = ls, x = ls$predictor1, y = ls$predictor2, z = ls$z,
      colorbar=list(title='Probability', x = 0.9, y = 0.85,
        titlefont = list(size =  18)),
      contours = list(x = list(show = TRUE), y = list(show = TRUE)),
      name = "Probability", showscale = TRUE, colorscale = viridis_col,
      showlegend = TRUE)  %>%
    layout( #font = list(size = 14),
      margin = list(t = 75),
      title = paste0("\n", title),
      titlefont = list(size =  25),
      scene = list(
        camera = list(
          center = list(x = 0, y = 0, z = -.1),
          eye = list(x = -1.25, y = -1.25, z = 1.25)),
      zaxis = list(title = "Probability"),
      yaxis = list(title = "Predictor 2", automargin = TRUE, dtick = .1,
        tickfont = list(color = "grey50")),
      xaxis = list(title = "Predictor 1", automargin = TRUE, dtick = .1,
        tickfont = list(color = "grey50"))))
  # Subplots together (returned when main_only == FALSE)
  p1 <- plot_ly(data = df) %>%
   add_trace(., type = "scatter", mode = "lines", x= ~predictor1,
     y= ~prob, color = ~pred2, colors = viridis::viridis(10, option = "D"))
  p2 <- plot_ly(data = df) %>%
   add_trace(., type = "scatter", mode = "lines", x= ~predictor2,
     y= ~prob, color = ~pred1, colors = viridis::viridis(10, option = "D"),
     showlegend = FALSE)
  p3 <- plot_ly() %>%
    add_surface(data = ls, x = ls$predictor1, y = ls$predictor2, z = ls$z,
      contours = list(x = list(show = TRUE), y = list(show = TRUE)),
      name = "Probability", showscale = TRUE, colorscale = viridis_col,
      showlegend = TRUE, name = "Function",
      colorbar = list(x = 0, y = 0.05, len = 0.3, lenmode = "fraction",
        thickness = 0.03, thicknessmode = "fraction",  ticklen = 2, title =
        "Probability", titlefont = list(size = 13), yanchor = "bottom")) %>%
      layout(xaxis = list(anchor = "y"), yaxis = list(anchor = "x"))
  p1_3 <-
    subplot(p3,
      subplot(p1, p2, nrows = 2, heights = c(.45, .45), margin = 0.1),
      nrows = 1, widths = c(.62, .38), margin = 0.05) %>%
    layout(title = title,
      showlegend = TRUE,
      legend = list(traceorder = "reversed",
        x = 1.025, y = 0.6,  bgcolor = "rgba(255,255,255,1)",
        bordercolor = "transparent", borderwidth = 2,
        font = list(color = "rgba(0, 0, 0, 1)", family = "", size = 11),
        orientation = "v", xanchor = "left", yanchor = "top"),
      xaxis = list(title = "TEST1"),
      yaxis = list(title = "TEST2"),
      xaxis2 = list(dtick = .1, title = "Predictor 1"),
      yaxis2 = list(dtick = .1, title = "Probability"),
      xaxis3 = list(dtick = .1, title = "Predictor 2"),
      yaxis3 = list(dtick = .1, title = "Probability"),
      annotations = list(
        list(x = 1.025, y = 0.6, ax = 0, ay = 0,
          font = list(color = "rgba(0, 0, 0, 1)", family = "", size = 12),
          showarrow = FALSE, text = "Other <br> predictor <br> value <br>",
          textangle = 0, xanchor = "left", xref = "paper", yanchor = "bottom",
          yref = "paper")),
      margin = list(l = 0, r = 50, t = 75, b = 50),
      scene = list(showlegend = FALSE,  showscale = FALSE,
        camera = list(
          center = list(x = 0, y = 0, z = -0.15),
          eye = list(x = -1.6, y = -1.6, z = 1.6)),
        xaxis = list(dtick = .1, title = "Predictor 1"),
        yaxis = list(dtick = .1, title = "Predictor 2"),
        zaxis = list(dtick = .25, title = "Prob"),
        name = "Pred1", domain=list(x=c(0.0,.65),y=c(0, 1))))
  if(main_only){
    return(p3_only)
  } else {
    return(p1_3)
  }
}

#' Plot Logistic Function for two continous variables [0, 1] (with a constant
#'     third)
#' @param beta0 numeric, intercept
#' @param beta1 numeric, slope parameter
#' @param beta2 numeric, slope parameter
#' @param beta3 numeric, slope parameter
#' @param pred_held integer, surface to hold constant
#' @param pred_value integer, value for constant surface
#' @param main_only logical, whether to return only main plot. Default is FALSE.
#' @import purrr
#' @import ggplot2
#' @importFrom plotly plot_ly add_surface add_trace layout subplot
#' @return plotly plot
#' @export
#' @details Requres a Mapbox token and plotly username and api_key.
PlotLogisticRange3Betas <- function(beta0 = 17.5,
                                    beta1 = -5,
                                    beta2 = -10,
                                    beta3 = -20,
                                    pred_held = 1,
                                    pred_value = .25,
                                    main_only = FALSE){
  viridis_col <- list(seq(0, 1, length.out = 100), (viridis::viridis(100,
    option ="C")))
  predictor1 <- seq(0, 1, by = .1)  #.01
  predictor2 <- seq(0, 1, by = .1)  #.01
  df <- tidyr::crossing(predictor1, predictor2)
  pred1_held <- pred2_held <- pred3_held <- ""
  if(pred_held == 1){
    f <- function(x, y) {r <- plogis(beta0 + pred_value*beta1 + x*beta2 +
      y*beta3)}
    pred1_held <- paste0("(pred1 = ", pred_value,")")
    pred_x_label = "Predictor 2"
    pred_y_label = "Predictor 3"
  } else if (pred_held == 2){
    f <- function(x, y) {r <- plogis(beta0 + x*beta1 + pred_value*beta2 +
      y*beta3)}
    pred2_held <- paste0("(pred2 = ", pred_value,")")
    pred_x_label = "Predictor 1"
    pred_y_label = "Predictor 3"
  } else if (pred_held == 3){
    f <- function(x, y) {r <- plogis(beta0 + x*beta1 + y*beta2 +
        pred_value*beta3)}
    pred3_held <- paste0("(pred3 = ", pred_value,")")
    pred_x_label = "Predictor 1"
    pred_y_label = "Predictor 2"
  }
  #  f <- function(x, y) { r <- beta0 + x + y^2} # FOR TESTING PLOT AXES
  df <- df %>%
    mutate(prob = purrr::map2_dbl(predictor1, predictor2, f)) %>%
    mutate(pred1 = as.factor(predictor1)) %>%
    mutate(pred2 = as.factor(predictor2))
  title <- paste0("Logistic (beta0 = ", round(beta0, 2), ", beta1 = ",
    beta1, pred1_held, ", beta2 = ", beta2, pred2_held, ", beta3 = ",
    beta3, pred3_held, ")")
  z <- t(outer(predictor1, predictor2, f))
  ls <- list(predictor1 = predictor1, predictor2 = predictor2, z = z)
  # Main plot only (returned when main_only == TRUE)
  p3_only <- plot_ly(width = 1024, height = 768) %>%
    add_surface(data = ls, x = ls$predictor1, y = ls$predictor2, z = ls$z,
      colorbar=list(title='Probability', x = 0.9, y = 0.85,
        titlefont = list(size =  18)),
      contours = list(x = list(show = TRUE), y = list(show = TRUE)),
      name = "Probability", showscale = TRUE, colorscale = viridis_col,
      showlegend = TRUE)  %>%
    layout( #font = list(size = 14),
      margin = list(t = 75),
      title = paste0("\n", title),
      titlefont = list(size =  25),
      scene = list(
        camera = list(
          center = list(x = 0, y = 0, z = -.1),
          eye = list(x = -1.25, y = -1.25, z = 1.25)),
      zaxis = list(title = "Probability"),
      xaxis = list(title = pred_x_label, automargin = TRUE, dtick = .1,
        tickfont = list(color = "grey50")),
      yaxis = list(title = pred_y_label, automargin = TRUE, dtick = .1,
        tickfont = list(color = "grey50")))
      )
  # Subplots together (returned when main_only == FALSE)
  p1 <- plot_ly(data = df) %>%
   add_trace(., type = "scatter", mode = "lines", x= ~predictor1,
     y= ~prob, color = ~pred2, colors = viridis::viridis(10, option = "D"))
  p2 <- plot_ly(data = df) %>%
   add_trace(., type = "scatter", mode = "lines", x= ~predictor2,
     y= ~prob, color = ~pred1, colors = viridis::viridis(10, option = "D"),
     showlegend = FALSE)
  p3 <- plot_ly() %>%
    add_surface(data = ls, x = ls$predictor1, y = ls$predictor2, z = ls$z,
      contours = list(x = list(show = TRUE), y = list(show = TRUE)),
      name = "Probability", showscale = TRUE, colorscale = viridis_col,
      showlegend = TRUE, name = "Function",
      colorbar = list(x = 0, y = 0.05, len = 0.3, lenmode = "fraction",
        thickness = 0.03, thicknessmode = "fraction",  ticklen = 2, title =
        "Probability", titlefont = list(size = 13), yanchor = "bottom")) %>%
      layout(xaxis = list(anchor = "y"), yaxis = list(anchor = "x"))
  p1_3 <-
    plotly::subplot(p3,
      plotly::subplot(p1, p2, nrows = 2, heights = c(.45, .45), margin = 0.1),
      nrows = 1, widths = c(.62, .38), margin = 0.05) %>%
    layout(title = title,
      showlegend = TRUE,
      legend = list(traceorder = "reversed",
        x = 1.025, y = 0.6,  bgcolor = "rgba(255,255,255,1)",
        bordercolor = "transparent", borderwidth = 2,
        font = list(color = "rgba(0, 0, 0, 1)", family = "", size = 11),
        orientation = "v", xanchor = "left", yanchor = "top"),
      xaxis = list(title = "TEST1"), # May not be neeeded
      yaxis = list(title = "TEST2"), # May not be neeeded
      xaxis2 = list(dtick = .1, title = pred_x_label),
      yaxis2 = list(dtick = .1, title = "Probability"),
      xaxis3 = list(dtick = .1, title = pred_y_label),
      yaxis3 = list(dtick = .1, title = "Probability"),
      annotations = list(
        list(x = 1.025, y = 0.6, ax = 0, ay = 0,
          font = list(color = "rgba(0, 0, 0, 1)", family = "", size = 12),
          showarrow = FALSE, text = "Other <br> predictor <br> value <br>",
          textangle = 0, xanchor = "left", xref = "paper", yanchor = "bottom",
          yref = "paper")),
      margin = list(l = 0, r = 50, t = 75, b = 50),
      scene = list(showlegend = FALSE,  showscale = FALSE,
        camera = list(
          center = list(x = 0, y = 0, z = -0.15),
          eye = list(x = -1.6, y = -1.6, z = 1.6)),
        xaxis = list(dtick = .1, title = pred_x_label),
        yaxis = list(dtick = .1, title = pred_y_label),
        zaxis = list(dtick = .25, title = "Prob"),
        name = "Pred1", domain = list(x = c(0.0, 0.65), y = c(0, 1))))
  if(main_only){
    return(p3_only)
  } else {
    return(p1_3)
  }
}

#' Plot a matrix, with arguments for labels and coordinates
#' @param mat matrix
#' @param digits integer, number of digits shown on map
#' @param title character, plot title
#' @param label logical, whether to label
#' @param all_coords logical, whether to show all coordinates on x/y axis
#' @param label_size numberic, label size
#' @return ggplot
#' @export
PlotMatrix <- function(mat,
                       digits = 2,
                       title = NULL,
                       label = TRUE,
                       all_coords = TRUE,
                       label_size = 4){
  mat_df <- reshape2::melt(mat, varnames = c("X", "Y"))
  g <- ggplot(mat_df, aes(X, Y)) + geom_tile(aes(fill = value)) +
    scale_fill_viridis_c(option = "D") +
    guides(fill = guide_colorbar(title = "Value", title_hjust = .5,
      barwidth = 0.5, barheight = 10)) +
    coord_fixed(ratio=1) + ggtitle(title) +
    theme(plot.margin = unit(c(1,1,1,1),"cm"),
    plot.title = element_text(size = 18, face = "bold", hjust = .5, vjust = 0),
    axis.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(angle = 0, vjust = .5),
    panel.grid = element_line(colour = NA),
    panel.background = element_rect(fill = "white", colour = "white"),
    axis.line = element_blank())
  if(isTRUE(label)) g <- g + geom_text(aes(label = round(value, digits)),
    size = label_size)
  exc <- c(0,0,0,0)
  if(isTRUE(all_coords)){
    g <- g + scale_x_continuous(breaks=seq(1, nrow(mat), 1), expand = exc) +
      scale_y_continuous(breaks=seq(1, ncol(mat), 1), expand = exc)
  } else {
    g <- g + scale_x_continuous(expand = exc)+ scale_y_continuous(expand = exc)
    }
  g
}

#' Plot a matrix's center row
#' @param matrix_in matrix
#' @param title character, title for plot
#' @return ggplot
#' @export
PlotMatrixCenterRow2D <- function(matrix_in,
                                  title = NULL){
  df <- ExtractMatrixCenterRow(matrix_in)
  cell_size = df$x[2] - df$x[1]
  g <- ggplot(df, aes(x, y)) +
    geom_col(aes(fill = y), width = cell_size, color="black") +
    scale_fill_viridis_c(guide = guide_colorbar(title = "Probability",
      title.hjust = .5)) +
    annotate("text", x = max(df$x), y = max(df$y),
      label = paste0("Sum: ", signif(sum(df$y), 3)), hjust = 1, vjust = 1) +
    ggtitle(title) +
    ylab("Probability") +  xlab("Cell Center (X)") +
    scale_x_continuous(breaks = df$x, expand = c(0, 0, 0, 0)) +
    scale_y_continuous(expand = c(0, 0, .1, 0)) +
    theme(plot.margin = unit(c(1,1,1,1),"cm"),
    plot.title = element_text(size = 18, face = "bold", hjust = .5, vjust = 0),
    axis.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    panel.grid.minor.x = element_line(colour = "grey80"),
    panel.grid.major.x = element_line(colour =  NA),
    panel.grid.minor.y = element_line(colour =  NA),
    panel.grid.major.y = element_line(colour = "grey80", size=1,
      linetype="dashed"),
    panel.background = element_rect(fill = "white", colour = "white"))
  return(g)
}

#' Plot a raster as a matrix
#' @param raster_in, RasterLayer
#' @param title character, title of plt
#' @param legend character, legend title
#' @param all_coords logical, whether to show all coordinates on x/y axes
#' @return ggplot
#' @export
PlotRasterAsMatrix <- function(raster_in,
                               title = NULL,
                               legend = "Value",
                               all_coords = TRUE){
  df_temp_ras <- ConvertRasterForGGPlot(raster_in, 200000)
  g <- ggplot(df_temp_ras, aes(x, y)) +
    geom_tile(aes(fill = value)) +
    scale_fill_viridis_c(guide = guide_colorbar(title = legend,
      title.hjust = .5, barwidth = 0.5, barheight = 5), option = "D") +
    theme(plot.margin = unit(c(1,1,1,1),"cm"),
      text =  element_text(size = 10),
      plot.title = element_text(size = rel(1.2), face = "bold", hjust = .5,
        vjust = 0),
      axis.text = element_text(size = rel(.8), face = "bold"),
      axis.title = element_text(size = rel(1), face = "bold"),
      axis.title.y = element_text(angle = 0, vjust = .5),
      panel.grid = element_line(colour = NA),
      panel.background = element_rect(fill = "white", colour = "white"),
      axis.line = element_blank()) +
    theme(axis.text.x = element_text(angle = 0, vjust = .75)) +
    coord_fixed(ratio = 1) + ggtitle(title) + xlab("X") + ylab("Y")
  exc <- c(0,0,0,0)
  if(isTRUE(all_coords)){
    g <- g + theme(axis.text.x = element_text(angle = 45, vjust = .75)) +
      scale_x_continuous(breaks=seq(min(df_temp_ras$x), max(df_temp_ras$x),
        by = res(raster_in)[1]), expand = exc) +
      scale_y_continuous(breaks=seq(min(df_temp_ras$y), max(df_temp_ras$y),
        by = res(raster_in)[2]), expand = exc)
  } else {
    g <- g + theme(axis.text.x = element_text(vjust = .75)) +
      scale_x_continuous(expand = exc) + scale_y_continuous(expand = exc)
  }
  g
  return(g)
}

#' Plot a raster's center row
#' @param raster_in RasterLayer
#' @param title character, title of plot
#' @return ggplot
#' @export
PlotRasterCenterRow2D <- function(raster_in,
                                  title = NULL){
  df = ExtractRasterCenterRow(raster_in)
  cell_size = res(raster_in)[1]
  ggplot(df, aes(x, y)) +
    geom_col(aes(fill = y), width = cell_size, color="black") +
    scale_fill_viridis_c(guide = guide_colorbar(title = "Probability",
      title.hjust = .5)) +
    annotate("text", x = max(df$x), y = max(df$y),
      label = paste0("Sum: ", signif(sum(df$y), 3)), hjust = 1, vjust = 1) +
    ggtitle(title) +
    ylab("Probability") +  xlab("Cell Center (X)") +
    scale_x_continuous(breaks = df$x, expand = c(0, 0, 0, 0)) +
    scale_y_continuous(expand = c(0, 0, .1, 0)) +
    theme(plot.margin = unit(c(1,1,1,1),"cm"),
    plot.title = element_text(size = 18, face = "bold", hjust = .5, vjust = 0),
    axis.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    panel.grid.minor.x = element_line(colour = "grey80"),
    panel.grid.major.x = element_line(colour =  NA),
    panel.grid.minor.y = element_line(colour =  NA),
    panel.grid.major.y = element_line(colour = "grey80", size=1,
      linetype="dashed"),
    panel.background = element_rect(fill = "white", colour = "white"))
}

#' Wrapper function to create, slice, and plot a Gaussian Kernel Raster
#' @param raster_in Raster*, overrides sigma and nrow below when provided.
#'     Default is NULL which then uses the parameters below to create a Raster.
#' @param sigma numeric, sigma (or sd) of distribution - scaled to cell_size
#' @param nrow numeric, number of rows/columns in RasterLayer
#' @param shift_n numberic, shift in lat/long origin position
#' @param cell_size numeric, cell size of RasterLayer
#' @param title character, main title. If not provided, a default is used.
#' @param normalize logical, whether or not to normalize values to sum to 1
#' @param dnorm_line logical, whether or not to add a normal distribution curve
#'     to plot. The curve will be scaled to the cell_size and sigma provided.
#' @return ggplot
#' @export
PlotRasterKernSlice <- function(raster_in = NULL,
                                sigma = 1,
                                nrow = 21,
                                shift_n = 0,
                                cell_size = 30,
                                title = NULL,
                                normalize = FALSE,
                                dnorm_line = FALSE){
  if(is.null(raster_in)){
    gkern_raster <- CreateGaussKernRaster(sigma = sigma, nrow = nrow,
      shift_n = shift_n, cell_size = cell_size)
  } else {
    gkern_raster <- raster_in
    cell_size <- xres(gkern_raster)
    nrow <- nrow(gkern_raster)
    gkern_raster <- shift(gkern_raster, shift_n, shift_n)
  }
  gkern_raster_center <- KeepRasterCenterRow(gkern_raster)
  gkern_raster_center_s1 <- gkern_raster_center / cellStats(gkern_raster_center,
    stat="sum")
  if(isTRUE(normalize)){
    slice_df <- ExtractRasterCenterRow(gkern_raster_center_s1)
  } else{
    slice_df <- ExtractRasterCenterRow(gkern_raster_center)
  }
  center_row <- ceiling(nrow/2)
  dnorm_spread <- data.frame(
    x = seq(slice_df[1,1], slice_df[nrow, 1], length.out = 1001),
    y = dnorm(seq(1, nrow(slice_df), length.out = 1001), mean = center_row,
      sd = sigma))
  if(is.null(title)){
    title <- paste0("Gaussian Kernel Slice (center = ", median(slice_df$x),
        ", sigma = ", sigma, ")")
  }
  g <- ggplot(slice_df, aes(x, y)) +
    geom_col(aes(fill = y), width = cell_size, color="black") +
    scale_fill_viridis_c(guide = guide_colorbar(title = "Probability",
      title.hjust = .5)) +
    annotate("text", x = max(slice_df$x), y = max(slice_df$y),
      label = paste0("Sum: ", signif(sum(slice_df$y), 3)), hjust = 1,
      vjust = 1) +
    ggtitle(title) +
    ylab("Probability") +  xlab("Cell Center (X)") +
    scale_x_continuous(breaks = slice_df$x, expand = c(0, 0, 0, 0)) +
    scale_y_continuous(expand = c(0, 0, .1, 0)) +
    theme(plot.margin = unit(c(1,1,1,1),"cm"),
    plot.title = element_text(size = 18, face = "bold", hjust = .5, vjust = 0),
    axis.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45),
    axis.title = element_text(size = 14, face = "bold"),
    panel.grid.minor.x = element_line(colour = "grey80"),
    panel.grid.major.x = element_line(colour =  NA),
    panel.grid.minor.y = element_line(colour =  NA),
    panel.grid.major.y = element_line(colour = "grey80", size=1,
      linetype="dashed"),
    panel.background = element_rect(fill = "white", colour = "white"))
  if(isTRUE(dnorm_line)){
    g_final <- g + geom_line(data = dnorm_spread, aes(x,y), color = "firebrick",
      size = 1.25, linetype = 5)
  } else {
    g_final <- g
  }
  return(g_final)
}

#' Wrapper function for hist3D() and plotrgl() that plots an rgl plot from a
#'   Raster layer
#' @usage Plot3DRaster(raster, azimuth, colaltitude, col, border, x_lab, y_lab,
#'   z_lab, z_lim, main, legend_lab, rgl, rgl_window, spin, movie, movie_name,
#'   ...)
#' @param raster Raster layer to plot
#' @param azimuth azimuth angle, default is 45
#' @param coaltitude coaltitude angle, default is 30
#' @param col color palette. Default is plot3D::gg.col()
#' @param border color of the lines drawn around surface facets, default is
#'   "black".
#' @param x_lab label for x-axis, default is "Longitude"
#' @param y_lab label for y-axis, default is "Latitude"
#' @param z_lab label for z-axis, default is ""
#' @param z_lim range of values for z axis, default is range(z)
#' @param main plot title, default is raster object name
#' @param legend_lab label for legend, default is z_lab
#' @param rgl logical, create an interactive rgl object, default is TRUE
#' @param rgl_window sets rgl window size, either "screen" or "image",
#'   optimized for viewing on screen or as an 1024x768 pixel image,
#'   respectively. Default is "screen".
#' @param spin logical, spin the plot 360 degrees once, default is TRUE
#' @param movie logical, create a .gif movie from the rgl plot. Default is
#'   FALSE.
#' @param movie_name name of output gif movie. Default is saved in in working
#'   directroy as "RasterSpin.gif"
#' @param ... additional arguments for the hist3D() function
#' @return Plot of Raster layer in RStudio, 3d plot in interactive rgl device
#'   (optional), and a .gif movie of the plot rotating 360 degrees (optional)
#' @export
#' @details For additional arguments see ?persp3D. If a movie is made, a new
#'   rgl window will open set with the proper dimensions, record the movie,
#'   then automatically close. All NA values are converted to 0 because the
#'   hist3d() plots were not working with NA values included in the matrix.
#'
Plot3DRaster <- function(raster,
                         azimuth = 45,
                         coaltitude = 30,
                         col = NULL,
                         border="black",
                         x_lab = NULL,
                         y_lab = NULL,
                         z_lab = NULL,
                         z_lim = NULL,
                         main = NULL,
                         legend_lab = NULL,
                         rgl = TRUE,
                         rgl_window = "screen",
                         spin = FALSE,
                         movie = FALSE,
                         movie_name = "RasterSpin",
                         ...) {
  raster <- raster
  x <- raster::xFromCol(raster, col=1:ncol(raster))
  y <- raster::yFromRow(raster, row=1:nrow(raster))
  z <- t(raster::as.matrix(raster))
  z[is.na(z)] <- 0  # otherwise the hist3d() plot does not work properly
  if (is.null(col)) col <- plot3D::gg.col(length(unique(z)))
  if (is.null(x_lab)) x_lab <- "Longitude"
  if (is.null(y_lab)) y_lab <- "Latitude"
  if (is.null(z_lab)) z_lab <- ""
  if (is.null(z_lim)) z_lim <- range(z, na.rm = TRUE)
  if (is.null(main)) main <- deparse(substitute(raster))
  if (is.null(legend_lab)) legend_lab <- z_lab
  plot3D::hist3D(x=x, y=y, z=z, shade=0, nticks=5, ticktype="detailed", col=col,
    bty="b2", expand=.25, phi=coaltitude, theta=azimuth, border=border,
    facets=TRUE, axes=TRUE, image=FALSE, contour=FALSE, panel.first=NULL,
    ltheta=-135, lphi=0, space=0, add=FALSE, plot=TRUE, clab=legend_lab,
    main=main, xlab="Longitude", ylab="Latitude", zlab="", zlim=z_lim,
    colkey=list(side=4, line.clab=1,  length=.5, width=.5, adj.clab=0.1,
      dist=-.03) , ...)
  ResetGraphics <- function(){
    rgl::rgl.clear(type = "bboxdeco")
    text_ids <- subset(rgl::rgl.ids(), type=="text", select="id")
    for (i in 1:nrow(text_ids)){
      rgl::rgl.pop(id=text_ids[i,"id"])
    }
    par(mar=c(2, 2, 2, 2)+.01, las=2)
    rgl::axis3d('x--', ntick=7)  # can be adjust to add more or fewer tick marks
    rgl::axis3d('y+-', ntick=7)  # can be adjust to add more or fewer tick marks
    rgl::axis3d('z--', ntick=4)  # can be adjust to add more or fewer tick marks
    rgl::mtext3d(x_lab, edge='x--', line=2)
    rgl::mtext3d(y_lab, edge='y+-', line=2)
    rgl::mtext3d(z_lab, edge='z--', line=2.5)
    r1 <- rgl::rotationMatrix((coaltitude + 270) * (pi / 180), 1, 0, 0)  #
    r2 <- rgl::rotationMatrix(-azimuth * pi / 180, 0, 0, 1)  #
    r <- r1 %*% r2
    rgl::rgl.viewpoint(interactive=TRUE, userMatrix=r) # rotate
    rgl::observer3d(-0.075, -0.15, 3)
    Sys.sleep(.5)
    bgplot3d_HD <- function(expression){  # Revised rgl::bgplot3d, more dpi
      viewport <- rgl::par3d("viewport")
      width <- viewport["width"]*2
      height <- viewport["height"]*2
      if (width > 0 && height > 0) {
          filename <- tempfile(fileext = ".png")
          png(filename = filename, width = width, height = height, res=288,
            antialias = "cleartype")
          value <- try(expression)
          dev.off()
          result <- rgl::bg3d(texture = filename, col = "white", lit = FALSE)
      }
      else {
          value <- NULL
          result <- rgl::bg3d(col = "white")
      }
      rgl::lowlevel(structure(result, value = value))
    }
    bgplot3d_HD({
      par(omd=c(0.75, 1, 0, 0.5), ps=12)
      #    par(cra=2)
      min_z <- ifelse(min(z) < 0, min(z), 0)
      plot3D::colkey(side = 4, clim = c(min_z, max(z)), add = FALSE, cex.clab=1,
        line.clab=.75, width = 2, length = 1.5, clab = legend_lab,
        col=col, adj.clab = 0.05, cex.axis = 1)
      par(omd = c(0, 1, 0, .975), ps=35)
      title(main=main, font.main=2, cex.main=1)
    })
  }
  if (rgl == TRUE) {
    plot3D::hist3D(x=x, y=y, z=z, shade=0, nticks=5, ticktype="detailed",
      col=col, bty="b2", expand=.25, phi=coaltitude, theta=azimuth,
      border=border, facets=TRUE, axes=TRUE, image=FALSE, contour=FALSE,
      panel.first=NULL, ltheta=-135, lphi=0, space=0, add=FALSE, plot=TRUE,
      clab=legend_lab, main=main, xlab="Longitude", ylab="Latitude", zlab="",
      zlim=z_lim, colkey=FALSE, ...)
    plot3Drgl::plotrgl(new = TRUE, colkey=FALSE) # new window
    if (rgl_window == "image") rgl::par3d(windowRect=c(150, 22, 1174, 790))  #
    if (rgl_window == "screen") rgl::par3d(windowRect=c(0, 23, 1920, 1040))  #
    ResetGraphics()
    if (spin == TRUE) {
      rgl::play3d(rgl::spin3d(axis=c(0,0,1), rpm=6), duration=10)
    }
    if (movie == TRUE) {
      if (rgl_window == "image") {
        cat("Creating a movie file, this will take a few seconds", "\n")
        rgl::movie3d(rgl::spin3d(axis=c(0,0,1), rpm=6), fps = 32, duration=10,
        movie=movie_name, dir=getwd(), clean=TRUE)
        cat(paste0("Created movie file: ", movie_name, ".gif"), "\n")
      }
      if (rgl_window == "screen") {
        org <- as.numeric(rgl::rgl.cur())
        cat("Opening new rgl device with proper dimensions for a movie.", "\n")
        plotrgl(new = TRUE)
        rgl::par3d(windowRect=c(150, 22, 1174, 790))  # dimensions: 1028 X 768
        ResetGraphics()
        cat("Creating a movie file - this will take a few seconds.")
        movie3d(rgl::spin3d(axis=c(0,0,1), rpm=6), fps = 32, duration=10,
          movie=movie_name, dir=getwd(), clean=TRUE, verbose=FALSE)
        cat(paste0("Created movie file: ", movie_name, ".gif"), "\n")
        cat("Returning to previous rgl device.")
        rgl::rgl.close()
        rgl::rgl.set(which=org)
      }
    }
  }
}

#' Prints the position and name of the rasters in a RasterStack or RasterBrick
#' @usage PrintRasterNames(raster)
#' @param raster RasterStack or RasterBrick
#' @return Prints a list of positions and names
#' @export
PrintRasterNames <- function(raster){
  raster <- raster
  for (i in 1:raster::nlayers(raster)){
  results <- paste(i,") ", names(raster[[i]]), sep="")
  cat(results, sep="\n")
  }
}

#' Convert basemaps download from 'OpenStreetMaps' to Raster for use in tm_map
#' @param osm_download download from OpenStreetMaps::openmap()
#' @return a RasterLayer
#' @export
RasterizeOMDownload <- function(osm_download){
  osm_d <- osm_download
  cols <- as.factor(osm_d[1][[1]][[1]]$colorData) # vec of hexdecimal colors
  osm_s <- raster::raster(osm_d) # convert OpenStreetMap to RasterStack
  osm_r <- raster::raster(osm_s) # Conver RasterStack to RasterLayer
  osm_r <- raster::setValues(osm_r, as.integer(cols) - 1L) # Ras w/values 1-255
  raster::colortable(osm_r) <- levels(cols)
  attr(osm_r, "is.OSM") <- TRUE
  return(osm_r) # RasterLayer with colortable
}

#' Convert basemaps download from 'rosm' to Raster for use in tm_map
#' @param rosm_download download from rosm::osm.raster()
#' @return a RasterLayer
#' @export
RasterizeROSMDownload <- function(rosm_download){
  rosm_d <- rosm_download
  rosm_s <- raster::trim(raster::stack(rosm_d)) # Make Stack, then trim NA cells
  rosm_s[is.na(rosm_s)] <- 0
  cols <- as.factor(grDevices::rgb(rosm_s[], maxColorValue = 255))
  rosm_r <- raster::raster(rosm_s)
  rosm_r <- raster::setValues(rosm_r, as.integer(cols) - 1L)
  raster::colortable(rosm_r) <- levels(as.factor(cols))
  return(rosm_r)
}

#' Rescale binary Raster to absolute distance Raster (0 at most 'internal' cell,
#'     highest value at most 'distant' cell)
#' @usage RescaleAndUniformRaster(raster)
#' @param raster RasterLayer
#' @return RasterLayer
#' @export
RescaleAbsoluteDistanceRaster <- function(ras){
  ras2 <- ras1 <- ras
  ras1[ras1 == 1] <- 1
  ras1[ras1 == 0] <- NA
  ras2[ras2 == 1] <- NA
  ras2[ras2 == 0] <- 1
  dist_ras1 <- raster::distance(ras1, doEdge = TRUE)
  dist_ras2 <- raster::distance(ras2, doEdge = TRUE)*-1
  out_ras <- dist_ras1 + dist_ras2
  names(out_ras) <- names(ras)
  return(out_ras)
}

#' Rescale raster from 0-1 and redistribute values to a uniform distribution
#' @usage RescaleAndUniformRaster(raster)
#' @param raster RasterLayer
#' @return RasterLayer
#' @export
RescaleAndUniformRaster <- function(ras){
  ras[] <- scales::rescale(ras[], to = c(0, 1))
  df <- data.frame(org_value = ras[]) %>%
    mutate(n_row = 1:n()) %>%
    arrange(org_value) %>%
    mutate(new_value = seq(0, 1, length.out = ncell(ras))) %>%
    arrange(n_row)
  ras[] <- df$new_value
  return(ras)
}

#' Converts radians to degrees
#' @usage Rad2Deg(radians)
#' @param radians numeric, angle in radians
#' @return Angle in degrees
#' @export
Rad2Deg <- function(radian){
  degree <- (radian * 180) / (pi)
  return(degree)
}

#' Rotates a Raster in 2-dimensional space
#' @usage RotateRaster(raster, angle, resolution)
#' @param raster Raster
#' @param angle numeric, degrees to rotate Raster
#' @param resolution numeric, resolution of output Raster
#' @return A raster
#' @export
#' @examples
#' library(raster)
#' x <- raster(matrix(1:(15*25), nrow = 15), xmn = -1000, xmx = 1000,
#' ymn = -1000, ymx = 1000)
#' plot(x, main="Original")
#' plot(RotateRaster(x, 30, 10), main = paste("Rotated by 30 degrees"))
#' plot(RotateRaster(x, 75, 10), main = paste("Rotated by 75 degrees"))
#' plot(RotateRaster(x, 180, 10), main = paste("Rotated by 180 degrees"))
#' plot(RotateRaster(x, 300, 10), main = paste("Rotated by 300 degrees"))
#'
RotateRaster <- function(raster,
                         angle = 0,
                         resolution=res(raster)) {
  raster_in <- raster
  raster::crs(raster_in) <- "+proj=aeqd +ellps=sphere +lat_0=90 +lon_0=0"
  raster_rotated <- raster::projectRaster(raster_in, res = resolution,
      crs = paste0("+proj=aeqd +ellps=sphere +lat_0=90 +lon_0=", -angle))
  raster::crs(raster_rotated) <- raster::crs(raster_in)
  return(raster_rotated)
}

#' Smooth a RasterLayer using 'smoothie' package
#' Wrapper function to convert a Raster to a matrix, smooth the matrix using
#'     gauss2dsmooth(), and convert the smoothed matrix back into a Raster
#' @param sigma numeric, bandwidth (in cells) for gauss2dsmooth
#' @param covar RasterLayer name in quotes (e.g., "covar1")
#' @return RasterLayer
#' @export
SmoothRaster <- function(sigma = sigma, covar = covar){
  print(paste0("Starting: ", covar, ", sigma = ", sigma))
  covar <- get(covar)
  if (sigma >= 1) {
    covar_smooth <- raster::raster(covar) # creates blank raster
    values(covar_smooth) <- smoothie::gauss2dsmooth(raster::as.matrix(covar),
      lambda = sigma, nx = DescTools::RoundTo(nrow(covar), 2),
      ny = DescTools::RoundTo(ncol(covar), 2))
  } else {
    covar_smooth <- covar
  }
  names(covar_smooth) <- paste0(names(covar), as.character(sigma))
  return(covar_smooth)
}
