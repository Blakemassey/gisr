#' CreateColorsByMetadata
#'
#' Creates and/or displays dataframe of IDs and their associated colors
#'
#' @usage CreateColorsByMetadata(file = , metadata_id)
#'
#' @param file CSV file with columns for "id" and "icon_color"
#' @param metadata_id column name for unique identifier, default is "id"
#'
#' @return df with ID names and hexidecimal colors
#' @export
#'
#' @details  Used in several other functions
#'
CreateColorsByMetadata <- function(file,
                                   metadata_id = "id"){
  metadata <- read.csv(file, header=TRUE, as.is=TRUE, na.strings = "")
  metadata$id <- metadata[ ,metadata_id]
  id_colors <- metadata$icon_color
  names(id_colors) <- metadata$id
  return(id_colors)
}

#' CreateColorsByVar
#'
#' Creates dataframe of a variable and the associated colors
#'
#' @usage CreateColorsByVar(df, by, b_pal, r_pal_num, pal, output, display)
#'
#' @param df dataframe that contains the variable column
#' @param by column name of variable used to create colors. Creates a palette
#'   based on the unique number of factors in the variable and assigned colors
#'   based on the 'pal', 'b_pal', or 'r_pal' parameters.
#' @param pal name of color palette funtions (e.g., rainbow, heat.colors,
#'   terrain.colors, topo.colors, or cm.colors) used to create colors. This
#'   parameter has priority over the other color palette parameters.
#'   Default is NULL.
#' @param r_pal Specifc number of 'R_pal' color palette from the PlotKML
#'   Package (e.g., 1 = R_pal[[1]]). This parameter has priority over the
#'   'b_pal' parameter for setting the colors. Default is NULL.
#' @param b_pal color palette name from 'RColorBrewer' package, default is
#'   "Set1". Automatically adjusts number of colors to match the unique number
#'   of factors in the 'by' parameter.
#' @param output logical, whether or not to output colors
#' @param plot logical, whether or not display a color pie plot
#'
#' @return df with "by" names and a unique hexidecimal colors for each
#' @export
#'
#' @details Used in  other functions. Color palette can either be a typical
#'   palette (e.g. rainbow, heat.heat.colors, terrain,colors, ) using the 'pal'
#'   parameter or a RColorBrewer palette using the 'r_pal' parameter, or a
#'   RColorBrewer palette using the "b_pal" parameter (e.g. "Accent"). The
#'   number of colors is always automatically adjusted to match the unique
#'   number of factors in the "by" parameter column of the df.
#'
CreateColorsByVar <- function (df,
                               by = NULL,
                               pal = NULL,
                               r_pal = NULL,
                               b_pal = "Set1",
                               output = TRUE,
                               plot = FALSE) {
  if(is.null(by)){
    var_colors <- c("all" = "#377EB8")  # blue from RColorBrewer's "Set1"
  } else {
  vars <- unique(df[,by])
  vars_n <- as.numeric(length(unique(df[,by])))
  PalFunction <- function(x, fun) {
    fun(x)
  }
  if (!is.null(pal)) var_colors <- PalFunction(vars_n, pal)
  if (is.null(pal) && (!is.null(r_pal))) {
    var_colors <- colorRampPalette(R_pal[[r_pal]])(vars_n)
  }
  if (is.null(pal) && (is.null(r_pal))) {
    ifelse(vars_n <= 8, var_colors <- RColorBrewer::brewer.pal(vars_n,b_pal),
      var_colors <- colorRampPalette(RColorBrewer::brewer.pal(vars_n,b_pal))
        (vars_n))
  }
  for (i in 1:length(vars)) {
    names(var_colors)[i] <- vars[i]
  }
  }
  if(plot) PlotColorPie(var_colors)
  if(output) return(var_colors)
}

#' ExportKMLPolygon
#'
#' Create a Google Earth KML file from a SpatialPolygonsDataFrame
#'
#' @usage ExportKMLPolygon(object, object_layer, outfile, kml_name,
#'   categorical, metadata, metadata_layer, color_pal, color_alpha, color_range,
#'   color_min, color_max, color_levels, color_increment, legend_levels,
#'   legend_values, log, signif_digtits, outline, alt_mode, extrude, labelscale,
#'   create_kmz, ...)
#'
#' @param object a 'SpatialPolygonsDataFrame' object
#' @param object_layer layer in object that is used for display
#' @param outfile location of output KML file. Extensions (.kml or .kmz) will
#'   automatically determine file type.
#' @param kml_name name of folder in KML file
#' @param categorical logical, whether the data is categorical, default is
#'   false.
#' @param metadata location of metadata .csv file for categorical data.
#'   Metadata file must contain "id" and "icon_color" columns.
#' @param metadata_layer not required, column name in metadata used for legend
#'   labels instead of "id"
#' @param color_pal color palette, can be a color ramp (i.e. c("white", "red")
#'   or a specific palette ("SAGA_pal[[1]]")
#' @param color_alpha display and legend alpha value, default "cc"
#' @param color_range range of object values to create color palette
#' @param color_min min object values to create color palette
#' @param color_max max object value to create color palette
#' @param color_levels number of breaks in color palette, ignored if
#'   color_interval not equal to NA
#' @param color_increment intervals for color palette breaks
#' @param legend_levels number of breakpoints in legend, ignored if
#'   legend_interval not equal to NA
#' @param legend_values vector of values in legend
#' @param log logical of whether to log transform. Default is FALSE.
#' @param signif_digits number of signifcant digits for polygon labels
#' @param outline 1 or 0, whether to draw an outline around each polygon
#' @param alt_mode based on KML code: "absolute","clampedToGround",
#'   "relativeToGround" (see KML documentation for description). Default is
#'   "clampedToGround"
#' @param extrude either 0 (default) or 1: 0 if for no line, 1 extends a line
#'   from the point to the ground.
#' @param labelscale adjusts the size of the Google Earth location point
#'   labels. Default is 0, which hides the labels. To show labels, change to a
#'   value between 0.7-1.
#' @param create_kmz will always create a KMZ, default is FALSE.
#' @param ... parameters passed to CreateCreateCategoricalLegend() or
#'   CreateColorPaletteLegend(), e.g., legend_levels, legend_values
#'
#' @return  KML or KMZ of polygons with an associated legend
#' @export
#'
#' @details uses CreateCategoricalLegend() or CreateColorPaletteLegend()
#'   depending on if categorical = TRUE or FALSE
#'
ExportKMLPolygon <- function (object,
                              object_layer = NULL,
                              outfile = NULL,
                              kml_name = NULL,
                              categorical = FALSE,
                              metadata = NULL,
                              metadata_layer = NULL,
                              color_pal = c("yellow","red"),
                              color_alpha = "cc",
                              color_range = NULL,
                              color_min = NULL,
                              color_max = NULL,
                              color_levels = 5,
                              color_increment = NULL,
                              legend_levels = 5,
                              legend_values = NULL,
                              log = FALSE,
                              signif_digits = 3,
                              outline = 0,
                              alt_mode = "clampToGround",
                              extrude = 0,
                              labelscale = 0,
                              create_kmz = FALSE,
                              ...){
  suppressPackageStartupMessages(require(maptools))
  suppressPackageStartupMessages(require(plotKML))
  suppressPackageStartupMessages(require(plyr))
  suppressPackageStartupMessages(require(tools))
  obj <- object
  if (class(obj) == "RasterLayer"){
    obj <- as(obj, 'SpatialPolygonsDataFrame')
  }
  ifelse(!is.null(object_layer), display_layer <- obj@data[[object_layer]],
  display_layer <- obj@data[[1]])
  if(categorical == TRUE) {
    metadata_df<-read.csv(metadata, header=TRUE, as.is=TRUE, na.strings = "")
    if (!is.null(metadata_layer)){
      categorical_layer = metadata_df[,metadata_layer]
      metadata_display <- data.frame(display_layer = metadata_df$id,
        categorical_layer = categorical_layer)
    } else {
      metadata_display <- data.frame(display_layer = metadata_df$id)
    }
    display_df <- data.frame(display_layer)
    suppressMessages(display_layer <- join(display_df, metadata_display))
      #  join is based on match btwn. "display_layer" and "metadata_df$id"
  }
  obj <- spCbind(obj, display_layer)  # used to subset object
  if (is.null(kml_name) == TRUE) {
    if (!is.null(outfile)) {
      kml_name <- basename(outfile)
      kml_name <- sub(".kml", "", kml_name, ignore.case =TRUE)
      kml_name <- sub(".kmz", "", kml_name, ignore.case =TRUE)
    } else {
      kml_name <- deparse(substitute(object))
    }
  }
  if (is.null(outfile)) {
    if (!is.null(kml_name)) {
      outfile <- paste(getwd(), "/", kml_name, sep="")
    } else {
      outfile <- paste(getwd(), "/", deparse(substitute(object)), sep="")
    }
  } else {
  if (dirname(outfile) == "."){
    outfile <- paste(getwd(), "/", outfile, sep="")
  }
  }
  if (file_ext(outfile) == "kmz" |file_ext(outfile) == "KMZ") {
    create_kmz <- TRUE  # an outfile with a .kmz overrides create_kmz=FALSE
    outfile <- sub(".kmz", ".kml", outfile, ignore.case = TRUE)  # zip filename
  }
  if (file_ext(outfile) == "") {
    outfile <- paste(outfile, ".kml", sep="")  # if object has no extension
  }
  if (categorical == FALSE) {
  if (is.null(color_range[1])){
    ifelse(!is.null(color_min), color_range[1] <- color_min,
      color_range[1] <- min(obj$display_layer))
    ifelse(!is.null(color_max), color_range[2] <- color_max,
      color_range[2] <- max(obj$display_layer))
  }
  color_interval_seq <- CreateColorIntervalSequence(color_range=color_range,
    color_levels=color_levels, color_increment=color_increment, log=log)
    # function used so the results are identical to CreateColorPaletteLegend()
  colors_rbg <- colorRampPalette(color_pal)(length(color_interval_seq)-1)
  colors_kml <- col2kml(colors_rbg)
  colors <- sub("#ff", color_alpha, colors_kml, ignore.case =FALSE)
  interval_num <- as.integer(seq(1, length(color_interval_seq)-1, 1))
  interval_colors <- data.frame(interval_num, colors)
  if(log){
  obj$display_layer <- log10(obj$display_layer)
  }
  obj <- obj[obj$display_layer >= min(color_range) &
    obj$display_layer <= max(color_range),]
  obj_display_layer <- obj$display_layer
  poly_style_num <- findInterval(obj_display_layer, color_interval_seq,
    rightmost.closed=TRUE)
  if (log == TRUE) {
    obj_display_layer <- format(obj$display_layer, nsmall=0)
  } else {
    obj_display_layer <- format(obj$display_layer, nsmall=0)
  }
  }
  prj.check <- check_projection(obj, control = TRUE)
  if (!prj.check) {
    obj <- reproject(obj)
  }
  pv <- length(obj@polygons)
  pvn <- lapply(lapply(obj@polygons, slot, "Polygons"), length)
  coords <- rep(list(NULL), pv)
  hole <- rep(list(NULL), pv)  # currently not used.
  display_values <- rep(list(NULL), pv)
  categorical_values <- rep(list(NULL), pv)
  labpts <- rep(list(NULL), pv)
  for (i in 1:pv) {
    for (k in 1:pvn[[i]]) {
      poly_xyz <- slot(slot(obj@polygons[[i]], "Polygons")[[k]], "coords")
      pt_xyz <- slot(slot(obj@polygons[[i]], "Polygons")[[k]], "labpt")
      if (ncol(poly_xyz) == 2) {
        poly_xyz <- cbind(poly_xyz, rep(0, nrow(poly_xyz)))
      }
      hole[[i]][[k]] <- slot(slot(obj@polygons[[i]], "Polygons")[[k]], "hole")
      coords[[i]][[k]] <- paste(poly_xyz[, 1], ",", poly_xyz[, 2], ",",
        poly_xyz[, 3], collapse = "\n ", sep = "")
      display_values[[i]][[k]] <- as.numeric(obj[["display_layer"]][[i]])
        # as numeric should remove any factor
      if (categorical == TRUE) {
        categorical_values[[i]][[k]] <-
          as.character(obj[["categorical_layer"]][[i]])
      }
      labpts[[i]][[k]] <- paste(pt_xyz[1], ",", pt_xyz[2], ",", 0,
      collapse = "\n ", sep = "")
    }
  }
  poly_coords <- rep(list(""), pv)
  hole_coords <- rep(list(""), pv)
  lab_pts <- rep(list(NA), pv)
  poly_names <- rep(matrix(NA), pv)
  for (i in 1:pv) {
    for (k in 1:pvn[[i]]) {
     lab_pts[i] <- paste(unlist(labpts[i]), collapse = "\n ", sep = "")
     if (hole[[i]][[k]] == TRUE) {
       hole_coords[[i]][[k]] <- paste("\t\t\t\t\t<LinearRing>\n",
       "\t\t\t\t<altitudeMode>", alt_mode, "</altitudeMode>\n",
       "\t\t\t\t\t\t<coordinates>\n", coords[[i]][[k]], "\n",
       "\t\t\t\t\t\t</coordinates>\n",
       "\t\t\t\t\t</LinearRing>\n")
     }
     if (hole[[i]][[k]] == FALSE) {
       poly_coords[[i]][[k]] <- paste("\t\t\t\t\t<LinearRing>\n",
       "\t\t\t\t\t<altitudeMode>", alt_mode, "</altitudeMode>\n",
       "\t\t\t\t\t\t<coordinates>\n", coords[[i]][[k]], "\n",
       "\t\t\t\t\t\t</coordinates>\n",
       "\t\t\t\t\t</LinearRing>\n")
     }
     if(is.numeric(display_values[[i]][[1]])){
       poly_names[[i]] <- signif(display_values[[i]][[1]], digits=signif_digits)
     } else {
       poly_names[[i]] <- display_values[[i]][[1]]
     }
    }
  }
  hole_coords2<- lapply(hole_coords, paste, collapse="")
  poly_coords2<- lapply(poly_coords, paste, collapse="")
  if (categorical == TRUE) {
  poly_names <- categorical_values
  poly_style_num <- display_values
  interval_num <- metadata_df$id
  colors <- sub("#ff", color_alpha, col2kml(metadata_df$icon_color))
  }
  placemarks <- sprintf(paste(
  "\t<Placemark>\n",
  "\t\t<name>%s</name>\n",
  "\t\t\t<styleUrl>#Poly%s</styleUrl>\n",
  "\t\t\t<MultiGeometry>\n",
  "\t\t\t<Point>\n",
  "\t\t\t\t\t\t<coordinates>\n%s\n",
  "\t\t\t\t\t\t</coordinates>\n",
  "\t\t\t</Point>\n",
  "\t\t\t<Polygon>\n",
  "\t\t\t\t<extrude>", extrude, "</extrude>\n",
  "\t\t\t\t<outerBoundaryIs>\n%s",
  "\t\t\t\t</outerBoundaryIs>\n",
  "\t\t\t\t<innerBoundaryIs>\n%s",
  "\t\t\t\t</innerBoundaryIs>\n",
  "\t\t\t</Polygon>\n",
  "\t\t\t</MultiGeometry>\n",
  "\t</Placemark>\n", sep=""), poly_names, poly_style_num, lab_pts,
  poly_coords2, hole_coords2)
  ## Icon Style Section ##
  hi_icon_label_scale <- 0.75
  icon_scale <- 0.7
  ball_bg_color <- "ff333333"
  ball_text_color <- "ffffffff"
  stylemaps <- sprintf(paste(
  "\t<StyleMap id=\"Poly%s\">\n",
  "\t\t<Pair>\n",
  "\t\t\t<key>normal</key>\n",
  "\t\t\t\t<Style>\n",
  "\t\t\t\t\t<LabelStyle>\n",
  "\t\t\t\t\t<scale>",labelscale,"</scale>\n",  # to show label
  "\t\t\t\t\t</LabelStyle>\n",
  "\t\t\t\t\t<IconStyle>\n",
  "\t\t\t\t\t\t<scale>0</scale>\n",
  "\t\t\t\t\t</IconStyle>\n",
  "\t\t\t\t\t<BalloonStyle>\n",
  "\t\t\t\t\t<text>$[description]</text>\n",
  "\t\t\t\t\t\t<bgColor>",ball_bg_color,"</bgColor>\n",
  "\t\t\t\t\t\t<textColor>",ball_text_color,"</textColor>\n",
  "\t\t\t\t\t</BalloonStyle>\n",
  "\t\t\t\t\t<PolyStyle>\n",
  "\t\t\t\t\t\t<outline>",outline,"</outline>\n",
  "\t\t\t\t\t\t<color>%s</color>\n",
  "\t\t\t\t\t\t<colorMode>normal</colorMode>\n",
  "\t\t\t\t\t</PolyStyle>\n",
  "\t\t\t\t</Style>\n",
  "\t\t</Pair>\n",
  "\t\t<Pair>\n",
  "\t\t\t<key>highlight</key>\n",
  "\t\t\t\t<Style>\n",
  "\t\t\t\t\t<LabelStyle>\n",
  "\t\t\t\t\t<scale>",hi_icon_label_scale,"</scale>\n",  # to show label
  "\t\t\t\t\t</LabelStyle>\n",
  "\t\t\t\t\t<IconStyle>\n",
  "\t\t\t\t\t\t<scale>0</scale>\n",
  "\t\t\t\t\t</IconStyle>\n",
  "\t\t\t\t\t<BalloonStyle>\n",
  "\t\t\t\t\t<text>$[description]</text>\n",
  "\t\t\t\t\t\t<bgColor>",ball_bg_color,"</bgColor>", "\n",
  "\t\t\t\t\t\t<textColor>",ball_text_color,"</textColor>", "\n",
  "\t\t\t\t\t</BalloonStyle>\n",
  "\t\t\t\t\t<PolyStyle>\n",
  "\t\t\t\t\t\t<outline>1</outline>\n",
  "\t\t\t\t\t\t<color>%s</color>\n",
  "\t\t\t\t\t\t<colorMode>normal</colorMode>\n",
  "\t\t\t\t\t</PolyStyle>\n",
  "\t\t\t\t</Style>\n",
  "\t\t</Pair>\n",
  "\t</StyleMap>\n",
  sep = ""), interval_num, colors, colors)
  if (create_kmz == TRUE){
    base_file_name <- basename(outfile)
    org_outfile <- outfile
    temp_dir <- file.path(dirname(outfile), "TEMP")
    temp_files_dir <- file.path(temp_dir, "files")
    dir.create(file.path(temp_dir), showWarnings = FALSE)
    dir.create(file.path(temp_files_dir), showWarnings = FALSE)
    png_name <- sub(".kml", " - Legend.png", base_file_name, ignore.case =TRUE)
    href_png <- file.path ("files", png_name)
    png_outfile <- file.path(temp_dir, href_png)
    kmz_outfile<- file.path(temp_dir, base_file_name)
    outfile <- file.path(temp_dir, base_file_name)
  } else {
    png_name <- sub(".kml", " - Legend.png", basename(outfile),
      ignore.case = TRUE)
    png_outfile <- file.path(dirname(outfile), png_name)
    href_png <- png_outfile  # for noticeablility, legend in same folder as .kml
  }
  png(png_outfile, width = 350, height = 600, units = "px", bg = "transparent",
    res = 250, pointsize = 5.5, type = "cairo")  # sets png output parameters
  if(categorical == TRUE){
    CreateCategoricalLegend (metadata=metadata, metadata_layer=metadata_layer,
      color_alpha = color_alpha, pos_x = c(0.05, 0.15), pos_y = c(0.00, 0.99),
      main = kml_name, main_cex = 1.1, main_col = "white", lab_col = "white",
      ...)
  } else {
    CreateColorPaletteLegend(color_pal = colors_rbg, color_alpha = color_alpha,
      color_range = color_range, color_levels = color_levels,
      color_increment = color_increment, legend_levels = legend_levels,
      legend_values = legend_values, log=log, signif_digits = signif_digits,
      pos_x = c(0.05, 0.15), pos_y = c(0.00, 0.99), main = kml_name,
      main_cex = 1.1, main_col = "white", lab_col ="white", ...)
  }
  dev.off()  # writes png_ouput file
  screenoverlay <- paste("\t<ScreenOverlay>\n",
  "\t\t<name>Legend</name>\n",
  "\t\t<Icon>\n",
  "\t\t<href>",href_png,"</href>\n",
  "\t\t</Icon>\n",
  "\t\t<overlayXY x=\"0\" y=\"1\" xunits=\"fraction\" yunits=\"fraction\"/>\n",
  "\t\t<screenXY x=\"0\" y=\"1\" xunits=\"fraction\" yunits=\"fraction\"/>\n",
  "\t\t<rotationXY x=\"0.5\" y=\"0.5\" xunits=\"fraction\" yunits=\"fraction\"",
  "/>\n", "\t\t<size x=\"-1\" y=\"-1\" xunits=\"pixels\" yunits=\"pixels\"/>\n",
  "\t</ScreenOverlay>\n", sep="")
  if (file.exists(outfile)) file.remove(outfile)  # delete KML if already exists
  cat("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
  "<kml xmlns=\"http://www.opengis.net/kml/2.2\"\n",
  "xmlns:gx=\"http://www.google.com/kml/ext/2.2\"\n",
  "xmlns:atom=\"http://www.w3.org/2005/Atom\">\n\n",
  "<Document>\n", "\t<name>",kml_name,"</name>\n",file = outfile,
  append = FALSE, sep = "")
  cat(screenoverlay, file = outfile, append = TRUE, sep = "")  # all stylemaps
  cat(stylemaps,file = outfile, append = TRUE, sep = "")  # all stylemaps
  cat("\t<Folder>\n","\t<name>",kml_name,"</name>\n", "\t<open>0</open>\n",
    file = outfile, append = TRUE, sep = "")
  cat(placemarks,file = outfile, append = TRUE, sep = "")  # all placemarks
  cat("\t</Folder>\n", file = outfile, append = TRUE, sep = "")
  cat("</Document>\n</kml>", file = outfile, append = TRUE)
  if (create_kmz == TRUE){
    zip_file <- sub(".kml", ".zip", org_outfile, ignore.case =TRUE)
    kmz_file <- sub(".kml", ".kmz", org_outfile, ignore.case =TRUE)
    command <- paste("7z a ","\"",zip_file,"\" ", "\"",outfile,"\" ", "\"",
      temp_files_dir,"\"",  sep="")
    system(command)  # runs as if from the command prompt
    file.rename(zip_file, kmz_file)
    do.call(file.remove,list(list.files(temp_files_dir, full.names=TRUE)))
    unlink(temp_files_dir, recursive=TRUE)
    do.call(file.remove,list(list.files(temp_dir, full.names=TRUE)))
    unlink(temp_dir, recursive=TRUE)
    writeLines(noquote(c("Writing: ", kmz_file)))
  } else {
  png_file <- file.path(dirname(outfile), png_outfile)
  writeLines(noquote(c("Writing: ", outfile, png_outfile)))
  }
}

#' ExportKMLProbContour
#'
#' Create a Google Earth KML or KMZ file based on a 'RasterLayer' of kernel
#'   probability levels
#'
#' @usage ExportKMLProbContour(kde_raster, kml_name, dissolve, ...)
#'
#' @param probs_raster RasterLayer of kde probability levels, usually made with
#'   CreateHRProbContourRaster()
#' @param kml_name name of folder in KML file, default= "prob_countours"
#' @param dissolve logical, TRUE = dissolve adjacent like polygons,
#'   FALSE = each cell remains intact; default is FALSE
#' @param ... other arguments to pass to ExportKMLPolygon()
#'
#' @return Exports a KML or KMZ of the probability contour levels
#' @export
#'
#' @details Use the dissolve functionality with caution - it is unable to
#'   properly handle polygon holes. This results from a known problem regarding
#'   holes in 'Polygon' objects. Therefore, at a given contour level holes may
#'   be improperly filled in.
ExportKMLProbContour <- function(probs_raster = probs_raster,
                                 kml_name = "prob_contours",
                                 dissolve = FALSE,
                                 ...){
  suppressPackageStartupMessages(require(raster))
  probs_raster <- probs_raster
  probs_polys <- rasterToPolygons(probs_raster, dissolve=dissolve)
    # 0 !in probs -> huge
  poly <- {}
  ID_list <- {}
  for (k in 1:length(probs_polys@polygons)){
    poly[k] <- lapply(probs_polys@polygons[k], slot, "Polygons")
    ID_list[k] <- probs_polys@data$layer[k]
  }
  poly_ID_list <- {}
  for (k in 1:length(ID_list)){
    poly_ID_list <- append(poly_ID_list, rep(ID_list[k], length(poly[[k]])))
    }
  poly_list <- unlist(poly)
  polys <- {}
  for (k in 1:length(poly_list)){
    polys <- c(polys, Polygons(poly_list[k],k))
  }
  # this may be the spot to add checkPolygonsHoles (but only if dissolve = TRUE)
  spolys = SpatialPolygons(unlist(polys), 1:length(unlist(polys)))
  spolysdf <- SpatialPolygonsDataFrame(spolys, data = data.frame(data =
    poly_ID_list))
  projection(spolysdf) <- projection(probs_raster)
  ExportKMLPolygon(spolysdf, kml_name = kml_name, ...)
}

#' ExportKMLRaster
#'
#' Create a Google Earth KML file from a RasterLayer or a set of KMLs from a
#'   RasterStack, or RasterBrick. Reliant on ExportKMLPolygon().
#'
#' @usage  ExportKMLRaster(object, object_layer, outfile, kml_name,categorical,
#'   metadata, metadata_layer, color_pal, color_alpha, color_range, color_min,
#'   color_max, color_levels, color_increment, legend_levels, legend_values,
#'   log, signif_digits, outline, alt_mode, extrude, labelscale, create_kmz)
#'
#' @param object a 'RasterLayer' or 'SpatialPolygonsDataFrame' object
#' @param object_layer layer in object that is used for display
#' @param outfile location of output KML file. Extensions (.kml or .kmz) will
#'   automatically determine file type.
#' @param kml_name name of folder in KML file
#' @param categorical logical, whether the data is categorical, default is
#'   false.
#' @param metadata location of metadata .csv file for categorical data.
#'   Metadata file must contain "id" and "icon_color" columns.
#' @param metadata_layer not required, column name in metadata used for legend
#'   labels instead of "id"
#' @param color_pal color palette, can be a color ramp (i.e. c("white", "red")
#'   or a specific palette ("SAGA_pal[[1]]"). Atuomatically set as Saga_pal[i]
#'   for RasterStack and RasterBrick.
#' @param color_alpha display and legend alpha value, default "cc"
#' @param color_range range of object values to create color palette
#' @param color_min min object values to create color palette
#' @param color_max max object value to create color palette
#' @param color_levels number of breaks in color palette, ignored if
#'   color_interval not equal to NA
#' @param color_increment intervals for color palette breaks
#' @param legend_levels number of breakpoints in legend, ignored if
#'   legend_interval not equal to NA
#' @param legend_values vector of values in legend
#' @param log logical of whether to log transform. Default is FALSE.
#' @param signif_digits number of signifcant digits for polygon labels
#' @param outline 1 or 0, whether to draw an outline around each polygon
#' @param alt_mode based on KML code: "absolute","clampedToGround",
#'   "relativeToGround" (see KML documentation for description). Default is
#'   "clampedToGround"
#' @param extrude either 0 (default) or 1: 0 for no line, 1 extends a line from
#'   the point to the ground.
#' @param labelscale adjusts the size of the Google Earth location point
#'   labels. Default is 0, which hides the labels. To show labels, change to a
#'   value between 0.7-1.
#' @param create_kmz will always create a KMZ, default is FALSE.
#'
#' @return KML or KMZ of polygons with an associated legend
#' @export
#'
#' @details uses ExportKMLPolygon() and CreateColorPaletteLegend()
ExportKMLRaster <- function (object = object,
                             object_layer = NULL,
                             outfile = NULL,
                             kml_name = NULL,
                             categorical = FALSE,
                             metadata = NULL,
                             metadata_layer = NULL,
                             color_pal = c("yellow","red"),
                             color_alpha = "cc",
                             color_range = NULL,
                             color_min = NULL,
                             color_max = NULL,
                             color_levels = 5,
                             color_increment = NULL,
                             legend_levels = 5,
                             legend_values = NULL,
                             log = FALSE,
                             signif_digits = 3,
                             outline = 0,
                             alt_mode = "clampToGround",
                             extrude = 0,
                             labelscale = 0,
                             create_kmz = FALSE) {
  suppressPackageStartupMessages(require(maptools))
  suppressPackageStartupMessages(require(plotKML))
  suppressPackageStartupMessages(require(plyr))
  suppressPackageStartupMessages(require(tools))
  object <- object
  object_layer_org <- object_layer
  outfile_org <- outfile
  kml_name_org <- kml_name
  categorical_org <- categorical
  metadata_org <- metadata
  metadata_layer_org <- metadata_layer
  color_pal_org <- color_pal
  color_alpha_org <- color_alpha
  color_range_org <- color_range
  color_min_org <- color_min
  color_max_org <- color_max
  color_levels_org <- color_levels
  color_increment_org <- color_increment
  legend_levels_org <- legend_levels
  legend_values_org <- legend_values
  log_org <- log
  signif_digits_org <- signif_digits
  outline_org <- outline
  alt_mode_org <- alt_mode
  extrude_org <- extrude
  labelscale_org <- labelscale
  create_kmz_org <- create_kmz
  specific_raster_list <- c("elev_50mc", "hydro_dir_50mc", "hydro_dist_50mc",
    "lc_50mc", "maine_50mc")
  for (i in 1:nlayers(object)) {
    if (names(object[[i]]) %in% specific_raster_list) {
      if (names(object[[i]]) == "elev_50mc") {
        color_pal = R_pal[[3]]
        color_min = 1
        color_max = 500  # Mt Kathadin is around 5500
        color_levels = 50
        legend_levels = 10
      }
      if (names(object[[i]]) == "hydro_dir_50mc") {
        color_pal = R_pal[[10]]
        color_min = 0
        color_max = 360
        color_levels = 12
        legend_levels = 12
      }
      if (names(object[[i]]) == "hydro_dist_50mc") {
        color_pal = R_pal[[9]]
        color_min = 0
        color_max = 2000
        color_levels = 20
        legend_levels = 10
      }
      if (names(object[[i]]) == "lc_50mc") {
        categorical = TRUE
        metadata = "C:/Work/R/projects/baea_ibm/Data/Assets/lc_50mc.csv"
        metadata_layer = "Land_Cover"
      }
      if (names(object[[i]]) == "maine_50mc") {
        color_pal = SAGA_pal[[22]]
        color_min = 0
        color_max = 10
        color_levels = 10
        legend_levels = 10
      }
      ExportKMLPolygon(object = object[[i]], object_layer = object_layer,
        outfile = outfile, kml_name = names(object[[i]]), categorical =
        categorical, metadata = metadata, metadata_layer, color_pal = color_pal,
        color_alpha = color_alpha, color_range = color_range, color_min =
        color_min, color_max = color_max, color_levels = color_levels,
        color_increment = color_increment, legend_levels = legend_levels,
        legend_values = legend_values, log = log, signif_digits = signif_digits,
        outline = outline, alt_mode = alt_mode, extrude = extrude,
        labelscale = labelscale, create_kmz = create_kmz)
      # This section returns all parameter values back to their original values
      object_layer <- object_layer_org
      outfile <- outfile_org
      kml_name <- kml_name_org
      categorical <- categorical_org
      metadata <- metadata_org
      metadata_layer <- metadata_layer_org
      color_pal <- color_pal_org
      color_alpha <- color_alpha_org
      color_range <- color_range_org
      color_min <- color_min_org
      color_max <- color_max_org
      color_levels <- color_levels_org
      color_increment <- color_increment_org
      legend_levels <- legend_levels_org
      legend_values <- legend_values_org
      log <- log_org
      signif_digits <- signif_digits_org
      outline <- outline_org
      alt_mode <- alt_mode_org
      extrude <- extrude_org
      labelscale <- labelscale_org
      create_kmz <- create_kmz_org
    } else {
      if (nlayers(object) > 1) {
        ExportKMLPolygon(object = object[[i]], object_layer = object_layer,
          outfile = outfile, kml_name = names(object[[i]]), categorical =
          categorical, metadata = metadata, metadata_layer, color_pal =
          SAGA_pal[[i]], color_alpha = color_alpha, color_range = color_range,
          color_min=color_min, color_max = color_max, color_levels =
          color_levels, color_increment = color_increment, legend_levels =
          legend_levels, legend_values = legend_values, log = log,
          signif_digits = signif_digits,outline = outline, alt_mode = alt_mode,
          extrude = extrude, labelscale = labelscale, create_kmz = create_kmz)
      } else {
        ExportKMLPolygon(object = object, object_layer = object_layer, outfile =
          outfile, kml_name = kml_name, categorical = categorical, metadata =
          metadata, metadata_layer, color_pal = color_pal, color_alpha =
          color_alpha, color_range = color_range, color_min=color_min,
          color_max = color_max, color_levels = color_levels, color_increment =
          color_increment, legend_levels = legend_levels, legend_values =
          legend_values, log = log, signif_digits = signif_digits, outline =
          outline, alt_mode = alt_mode, extrude = extrude, labelscale =
          labelscale, create_kmz = create_kmz)
      }
    }
  }
}

#' ExportKMLRasterOverlay
#'
#' Export KML Raster function
#'
#' @usage ExportKMLRasterOverlay(raster, color_pal, alpha, maxpixels, blur,
#'   colNA, outfile, output_dir)
#'
#' @param raster a Raster* object
#' @param color_pal color palette, can be a color ramp (e.g., c("white", "red")
#'   or a specific palette (e.g., "SAGA_pal[[1]]")
#' @param alpha  numeric (0-1), transparency level of the kml. Default is 1.
#' @param method  method used to compute values for the new RasterLayer. Either
#'   'ngb' (nearest neighbor), which is useful for categorical variables, or
#'   'bilinear' (bilinear interpolation; the default value), which is
#'   appropriate for continuous variables.
#' @param overwrite logical, whether or not to overwrite result file. Default
#'   is TRUE.
#' @param maxpixels maximum number of pixels. If ncell(raster) > maxpixels,
#'   sample is used to reduce the number of pixels.
#' @param blur integer (default=10). Higher values help avoid blurring of
#'   isolated pixels (at the expense of a png file that is blur^2 times larger)
#' @param colNA color to use for the background (default is transparent)
#' @param outfile name of KML, default is to use name of raster
#' @param output_dir output folder location, default is getwd()
#' @param zip logical, whether or not to convert .kml to .kmz
#'
#' @return KML of a Raster
#' @export
#'
#' @details Modified from functions in the 'kml' and 'raster' packages
#'
ExportKMLRasterOverlay <- function(raster = raster,
                                   color_pal = rev(terrain.colors(255)),
                                   alpha = 1,
                                   method = "ngb",
                                   overwrite = TRUE,
                                   maxpixels = 500000,
                                   blur = 10,
                                   colNA = "transparent",
                                   outfile = NULL,
                                   output_dir= getwd(),
                                   zip = TRUE) {
  x <- raster
  if (nlayers(x) > 1) {
    x <- x[[1]]
  }
  if(!is.null(outfile)){
    name <- outfile
    outfile <- paste(output_dir, "/", name, ".kml", sep="")
  } else {
    name <- names(x)
    if (name == "layer") {
      name <- deparse(substitute(raster))
    }
    outfile <- paste(output_dir, "/", name, ".kml", sep="")
  }
  stopifnot(hasValues(x))
  x <- raster::projectRaster(x, crs="+proj=longlat +datum=WGS84", method=method)
  unique_x <- length(unique(getValues(x)))
  col <- colorRampPalette(color_pal, alpha=TRUE)(unique_x)
  cols <- adjustcolor(col, alpha)
  #  if (unique_x > 250) unique_x <- 250
  filename <- raster::extension(outfile, ".kml")
  x <- raster::sampleRegular(x, size=maxpixels, asRaster=TRUE, useGDAL=TRUE)
  imagefile <- filename
  extension(imagefile) <- ".png"
  kmlfile <- kmzfile <- filename
  extension(kmlfile) <- ".kml"
  if (file.exists(kmlfile)) {
    if (overwrite) {
      file.remove(kmlfile)
    } else {
      stop("kml file exists, use \"overwrite=TRUE\" to overwrite it")
    }
  }
  png(filename=imagefile, width=max(480, blur * ncol(x)), height=max(480,
    blur * nrow(x)), bg="transparent", type="cairo-png")
  if (!is.na(colNA)) {
    par(mar = c(0, 0, 0, 0), bg = colNA)
  } else {
    par(mar = c(0, 0, 0, 0))
  }
  image(x, col=cols, axes=FALSE, useRaster=TRUE, maxpixels=maxpixels)
  dev.off()
  kml <- c("<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
    "<kml xmlns=\"http://www.opengis.net/kml/2.2\">", "<GroundOverlay>")
  kmlname <- paste("<name>", name, "</name>", sep="")
  icon <- paste("<Icon><href>", basename(imagefile),"</href><viewBoundScale>",
    "0.75</viewBoundScale></Icon>", sep="")
  e <- extent(x)
  latlonbox <- c("\t<LatLonBox>", paste("\t\t<north>", e@ymax,"</north><south>",
    e@ymin, "</south><east>", e@xmax, "</east><west>", e@xmin, "</west>",
    sep = ""), "\t</LatLonBox>")
  footer <- "</GroundOverlay></kml>"
  kml <- c(kml, kmlname, icon, latlonbox, footer)
  cat(paste(kml, sep = "", collapse = "\n"), file = kmlfile, sep = "")
  if(zip) ZipKML(kmlfile, imagefile)
}

#' ExportKMLRasterOverlayWithTime
#'
#' Exports kml of a raster with time properties
#'
#' @usage ExportKMLRasterOverlayWithTime(raster, time, color_pal, alpha,
#'   maxpixels, blur, colNA, outfile, output_dir)
#'
#' @param raster a Raster* object
#' @param time Interval* object, default = NULL
#' @param color_pal color palette, can be a color ramp (e.g., c("white", "red")
#'   or a specific palette (e.g., "SAGA_pal[[1]]")
#' @param alpha numeric (0-1), transparency level of the kml. Default is 1.
#' @param method method used to compute values for the new RasterLayer.
#'   Either 'ngb' (nearest neighbor), which is useful for categorical
#'   variables, or 'bilinear' (bilinear interpolation; the default value),
#'   which is appropriate for continuous variables.
#' @param overwrite logical, whether or not to overwrite result file. Default
#'   is TRUE.
#' @param maxpixels maximum number of pixels. If ncell(raster) > maxpixels,
#'   sample is used to reduce the number of pixels.
#' @param blur integer (default=10). Higher values help avoid blurring of
#'   isolated pixels (at the expense of a png file that is blur^2 times larger)
#' @param colNA color to use for the background (default is transparent)
#' @param outfile name of KML, default is to use name of raster
#' @param output_dir output folder location, default is getwd()
#' @param zip logical, whether or not to convert .kml to .kmz
#' @import raster
#' @return KML of a Raster
#' @export
#'
#' @details Modified from functions in the 'kml' and 'raster' packages
ExportKMLRasterOverlayWithTime <- function(raster = raster,
                                           time = NULL,
                                           color_pal = rev(terrain.colors(255)),
                                           alpha = 1,
                                           method = "ngb",
                                           overwrite = TRUE,
                                           maxpixels = 500000,
                                           blur = 10,
                                           colNA = "transparent",
                                           outfile = NULL,
                                           output_dir= getwd(),
                                           zip = TRUE) {
  x <- raster
  if (raster::nlayers(x) > 1) {
    x <- x[[1]]
  }
  if(!is.null(outfile)){
    name <- outfile
    outfile <- paste(output_dir, "/", name, ".kml", sep="")
  } else {
    name <- names(x)
    if (name == "layer") {
      name <- deparse(substitute(raster))
    }
    outfile <- paste(output_dir, "/", name, ".kml", sep="")
  }
  stopifnot(raster::hasValues(x))
  x <- raster::projectRaster(x, crs="+proj=longlat +datum=WGS84", method=method)
  unique_x <- length(unique(raster::getValues(x)))
  col <- colorRampPalette(color_pal, alpha=TRUE)(unique_x)
  cols <- adjustcolor(col, alpha)
  #  if (unique_x > 250) unique_x <- 250
  filename <- raster::extension(outfile, ".kml")
  x <- raster::sampleRegular(x, size = maxpixels, asRaster = TRUE,
    useGDAL = TRUE)
  imagefile <- filename
  raster::extension(imagefile) <- ".png"
  kmlfile <- kmzfile <- filename
  raster::extension(kmlfile) <- ".kml"
  if (file.exists(kmlfile)) {
    if (overwrite) {
      file.remove(kmlfile)
    } else {
      stop("kml file exists, use \"overwrite=TRUE\" to overwrite it")
    }
  }
  png(filename=imagefile, width=max(480, blur * ncol(x)), height=max(480,
    blur * nrow(x)), bg="transparent", type="cairo-png")
  if (!is.na(colNA)) {
    par(mar = c(0, 0, 0, 0), bg = colNA)
  } else {
    par(mar = c(0, 0, 0, 0))
  }
  x[x== 0] <- NA
  raster::image(x, col=cols, axes=FALSE, useRaster=TRUE, maxpixels=maxpixels)
  #plot(x, colNA="transparent")
  dev.off()
  kml <- c("<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
    "<kml xmlns=\"http://www.opengis.net/kml/2.2\">", "<GroundOverlay>")
  kmlname <- paste("<name>", name, "</name>", sep="")
  if(!is.null(time)){
    start_time <- paste0(strftime(lubridate::int_start(time), "%Y-%m-%d",
      tz = time@tzone), "T", strftime(lubridate::int_start(time), "%H:%M",
      tz=time@tzone), "Z")
    end_time <- paste0(strftime(lubridate::int_end(time), "%Y-%m-%d",
      tz = time@tzone), "T", strftime(lubridate::int_end(time), "%H:%M",
      tz=time@tzone), "Z")
    timespan <- paste0("<TimeSpan>", "<begin>", start_time, "</begin>", "<end>",
      end_time, "</end>", "</TimeSpan>")
  } else {
    timespan <- ""
  }
  icon <- paste("<Icon><href>", basename(imagefile),"</href><viewBoundScale>",
    "0.75</viewBoundScale></Icon>", sep="")
  e <- raster::extent(x)
  latlonbox <- c("\t<LatLonBox>", paste("\t\t<north>", e@ymax,"</north><south>",
    e@ymin, "</south><east>", e@xmax, "</east><west>", e@xmin, "</west>",
    sep = ""), "\t</LatLonBox>")
  footer <- "</GroundOverlay></kml>"
  kml <- c(kml, kmlname, timespan, icon, latlonbox, footer)
  cat(paste(kml, sep = "", collapse = "\n"), file = kmlfile, sep = "")
  if(zip) ZipKML(kmlfile, imagefile)
}

#' ExportKMLPoints
#'
#' Create a Google Earth KML file (points and multitrack) from lat/long
#'   coordinates
#'
#' @param df input dataframe, must have id, lat, long, and datetime
#' @param id column name of unique identifier, data is split into unique paths
#'   and separate folders based on this parameter
#' @param lat column name of latitude coordinates (WGS84, dec. degree)
#' @param long column name of longitude coordinates (WGS84, dec. degree)
#' @param alt input dataframe column name for altitude(m). Optional.
#' @param alt_mode based on KML code: "absolute","clampedToGround",
#'   "relativeToGround" (see KML documentation for description). Default is
#'   "clampedToGround".
#' @param icon_href HTML address of the icon used for points. Default:
#'   "http://maps.google.com/mapfiles/kml/paddle/wht-blank.png"
#' @param icon_scale numeric, scale for icon. Default is 0.7.
#' @param point_col_var column name that determines the color for each point,
#'   may be same as 'id' parameter, but may also be sex, behavior, season, etc.
#'   Default is 'id' parameter
#' @param point_color  R color name (e.g. "yellow3") for the color of all the
#'   points
#' @param point_metadata location of metadata .csv file. Metadata file must
#'   have a column that matches name of 'point_col_var' parameter and
#'   "icon_color" column with hexadecimal colors.
#' @param point_pal name of color palette funtions (e.g., rainbow, heat.colors,
#'   terrain.colors, topo.colors, cm.colors used to create colors. This
#'   parameter has priority over the other point color palette parameters.
#'   Default is NULL.
#' @param point_r_pal  Specifc number of 'R_pal' color palette from the
#'   'PlotKML' Package (e.g., 1 = R_pal[[1]]). This parameter has priority over
#'   the 'b_pal' parameter for setting the colors. Default is NULL.
#' @param point_b_pal color palette name from RColorBrewer package, default is
#'   "Set1". Automatically adjusts number of colors to match the unique number
#'   of factors in the 'point_col_var' column of the input dataframe.
#' @param extrude logical, either FALSE (default) for no line, or TRUE which
#'   extends a line from the point to the ground.
#' @param labelscale  adjusts the size of the Google Earth location point
#'   labels. Default is 0, which hides the labels. To show labels, change to a
#'   value between 0.7-1.
#' @param file filename of output KML file, default is name of input dataframe
#' @param output_dir output folder location, default is getwd()
#'
#' @return KML of points
#'
#' @importFrom magrittr "%>%"
#' @export
#'
ExportKMLPoints <- function (df,
                             id = "id",
                             lat = "lat",
                             long = "long",
                             alt = NULL,
                             alt_mode = "clampToGround",
                             icon_href = file.path("http://maps.google.com",
                               "mapfiles/kml//paddle/wht-blank.png"),
                             icon_scale = 0.7,
                             point_col_var = NULL,
                             point_color = NULL,
                             point_metadata = NULL,
                             point_pal = NULL,
                             point_r_pal = NULL,
                             point_b_pal = "Set1",
                             extrude = FALSE,
                             labelscale = 0,
                             file = NULL,
                             output_dir = NULL) {
  if (is.null(output_dir) == TRUE) {
    if (!is.null(file)) {
      outfile <- file.path(getwd(), file)
    } else {
      outfile <- file.path(getwd(), deparse(substitute(df)))
    }
  } else {
    if (!is.null(file)) {
      outfile <- file.path(output_dir, file)
    } else {
      outfile <- file.path(output_dir, deparse(substitute(df)))
    }
  }
  if (file_ext(outfile) == "") {
    outfile <- paste0(outfile, ".kml")  # if object has no extension
  }
  df <- as.data.frame(df)
  df$id <- df[ ,id] #
  df$lat <- df[ ,lat]
  df$long <- df[ ,long]
  if (!is.null(alt)){
    df$desc_alt <- df[ ,alt] %>% #dplyr::pull()  # writes alti to the "alt" col
    alt1 <- '\t\t\t\t\tAltitude: '  # first part of "Altitude" description
    alt2 <- '\n'  # second part of "Altitude" description
  } else {
    df$alt <- 0  # makes the altitude column a vector of NA
    df$desc_alt <- ""  # makes the altitude description blank values
    alt1 <- NULL  # prevents "Altitude" description from being written
    alt2 <- NULL  # prevents "Altitude" description from being written
  }
  ids <- as.character(df$id)  # as.character removes factor levels
  ifelse(extrude == TRUE, extrude <- 1, extrude <- FALSE)
  PlacemarkPoint <- function(X, Y, Z, ZD, PS, ID) {
    cat("\t<Placemark>\n",
      "\t\t<name>",ID, "</name>\n",
      "\t\t\t<Snippet></Snippet>", "\n",
      "\t\t\t\t<description>\n",
      "\t\t\t\t\tID: ", ID, "\n",
      "\t\t\t\t\tLongitude: ", X, "\n",
      "\t\t\t\t\tLatitude: ", Y, "\n",
      alt1, ZD, alt2,  # written when !is.null(agl)
      "\t\t\t\t</description>", "\n",
      "\t\t\t<styleUrl>#Point_",PS,"</styleUrl>\n",
      "\t\t\t<Point>\n",
      "\t\t\t\t<extrude>", extrude, "</extrude>\n",
      "\t\t\t\t<altitudeMode>", alt_mode, "</altitudeMode>\n",
      "\t\t\t\t\t<coordinates>", X, ",", Y, ",", Z, "</coordinates>\n",
      "\t\t\t</Point>\n",
      "\t</Placemark>\n",
      file = outfile, append = TRUE, sep = "")
    }
  if (file.exists(outfile)) file.remove(outfile)  # delete KML if already exists
  writeLines(noquote(c("Writing: ", outfile)))
  ## Title Section ##
  cat("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
  "<kml xmlns=\"http://www.opengis.net/kml/2.2\"\n",
  "xmlns:gx=\"http://www.google.com/kml/ext/2.2\"\n",
  "xmlns:atom=\"http://www.w3.org/2005/Atom\">\n\n",
  "<Document>\n", "\t<name>",file,"</name>\n",file = outfile,
  append = FALSE, sep = "")
  ## Icon Style Section ##
  if (is.null(point_col_var)) point_col_var <- id
  df$point_col_var <- df[ ,point_col_var]
  if (!is.null(point_metadata)) {
    point_colors <- CreateColorsByMetadata(file=point_metadata,
      metadata_id=point_col_var)
    point_colors <- subset(point_colors, names(point_colors) %in%
      unique(df$point_col_var))
  } else {
    suppressWarnings(point_colors <- CreateColorsByVar(by=point_col_var, df=df,
      pal=point_pal, r_pal=point_r_pal, b_pal=point_b_pal))
  }
  point_colors <- sapply(point_colors, col2kml)
  if (!is.null(point_color)) point_colors[] <- col2kml(point_color)
  point_colors <- sapply(point_colors, substring, 4, 9)
  df$point_col_var <- df[, point_col_var]
  hi_icon_label_scale <- 0.75
  icon_scale <- 0.7
  hi_icon_label_scale <- 0.75
  ball_bg_color <- "ff333333"
  ball_text_color <- "ffffffff"
  for (i in 1:length(point_colors)) {
    cat("\t<StyleMap id=\"Point_",names(point_colors)[i],"\">\n",
      "\t\t<Pair>\n",
      "\t\t\t<key>normal</key>\n",
      "\t\t\t\t<Style>\n",
      "\t\t\t\t\t<LabelStyle>\n",
      "\t\t\t\t\t<scale>",labelscale,"</scale>\n",  # to show label
      "\t\t\t\t\t</LabelStyle>\n",
      "\t\t\t\t\t<IconStyle>\n",
      "\t\t\t\t\t\t<color>FF",point_colors[i],"</color>\n",
      "\t\t\t\t\t\t<scale>",icon_scale,"</scale>\n",
      "\t\t\t\t\t<Icon>\n",
      "\t\t\t\t\t<href>",icon_href,"</href>\n",
      "\t\t\t\t\t</Icon>\n",
      "\t\t\t\t\t</IconStyle>\n",
      "\t\t\t\t\t<BalloonStyle>\n",
      "\t\t\t\t\t<text>$[description]</text>\n",
      "\t\t\t\t\t\t<bgColor>",ball_bg_color,"</bgColor>\n",
      "\t\t\t\t\t\t<textColor>",ball_text_color,"</textColor>\n",
      "\t\t\t\t\t</BalloonStyle>\n",
      "\t\t\t\t</Style>\n",
      "\t\t</Pair>\n",
      "\t\t<Pair>\n",
      "\t\t\t<key>highlight</key>\n",
      "\t\t\t\t<Style>\n",
      "\t\t\t\t\t<LabelStyle>\n",
      "\t\t\t\t\t<scale>",hi_icon_label_scale,"</scale>\n",  # to show label
      "\t\t\t\t\t</LabelStyle>\n",
      "\t\t\t\t\t<IconStyle>\n",
      "\t\t\t\t\t\t<color>FF",point_colors[i],"</color>\n",
      "\t\t\t\t\t\t<scale>0.8</scale>\n",
      "\t\t\t\t\t<Icon>\n",
      "\t\t\t\t\t<href>",icon_href,"</href>\n",
      "\t\t\t\t\t</Icon>\n",
      "\t\t\t\t\t</IconStyle>\n",
      "\t\t\t\t\t<BalloonStyle>\n",
      "\t\t\t\t\t<text>$[description]</text>\n",
      "\t\t\t\t\t\t<bgColor>",ball_bg_color,"</bgColor>", "\n",
      "\t\t\t\t\t\t<textColor>",ball_text_color,"</textColor>", "\n",
      "\t\t\t\t\t</BalloonStyle>\n",
      "\t\t\t\t</Style>\n",
      "\t\t</Pair>\n",
      "\t</StyleMap>\n",
      file = outfile, append = TRUE, sep = "")
  }
  ids <- unique(df$id)  # as.character removes factor levels
  for (i in 1:length(ids)) {
    id_i <- ids[i]
#    cat("<Folder>\n","<name>",unique_id,"</name>\n","<open>0</open>\n",
#      file = outfile, append = TRUE, sep = "")
#    cat("\t<Folder>\n","\t<name>",unique_id,"</name>\n",
#      "\t<open>0</open>\n", file = outfile, append = TRUE, sep = "")
    locs <- df %>% dplyr::filter(id == id_i)
    for (j in 1:nrow(locs)){
      loc <- locs[j,]
      Xs <- loc$long
      Ys <- loc$lat
      Zs <- loc$alt
      ZDs <- loc$desc_alt
      PSs <- loc$point_col_var
      IDs <- id_i
      PlacemarkPoint(Xs, Ys, Zs, ZDs, PSs, IDs)
    }
#    cat("\t</Folder>\n", file = outfile, append = TRUE, sep = "")
#    bloc2 <- NULL
#    cat("\t</Folder>\n", file = outfile, append = TRUE)
  }
  cat("</Document>\n</kml>", file = outfile, append = TRUE)
}


#' ExportKMLTelemetry
#'
#' Create a Google Earth KML file (points and multitrack) from lat/long
#'   coordinates.
#'
#' @usage ExportKMLTelemetry(df, id, datetime, lat, long, alt, alt_mode, speed,
#'   agl, behavior, point_color, point_metadata, point_pal, point_r_pal,
#'   point_b_pal, extrude, path, path_color, path_metadata, path_pal,
#'   path_r_pal, path_b_pal, arrow, icon_by_sex, labelscale, dateformat,
#'   timeformat, datetimeformat, file, output_dir)
#'
#' @param df input dataframe, must have id, lat, long, and datetime
#' @param id column name of unique identifier, data is split into unique paths
#'   and separate folders based on this parameter
#' @param datetime column name of datetime in POSIXct format or as a character
#'   in the format (\%Y/\%m/\%d \%H:\%M)
#' @param lat column name of latitude coordinates (WGS84, dec. degree)
#' @param long column name of longitude coordinates (WGS84, dec. degree)
#' @param alt input dataframe column name for altitude(m). Optional.
#' @param alt_mode based on KML code: "absolute","clampedToGround",
#'   "relativeToGround" (see KML documentation for description). Default is
#'   "clampedToGround".
#' @param speed input dataframe column name for speed. Optional
#' @param agl input dataframe column name for "altitude above ground level",
#'   optional
#' @param behavior input dataframe column name for behavior. Optional
#' @param point_color column name that determines the color for each point, may
#'   be same as 'id' parameter, but may also be sex, behavior, season, etc.
#'   Default is 'id' parameter
#' @param point_metadata location of metadata .csv file. Metadata file must
#'   have a column that matches name of 'point_color'parameter and "icon_color"
#'   column with hexadecimal colors.
#' @param point_pal name of color palette funtions (e.g., rainbow, heat.colors,
#'   terrain.colors, topo.colors, cm.colors used to create colors. This
#'   parameter has priority over the other point color palette parameters.
#'   Default is NULL.
#' @param point_r_pal Specifc number of 'R_pal' color palette from the
#'   'PlotKML' Package (e.g., 1 = R_pal[[1]]). This parameter has priority over
#'   the 'b_pal' parameter for setting the colors. Default is NULL.
#' @param point_b_pal color palette name from RColorBrewer package, default is
#'   "Set1". Automatically adjusts number of colors to match the unique number
#'   of factors in the 'point_color' column of the input dataframe.
#' @param extrude logical, either FALSE (default) for no line, or TRUE which
#'   extends a line from the point to the ground.
#' @param path logical, to create Track paths. Default is TRUE.
#' @param path_color similar to 'point_color' parameter, but the value must
#'   have the same factor level structure as the id file, because each path is
#'   constructed for each id factor. Default will use 'id' parameter.
#' @param path_metadata location of metadata .csv file. Metadata file must have
#'   a column that matches name of 'path_color' parameter and an "icon_color"
#'   column with hexadecimal colors.
#' @param path_pal name of color palette funtions (e.g., rainbow, heat.colors,
#'   terrain.colors, topo.colors, cm.colors used to create colors. This
#'   parameter has priority over the other point color palette parameters.
#'   Default is NULL.
#' @param path_r_pal Specifc number of 'R_pal' color palette from the 'PlotKML'
#'   Package (e.g., 1 = R_pal[[1]]). This parameter has priority over the
#'   'b_pal' parameter for setting the colors. Default is NULL.
#' @param path_b_pal color palette name from RColorBrewer package, default is
#'   "Set1". Automatically adjusts number of colors to match the unique number
#'   of factors in the 'point_color' column of the input dataframe.
#' @param arrow logical, use arrow heads on path icons.
#' @param icon_by_sex logical, use different icons based on "sex" column
#' @param labelscale adjusts the size of the Google Earth location point
#'   labels. Default is 0, which hides the labels. To show labels, change to a
#'   value between 0.7-1.
#' @param dateformat changes the format of the date in the Google Earth
#'   location pop-up windows. Default is "\%Y/\%m/\%d".
#' @param timeformat changes the format of the time in the Google Earth
#'   locations pop-up windows. Default is "\%I:\%M \%p".
#' @param datetimeformat changes the datetime format for the label of
#'   highlighted points. Default is "\%Y/\%m/\%d \%I:\%M \%p"
#' @param file filename of output KML file, default is name of input dataframe
#' @param output_dir name for folder in the KML file, default is working
#'   directory
#'
#' @return KML of points and multitracks
#' @export
#'
ExportKMLTelemetry <- function (df,
                                id = "id",
                                datetime = "datetime",
                                lat = "lat",
                                long = "long",
                                alt = NULL,
                                alt_mode = "clampToGround",
                                speed = NULL,
                                agl = NULL,
                                behavior = NULL,
                                point_color = NULL,
                                point_metadata = NULL,
                                point_pal = NULL,
                                point_r_pal = NULL,
                                point_b_pal = "Set1",
                                extrude = FALSE,
                                path = TRUE,
                                path_color = NULL,
                                path_metadata = NULL,
                                path_pal = NULL,
                                path_r_pal = NULL,
                                path_b_pal = NULL,
                                arrow = TRUE,
                                icon_by_sex = FALSE,
                                labelscale = 0,
                                dateformat = "%Y-%m-%d",
                                timeformat = "%I:%M %p",
                                datetimeformat = "%Y-%m-%d %I:%M %p",
                                file = NULL,
                                output_dir = NULL) {
  suppressPackageStartupMessages(require(lubridate))
  suppressPackageStartupMessages(require(plotKML))
  suppressPackageStartupMessages(require(tools))
  outfile_temp <- NA
  if (is.null(output_dir) == TRUE) {
    if (!is.null(file)) {
      outfile <- file.path(getwd(), file)
    } else {
      outfile <- file.path(getwd(), deparse(substitute(df)))
    }
  } else {
    if (!is.null(file)) {
      outfile <- file.path(output_dir, file)
    } else {
      outfile <- file.path(output_dir, deparse(substitute(df)))
    }
  }
  if (file_ext(outfile) == "") {
    outfile <- paste0(outfile, ".kml")  # if object has no extension
  }
  df <- as.data.frame(df)
  df[, "id"] <- df[, id]
  df[, "lat"] <- df[, lat]
  df[, "long"] <- df[, long]
  if(!"sex" %in% colnames(df))  df$sex <- NA
  if (!is.null(alt)){
    df[, "desc_alt"] <- df[, alt]  # writes altitude to the "alt" column
    alt1 <- '\t\t\t\t\tAltitude: '  # first part of "Altitude" description
    alt2 <- '\n'  # second part of "Altitude" description
  } else {
    df[, "alt"] <- 0  # makes the altitude column a vector of NA
    df[, "desc_alt"] <- ""  # makes the altitude description blank values
    alt1 <- NULL  # prevents "Altitude" description from being written
    alt2 <- NULL  # prevents "Altitude" description from being written
  }
  if (!is.null(agl)) {
    df[, "desc_agl"] <- df[, agl]
    agl1 <- '\t\t\t\t\tAGL: '  # first part of the "Altitude" description
    agl2 <- '\n'  # second part of "Altitude" description
  } else {
    df$desc_agl <- ""  # makes the altitude column a vector of blank values
    agl1 <- NULL  # prevents "Altitude Above Ground Level" from being written
    agl2 <- NULL  # prevents "Altitude Above Ground Level" from being written
  }
  if (!is.null(speed)) {
    df$desc_speed <- df[, speed]  # writes speed to the "speed" column
    spd1 <- '\t\t\t\t\tSpeed: '  # writes first part of the "Speed" description
    spd2 <- '\n'  # writes second part of the "Speed" description
  } else {
    df$desc_speed <- ""  # makes the speed column a vector of blank values
    spd1 <- NULL  # prevents "Speed" description from being written
    spd2 <- NULL  # prevents "Speed" description from being written
  }
  if (!is.null(behavior)) {
    df$desc_behavior <- df[, behavior]  # writes behavior to "behavior" column
    beh1 <- '\t\t\t\t\tBehavior: '  # first part of the "Behavior" description
    beh2 <- '\n'  # second part of the "Behavior" description
  } else {
    df$desc_behavior <- ""  # makes the behavior column a vector of blank values
    beh1 <- NULL  # prevents "Behavior" description from being written
    beh2 <- NULL  # prevents "Behavior" description from being written
  }
  df$datetime <- strftime(df[,datetime],'%Y-%m-%d %H:%M:%S', tz =
    tz(df[,datetime]))
  df$datetime <- as.character(df$datetime)  # needed for KML parsing
  df$datetimebegin <- df$datetime
  EndTimes <- function(data) { # locates last time in data
    if (nrow(data) == 1) {
      data$datetime2 <- data$datetime[1]
    } else {
      data$datetime2 <- data$datetime[c(2:length(data$datetime),
      length(data$datetime))]
    }
  }
  ids <- as.character(df$id)  # as.character removes factor levels
  df_split <- split(df, ids)  # divides data by ids
  df_split <- lapply(df_split, EndTimes)
  datetimeend <- unsplit(df_split, ids)  # returns array of returned values
  df <- cbind(df, datetimeend)  # adds datetime end column to original baea data
  ifelse(extrude == TRUE, extrude <- 1, extrude <- FALSE)
  PlacemarkPoint <- function(PN, X,  Y, Z, ZD,
                             AG, SP, BH, SX, PS,
                             ID, SD, ST, ED, ET,
                             DA, TM) {
    if (icon_by_sex == TRUE) PS <- paste0(PS,"-",SX)
    placemark <- paste0(
      "\t<Placemark>\n",
      "\t\t<name>",PN, "</name>\n",
      "\t\t<TimeSpan>\n",
      "\t\t\t<begin>",SD ,"T" ,ST ,"</begin> " , "\n",
      "\t\t\t<end>", ED, "T", ET, "</end> ", "\n",
      "\t\t</TimeSpan>\n",
      "\t\t\t<Snippet></Snippet>", "\n",
      "\t\t\t\t<description>\n",
      "\t\t\t\t\tID: ", ID, "\n",
      "\t\t\t\t\tDate: ", DA, "\n",
      "\t\t\t\t\tTime: ", TM, "\n",
      "\t\t\t\t\tLongitude: ", X, "\n",
      "\t\t\t\t\tLatitude: ", Y, "\n",
      alt1, ZD, alt2,  # written when !is.null(agl)
      agl1, AG, agl2, # written when !is.null(agl)
      spd1, SP, spd2,  # written when !is.null(speed)
      beh1, BH, beh2, # written when !is.null(behavior)
      "\t\t\t\t</description>", "\n",
      "\t\t\t<styleUrl>#Point_",PS,"</styleUrl>\n",
      "\t\t\t<Point>\n",
      "\t\t\t\t<extrude>", extrude, "</extrude>\n",
      "\t\t\t\t<altitudeMode>", alt_mode, "</altitudeMode>\n",
      "\t\t\t\t\t<coordinates>", X, ",", Y, ",", Z, "</coordinates>\n",
      "\t\t\t</Point>\n",
      "\t</Placemark>\n")
    return(placemark)
  }
  if (file.exists(outfile)) file.remove(outfile)  # delete KML if already exists
  writeLines(noquote(c("Writing: ", outfile)))
  ## Title Section ##
  title_section <- paste0("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
    "<kml xmlns=\"http://www.opengis.net/kml/2.2\"\n",
    "xmlns:gx=\"http://www.google.com/kml/ext/2.2\"\n",
    "xmlns:atom=\"http://www.w3.org/2005/Atom\">\n\n",
    "<Document>\n", "\t<name>",file,"</name>\n")
  ## Icon Style Section ##
  if (is.null(point_color)) point_color <- id
  df[, "point_color"] <- df[, point_color]
  if (!is.null(point_metadata)) {
    point_colors <- CreateColorsByMetadata(file=point_metadata,
      metadata_id=point_color)
    point_colors <- subset(point_colors, names(point_colors) %in%
      unique(df$point_color))
  } else {
    suppressWarnings(point_colors <- CreateColorsByVar(by=point_color, df=df,
      pal=point_pal, r_pal=point_r_pal, b_pal=point_b_pal))
  }
  if (icon_by_sex == TRUE) {
    point_colors_names <- c(sapply(names(point_colors), paste0, "-female"),
      sapply(names(point_colors), paste0,"-male"))
    point_colors <- rep(point_colors, 2)
    names(point_colors) <- point_colors_names
  }
  point_colors <- sapply(point_colors, col2kml)
  point_colors <- sapply(point_colors, substring, 4, 9)
  df[, "point_color"] <- df[, point_color]
  icon_scale <- 0.7
  hi_icon_label_scale <- 0.75
  ball_bg_color <- "ff333333"
  ball_text_color <- "ffffffff"
  mt_icon_href <- file.path("http://earth.google.com/images/kml-icons",
    "track-directional/track-0.png")
  icon_href <- "http://maps.google.com/mapfiles/kml/shapes/placemark_square.png"

  point_colors_section <- vector(mode = "character", 0)

  for (i in 1:length(point_colors)) {
    if (icon_by_sex == TRUE){
      if (grepl("male", names(point_colors)[i]) == TRUE)  icon_href <-
        "http://maps.google.com/mapfiles/kml/shapes/placemark_square.png"
      if (grepl("female", names(point_colors)[i]) == TRUE) icon_href <-
        "http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png"
    }
    point_colors_section <- paste0(point_colors_section,
      "\t<StyleMap id=\"Point_",names(point_colors)[i],"\">\n",
      "\t\t<Pair>\n",
      "\t\t\t<key>normal</key>\n",
      "\t\t\t\t<Style>\n",
      "\t\t\t\t\t<LabelStyle>\n",
      "\t\t\t\t\t<scale>",labelscale,"</scale>\n",  # to show label
      "\t\t\t\t\t</LabelStyle>\n",
      "\t\t\t\t\t<IconStyle>\n",
      "\t\t\t\t\t\t<color>FF",point_colors[i],"</color>\n",
      "\t\t\t\t\t\t<scale>",icon_scale,"</scale>\n",
      "\t\t\t\t\t<Icon>\n",
      "\t\t\t\t\t<href>",icon_href,"</href>\n",
      "\t\t\t\t\t</Icon>\n",
      "\t\t\t\t\t</IconStyle>\n",
      "\t\t\t\t\t<BalloonStyle>\n",
      "\t\t\t\t\t<text>$[description]</text>\n",
      "\t\t\t\t\t\t<bgColor>",ball_bg_color,"</bgColor>\n",
      "\t\t\t\t\t\t<textColor>",ball_text_color,"</textColor>\n",
      "\t\t\t\t\t</BalloonStyle>\n",
      "\t\t\t\t</Style>\n",
      "\t\t</Pair>\n",
      "\t\t<Pair>\n",
      "\t\t\t<key>highlight</key>\n",
      "\t\t\t\t<Style>\n",
      "\t\t\t\t\t<LabelStyle>\n",
      "\t\t\t\t\t<scale>",hi_icon_label_scale,"</scale>\n",  # to show label
      "\t\t\t\t\t</LabelStyle>\n",
      "\t\t\t\t\t<IconStyle>\n",
      "\t\t\t\t\t\t<color>FF",point_colors[i],"</color>\n",
      "\t\t\t\t\t\t<scale>0.8</scale>\n",
      "\t\t\t\t\t<Icon>\n",
      "\t\t\t\t\t<href>",icon_href,"</href>\n",
      "\t\t\t\t\t</Icon>\n",
      "\t\t\t\t\t</IconStyle>\n",
      "\t\t\t\t\t<BalloonStyle>\n",
      "\t\t\t\t\t<text>$[description]</text>\n",
      "\t\t\t\t\t\t<bgColor>",ball_bg_color,"</bgColor>", "\n",
      "\t\t\t\t\t\t<textColor>",ball_text_color,"</textColor>", "\n",
      "\t\t\t\t\t</BalloonStyle>\n",
      "\t\t\t\t</Style>\n",
      "\t\t</Pair>\n",
      "\t</StyleMap>\n")
  }
  point_colors_section <- paste0(point_colors_section, collapse = "")
  if (path ==TRUE) {
    if (is.null(path_color)) path_color <- id
    if (is.null(path_b_pal)) path_b_pal <- point_b_pal
    if (!is.null(path_metadata)) {
      path_colors <- CreateColorsByMetadata(file=path_metadata,
        metadata_id=path_color)
      path_colors <- subset(path_colors, names(path_colors) %in%
        unique(df[,path_color]))
    } else {
      suppressWarnings(path_colors <- CreateColorsByVar(by=path_color, df=df,
        pal=path_pal, r_pal=path_r_pal, b_pal=path_b_pal))
    }
    path_colors <- sapply(path_colors, col2kml)
    path_colors <- sapply(path_colors, substring, 4, 9)
    if (icon_by_sex == TRUE) {
      path_colors_names <- c(sapply(names(path_colors), paste0, "-female"),
      sapply(names(path_colors), paste0,"-male"))
      path_colors <- rep(path_colors, 2)
      names(path_colors) <- path_colors_names
    }
    ifelse(arrow == TRUE, arrow <- 1, arrow <- 0)
  ## Style Map for Track ##
    path_colors_section <- vector(mode = "character", 0)

    for (i in 1:length(path_colors)) {
      path_colors_section <- paste0(path_colors_section,
        "\t<StyleMap id=\"Track_",names(path_colors)[i],"\">\n",
        "\t\t<Pair>\n",
        "\t\t\t<key>normal</key>\n",
        "\t\t\t\t<Style>\n",
        "\t\t\t\t\t<LabelStyle>\n",
        "\t\t\t\t\t<scale>0</scale>\n",  # to show label
        "\t\t\t\t\t</LabelStyle>\n",
        "\t\t\t<IconStyle>\n",
        "\t\t\t\t<scale>",arrow,"</scale>\n",
        "\t\t\t\t<Icon>\n",
        "\t\t\t\t\t<href>",mt_icon_href,"</href>\n",
        "\t\t\t\t</Icon>\n",
        "\t\t\t</IconStyle>\n",
        "\t\t\t\t\t<BalloonStyle>\n",
        "\t\t\t\t\t<text>",names(path_colors)[i]," - Path</text>\n",
        "\t\t\t\t\t\t<bgColor>",ball_bg_color,"</bgColor>\n",
        "\t\t\t\t\t\t<textColor>",ball_text_color,"</textColor>\n",
        "\t\t\t\t\t</BalloonStyle>\n",
        "\t\t\t<LineStyle>\n",
        "\t\t\t\t<color>dd",path_colors[i],"</color>\n",
        "\t\t\t\t<width>1</width>\n",
        "\t\t\t</LineStyle>\n",
        "\t\t\t\t</Style>\n",
        "\t\t</Pair>\n",
        "\t\t<Pair>\n",
        "\t\t\t<key>highlight</key>\n",
        "\t\t\t\t<Style>\n",
        "\t\t\t\t\t<LabelStyle>\n",
        "\t\t\t\t\t<scale>0</scale>\n",  # to show label, change to >= 0.7
        "\t\t\t\t\t</LabelStyle>\n",
        "\t\t\t<IconStyle>\n",
        "\t\t\t\t<scale>1</scale>\n",
        "\t\t\t\t<Icon>\n",
        "\t\t\t\t\t<href>",mt_icon_href,"</href>\n",
        "\t\t\t\t</Icon>\n",
        "\t\t\t</IconStyle>\n",
        "\t\t\t\t\t<BalloonStyle>\n",
        "\t\t\t\t\t<text>",names(path_colors)[i]," - Path</text>\n",
        "\t\t\t\t\t\t<bgColor>",ball_bg_color,"</bgColor>\n",
        "\t\t\t\t\t\t<textColor>",ball_text_color,"</textColor>\n",
        "\t\t\t\t\t</BalloonStyle>\n",
        "\t\t\t<LineStyle>\n",
        "\t\t\t\t<color>ee",path_colors[i],"</color>\n",
        "\t\t\t\t<width>1</width>\n",
        "\t\t\t</LineStyle>\n",
        "\t\t\t\t</Style>\n",
        "\t\t</Pair>\n",
        "\t</StyleMap>\n")
    }  # end of Track icon loop
  } # end of (path == TRUE)
  path_colors_section <- paste0(path_colors_section, collapse = "")
  ids <- as.character(unique(df$id))  # as.character removes factor levels
  id_section <- vector(mode = "character", 0)
  for (i in ids) {
    sv = df$id %in% i
    unique_id <- as.character(unique(df$id[sv]))
    id_header_section <- paste0("<Folder>\n","<name>",unique_id,"</name>\n",
      "<open>0</open>\n","\t<Folder>\n","\t<name>",unique_id,
      " - Locations</name>\n","\t<open>0</open>\n")
    id_placemark_section <- vector(mode = "character", 0)
    locs <- subset(df, id == unique_id)
    for (i in 1:nrow(locs)){
      loc <- locs[i,]
      PNs <- strftime(loc[, "datetimebegin"], datetimeformat)
      Xs <- loc[, "long"]
      Ys <- loc[, "lat"]
      Zs <- loc[, "alt"]
      ZDs <- loc[, "desc_alt"]
      SPs <- loc[, "desc_speed"]
      AGs <- loc[, "desc_agl"]
      BHs <- loc[, "desc_behavior"]
      SXs <- loc[, "sex"]
      PSs <- loc[, "point_color"]
      IDs <- unique_id
      SDs <- substring(loc$datetime, 1, 10) #start date
      STs <- substring(loc$datetime, 12,16) #start time
      EDs <- substring(loc$datetimeend, 1,10) #end date
      ETs <- substring(loc$datetimeend, 12,19) #end time
      DAs <- strftime(loc[, "datetimebegin"], dateformat)
      TMs <- strftime(loc[, "datetimebegin"], timeformat)
      id_placemark_section <- paste0(id_placemark_section,
        PlacemarkPoint(PNs,  Xs,  Ys,  Zs, ZDs,
                       AGs, SPs, BHs, SXs, PSs,
                       IDs, SDs, STs, EDs, ETs,
                       DAs, TMs))
    }
    id_placemark_section <- paste0(id_header_section, id_placemark_section,
      "\t</Folder>\n", collapse = "")
    locs$Ts <- "T"
    locs$Zs <- "Z"
    locs$datetimedate <- substring(locs$datetime, 1,10) #start date
    locs$datetimetime <- substring(locs$datetime, 12,16) #start time
    whens <- locs[, c("datetimedate","Ts","datetimetime", "Zs")]
    sgmts <- locs[, c("long","lat","alt")]
    unique_id <- unique(locs$id)
    ifelse(icon_by_sex == TRUE, path_id <- paste0(unique_id, "-",
      unique(locs$sex)), path_id <- unique_id)
    id_path_section <- vector(mode = "character", 0)
    if (path == TRUE && "path_seg" %in% colnames(locs)){
      whens <- locs[, c("datetimedate","Ts","datetimetime", "Zs", "path_seg")]
      sgmts <- locs[, c("long","lat","alt", "path_seg")]
      tracks <- NULL
      for (i in unique(whens$path_seg)){
        whens_i <- whens %>% dplyr::filter(path_seg == i) %>%
          dplyr::select(-c(path_seg))
        sgmts_i <- sgmts %>% dplyr::filter(path_seg == i) %>%
          dplyr::select(-c(path_seg))
        tracks <- paste0(tracks, "\t\t<gx:Track>\n",
          "\t\t<altitudeMode>",alt_mode,"</altitudeMode>\n",
          paste(paste("\t\t\t\t\t<when>", apply(whens_i, 1, paste, collapse=""),
            sep=""), "</when>", collapse="\n"), "\n",
          paste(paste("\t\t\t\t\t<gx:coord>", apply(sgmts_i, 1, paste,
            collapse=" "), sep = ""),"</gx:coord>", collapse = "\n"), "\n",
          "\t\t</gx:Track>\n")
      }
      id_path_section <- c(id_path_section, paste0(
        "\t<Placemark>\n",
        "\t\t<name>",unique_id," - Path</name>\n",
        "\t\t<styleUrl>#Track_",path_id,"</styleUrl>\n",
        "\t\t<gx:balloonVisibility>0</gx:balloonVisibility>\n",
        "\t\t<gx:MultiTrack>\n",
        tracks,
        "\t\t</gx:MultiTrack>\n",
        "\t</Placemark>\n"))
    }
    if (path == TRUE && !"path_seg" %in% colnames(locs)) {
      whens <- locs[, c("datetimedate","Ts","datetimetime", "Zs")]
      sgmts <- locs[, c("long","lat","alt")]
      id_path_section <- c(id_path_section, paste0(
        "\t<Placemark>\n",
        "\t\t<name>",unique_id," - Path</name>\n",
        "\t\t<styleUrl>#Track_",path_id,"</styleUrl>\n",
        "\t\t<gx:balloonVisibility>0</gx:balloonVisibility>\n",
        "\t\t<gx:Track>\n",
        "\t\t<altitudeMode>",alt_mode,"</altitudeMode>\n",
        paste(paste("\t\t\t\t\t<when>", apply(whens, 1, paste, collapse=""),
          sep=""), "</when>", collapse="\n"), "\n",
        paste(paste("\t\t\t\t\t<gx:coord>", apply(sgmts, 1, paste, collapse=" "),
          sep = ""),"</gx:coord>", collapse = "\n"),"\n",
        "\t\t</gx:Track>\n",
        "\t</Placemark>\n"))
    }
    id_section <- paste0(id_section, id_placemark_section, id_path_section,
      "\t</Folder>\n")
  }
  all_sections <- paste0(title_section, point_colors_section,
    path_colors_section, id_section, "</Document>\n</kml>")
  cat(all_sections, file = outfile, append = FALSE)
}


#' Export BAEA telemetry data at KML
#'
#' Create a Google Earth KML file (points and multitrack) from lat/long
#'   coordinates of BAEA telemetry data. Uses the same functionality as
#'   ExportKMLTelemetry(), but all the defaults are set for specific BAEA
#'   telemetry data from Maine
#'
#' @usage ExportKMLTelemetryBAEA(df, id, datetime, lat, long, alt, alt_mode,
#'   speed, agl, behavior, point_color, point_metadata, point_pal, point_r_pal,
#'   point_b_pal, extrude, path, path_color, path_metadata, path_pal,
#'   path_r_pal, path_b_pal, arrow, icon_by_sex, labelscale, dateformat,
#'   timeformat, datetimeformat, file, output_dir)
#'
#' @param df Input dataframe, must have id, lat, long, and datetime.
#' @param id Column name of unique identifier, data is split into unique paths
#'   and separate folders based on this parameter. Default is "id".
#' @param datetime Column name of datetime in POSIXct format or as a character
#'   in the format (\%Y/\%m/\%d \%H:\%M). Default is "datetime".
#' @param lat Column name of latitude coordinates (WGS84, dec. degree). Default
#'   "lat".
#' @param long Column name of longitude coordinates (WGS84, dec. degree).
#'   Default is "long".
#' @param alt Input dataframe column name for altitude(m). Optional. Default is
#'   "alt".
#' @param alt_mode based on KML code: "absolute","clampedToGround",
#'   "relativeToGround" (see KML documentation for description). Default is
#'   "absolute".
#' @param speed input dataframe column name for speed. Optional. Default is
#'   "speed".
#' @param agl input dataframe column name for "altitude above ground level",
#'   optional. Default is NULL.
#' @param behavior input dataframe column name for behavior. Optional. Default
#'   is NULL.
#' @param point_color column name that determines the color for each point, may
#'   be same as 'id' parameter, but may also be sex, behavior, season, etc.
#'   Default is 'deploy_location".
#' @param point_metadata location of metadata .csv file. Metadata file must
#'   have a column that matches name of 'point_color'parameter and "icon_color"
#'   column with hexadecimal colors. Default is:
#'   "Data/GPS/GPS_Deployments.csv".
#' @param point_pal name of color palette funtions (e.g., rainbow, heat.colors,
#'   terrain.colors, topo.colors, cm.colors used to create colors. This
#'   parameter has priority over the other point color palette parameters.
#'   Default is NULL.
#' @param point_r_pal Specifc number of 'R_pal' color palette from the
#'   'PlotKML' Package (e.g., 1 = R_pal[[1]]). This parameter has priority over
#'   the 'b_pal' parameter for setting the colors. Default is NULL.
#' @param point_b_pal color palette name from RColorBrewer package, default is
#'   "Set1". Automatically adjusts number of colors to match the unique number
#'   of factors in the 'point_color' column of the input dataframe.
#' @param extrude Logical, either FALSE (default) for no line, or TRUE which
#'   extends a line from the point to the ground.
#' @param path Logical, to create Track paths. Default is TRUE.
#' @param path_color Similar to 'point_color' parameter, but the value must
#'   have the same factor level structure as the id file, because each path is
#'   constructed for each id factor. Default is "deploy_location".
#' @param path_metadata Location of metadata .csv file. Metadata file must have
#'   a column that matches name of 'path_color' parameter and an "icon_color"
#'   column with hexadecimal colors. Default is:
#'   "Data/GPS/GPS_Deployments.csv".
#' @param path_pal Name of color palette funtions (e.g., rainbow, heat.colors,
#'   terrain.colors, topo.colors, cm.colors used to create colors. This
#'   parameter has priority over the other point color palette parameters.
#'   Default is NULL.
#' @param path_r_pal Specifc number of 'R_pal' color palette from the 'PlotKML'
#'   Package (e.g., 1 = R_pal[[1]]). This parameter has priority over the
#'   'b_pal' parameter for setting the colors. Default is NULL.
#' @param path_b_pal color palette name from RColorBrewer package, default is
#'   "Set1". Automatically adjusts number of colors to match the unique number
#'   of factors in the 'point_color' column of the input dataframe.
#' @param arrow Logical, use arrow heads on path icons. Default is TRUE.
#' @param icon_by_sex Logical, use different icons based on "sex" column.
#'   Default is FALSE.
#' @param labelscale Numeric, adjusts the size of the Google Earth location
#'   point labels. Default is 0, which hides the labels. To show labels, change
#'   to a value between 0.7-1.
#' @param dateformat String, changes the format of the date in the Google Earth
#'   location pop-up windows. Default is "\%Y/\%m/\%d".
#' @param timeformat String, changes the format of the time in the Google Earth
#'   locations pop-up windows. Default is "\%I:\%M \%p".
#' @param datetimeformat String, changes the datetime format for the label of
#'   highlighted points. Default is "\%Y/\%m/\%d \%I:\%M \%p"
#' @param file String, filename of output KML file, default is name of input
#'  dataframe. Default is: "BAEA Data.kml"
#' @param output_dir Name for folder in the KML file, default is working
#'   directory. Default is: "C:/Users/Blake/Desktop".
#'
#' @return KML of points and multitracks
#' @export
#'
ExportKMLTelemetryBAEA <- function (df,
                                    id = "id",
                                    datetime = "datetime",
                                    lat = "lat",
                                    long = "long",
                                    alt = "alt",
                                    alt_mode = "absolute",
                                    speed = "speed",
                                    agl = NULL,
                                    behavior = NULL,
                                    point_color = "deploy_location",
                                    point_metadata = file.path("Data/GPS",
                                      "GPS_Deployments.csv"),
                                    point_pal = NULL,
                                    point_r_pal = NULL,
                                    point_b_pal = "Set1",
                                    extrude = FALSE,
                                    path = TRUE,
                                    path_color = "deploy_location",
                                    path_metadata = file.path("Data/GPS",
                                      "GPS_Deployments.csv"),
                                    path_pal = NULL,
                                    path_r_pal = NULL,
                                    path_b_pal = NULL,
                                    arrow = TRUE,
                                    icon_by_sex = FALSE,
                                    labelscale = 0,
                                    dateformat = "%Y-%m-%d",
                                    timeformat = "%I:%M %p",
                                    datetimeformat = "%Y-%m-%d %I:%M %p",
                                    file = "BAEA Data.kml",
                                    output_dir = "C:/Users/Blake/Desktop") {
  if (point_color == "behavior" || point_color == "sex") {
      point_metadata = "Data/Visualization/behavior_colors.csv"
  }
  if (!is.null(path_color) &&  path_color == "sex") {
      path_metadata = "Data/Visualization/behavior_colors.csv"
  }
  gisr::ExportKMLTelemetry(df=df, id=id, datetime=datetime, lat=lat, long=long,
    alt=alt, alt_mode=alt_mode, speed=speed, agl=agl, behavior=behavior,
    point_color=point_color, point_metadata=point_metadata, point_pal=point_pal,
    point_r_pal=point_r_pal, point_b_pal=point_b_pal, extrude=extrude,
    path=path, path_color=path_color, path_metadata=path_metadata,
    path_pal=path_pal, path_r_pal= path_r_pal, path_b_pal=path_b_pal,
    arrow=arrow, icon_by_sex=icon_by_sex, labelscale=labelscale,
    dateformat=dateformat, timeformat=timeformat, datetimeformat=datetimeformat,
    file=file, output_dir=output_dir)
}

#' ExportKMLWindProjects
#'
#' Exports KML of wind project locations
#'
#' @usage ExportKMLWindProjects(df, labelscale, outfile, metadata)
#'
#' @param df the location of the .csv file
#' @param labelscale adjusts the size of the Google Earth location point
#'   labels. Default is 0, which hides the labels. To show labels, change to a
#'   value between 0.7-1.
#' @param outfile location of output KML file
#' @param metadata location of metadata .csv file. Metadata file must contain
#'   columns for project ID, hexadecimal colors, and additional icon data.
#'
#' @return creates KML
#' @export
#'
#' @details Defaults are specific to my file directories and locations
#'
ExportKMLWindProjects <- function(df = "Data/Turbines/Maine Wind Projects.csv",
                                  labelscale = 0,
                                  outfile = file.path("Data/Turbines/",
                                    "Maine Wind Projects.kml"),
                                  metadata="Data/Turbines/Project_records.csv"){
  PointFunction <- function(project_name, developer, status, model,
                             num_turbine, total_mw, hub_height, rotor_diam,
                             rotor_top, rotor_btm, X, Y) {
    cat("\t<Placemark>\n",
        "\t\t<name>",project_name,"</name>\n",
        "<visibility>1</visibility>\n",
        "\t\t\t<Snippet></Snippet>", "\n",
        "\t\t\t\t<description>\n",
        "\t\t\t\t\tDeveloper: ",developer, "\n",
        "\t\t\t\t\tStatus: ",status, "\n",
        "\t\t\t\t\tTurbine Models: ",model, "\n",
        "\t\t\t\t\tNumber of Turbines: ",num_turbine, "\n",
        "\t\t\t\t\tTotal MW: ",total_mw, "\n",
        "\t\t\t\t\tHub Height: ",hub_height, " meters\n",
        "\t\t\t\t\tRotor Diameter: ",rotor_diam," meters\n",
        "\t\t\t\t\tRotor Sweep Top: ",rotor_top, " meters\n",
        "\t\t\t\t\tRotor Sweep Bottom: ",rotor_btm, " meters\n",
        "\t\t\t\t\tLongitude: ",X, "\n",
        "\t\t\t\t\tLatitude: ",Y, "\n",
        "\t\t\t\t</description>\n",
        "\t\t\t<styleUrl>#Style_",status,"</styleUrl>\n",
        "\t\t\t<Point>\n",
        "\t\t\t\t<extrude>0</extrude>\n",
        "\t\t\t\t<altitudeMode>clampedtoGround</altitudeMode>\n",
        "\t\t\t\t\t<coordinates>",X,",",Y,"0 </coordinates>\n",
        "\t\t\t</Point>\n",
        "\t</Placemark>\n",
        file = outfile, append = TRUE, sep = "")
  }
  if (file.exists(outfile)) file.remove(outfile)  # overwrite KML file
  writeLines(noquote(c("Writing: ",outfile)))  # print "Writing ... .kml"
  ## Title Section ##
  cat("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
      "<kml xmlns=\"http://www.opengis.net/kml/2.2\"\n",
      "xmlns:kml=\"http://www.opengis.net/kml/2.2\"\n",
      "xmlns:gx=\"http://www.google.com/kml/ext/2.2\"\n",
      "xmlns:atom=\"http://www.w3.org/2005/Atom\">\n\n",
      "<Document>\n",
      "\t<name>Maine Wind Projects</name>\n",
      file = outfile, append = FALSE, sep = "")
  ## Icon Style Section ##
  metadata<-read.csv(metadata, header=TRUE, as.is=TRUE)
  for (i in 1:nrow(metadata)){
    row <- i
    sn<-metadata[row,]
    status <- sn[, "status"]
    icon_label_scale <- sn[, "icon_label_scale"]
    icon_color <-sn[, "icon_color"]
    icon_scale <-sn[, "icon_scale"]
    icon_href <-as.character(sn[, "icon_href"])
    icon_ball_bg_color <- sn[, "icon_ball_bg_color"]
    icon_ball_text_color <- sn[, "icon_ball_text_color"]
    hi_icon_label_scale <- sn[, "hi_icon_label_scale"]
    hi_icon_color <- sn[, "hi_icon_color"]
    hi_icon_scale <- sn[, "hi_icon_scale"]
    hi_icon_href <- sn[, "hi_icon_href"]
    hi_icon_ball_bg_color <- sn[, "hi_icon_ball_bg_color"]
    hi_icon_ball_text_color <- sn[, "hi_icon_ball_text_color"]
    cat("\t<StyleMap id=\"Style_",status,"\">\n",
        "\t\t<Pair>\n",
        "\t\t\t<key>normal</key>\n",
        "\t\t\t\t<Style>\n",
        "\t\t\t\t\t<LabelStyle>\n",
        "\t\t\t\t\t<scale>", icon_label_scale, "</scale>\n",  # to show label
        "\t\t\t\t\t</LabelStyle>\n",
        "\t\t\t\t\t<IconStyle>\n",
        "\t\t\t\t\t\t<color>", icon_color, "</color>\n",
        "\t\t\t\t\t\t<scale>", icon_scale, "</scale>\n",
        "\t\t\t\t\t<Icon>\n",
        "\t\t\t\t\t<href>", icon_href, "</href>\n",
        "\t\t\t\t\t</Icon>\n",
        "\t\t\t\t\t</IconStyle>\n",
        "\t\t\t\t\t<BalloonStyle>\n",
        "\t\t\t\t\t<text>$[description]</text>\n",
        "\t\t\t\t\t\t<bgColor>", icon_ball_bg_color, "</bgColor>\n",
        "\t\t\t\t\t\t<textColor>", icon_ball_text_color, "</textColor>\n",
        "\t\t\t\t\t</BalloonStyle>\n",
        "\t\t\t\t</Style>\n",
        "\t\t</Pair>\n",
        "\t\t<Pair>\n",
        "\t\t\t<key>highlight</key>\n",
        "\t\t\t\t<Style>\n",
        "\t\t\t\t\t<LabelStyle>\n",
        "\t\t\t\t\t<scale>", hi_icon_label_scale, "</scale>\n",  # to show label
        "\t\t\t\t\t</LabelStyle>\n",
        "\t\t\t\t\t<IconStyle>\n",
        "\t\t\t\t\t\t<color>", hi_icon_color, "</color>\n",
        "\t\t\t\t\t\t<scale>", hi_icon_scale, "</scale>\n",
        "\t\t\t\t\t<Icon>\n",
        "\t\t\t\t\t<href>", hi_icon_href, "</href>\n",
        "\t\t\t\t\t</Icon>\n",
        "\t\t\t\t\t</IconStyle>\n",
        "\t\t\t\t\t<BalloonStyle>\n",
        "\t\t\t\t\t<text>$[description]</text>\n",
        "\t\t\t\t\t\t<bgColor>", hi_icon_ball_bg_color, "</bgColor>", "\n",
        "\t\t\t\t\t\t<textColor>", hi_icon_ball_text_color, "</textColor>","\n",
        "\t\t\t\t\t</BalloonStyle>\n",
        "\t\t\t\t</Style>\n",
        "\t\t</Pair>\n",
        "\t</StyleMap>\n",
        file = outfile, append = TRUE, sep = "")
  }
  ## For Loop Section ##
  df <- read.csv(df, header=TRUE, as.is=TRUE)
  df <- subset(df, show == 1)  # only include projects with 1 in "show" column
  status <- unique(df$status)
  for (i in status){
    sv = df$status %in% i
    status <- as.character(unique(df$status[sv]))
    cat("<Folder>\n",
        "<name>", status, "</name>\n",
        "<open>0</open>\n",
        file = outfile, append = TRUE, sep = "")
    projects <- subset(df, status==i)
    for (i in 1:nrow(projects)){
      row <- i
      project <- projects[row,]
      project_names <- project[, "project"]
      developers <- project[, "developer"]
      statuses <- project[, "status"]
      models <- project [, "turb_model"]
      num_turbines <- project [, "num_turbine"]
      total_mws <- project [, "total_mw"]
      hub_heights <- project[, "hub_ht_m"]
      rotor_diams <- project [, "rotor_diam"]
      rotor_tops <- project [, "rotor_top"]
      rotor_btms <- project [, "rotor_btm"]
      Xs <- project [, "long"]
      Ys <- project [, "lat"]
      PointFunction(project_names, developers, statuses, models, num_turbines,
                     total_mws, hub_heights, rotor_diams, rotor_tops,
                     rotor_btms, Xs, Ys)
    }
    cat("</Folder>\n", file = outfile, append = TRUE, sep = "")
  }
  cat("</Document>\n</kml>", file = outfile, append = TRUE)
}

#' ExportKMLWindTurbines
#'
#' Exports KML of wind turbine locations
#'
#' @usage ExportKMLWindTurbines(df, labelscale, outfile, metadata)
#' @param df the location of the .csv file
#' @param labelscale adjusts the size of the Google Earth location point
#'   labels. Default is 0, which hides the labels. To show labels, change to a
#'   value between 0.7-1.
#' @param outfile location of output KML file
#' @param metadata ocation of metadata .csv file. Metadata file must contain
#'   columns for project ID, hexadecimal colors, and additional icon data.
#'
#' @return creates KML
#' @export
#'
#' @details Defaults are specific to my file directories and locations
#'
ExportKMLWindTurbines <- function (df = "Data/Turbines/Maine Wind Turbines.csv",
                                   labelscale = 0,
                                   outfile = file.path("C:/Users/Blake/Desktop",
                                     "Maine Wind Turbines.kml"),
                                   metadata = file.path("Data/Turbines",
                                     "Turbine_records.csv")) {
  PointFunction <- function(plname, X, Y, pad, model, hub, rotor, sweep_top,
                             sweep_btm, XP, XN, YP, YN) {
    cat("\t<Placemark>\n",
        "\t\t<name>Turbine ",plname,"</name>\n",
        "<visibility>1</visibility>\n",
        "\t\t\t<Snippet></Snippet>", "\n",
        "\t\t\t\t<description>\n",
        "\t\t\t\t\tTurbine ",plname,"\n",
        "\t\t\t\t\tLongitude: ",X, "\n",
        "\t\t\t\t\tLatitude: ",Y, "\n",
        "\t\t\t\t\tPad Elevation: ",pad, " meters\n",
        "\t\t\t\t\tModel: ",model, "\n",
        "\t\t\t\t\tHub Height: ",hub, " meters\n",
        "\t\t\t\t\tRotor Diameter: ",rotor," meters\n",
        "\t\t\t\t\tRotor Sweep Top: ",sweep_top, " meters\n",
        "\t\t\t\t\tRotor Sweep Bottom: ",sweep_btm, " meters\n",
        "\t\t\t\t</description>\n",
        "\t\t\t<styleUrl>#Style_",model,"</styleUrl>\n",
        "\t\t\t<MultiGeometry>\n",
        "\t\t\t<Model>\n",
        "\t\t\t\t<range>500</range>\n",
        "\t\t\t\t<altitudeMode>clampedToGround</altitudeMode>\n",
        "\t\t\t\t<Location>\n",
        "\t\t\t\t\t<longitude>",X,"</longitude>\n",
        "\t\t\t\t\t<latitude>",Y,"</latitude>\n",
        "\t\t\t\t\t<altitude>0</altitude>\n",
        "\t\t\t\t</Location>\n",
        "\t\t\t\t<Orientation>\n",
        "\t\t\t\t\t<heading>120</heading>\n",
        "\t\t\t\t\t<tilt>0</tilt>\n",
        "\t\t\t\t\t<roll>0</roll>\n",
        "\t\t\t\t</Orientation>\n",
        "\t\t\t\t<Scale>\n",
        "\t\t\t\t\t<x>1</x>\n",
        "\t\t\t\t\t<y>1</y>\n",
        "\t\t\t\t\t<z>1</z>\n",
        "\t\t\t\t</Scale>\n",
        "\t\t\t\t<Link>\n",
        "\t\t\t\t\t<href>C:/ArcGIS/Data/Wind/Turbine Models/Collada Models/",
          model,".dae</href>\n",
        "\t\t\t\t</Link>\n",
        "\t\t\t</Model>\n",
        "\t\t\t<Polygon>\n",
        "\t\t\t\t<tesselate>1</tesselate>\n",
        "\t\t\t\t\t<outerBoundaryIs>\n",
        "\t\t\t\t\t\t<LinearRing>\n",
        "\t\t\t\t\t\t\t<coordinates>\n",
        "\t\t\t\t\t\t\t\t",XN,",",YP,",0 ",XN,",",YN,",0 ", XP,",",YN,",0 ",XP,
          ",",YP,",0 ",XN,",",YP,",0 \n",
        "\t\t\t\t\t\t\t</coordinates>\n",
        "\t\t\t\t\t\t</LinearRing>\n",
        "\t\t\t\t\t</outerBoundaryIs>\n",
        "\t\t\t</Polygon>\n",
        "\t\t\t</MultiGeometry>\n",
        "\t</Placemark>\n",
        file = outfile, append = TRUE, sep = "")
  }
  if (file.exists(outfile)) file.remove(outfile)  # overwrites KML file
  writeLines(noquote(c("Writing: ",outfile)))  # print "Writing ... .kml"
  ## Title Section ##
  cat("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
      "<kml xmlns=\"http://www.opengis.net/kml/2.2\"\n",
      "xmlns:kml=\"http://www.opengis.net/kml/2.2\"\n",
      "xmlns:gx=\"http://www.google.com/kml/ext/2.2\"\n",
      "xmlns:atom=\"http://www.w3.org/2005/Atom\">\n\n",
      "<Document>\n",
      "\t<name>Maine Wind Turbines</name>\n",
      file = outfile, append = FALSE, sep = "")
  ## Icon Style Section ##
  metadata <- read.csv(metadata, header=TRUE, as.is=TRUE)
  for (i in 1:nrow(metadata)){
    row <- i
    sn <- metadata[row,]
    id <- sn[, "id"]
    model <- sn[, "model"]
    icon_label_scale <- sn[, "icon_label_scale"]
    icon_ball_bg_color <- sn[, "icon_ball_bg_color"]
    icon_ball_text_color <- sn[, "icon_ball_text_color"]
    hi_icon_label_scale <- sn[, "hi_icon_label_scale"]
    hi_icon_ball_bg_color <- sn[, "hi_icon_ball_bg_color"]
    hi_icon_ball_text_color <- sn[, "hi_icon_ball_text_color"]
    cat("\t<StyleMap id=\"Style_", model, "\">\n",
        "\t\t<Pair>\n",
        "\t\t\t<key>normal</key>\n",
        "\t\t\t\t<Style>\n",
        "\t\t\t\t\t<LabelStyle>\n",
        "\t\t\t\t\t<scale>", icon_label_scale, "</scale>\n",  # to show label
        "\t\t\t\t\t</LabelStyle>\n",
        "\t\t\t\t\t<IconStyle>\n",
        "\t\t\t\t\t\t<color>ff00aaff</color>\n",
        "\t\t\t\t\t\t<scale>1</scale>\n",
        "\t\t\t\t\t\t<Icon>\n",
        "\t\t\t\t\t\t<href>",
          "http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>\n",
        "\t\t\t\t\t\t</Icon>\n",
        "\t\t\t\t\t</IconStyle>\n",
        "\t\t\t\t\t<BalloonStyle>\n",
        "\t\t\t\t\t<text>$[description]</text>\n",
        "\t\t\t\t\t\t<bgColor>", icon_ball_bg_color, "</bgColor>\n",
        "\t\t\t\t\t\t<textColor>", icon_ball_text_color, "</textColor>\n",
        "\t\t\t\t\t</BalloonStyle>\n",
        "\t\t\t\t\t<LineStyle>\n",
        "\t\t\t\t\t\t<color>01ffffff</color>\n",
        "\t\t\t\t\t</LineStyle>\n",
        "\t\t\t\t\t<PolyStyle>\n",
        "\t\t\t\t\t\t<color>01ffffff</color>\n",
        "\t\t\t\t\t</PolyStyle>\n",
        "\t\t\t\t</Style>\n",
        "\t\t</Pair>\n",
        "\t\t<Pair>\n",
        "\t\t\t<key>highlight</key>\n",
        "\t\t\t\t<Style>\n",
        "\t\t\t\t\t<LabelStyle>\n",
        "\t\t\t\t\t<scale>", hi_icon_label_scale, "</scale>\n", # to show label
        "\t\t\t\t\t</LabelStyle>\n",
        "\t\t\t\t\t<IconStyle>\n",
        "\t\t\t\t\t\t<color>ff00aaff</color>\n",
        "\t\t\t\t\t\t<scale>1</scale>\n",
        "\t\t\t\t\t\t<Icon>\n",
        "\t\t\t\t\t\t<href>",
          "http://maps.google.com/mapfiles/kml/shapes/shaded_dot.png</href>\n",
        "\t\t\t\t\t\t</Icon>\n",
        "\t\t\t\t\t</IconStyle>\n",
        "\t\t\t\t\t<BalloonStyle>\n",
        "\t\t\t\t\t<text>$[description]</text>\n",
        "\t\t\t\t\t\t<bgColor>", hi_icon_ball_bg_color, "</bgColor>", "\n",
        "\t\t\t\t\t\t<textColor>", hi_icon_ball_text_color,"</textColor>", "\n",
        "\t\t\t\t\t</BalloonStyle>\n",
        "\t\t\t\t\t<LineStyle>\n",
        "\t\t\t\t\t\t<color>01ffffff</color>\n",
        "\t\t\t\t\t</LineStyle>\n",
        "\t\t\t\t\t<PolyStyle>\n",
        "\t\t\t\t\t\t<color>01ffffff</color>\n",
        "\t\t\t\t\t</PolyStyle>\n",
        "\t\t\t\t</Style>\n",
        "\t\t</Pair>\n",
        "\t</StyleMap>\n",
        file = outfile, append = TRUE, sep = "")
  }
  ## For Loop Section ##
  df <- read.csv(df, header=TRUE, as.is=TRUE)
  developer <- unique(df$developer)
  for (i in developer){
    sv = df$developer %in% i
    developer <- as.character(unique(df$developer[sv]))
    cat("<Folder>\n",
        "<name>", developer, "</name>\n",
        "<open>0</open>\n",
        file = outfile, append = TRUE, sep = "")
    projects <- subset(df, developer==i)
    project <- unique(projects$project)
    for (i in project){
      sv = projects$project %in% i
      project_name <- as.character(unique(projects$project[sv]))
      cat("<Folder>\n",
          "<name>", project_name, " Project</name>\n",
          "<open>0</open>\n",
          file = outfile, append = TRUE, sep = "")
      turbines <- subset(projects, project==i)
      for (i in 1:nrow(turbines)){
        row <- i
        turbine <- turbines[row,]
        plnms <- turbine[, "turbine_id"]
        Xs <- turbine[, "long"]
        XPs <- Xs + .001
        XNs <- Xs - .001
        Ys <- turbine[, "lat"]
        YPs <- Ys +.001
        YNs <- Ys -.001
        pads <- turbine[, "pad_elev_m"]
        models <- turbine[, "model"]
        hubs <- turbine[, "hub_ht_m"]
        rotors <- turbine [, "rotor_diam"]
        sweep_tops <- turbine [, "sweep_top"]
        sweep_btms <- turbine [, "sweep_btm"]
        PointFunction(plnms, Xs, Ys, pads, models, hubs, rotors, sweep_tops,
          sweep_btms, XPs, XNs, YPs, YNs)
      }
      cat("</Folder>\n", file = outfile, append = TRUE, sep = "")
    }
    cat("</Folder>\n", file = outfile, append = TRUE, sep = "")
  }
  cat("</Document>\n</kml>", file = outfile, append = TRUE)
}

#' Creates or updates .kml files of each individual by year
#'
#' Creates and updates year KML files on local and GDrive folders.
#'
#' @usage ExportKMLPolygon(df, update_year, update_gdrive)
#'
#' @param df Dataframe of location data.
#' @param update_year  integer, year to update. Default is NULL, which means all
#'     years are updated.
#' @param update_gdrive Logical, whether to update the files on the GDrive.
#'     Default is TRUE.
#' @return Creates .kml files in "Data/GPS" and copies them to
#'     "Google Drive/BAEA Project/Telemetry Data/Individuals" folders.
#' @export
UpdateIndByYearKMLs <- function(df,
                                update_year = NULL,
                                update_gdrive = TRUE){
  # Update local individual files
  df <- df
  ids <- unique(df$id)
  for (i in 1:length(ids)){
    id <- ids[i]
    df_i <- df[df$id == id, ]
    if(!is.null(update_year)){
      df_year <- df_i[df_i$year == update_year, ]
      if (nrow(df_year) > 1){
        ExportKMLTelemetryBAEA(df_year, file = paste0(id, "_", update_year,
          ".kml"), output_dir = "Data/GPS/KMLs")
      }
    } else {
      start_year <- lubridate::year(first(df_i$datetime))
      end_year <- lubridate::year(last(df_i$datetime))
      years <- start_year:end_year
      for (j in 1:length(years)){
        year <- years[j]
        df_year <- df_i[df_i$year == year, ]
        if (nrow(df_year) > 1){
          ExportKMLTelemetryBAEA(df_year, file = paste0(id, "_", year, ".kml"),
            output_dir = "Data/GPS/KMLs")
        }
      }
    }
  }
  if (update_gdrive == TRUE){
    # Copy individual files to Google Drive
    kml_files <- list.files("Data/GPS/KMLs", full.names=TRUE)
    if (!is.null(update_year)) kml_files <- stringr::str_subset(kml_files,
      as.character(update_year))
    output_dir = file.path("C:/Users/Blake/Google Drive/PhD Program",
      "BAEA Project/Telemetry Data/Individuals")
    file.copy(kml_files, output_dir, overwrite=TRUE)
  }
}

#' Zips KML
#'
#' Zip a kml file
#'
#' @param kml filename for RasterStack or RasterBrick
#' @param image filename for image file
#'
#' @return writes a zip file from a kml
#' @export
#'
#' @details Based on function from 'raster' package
#'
ZipKML <- function(kml,
                   image) {
  suppressPackageStartupMessages(require(raster))
	wd <- getwd()
	on.exit(setwd(wd))
 	setwd(dirname(kml))
	kml <- basename(kml)
	kmz <- extension(kml, '.kmz')
	image <- basename(image)
	if (file.exists(kmz)) {
		x <- file.remove(kmz)
	}
	kmzzip <- extension(kmz, '.zip')
	cmd <- paste('7z', 'a', kmzzip, kml, image, collapse=" ")
  sss <- try(system(cmd, intern=TRUE), silent=TRUE)
  file.rename(kmzzip, kmz)
  if (file.exists(kmz)) {
		file.remove(kml, image)
		return(invisible(kmz))
	} else {
		return(invisible(kml))
	}
}

