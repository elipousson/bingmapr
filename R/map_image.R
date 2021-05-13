#' Get and plot Bing Static Maps
#'
#' See the documentation on Bing Static Maps for reference: https://docs.microsoft.com/en-us/bingmaps/rest-services/imagery/get-a-static-map
#'
#' Get API key from https://www.bingmapsportal.com/
#'
#' @param location PARAM_DESCRIPTION, sf object (centroid point is used for location) or numeric vector in format c(latitude, longitude)
#' @param query String with query for location. Ignored if a location is provided.
#' @param imagery String with imagery type, Default: 'BirdsEye' Supported values include:
#' - Aerial: Aerial imagery.
#' - AerialWithLabels: Aerial imagery with a road overlay.
#' - AerialWithLabelsOnDemand: Aerial imagery with on-demand road overlay.
#' - Streetside: Street-level imagery.
#' - BirdsEye: Birds Eye (oblique-angle) imagery.
#' - BirdsEyeWithLabels: Birds Eye (oblique-angle) imagery with a road overlay.
#' - Road: Roads without additional imagery.
#' - CanvasDark: A dark version of the road maps.
#' - CanvasLight: A lighter version of the road maps which also has some of the details such as hill shading disabled.
#' - CanvasGray: A grayscale version of the road maps
#' @param mapsize Numeric vector with width and height in pixels, Default: c(600, 400) 600px wide, 400px high
#' @param zoom Numeric vector between 0 and 20 for imagery other than Bird's Eye maps or 18 to 22 for Bird's Eye maps. Default: 18
#' @param orientation Character string with orientation  ("N", "E", "S", "W") or numeric direction (0,90,180,270), Default: 0
#' @param key Bing Maps API Key, Default: Sys.getenv("BING_MAPS_API_KEY")
#' @name bing_static_map
#' @md
NULL

#' @return get_request_url returns the request URL for the Static Map API
#' @rdname bing_static_map
#' @importFrom sf st_centroid st_transform st_coordinates
#' @importFrom utils URLencode
get_request_url <- function(location = NULL,
                            query = NULL,
                            imagery = "BirdsEye",
                            mapsize = c(600, 400),
                            zoom = 18,
                            orientation = 0,
                            key = Sys.getenv("BING_MAPS_API_KEY")) {
  imagery_options <- c("Aerial", "AerialWithLabels", "AerialWithLabelsOnDemand", "Streetside", "BirdsEye", "BirdsEyeWithLabels", "Road", "CanvasDark", "CanvasLight", "CanvasGray")
  imagery <- match.arg(imagery, imagery_options)

  if (is.null(location) && is.character(query) && !(imagery %in% imagery_options[5:6])) {
    location <- utils::URLencode(query)
  } else if (is.null(location)) {
    stop("location must be provided to use the Bird's Eye imagery types. The query parameter is not supported.")
  }

  if ("sf" %in% class(location)) {
    location <- suppressWarnings(sf::st_centroid(location))
    location <- sf::st_coordinates(sf::st_transform(location, 4326))
    location <- rev(c(location))
  }

  location <- paste(location, collapse = ",")

  mapsize <- paste(mapsize, collapse = ",")

  if ((imagery %in% imagery_options[5:6]) && !(zoom %in% c(18:22))) {
    zoom <- 22
  }

  if (is.character(orientation)) {
    orientation <- match.arg(orientation, c("N", "E", "S", "W"))
    orientation <- switch(orientation,
      "N" = 0,
      "E" = 90,
      "S" = 180,
      "W" = 270
    )
  } else {
    orientation <- match.arg(as.character(orientation), c(0, 90, 180, 270))
  }

  base <- "https://dev.virtualearth.net/REST/V1/Imagery/Map"

  path <- paste(base, imagery, location, zoom, sep = "/")

  orientation <- paste0("dir=", orientation)
  mapsize <- paste0("ms=", mapsize)
  key <- paste0("key=", key)

  query_string <-
    paste(orientation, mapsize, key, sep = "&")

  path <-
    paste0(path, "?", query_string)

  return(path)
}

#' @return get_map_image returns a raster array with JPEG file/content for the map
#' @rdname bing_static_map
#' @importFrom jpeg readJPEG
#' @importFrom RCurl getURLContent
get_map_image <- function(location = NULL,
                          query = NULL,
                          imagery = "BirdsEye",
                          mapsize = c(600, 400),
                          zoom = 18,
                          orientation = 0,
                          key = Sys.getenv("BING_MAPS_API_KEY")) {
  path <- get_request_url(
    location,
    query,
    imagery,
    mapsize,
    zoom,
    orientation,
    key
  )

  map <- jpeg::readJPEG(RCurl::getURLContent(path))

  return(map)
}

#' @return plot_map plots the static map image
#' @rdname bing_static_map
#' @importFrom graphics rasterImage
plot_map_image <- function(location = NULL,
                           query = NULL,
                           imagery = "BirdsEye",
                           mapsize = c(600, 400),
                           zoom = 18,
                           orientation = 0,
                           key = Sys.getenv("BING_MAPS_API_KEY")) {
  map <- get_map_image(
    location,
    query,
    imagery,
    mapsize,
    zoom,
    orientation,
    key
  )

  # from https://stackoverflow.com/questions/9543343/plot-a-jpg-image-using-base-graphics-in-r#28729601
  map_res <- dim(map)[2:1] # get the resolution, [x, y]
  plot(1, 1, xlim = c(1, map_res[1]), ylim = c(1, map_res[2]), asp = 1, type = "n", xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
  graphics::rasterImage(map, 1, 1, map_res[1], map_res[2])
}
