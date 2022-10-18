.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Use of the Bing Maps APIs is governed by the Microsoft Bing Maps Platform APIs Terms Of Use.\nPlease visit https://www.microsoft.com/en-us/maps/product/ for more information.")
}

#' Get and plot Bing Static Maps
#'
#' See the documentation on Bing Static Maps for reference:
#' <https://docs.microsoft.com/en-us/bingmaps/rest-services/imagery/get-a-static-map>
#'
#' Get API key from <https://www.bingmapsportal.com/>
#'
#' @param location A `sf`, `sfc`, or `bbox` object (centroid point is used for
#'   location) or numeric vector in format `c(latitude, longitude)`. Defaults to
#'   `NULL`.
#' @param query String with query for location. query is ignored if a location
#'   is provided.  Defaults to `NULL`.
#' @param imagery String with imagery type, Default: 'BirdsEye' Supported values
#'   include:
#' - Aerial: Aerial imagery.
#' - AerialWithLabels: Aerial imagery with a road overlay.
#' - AerialWithLabelsOnDemand: Aerial imagery with on-demand road overlay.
#' - Streetside: Street-level imagery.
#' - BirdsEye: Birds Eye (oblique-angle) imagery.
#' - BirdsEyeWithLabels: Birds Eye (oblique-angle) imagery with a road overlay.
#' - Road: Roads without additional imagery.
#' - CanvasDark: A dark version of the road maps.
#' - CanvasLight: A lighter version of the road maps which also has some of the
#' details such as hill shading disabled.
#' - CanvasGray: A grayscale version of the road maps
#' @param width,height,mapsize Width and height in pixels or use mapsize to
#'   provide a vector of c(width, height). If mapsize is provided, width and
#'   height are ignored. Default: 600px width, 400px height, mapsize is `NULL`.
#' @param zoom Numeric vector between 0 and 20 for imagery other than Bird's Eye
#'   maps or 18 to 22 for Bird's Eye maps. Default: 18
#' @param orientation Orientation as a character string ("N", "E", "S", "W") or
#'   length 1 numeric vector (0,90,180,270). Other numeric orientations (from
#'   -360 to 720) are matched to the closest value, e.g. 35 to 0 or 75 to 90.
#'   Default: 0
#' @param nudge Numeric vector in the format, `c(meters to shift latitude,
#'   meters to shift longitude)`, e.g. `c(100, 0)` to shift center 100 meters in
#'   latitude.  Defaults to `NULL`.
#' @param bbox If `TRUE`, return a `bbox` class object based on the bounding box
#'   values from the map metadata. If `FALSE` (default), return the full JSON
#'   metadata. ([get_map_meta()] only)
#' @param key Bing Maps API Key, Default: `Sys.getenv("BING_MAPS_API_KEY")`
#' @param check If `TRUE`, check the map metadata which returns an error if the
#'   image is unavailable (Default `FALSE` for [get_request_url()] and `TRUE`
#'   for [get_map_image()])
#' @inheritParams magick::image_read
#' @name bing_static_map
#' @md
NULL

#' @return get_request_url returns the request URL for the Static Map API
#' @rdname bing_static_map
#' @export
#' @importFrom utils URLencode
#' @importFrom jsonlite read_json
get_request_url <- function(location = NULL,
                            query = NULL,
                            imagery = "BirdsEye",
                            width = 600,
                            height = 400,
                            mapsize = NULL,
                            zoom = 18,
                            orientation = 0,
                            nudge = NULL,
                            key = Sys.getenv("BING_MAPS_API_KEY"),
                            check = FALSE) {
  imagery_options <-
    c(
      "Aerial", "AerialWithLabels", "AerialWithLabelsOnDemand",
      "Streetside",
      "BirdsEye", "BirdsEyeWithLabels",
      "Road",
      "CanvasDark", "CanvasLight", "CanvasGray"
    )

  imagery <-
    match.arg(
      imagery,
      imagery_options
    )

  if (is.null(location) &&
    is.character(query) &&
    !(imagery %in% imagery_options[5:6])) {
    location <- utils::URLencode(query)
  } else if (is.null(location)) {
    stop("location must be provided to use the Bird's Eye imagery types.
         The query parameter is not supported.")
  }

  if (inherits(location, c("sfc", "sf", "bbox"))) {
    location <- sf_to_coords(location)
  }

  if (is.numeric(nudge)) {
    location <- nudge_location(location, nudge)
  }

  location <- paste(location, collapse = ",")

  if (is.null(mapsize)) {
    mapsize <- c(width, height)
  }

  stopifnot(
    is.numeric(mapsize) && (length(mapsize) == 2)
  )

  mapsize <- paste(mapsize, collapse = ",")

  if (is.null(zoom)) {
    zoom <- 18
  }

  if ((imagery %in% imagery_options[5:6])) {
    zoom_default <- NULL

    if (zoom < 18) {
      zoom_default <- 18
    } else if (zoom > 22) {
      zoom_default <- 22
    }

    if (!is.null(zoom_default)) {
      zoom <- zoom_default
      warning(
        paste0(imagery,
        " imagery only supports zoom levels between 18 and 22.",
        "\nSetting zoom to ", zoom_default, ".")
      )
    }
  }

  if (is.numeric(orientation)) {
    if ((orientation > 360) && (orientation <= 720)) {
      orientation <- orientation - 360
    } else if ((orientation < 0) && (orientation >= -360)) {
      orientation <- 360 + orientation
    }

    if (orientation <= 45) {
      orientation <- "N"
    } else if (orientation <= 135) {
      orientation <- "E"
    } else if (orientation <= 225) {
      orientation <- "S"
    } else if (orientation <= 360) {
      orientation <- "W"
    }
  }

  orientation <- match.arg(orientation, c("N", "E", "S", "W"))
  orientation <-
    switch(orientation,
      "N" = 0,
      "E" = 90,
      "S" = 180,
      "W" = 270
    )

  base <- "https://dev.virtualearth.net/REST/V1/Imagery/Map"

  path <- paste(base, imagery, location, zoom, sep = "/")

  orientation <- paste0("dir=", orientation)
  mapsize <- paste0("ms=", mapsize)

  if (is.null(key)) {
    key <- Sys.getenv("BING_MAPS_API_KEY")
  }

  stopifnot(
    "A valid Bing Maps API key is required" = is.character(key) && (key != "" )
  )

  key <- paste0("key=", key)

  query_string <-
    paste(orientation, mapsize, key, sep = "&")

  path <-
    paste0(path, "?", query_string)

  if (check) {
    meta <- jsonlite::read_json(paste0(path, "&mapMetadata=1"))
  }

  path
}

#' @return get_map_image returns an image from `magick::image_read`
#' @rdname bing_static_map
#' @export
#' @importFrom RCurl getURLContent
#' @importFrom magick image_read
get_map_image <- function(location = NULL,
                          query = NULL,
                          imagery = "BirdsEye",
                          width = 600,
                          height = 400,
                          mapsize = NULL,
                          zoom = 18,
                          orientation = 0,
                          nudge = NULL,
                          key = Sys.getenv("BING_MAPS_API_KEY"),
                          check = TRUE,
                          strip = TRUE) {
  path <-
    get_request_url(
      location = location,
      query = query,
      imagery = imagery,
      width = width,
      height = height,
      mapsize = mapsize,
      zoom = zoom,
      orientation = orientation,
      nudge = nudge,
      key = key,
      check = check
    )

  magick::image_read(RCurl::getURLContent(path), strip = strip)
}


#' @return get_map_meta returns the JSON with the map metadata or a bbox for the
#'   map area
#' @rdname bing_static_map
#' @export
#' @importFrom jsonlite read_json
#' @importFrom sf st_bbox st_crs
get_map_meta <- function(location = NULL,
                         query = NULL,
                         imagery = "BirdsEye",
                         width = 600,
                         height = 400,
                         mapsize = NULL,
                         zoom = 18,
                         orientation = 0,
                         nudge = NULL,
                         key = Sys.getenv("BING_MAPS_API_KEY"),
                         bbox = FALSE) {
  path <-
    get_request_url(
      location = location,
      query = query,
      imagery = imagery,
      width = width,
      height = height,
      mapsize = mapsize,
      zoom = zoom,
      orientation = orientation,
      nudge = nudge,
      key = key
    )

  meta <- jsonlite::read_json(paste0(path, "&mapMetadata=1"))

  if (bbox) {
    bbox <- sf::st_bbox(
      c(
        ymin = meta$resourceSets[[1]]$resources[[1]]$bbox[[1]],
        xmin = meta$resourceSets[[1]]$resources[[1]]$bbox[[2]],
        ymax = meta$resourceSets[[1]]$resources[[1]]$bbox[[3]],
        xmax = meta$resourceSets[[1]]$resources[[1]]$bbox[[4]]
      ),
      crs = sf::st_crs(4326)
    )

    return(bbox)
  }

  meta
}
