.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Use of the Bing Maps APIs is governed by the Microsoft Bing Maps Platform APIs Terms Of Use.\nPlease visit https://www.microsoft.com/en-us/maps/product/ for more information.")
}

#' @noRd
imagery_options <-
  c(
    "Aerial", "AerialWithLabels", "AerialWithLabelsOnDemand",
    "Streetside",
    "BirdsEye", "BirdsEyeWithLabels",
    "Road",
    "CanvasDark", "CanvasLight", "CanvasGray"
  )

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
#'   image is unavailable. Default `FALSE`. Not currently supported.
#' @param .perform If `TRUE`, return results from [httr2::req_perform()]. If
#'   `FALSE`, return request.
#' @param ... Additional parameters passed to [httr2::req_url_query()]
#' @inheritParams magick::image_read
#' @name bing_static_map
#' @md
NULL

#' @return get_request_url returns the request URL for the Static Map API
#' @rdname bing_static_map
#' @export
#' @importFrom httr2 request req_url_path_append req_url_query req_user_agent
#'   req_error resp_body_json req_perform
req_bingmapr <- function(location = NULL,
                         query = NULL,
                         imagery = "BirdsEye",
                         width = 600,
                         height = 400,
                         mapsize = NULL,
                         zoom = 18,
                         orientation = 0,
                         nudge = NULL,
                         key = Sys.getenv("BING_MAPS_API_KEY"),
                         check = FALSE,
                         .perform = TRUE,
                         ...) {
  req <-
    httr2::request(
      "https://dev.virtualearth.net/REST/V1/Imagery/Map"
    )

  imagery <-
    match.arg(
      imagery,
      imagery_options
    )

  req <-
    httr2::req_url_path_append(
      req,
      imagery,
      get_location_or_query(location, query, nudge, imagery),
      get_zoom_for_imagery(zoom, imagery)
    )

  req <-
    httr2::req_url_query(
      req,
      dir = get_dir_from_orientation(orientation),
      ms = get_ms_from_mapsize(width, height, mapsize),
      key = get_bing_maps_api_key(key),
      ...
    )

  req <-
    httr2::req_user_agent(
      req,
      "bingmapr (https://github.com/elipousson/bingmapr)"
    )

  if (check) {
    req <-
      httr2::req_error(
        req,
        body = function(resp) {
          details <- httr2::resp_body_json(resp)$errorDetails[[1]]
          if (details == "The zoom level is not between 0 and 22, or there is no Birdseye imagery at the specified location.") {
            return("There is no Birdseye imagery at the specified location.")
          } else {
            details
          }
        }
      )
  }


  if (!.perform) {
    return(req)
  }

  httr2::req_perform(req)
}


#' Convert numeric or character vector for orientation into dir query parameter
#' @noRd
get_location_or_query <- function(location = NULL,
                                  query = NULL,
                                  nudge = NULL,
                                  imagery = NULL) {
  if (is.null(location)) {
    if (is.character(query) && !(imagery %in% imagery_options[5:6])) {
      location <- query
    } else {
      stop(
        paste0(
          "location must be provided to use the Bird's Eye imagery types.",
          "\nThe query parameter is not supported."
        )
      )
    }
  } else {
    if (inherits(location, c("sfc", "sf", "bbox"))) {
      location <- sf_to_coords(location)
    }
    stopifnot(
      "location must be a numeric coordinate pair or a sf, sfc, or bbox object." = is.numeric(location)
    )

    if (is.numeric(nudge)) {
      location <- nudge_location(location, nudge)
    }
  }

  paste0(location, collapse = ",")
}

#' @noRd
get_ms_from_mapsize <- function(width = NULL,
                                height = NULL,
                                mapsize = NULL) {
  if (!is.null(width) && !is.null(height)) {
    mapsize <- c(width, height)
  }

  mapsize <- round(as.numeric(mapsize))

  stopifnot(
    is.numeric(mapsize) && (length(mapsize) == 2)
  )

  paste(mapsize, collapse = ",")
}

#' Convert numeric or character vector for orientation into dir query parameter
#' @noRd
get_dir_from_orientation <- function(orientation = NULL) {
  if (is.character(orientation)) {
    orientation <-
      match.arg(orientation, c("N", "E", "S", "W"))

    orientation <-
      switch(
        EXPR = orientation,
        "N" = 0,
        "E" = 90,
        "S" = 180,
        "W" = 270
      )
  }

  # FIXME: I'm unsure if orientation is required
  stopifnot(
    is.numeric(orientation)
  )

  if (orientation > 360) {
    orientation <-
      orientation - (360 * floor(orientation / 360))
  } else if (orientation < 0) {
    orientation <-
      orientation + (360 * ceiling(abs(orientation) / 360))
  }

  closest_cardinal_dir(orientation)
}

#' Get zoom query parameter
#' @noRd
get_zoom_for_imagery <- function(zoom = NULL, imagery = NULL) {
  if (is.null(zoom)) {
    zoom <- 18
  }

  if (imagery %in% imagery_options[5:6]) {
    reset_zoom <- NULL

    if (zoom < 18) {
      reset_zoom <- 18
    } else if (zoom > 22) {
      reset_zoom <- 22
    }

    if (!is.null(reset_zoom)) {
      zoom <- reset_zoom
      warning(
        paste0(
          imagery,
          " imagery only supports zoom levels between 18 and 22.",
          "\nSetting zoom to ", reset_zoom, "."
        )
      )
    }
  } else if (zoom < 0) {
    zoom <- 0
    warning("zoom must be between 0 and 22. Setting zoom to 0.")
  } else if (zoom > 22) {
    zoom <- 22
    warning("zoom must be between 0 and 22. Setting zoom to 22.")
  }

  round(as.numeric(zoom))
}

#' @rdname bing_static_map
#' @name get_request_url
#' @export
get_request_url <- function(...) {
  req_bingmapr(..., .perform = FALSE)$url
}

#' @return get_map_image returns an image from `magick::image_read`
#' @rdname bing_static_map
#' @name get_map_image
#' @export
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
  resp <-
    req_bingmapr(
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
      check = check,
      .perform = TRUE
    )

  magick::image_read(httr2::resp_body_raw(resp), strip = strip)
}


#' @return get_map_meta returns the JSON with the map metadata or a bbox for the
#'   map area
#' @rdname bing_static_map
#' @export
#' @importFrom httr2 resp_body_json
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
  resp <-
    req_bingmapr(
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
      mapMetadata = 1
    )

  meta <-
    httr2::resp_body_json(resp)

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
