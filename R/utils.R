#' @noRd
#' @importFrom sf st_centroid st_coordinates st_transform st_as_sfc st_union
sf_to_coords <- function(location) {
  if (inherits(location, "bbox")) {
    location <- sf::st_as_sfc(location)
  }

  location <- suppressWarnings(sf::st_centroid(sf::st_union(location)))
  location <- sf::st_coordinates(sf::st_transform(location, 4326))
  rev(c(location))
}

#' @noRd
nudge_location <- function(location, nudge = NULL) {
  nudge_lat <- nudge[1]
  nudge_lon <- nudge[2]

  lat <- location[1]
  lon <- location[2]

  # from https://stackoverflow.com/questions/7477003/calculating-new-longitude-latitude-from-old-n-meters
  # 6371000.0 is approximate radius of the earth in meters
  new_lat <- lat + (nudge_lat / 6371000.0) * (180 / pi)
  new_lon <- lon + (nudge_lon / 6371000.0) * (180 / pi) / cos(lat * pi / 180)

  c(new_lat, new_lon)
}


#' Get closest cardinal direction based on angle orientation
#' @noRd
closest_cardinal_dir <- function(orientation = NULL) {
 cardinal_dirs_between <- c(0, 45, 135, 225, 315)
 cardinal_dirs <- c(0, 90, 180, 270, 360)

  cardinal_dirs[
    findInterval(
      orientation,
      cardinal_dirs_between
    )
  ]
}
