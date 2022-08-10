#' @noRd
#' @importFrom sf st_centroid st_coordinates st_transform
sf_to_coords <- function(location) {
  location <- suppressWarnings(sf::st_centroid(location))
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

  location <- c(new_lat, new_lon)
}
