
# bingmapr

<!-- badges: start -->
<!-- badges: end -->

bingmapr is an R packge for retrieving and plotting maps from the [Bing Static Maps API](https://docs.microsoft.com/en-us/bingmaps/rest-services/imagery/get-a-static-map). As of May 2021, I created and plan to use package mainly as a convenient way to access Bing's Bird's Eye (oblique-angle) imagery so the routes and traffic features of the API are all not currently supported.

This package is inspired by the [snapbox package](https://github.com/anthonynorth/snapbox/) that allows the use of the maps from the Mapbox static map API in ggplot maps. I hope to integrate bingmapr with ggplot in the future.

You can also explore the static map API in the browser using the [Bing Static Map Maker](https://staticmapmaker.com/bing/) made by [Katy DeCorah](https://katydecorah.com/).

## Installation

You can install this development version from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("elipousson/bingmapr")
```

## Example

``` r
library(bingmapr)

# Get a Bing Maps API Key https://www.bingmapsportal.com/
bing_maps_api_key(key = "put your key here", install = TRUE)

# Get request URL centered on Washington Square Park in New York, NY
get_request_url(location = c(40.7308349, -73.99746074), orientation = "W")

# Get an sf object to show how bingmapr can use sf objects for locations
nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))

# Plot a static map showing the center of Wake County, North Carolina
plot_map_image(location = nc[nc$NAME == "Wake",], orientation = 180)

# Plot static map centered 300 meters north and 500 meters west of the center
plot_map_image(location = nc[nc$NAME == "Wake",], orientation = 180, nudge = c(300, 500))
```

