
# bingmapr

<!-- badges: start -->
<!-- badges: end -->

bingmapr is an R packge for retrieving and plotting maps from the [Bing Static Maps API](). I created this package primarily to provide a convenient way to access Bing's Bird's Eye imagery and not all API parameters are currently supported.

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

# Get request URL centered on Washington Square Park in New York, NY
get_request_url(location = c(40.7308349, -73.99746074), orientation = "W")

# Plot a static map showing the center of Wake County, North Carolina
nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
plot_map_image(location = nc[nc$NAME == "Wake",], orientation = 180)
```

