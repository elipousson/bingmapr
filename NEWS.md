<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

# bingmapr 0.1.0.9000 (2022-10-18)

- add support for bbox and sfc locations to `get_request_url()`
- remove `plot_map_image()` by incorporating `magick::image_read()` into `get_map_image()`
- add `sf_to_coords()` and `nudge_location()` helper functions
- drop jpeg and graphics package dependencies
- `get_map_meta()` function now optionally returns a bbox from the metadata


