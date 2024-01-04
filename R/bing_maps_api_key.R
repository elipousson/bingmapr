#' Install a Bing Maps API Key in Your '.Renviron' File for Repeated Use
#'
#' This function will add your CENSUS API key to your .Renviron file so it can
#' be called securely without being stored in your code. After you have
#' installed your key, it can be called any time with `get_bing_maps_api_key()`.
#'
#' @param key The API key provided to you from the Bing Maps Dev Center
#'   formatted in quotes. A key can be acquired at
#'   <https://www.bingmapsportal.com/>
#' @param overwrite If this is set to `TRUE`, it will overwrite an existing
#'   BING_MAPS_API_KEY that you already have in your .Renviron file.
#' @param install if `TRUE`, will install the key in your .Renviron file for use
#'   in future sessions. Defaults to `FALSE`
#' @rdname bing_maps_api_key
#' @export
#' @importFrom utils read.table write.table
bing_maps_api_key <- function(key,
                              overwrite = FALSE,
                              install = FALSE,
                              default = "BING_MAPS_API_KEY") {
  set_r_environ_token(
    token = key,
    overwrite = overwrite,
    install = install,
    default = default
  )
}

#' @rdname bing_maps_api_key
#' @name get_bing_maps_api_key
#' @export
get_bing_maps_api_key <- function(key = NULL,
                                  default = "BING_MAPS_API_KEY",
                                  call = caller_env()) {

  get_r_environ_token(
    token = key,
    default = default,
    message = "{.arg key} is empty and {.arg default} variable {.val {default}} can't be found in {.file .Renviron}",
    call = call
  )
}
