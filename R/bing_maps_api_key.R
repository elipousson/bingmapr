#' Install a Bing Maps API Key in Your '.Renviron' File for Repeated Use
#'
#' This function will add your CENSUS API key to your .Renviron file so it can
#' be called securely without being stored in your code. After you have
#' installed your key, it can be called any time by typing
#' `Sys.getenv("BING_MAPS_API_KEY")`.
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
bing_maps_api_key <- function(key, overwrite = FALSE, install = FALSE) {
  if (install) {
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")
    if (file.exists(renv)) {
      file.copy(renv, file.path(home, ".Renviron_backup"))
    }
    if (!file.exists(renv)) {
      file.create(renv)
    } else {
      if (isTRUE(overwrite)) {
        message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
        oldenv <- utils::read.table(renv, stringsAsFactors = FALSE)
        newenv <- oldenv[-grep(
          "BING_MAPS_API_KEY",
          oldenv
        ), ]
        utils::write.table(newenv, renv,
          quote = FALSE, sep = "\n",
          col.names = FALSE, row.names = FALSE
        )
      } else {
        tv <- readLines(renv)
        if (any(grepl("BING_MAPS_API_KEY", tv))) {
          stop("A BING_MAPS_API_KEY already exists. You can overwrite it with the argument overwrite=TRUE",
            call. = FALSE
          )
        }
      }
    }
    keyconcat <- paste0("BING_MAPS_API_KEY='", key, "'")
    write(keyconcat, renv, sep = "\n", append = TRUE)
    message("Your API key has been stored in your .Renviron and can be accessed by Sys.getenv(\"BING_MAPS_API_KEY\"). \nTo use now, restart R or run `readRenviron(\"~/.Renviron\")`")
    return(key)
  } else {
    message("To install your API key for use in future sessions, run this function with `install = TRUE`.")
    Sys.setenv("BING_MAPS_API_KEY" = key)
  }
}
