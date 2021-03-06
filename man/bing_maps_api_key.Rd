% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bing_maps_api_key.R
\name{bing_maps_api_key}
\alias{bing_maps_api_key}
\title{Install a Bing Maps API Key in Your '.Renviron' File for Repeated Use}
\usage{
bing_maps_api_key(key, overwrite = FALSE, install = FALSE)
}
\arguments{
\item{key}{The API key provided to you from the Bing Maps Dev Center formatted in quotes. A key can be acquired at https://www.bingmapsportal.com/}

\item{overwrite}{If this is set to TRUE, it will overwrite an existing BING_MAPS_API_KEY that you already have in your .Renviron file.}

\item{install}{if TRUE, will install the key in your .Renviron file for use in future sessions. Defaults to FALSE}
}
\description{
This function will add your CENSUS API key to your .Renviron file so it can
be called securely without being stored in your code. After you have
installed your key, it can be called any time by typing
Sys.getenv("BING_MAPS_API_KEY").
}
