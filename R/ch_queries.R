#' Build a list of files to download
#'
#' This function constructs a set of download URLs for the requested
#' combinations of metadata variables. It currently supports download of
#' historic climatologies and future CMIP5 model predictions for both basic
#' monthly variables and bioclimatic variables. CHELSA provides historic data in
#' integer*10 format and floating point format, but only the former is available
#' using this function, for consistency with CMIP5 futures which are available
#' only in integer*10 format.
#'
#' @param variables character vector, options include "tmin", "tmax", "temp",
#'   "prec", and "bio".
#' @param layers integer vector, options include 1:12 for base variables
#'   (representing months) and 1:19 for bio (representing biovariable number).
#' @param models character vector, specify only for future data.
#' @param scenarios character vector, specify only for future data.
#' @param timeframes character vector, options include "1979-2013", "2014-2060",
#'   and "2061-2080".
#' @return a data frame of metadata for all factorial combinations of the
#'   requested variables
#' @importFrom dplyr select case_when
#' @importFrom magrittr %>%
#' @export
ch_queries <- function(variables, layers, models=NA, scenarios=NA, timeframes){
      base_url <- "https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/"
      expand.grid(model = models, scenario = scenarios, timeframe = timeframes,
                  variable = variables, layer = layers) %>%
            dplyr::mutate(variable2 = dplyr::case_when(variable == "tmin" ~ "tasmin",
                                         variable == "tmax" ~ "tasmax",
                                         variable ==
                                               "temp" ~ "tas",
                                         variable == "prec" ~ "pr",
                                         variable ==
                                               "bio" ~ "bio"),
                   addendum = case_when(variable == "prec" ~ "",
                                        TRUE ~ "_V1.2"),
                   histdir = case_when(variable == "bio" ~ "bioclim/integer/",
                                       variable == "prec" ~ "climatologies/prec/",
                                       TRUE ~ paste0("climatologies/temp/integer/", variable, "/")),
                   file = case_when(timeframe == "1979-2013" &  variable != "bio" ~
                                          paste0(histdir, "CHELSA_", variable, "10_", str_pad(layer, 2, "left", "0"), "_land.7z"),
                                    timeframe == "1979-2013" & variable == "bio" ~
                                          paste0(histdir, "CHELSA_", variable, "10_", str_pad(layer, 2, "left", "0"), ".tif"),
                                    TRUE ~ paste0("cmip5/", timeframe, "/", variable, "/CHELSA_",
                                                  variable2, "_mon_", model, "_", scenario, "_r*i1p1_g025.nc_",
                                                  layer, "_", timeframe, addendum, ".tif")),
                   url = paste0(base_url, file))
}
