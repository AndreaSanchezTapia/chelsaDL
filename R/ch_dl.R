#' Download CHELSA data
#'
#' Many standard palette generators use only a slice of color space, which can
#' cause a lack of differentiability in palettes used to visualize categorical
#' factors with many levels. This function attempts to overcome this by
#' generating colors using nearest-neighbor distance maximization in 3D RGB
#' space.
#'
#' @param md Variables to download (a data frame created by ch_queries).
#' @param dest Path to file folder where downloaded files should be stored
#'   (character).
#' @param skip_existing Should files that are already present in the destination
#'   be ignored (logical).
#' @param method Download method, passed to download.file (character).
#' @param crop Spatial bounding box to crop each downloaded raster to (an extent
#'   object, or any spatial object with an extent).
#' @importFrom utils download.file
#' @importFrom raster raster crop writeRaster
#' @importFrom fs path_wd
#'
#' @export
ch_dl <- function(md,
                  dest = NULL,
                  skip_existing = TRUE,
                  method = "curl",
                  crop = NULL) {
   if (is.null(dest))
      dest <- fs::path_wd()

   for(i in 1:nrow(md)) {
      message(paste("File", i, "of", nrow(md), "..."))
      md$status[i] <- "incomplete"
      md$path[i] <- paste0(dest, "/", basename(md$file[i]))

      runs <- c("1", "2", "12")

      if (skip_existing) {
         # previously-failed downloads have small file size
         paths <- sapply(runs, function(x) sub("\\*", x, md$path[i]))
         size <- file.size(paths)
         if (any(!is.na(size) & log(size) > 10)) {
            md$path[i] <- paths[!is.na(size) & log(size) > 10]
            md$status[i] <- "already done"
            next()
         }
      }

      # run numbers vary by model. try all options.
      for (run in runs) {
         url <- sub("\\*", run, md$url[i])
         path <- sub("\\*", run, md$path[i])
         r <- try(download.file(url, path, method = method, quiet = T))
         size <- file.size(path)
         if (!is.na(size) & log(size) > 10) {
            md$url[i] <- url
            md$path[i] <- path
            break()
         }
         file.remove(path)
      }

      if (class(r) == "try-error") {
         md$status[i] <- as.character(r)
         next()
      }
      if (file.exists(md$path[i])) md$status[i] <- "download completed"
      if (!is.null(crop) & file.exists(md$path[i])) {
         r <- raster(md$path[i]) %>%
            crop(crop) %>%
            writeRaster(md$path[i], overwrite = T)
         md$status[i] <- "raster cropped"
      }
   }
   return(md)
}
