#' Spatial Data
#' @export readDT_Sp
#' @param dir dir with .nc files
#'
#' @param ext coordinate system for the area
#' @return A dataset with precip, lon, lat
#'
#' @examples
readDT_Sp <- function(dir, ext){
  nc <- nc_open(dir)
  #pr for precipitation, lon fot Lontitude AND lat fot Latitude
  e <- try(
    expr = {

      pr <- ncvar_get(nc = nc,
                      varid = "pr")

    }
    , silent = TRUE
  )

  if (inherits(x = e,
               what = "try-error")) {

    e <- try(
      expr = {

        pr <- ncvar_get(nc = nc,
                        varid = "precipitation_flux")

      }
      , silent = TRUE
    )
  }

  e <- try(
    expr = {

      lon <- ncvar_get(nc = nc,
                       varid = "lon")
      lat <- ncvar_get(nc = nc,
                       varid = "lat")

    }
    , silent = TRUE
  )

  if (inherits(x = e,
               what = "try-error")) {

    e <- try(
      expr = {

        lon <- ncvar_get(nc = nc,
                         varid = "longtitude")
        lat <- ncvar_get(nc = nc,
                         varid = "latitude")

      }
      , silent = TRUE
    )
  }
  #Creates a data table from raster
  rast <- brick(p)
  extent(rast) <- c(range(lon), range(lat))
  aux <- as.data.table(t(extract(rast, ext)))
  names(aux) <- as.character(cellsFromExtent(rast, ext))
  return(aux)
}

