#' Spatial data lecture
#'
#' @param dir A directory of data
#' @param aux data with precipitation value and id
#' @param create_dates_forcing data with forcing and dates
#'
#' @return a dataset from spatial data course
#' @export spatial_function
#'
#' @examples
spatial_function <- function(dir,aux, dates_forcing){
  nc = nc_open(fls[i])
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

  raster <- brick(p)
  extent(raster) <- c(range(lon), range(lat))

  dt <- data.table(dates_forcing, aux)

  dt_m <- melt(dt, id.vars = c("date",'forcing'), variable.name = "id", variable.factor = FALSE)

  dt_m <- dt_m[, c("lon", "lat") := as.data.table(xyFromCell(raster, as.numeric(as.character(id))))]

  setnames(dt_m, 'value', 'precipitation')

  return(dt_m)
}
