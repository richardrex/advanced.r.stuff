#' Reading dates and focing from filename
#'
#' @param dir dir with raw files
#'
#' @return a database with Date(POSIX) and forcing from filenames
#' @export create_dates_forcing
#'
#' @examples
create_dates_forcing <- function(dir){
  Dates <- strsplit(substr(dir, start = nchar(dir) - 27, stop = nchar(dir) - 3), '-')
  Date_ID <- data.table(date = seq(as.POSIXct(dates[[1]][1], format = "%Y%m%d%H%M"),
                                         as.POSIXct(dates[[1]][2], format = "%Y%m%d%H%M"), by = 'hour'),
                              forcing = sapply(strsplit(substr(fls[i], start = 1, stop = n - 29), split = "-")))}



