#' Historical DDF calculation
#'
#' @param ddf_data A dataset with historical RCP
#'
#' @return Creates a data table with all variables
#' @export ddf_calculator
#'
#' @examples
ddf_calculator <- function(ddf_data){
  #Calculating Historical climate model
  hddf <- ddf_data[RCP == 'historical', .(Duration, hdepth = depth, freq, hour, RCP, GCM, RCM, Run)]
  ddf_data <- ddf_data[RCP != 'historical']
  #Calculating Delta
  cddf <- hddf[ddf_data, on = c('Duration', 'freq', 'hour', 'GCM', 'RCM', 'Run')]
  cddf[, delta := depth / hdepth]
  return(cddf)
}
